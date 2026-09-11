#!/usr/bin/env python3
"""Error rate against noise, both ways, every mode.

Each call: modec answers, the reference dials in pinned to one mode with
error control and compression off, a clean handshake, then --line-snr
puts the stated noise on both directions and a payload of indexed lines
goes each way.  What each end received is scored line by line against
what was sent: a line is found by its index, compared character for
character, and counted lost if its index never arrives.  The
reference's own line-quality readings (AT%Q, AT%L) are taken after the
call where the modem answers them.

    scripts/bench/ber.py                 # the whole campaign
    scripts/bench/ber.py v22bis v32-4800 # some modes
    scripts/bench/ber.py v22bis:12,9     # a mode at these SNRs

Results append to recordings/bench/ber.csv, one row per call and
direction; the captures and logs sit beside it under ber-<mode>-<snr>.
"""
import os, re, sys, csv, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sweep
from sweep import one, SC

# A receiver that has just stopped transmitting takes a moment to
# re-acquire, and the payload's first lines pay for it: at 4 dB, on a
# link whose textbook error rate is 3e-8, modec lost exactly lines 0 and
# 1 behind fifty bytes of garbage.  So each direction is given a
# preamble to lock onto that is not scored.  Both directions, not just
# the one that showed it: giving one receiver a run-up and not the other
# would bias the comparison they exist to make.  It carries no index, so
# `score` ignores it wherever it lands.
WARMUP = b'#' * 72 + b'\r\n'

def payload(prefix, nlines):
    # printable, varied, no '+' (the escape) and no line-ending bytes
    alphabet = ''.join(c for c in (chr(k) for k in range(33, 127)) if c != '+')
    out = []
    for i in range(nlines):
        body = ''.join(alphabet[(i * 7 + j * 11 + (j * j) // 3) % len(alphabet)] for j in range(54))
        out.append(f'{prefix}{i:04d} {body}\r\n'.encode())
    return b''.join(out)

def levenshtein(a, b):
    prev = list(range(len(b) + 1))
    for i, ca in enumerate(a, 1):
        cur = [i]
        for j, cb in enumerate(b, 1):
            cur.append(min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + (ca != cb)))
        prev = cur
    return prev[-1]

def score(sent, got, prefix):
    """Per-line comparison.  Returns a dict of counts."""
    want = {}
    for ln in sent.split(b'\r\n'):
        if ln: want[int(ln[1:5])] = ln
    found = {}
    for m in re.finditer(re.escape(prefix.encode()) + rb'(\d{4}) ([^\r\n]{0,70})', got):
        idx = int(m.group(1))
        if idx in want and idx not in found:
            found[idx] = m.group(0)
    chars = errs = intact = 0
    for idx, ln in found.items():
        w = want[idx]
        chars += len(w)
        if ln == w:
            intact += 1
        elif len(ln) == len(w):
            errs += sum(1 for x, y in zip(ln, w) if x != y)
        else:
            errs += levenshtein(ln[:len(w) + 8], w)
    lost = len(want) - len(found)
    # every character carries ten bits on the line; a wrong character is
    # at least one wrong bit, so this is a floor on the bit error rate
    ber_floor = errs / (10.0 * chars) if chars else float('nan')
    return dict(lines=len(want), found=len(found), intact=intact, lost=lost,
                chars=chars, char_errs=errs,
                cer=(errs / chars if chars else float('nan')),
                ber_floor=ber_floor, bytes=len(got))

def modec_evm(tag):
    """Mean decision error the receiver reported while the payload ran."""
    try:
        txt = open(f'{SC}/sweep-{tag}.log', 'rb').read().decode('latin1')
    except FileNotFoundError:
        return float('nan')
    vals = [abs(float(v)) for v in re.findall(r'decision error (-?[0-9.]+)', txt)]
    # drop the first few (handshake settling) and take the median of the rest
    vals = vals[3:] if len(vals) > 6 else vals
    if not vals: return float('nan')
    vals.sort(); return vals[len(vals) // 2]

# Roughly when CONNECT lands, measured from the call coming up, per
# mode: the FSK and DPSK handshakes are done in about five seconds and
# V.32's start-up takes eleven.  The noise steps in two seconds after
# that and the payload goes three seconds later still, so every byte of
# every payload crosses a line that is already at the stated ratio.
#
# Fixed at thirteen seconds it was not: on the low-rate modes the
# payload began at nine seconds and the first quarter of it went over a
# clean line, which leaves the character error rate about right -- a
# waterfall that steep moves a tenth of a decibel -- and the count of
# intact lines badly wrong, because the intact ones were the ones sent
# before the noise arrived.
CONNECT_AT = {'bell103': 5, 'v21': 5, 'bell212a': 5, 'v22': 5, 'v22bis': 5,
              'v32-4800': 11, 'v32-9600': 11, 'v32b-9600t': 11, 'v32b-7200': 11,
              'v32b-12000': 11, 'v32b-14400': 11}

# mode: (modec --mode, AT+MS, modec extra, bit rate, SNRs to try)
MODES = {
  'bell103':    ('bell103',  'AT+MS=B103,0,300,300',      [],                      300,  [40, 8, 4, 1, -2, -5]),
  'v21':        ('v21',      'AT+MS=V21,0,300,300',       [],                      300,  [40, 8, 4, 1, -2, -5]),
  'bell212a':   ('bell212a', 'AT+MS=B212,0,1200,1200',    [],                      1200, [40, 14, 11, 8, 5, 2]),
  'v22':        ('v22',      'AT+MS=V22,0,1200,1200',     [],                      1200, [40, 14, 11, 8, 5, 2]),
  'v22bis':     ('v22bis',   'AT+MS=V22B,0,2400,2400',    [],                      2400, [40, 20, 16, 13, 10, 7]),
  'v32-4800':   ('v32',      'AT+MS=V32,0,4800,4800',     ['--v32-rate','4800'],   4800, [40, 14, 11, 8, 5]),
  'v32-9600':   ('v32',      'AT+MS=V32,0,9600,9600',     ['--v32-rate','9600'],   9600, [40, 24, 20, 17, 14]),
  'v32b-9600t': ('v32bis',   'AT+MS=V32B,0,9600,9600',    ['--v32-rate','9600t'],  9600, [40, 20, 17, 14, 11]),
  'v32b-7200':  ('v32bis',   'AT+MS=V32B,0,7200,7200',    ['--v32-rate','7200'],   7200, [40, 16, 13, 10, 7]),
  'v32b-12000': ('v32bis',   'AT+MS=V32B,0,12000,12000',  ['--v32-rate','12000'],  12000,[40, 26, 22, 19, 16]),
  'v32b-14400': ('v32bis',   'AT+MS=V32B,0,14400,14400',  ['--v32-rate','14400'],  14400,[40, 30, 26, 22]),
}
ORDER = ['bell103','v21','bell212a','v22','v22bis','v32-4800','v32-9600','v32b-9600t','v32b-7200','v32b-12000','v32b-14400']

def run(mode, snr):
    m, ms, extra, rate, _ = MODES[mode]
    # about twenty seconds of data each way, capped for the fast rates
    # about 1500 characters each way, which resolves a character error
    # rate down to roughly 7e-4 and keeps a call near a minute -- half
    # that at 300 bit/s, where 1500 characters is fifty seconds in each
    # direction and the interesting region is coarse anyway
    nlines = 12 if rate <= 300 else 24
    nbytes = nlines * 62
    pa = payload('A', nlines); pb = payload('B', nlines)
    tag = f'ber-{mode}-{snr}'
    # clean through the handshake, then the stated ratio, then the
    # payload: see CONNECT_AT
    step = CONNECT_AT[mode] + 2
    sched = f'40@0,{snr}@{step}'
    ex = extra + ['--line-snr', sched, '--line-every', '25',
                  # deliver bits however wrong: this measures errors, not
                  # the gate that would otherwise withhold them
                  '--max-evm-v32', '3']
    secs = (nbytes + len(WARMUP)) * 10 / rate
    window = int(2 * (secs + 5))
    r = one(tag, m, ms, ex, ['AT%E0'], window=window, pay_a=WARMUP + pa, pay_b=WARMUP + pb,
            settle=step - CONNECT_AT[mode] + 3, post=['AT#UD'], taketurns=True)
    row = {'mode': mode, 'snr': snr, 'rate': rate, 'ref': r.get('ref'),
           'ref_connect': r.get('ref_connect',''), 'modec_connect': r.get('modec_connect')}
    if r.get('ref') == 'CONNECT':
        got_ref = open(f'{SC}/sweep-{tag}-at-ref.bin','rb').read()
        got_modec = open(f'{SC}/sweep-{tag}-at-modec.bin','rb').read()
        a = score(pa, got_ref, 'A')      # modec -> reference: the reference's receiver
        b = score(pb, got_modec, 'B')    # reference -> modec: modec's receiver
        row.update({f'ref_{k}': v for k, v in a.items()})
        row.update({f'modec_{k}': v for k, v in b.items()})
        row['modec_evm'] = modec_evm(tag)
        row['post'] = ' '.join(f'{k}={v}' for k, v in r.get('post', {}).items())
    return row

if __name__ == '__main__':
    want = sys.argv[1:] or ORDER
    out = f'{SC}/ber.csv'
    new = not os.path.exists(out)
    fields = ['mode','snr','rate','ref','ref_connect','modec_connect',
              'ref_lines','ref_found','ref_intact','ref_lost','ref_chars','ref_char_errs','ref_cer','ref_ber_floor','ref_bytes',
              'modec_lines','modec_found','modec_intact','modec_lost','modec_chars','modec_char_errs','modec_cer','modec_ber_floor','modec_bytes',
              'modec_evm','post']
    with open(out, 'a', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, extrasaction='ignore')
        if new: w.writeheader()
        for spec in want:
            mode, _, snrs = spec.partition(':')
            snrs = [float(x) if '.' in x else int(x) for x in snrs.split(',')] if snrs else MODES[mode][4]
            for snr in snrs:
                print(f'>>> {mode} @ {snr} dB', flush=True)
                row = run(mode, snr)
                w.writerow(row); f.flush()
                keep = {k: row.get(k) for k in ['ref','ref_connect','ref_intact','ref_lines','ref_cer','modec_intact','modec_lines','modec_cer','modec_evm','post']}
                print('   ', keep, flush=True)
