#!/usr/bin/env python3
"""Error rate under every other impairment the simulator has, live.

The line is clean of added noise; one impairment at a time is put on
both directions with `--impair`, run a block at a time by
Modec.Channel.Live, and the payload scored each way exactly as ber.py
does.  Where the offline survey (modec-bench v32-survey) has an axis
at the same value, the live number and the offline one can be laid
side by side.

    scripts/bench/distort.py                    # the whole matrix
    scripts/bench/distort.py v22bis v32b-12000  # some modes
    scripts/bench/distort.py v32b-9600t:jit-s3,clock1
"""
import os, sys, csv
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from sweep import one, SC
from ber import MODES, WARMUP, payload, score, modec_evm

# name -> --impair settings.  Comments give the survey's matching axis.
CONDS = [
  ('clean',     []),
  ('jit-s1',    ['sinejit=1']),        # survey: sine jitter a=1 f=2
  ('jit-s3',    ['sinejit=3']),        # survey: a=3 f=2
  ('jit-walk',  ['jitter=0.5']),       # random walk, step 0.5, bounded 2 samples
  ('slips2',    ['slips=2']),          # survey: slips every 0.5 s of 2 (here every 1 s)
  ('wow',       ['wow=0.3']),          # 0.3 % at 1 Hz
  ('flutter',   ['flutter=0.1']),      # 0.1 % at 25 Hz
  ('clock1',    ['rate=0.01']),        # survey: clock 1.0e-2
  ('carrier15', ['freq=15']),          # survey: carrier 15 Hz
  ('delay2',    ['delaydist=2']),      # survey: delay dist 2 ms
  ('wobble',    ['wobble=2']),         # 2 Hz peak deviation at 4 Hz
  ('phasejit',  ['phasejit=10']),      # 10 degrees peak at 60 Hz
  ('softclip',  ['softclip=3']),
  ('harm2',     ['harm2=0.1']),
  ('hum',       ['hum=0.03']),
  ('impulse',   ['impulse=5']),        # 5 clicks a second, 0.3 peak, ringing at 1400 Hz
  ('hits',      ['hits=2']),           # 2 gain hits a second, 6 ms, -6 dB
  ('dropout',   ['dropout=0.02']),     # 2 % of 20 ms blocks zeroed
  ('biterr',    ['ulaw=1','biterr=0.001']),  # G.711 with one code bit in a thousand flipped
  ('loss',      ['loss=0.02']),        # RTP frame loss, bursts of two on average, repeated frame
  ('echo',      ['echo=0.2']),         # a 20 ms reflection at -14 dB
]
FSK_CONDS = ['clean','jit-s3','slips2','wow','carrier15','impulse','loss','hum']
DEFAULT_MODES = ['v22bis', 'v32-9600', 'v32b-9600t', 'v32b-12000', 'v21']

def run(mode, cond, kvs):
    m, ms, extra, rate, _ = MODES[mode]
    nlines = 24; nbytes = nlines * 62
    pa = payload('A', nlines); pb = payload('B', nlines)
    tag = f'dist-{mode}-{cond}'
    ex = extra + ['--line-every', '25', '--max-evm-v32', '3']
    for kv in kvs: ex += ['--impair', kv]
    window = int(2 * ((nbytes + len(WARMUP)) * 10 / rate + 5))
    r = one(tag, m, ms, ex, ['AT%E0'], window=window, pay_a=WARMUP + pa, pay_b=WARMUP + pb,
            settle=3.0, taketurns=True)
    row = {'mode': mode, 'cond': cond, 'impair': ' '.join(kvs), 'rate': rate, 'ref': r.get('ref'),
           'ref_connect': r.get('ref_connect', ''), 'modec_connect': r.get('modec_connect')}
    if r.get('ref') == 'CONNECT':
        a = score(pa, open(f'{SC}/sweep-{tag}-at-ref.bin', 'rb').read(), 'A')
        b = score(pb, open(f'{SC}/sweep-{tag}-at-modec.bin', 'rb').read(), 'B')
        row.update({f'ref_{k}': v for k, v in a.items()})
        row.update({f'modec_{k}': v for k, v in b.items()})
        row['modec_evm'] = modec_evm(tag)
    return row

if __name__ == '__main__':
    want = sys.argv[1:] or DEFAULT_MODES
    out = f'{SC}/distort.csv'; new = not os.path.exists(out)
    fields = ['mode','cond','impair','rate','ref','ref_connect','modec_connect',
              'ref_lines','ref_found','ref_intact','ref_lost','ref_chars','ref_char_errs','ref_cer','ref_ber_floor','ref_bytes',
              'modec_lines','modec_found','modec_intact','modec_lost','modec_chars','modec_char_errs','modec_cer','modec_ber_floor','modec_bytes','modec_evm']
    with open(out, 'a', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, extrasaction='ignore')
        if new: w.writeheader()
        for spec in want:
            mode, _, conds = spec.partition(':')
            names = conds.split(',') if conds else ([c for c, _ in CONDS] if mode not in ('v21','bell103') else FSK_CONDS)
            for cond in names:
                kvs = dict(CONDS)[cond]
                print(f'>>> {mode} / {cond} ({" ".join(kvs) or "nothing"})', flush=True)
                row = run(mode, cond, kvs); w.writerow(row); f.flush()
                print('   ', {k: row.get(k) for k in ['ref','ref_connect','ref_intact','ref_lines','ref_cer','modec_intact','modec_lines','modec_cer','modec_evm']}, flush=True)
