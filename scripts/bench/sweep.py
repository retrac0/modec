#!/usr/bin/env python3
"""The bench sweep: one call per mode between modec and the reference modem.

modec answers over SIP, driven through baresip's ctrl_tcp so the modem
starts when the call does (see docs/reference-modem.md, "0a").  The
reference -- a Conexant on /dev/ttyACM0 behind the ATA's FXS 1 -- dials
in pinned by AT+MS with error control and compression off, a known
payload goes each way, and both are compared byte for byte.

    scripts/bench/sweep.py                 # every mode in MODES
    scripts/bench/sweep.py v22bis v32b-9600
    OUT=recordings/bench PORT=/dev/modem-ref scripts/bench/sweep.py ...

Needs: baresip with a config dir at ~/.baresip-bench (ctrl_tcp on
127.0.0.1:4444, an auto-answering account for each number dialled),
and the ATA pointed at this host as its SIP server.
"""
import os, subprocess, sys, time, fcntl
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from atlib import AT

SC = os.environ.get('OUT', 'recordings/bench')
os.makedirs(SC, exist_ok=True)
PORT = os.environ.get('PORT', '/dev/ttyACM0')
BIN = subprocess.run(['cabal','list-bin','modec'], capture_output=True, text=True).stdout.strip()
PAY_A = b''.join(('A%04d the quick brown fox jumps over the lazy dog 0123456789\r\n' % i).encode() for i in range(8))
PAY_B = b''.join(('B%04d pack my box with five dozen liquor jugs 9876543210\r\n' % i).encode() for i in range(8))

def nonblock(f):
    fcntl.fcntl(f, fcntl.F_SETFL, fcntl.fcntl(f, fcntl.F_GETFL) | os.O_NONBLOCK)

def read_for(f, secs, until=None):
    buf=b''; t0=time.time()
    while time.time()-t0 < secs:
        try:
            d=f.read()
            if d: buf+=d
        except (BlockingIOError, TypeError): pass
        if until and until in buf: break
        time.sleep(0.02)
    return buf

def one(tag, mode, ms, extra=None, ref_extra=None, window=16, slow=False):
    extra = extra or []; ref_extra = ref_extra or []
    rx = f'{SC}/sweep-{tag}-rx.wav'; tx = f'{SC}/sweep-{tag}-tx.wav'
    log = open(f'{SC}/sweep-{tag}.log','wb')
    bl  = open(f'{SC}/sweep-{tag}-baresip.log','wb')
    bare = subprocess.Popen(['baresip','-f',os.path.expanduser('~/.baresip-bench')],
                            stdout=bl, stderr=subprocess.STDOUT)
    time.sleep(3)
    # --ans-plain always: through the ATA the V.25 reversals on the answer
    # tone stand its echo canceller down (docs/reference-modem.md); it
    # touches nothing but the V.32 answer tone
    args = [BIN,'modem','--answer','--sip','127.0.0.1:4444','--audio-sip-loop','modec',
            '--mode',mode,'--hayes','--data-stdio','--ans-plain','--record-rx',rx,'--record-tx',tx]+extra
    mo = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log)
    nonblock(mo.stdout)
    time.sleep(3)
    result = {'tag':tag,'mode':mode,'ms':ms}
    m = AT(PORT)
    try:
        # inside the try: if modec has already died (an option it does not
        # know, say) this raises, and the far end and baresip still get
        # cleaned up rather than left off hook
        mo.stdin.write(b'ATS0=1\r'); mo.stdin.flush()
        read_for(mo.stdout, 1.5)
        for c in ['AT&F','AT&K0','AT%C0','AT\\N0','ATX4','ATS7=40', ms] + ref_extra:
            m.cmd(c,3,quiet=True)
        r = m.cmd('ATDT2001',45,stop=[b'CONNECT',b'NO CARRIER',b'BUSY',b'NO ANSWER',b'ERROR'],quiet=True)
        rtxt = r.decode('latin1').replace('\r',' ').strip()
        result['ref'] = 'CONNECT' if b'CONNECT' in r else rtxt.split()[-1] if rtxt else '?'
        if b'CONNECT' in r:
            out = read_for(mo.stdout, 12, until=b'CONNECT')
            result['modec_connect'] = b'CONNECT' in out
            time.sleep(1.0)
            if slow:
                # one line at a time with idle between them, so a
                # start-stop framer that lost alignment on a bad bit has
                # marks to find the next start bit against
                for ln in PAY_B.split(b'\r\n'):
                    if ln: os.write(m.fd, ln + b'\r\n'); time.sleep(0.4)
            else:
                os.write(m.fd, PAY_B)                  # reference -> modec
            mo.stdin.write(PAY_A); mo.stdin.flush()    # modec -> reference
            got_ref = b''; t0=time.time()
            while time.time()-t0 < window:
                try: got_ref += os.read(m.fd, 65536)
                except BlockingIOError: time.sleep(0.02)
            got_modec = read_for(mo.stdout, 1.0)
            open(f'{SC}/sweep-{tag}-at-ref.bin','wb').write(got_ref)
            open(f'{SC}/sweep-{tag}-at-modec.bin','wb').write(got_modec)
            result['ref_got'] = got_ref.count(PAY_A[:40]) and PAY_A in got_ref
            result['modec_got'] = PAY_B in got_modec
            result['ref_bytes'] = len(got_ref); result['modec_bytes'] = len(got_modec)
            time.sleep(0.3); os.write(m.fd, b'+++'); time.sleep(1.5)
        m.cmd('ATH',4,quiet=True)
    finally:
        m.close()
        try: mo.stdin.write(b'\r'); mo.stdin.flush()
        except Exception: pass
        mo.terminate(); bare.terminate()
        try: mo.wait(timeout=5)
        except Exception: mo.kill()
        try: bare.wait(timeout=5)
        except Exception: bare.kill()
        log.close(); bl.close()
        time.sleep(2)
    return result

MODES = [
    # Adverse conditions: noise on the line, scheduled from the call coming
    # up, so a call that trained at its best rate has to hold it or step
    # down.  The reference is in automode (the 1 in AT+MS) so it may
    # renegotiate rather than only retrain.
    # No --v32-rate: pinning offers exactly one rate, so a retrain that
    # narrows the offer finds nothing below it and the call clears
    # instead of stepping.  --mode v32bis offers the whole ladder.
    # The schedule walks down through the survey's thresholds: 14400
    # wants 22 dB, 12000 20, 9600 trellis 16.
    ('v32b-stepdown', 'v32bis', 'AT+MS=V32B,1,4800,14400',
     ['--line-snr','40@0,21@15,18@27,15@39','--line-every','25'], [], 48),
    ('v32b-stepdown-rx', 'v32bis', 'AT+MS=V32B,1,4800,14400',
     ['--line-snr','40@0,21@15,18@27,15@39','--line-snr-dir','rx','--line-every','25'], [], 48),
    # Degraded past what any rate can hold: does the link retrain and
    # come back, or drop cleanly, or hang?
    ('v32b-collapse', 'v32bis', 'AT+MS=V32B,1,4800,14400',
     ['--line-snr','40@0,10@20','--line-every','25'], [], 40),
    # The whole ladder, with the 14400 defect kept out of it: the far end
    # offers no more than 12000, which modec can hold, so every step that
    # follows is the noise and nothing else.  Each retrain costs 9-11 s,
    # so the window has to be long enough for four of them.
    ('v32b-ladder', 'v32bis', 'AT+MS=V32B,1,4800,12000,4800,12000',
     ['--line-snr','40@0,19@16,15@32,12@48','--line-every','25'], [], 85),
    # Is 7200 reachable at all?  Four step-downs in a row went 12000,
    # 9600, 4800 and never 7200, and modec prefers 7200 over 4800
    # (allV32Rates), so the question is whether the far end ever offers
    # it.  Cap the reference at 7200 and see what the pair lands on,
    # then take the line below it.
    #
    # +MS takes six fields: modulation, automode, min and max transmit,
    # min and max receive.  Four of them caps the transmit direction
    # only, which is why the run capped at 12000 connected at 14400.
    # Decisive for the skip: the far end can do 9600, 7200 and 4800 and
    # nothing else.  A step to 7200 means the ladder uses it; a step
    # straight to 4800 means something rules 7200 out, and the only
    # thing that can is bestCommonRate's V.32bis guard.
    ('v32b-9600-cap', 'v32bis', 'AT+MS=V32B,1,4800,9600,4800,9600',
     ['--line-snr','40@0,13@20','--line-every','25'], [], 45),
    ('v32b-7200-cap', 'v32bis', 'AT+MS=V32B,1,4800,7200,4800,7200',
     ['--line-snr','40@0,12@20','--line-every','25'], [], 45),
    # payload one line at a time: bits mostly right shows as lines mostly intact
    ('v32b-14400-slow', 'v32bis', 'AT+MS=V32B,0,14400,14400', ['--v32-rate','14400','--max-evm-v32','0.7'], [], 16, True),
    ('v32b-12000-slow', 'v32bis', 'AT+MS=V32B,0,12000,12000', ['--v32-rate','12000'], [], 16, True),
    ('v32b-14400-g07', 'v32bis', 'AT+MS=V32B,0,14400,14400', ['--v32-rate','14400','--max-evm-v32','0.7','--line-every','12'], [], 16),
    ('v32b-14400-g08', 'v32bis', 'AT+MS=V32B,0,14400,14400', ['--v32-rate','14400','--max-evm-v32','0.8','--line-every','12'], [], 16),
    ('v32b-12000-g07', 'v32bis', 'AT+MS=V32B,0,12000,12000', ['--v32-rate','12000','--max-evm-v32','0.7','--line-every','12'], [], 16),
    # line reports every quarter second: the decision error in the second after CONNECT
    ('v32b-14400-fast', 'v32bis', 'AT+MS=V32B,0,14400,14400', ['--v32-rate','14400','--line-every','12'], [], 16),
    ('v32b-12000-fast', 'v32bis', 'AT+MS=V32B,0,12000,12000', ['--v32-rate','12000','--line-every','12'], [], 16),
    # trellis or constellation?  9600 non-trellis passed; every trellis rate failed
    ('v32b-9600t', 'v32bis', 'AT+MS=V32B,0,9600,9600', ['--v32-rate','9600t'], [], 16),
    ('v32b-4800',  'v32bis', 'AT+MS=V32B,0,4800,4800', ['--v32-rate','4800'],  [], 16),
    ('bell103-long', 'bell103', 'AT+MS=B103,0,300,300', [], [], 24),
    ('v21-long',     'v21',     'AT+MS=V21,0,300,300',  [], [], 24),
    # T2.3: reference in automode, so it will use V.8; modec answers with V.8
    ('v8-auto',      'v32bis',  'AT+MS=V32B,1,4800,14400', ['--v8'], [], 16),
    # T2.3b: reference in automode, modec WITHOUT V.8 -- does the pair still land?
    ('auto-nov8',    'v32bis',  'AT+MS=V32B,1,4800,14400', [], [], 16),
    # T3.4: reference speaks LAPM (\N3), modec offers MNP: must settle, not hang
    ('lapm-v22bis',  'v22bis',  'AT+MS=V22B,0,2400,2400', ['--mnp'], ['AT\\N3'], 16),
    # T3.5: reference sends the 1800 Hz guard tone; modec must tolerate it on receive
    ('guard-v22bis', 'v22bis',  'AT+MS=V22B,0,2400,2400', [], ['AT&G2'], 16),
    ('bell103',  'bell103',  'AT+MS=B103,0,300,300'),
    ('v21',      'v21',      'AT+MS=V21,0,300,300'),
    ('bell212a', 'bell212a', 'AT+MS=B212,0,1200,1200'),
    ('v22',      'v22',      'AT+MS=V22,0,1200,1200'),
    ('v22bis',   'v22bis',   'AT+MS=V22B,0,2400,2400'),
    ('v32-4800', 'v32',      'AT+MS=V32,0,4800,4800',  ['--v32-rate','4800']),
    ('v32-9600', 'v32',      'AT+MS=V32,0,9600,9600',  ['--v32-rate','9600']),
    ('v32b-7200','v32bis',   'AT+MS=V32B,0,7200,7200', ['--v32-rate','7200']),
    ('v32b-9600','v32bis',   'AT+MS=V32B,0,9600,9600', ['--v32-rate','9600']),
    ('v32b-12000','v32bis',  'AT+MS=V32B,0,12000,12000',['--v32-rate','12000']),
    ('v32b-14400','v32bis',  'AT+MS=V32B,0,14400,14400',['--v32-rate','14400']),
]
only = sys.argv[1:] or None
rows=[]
for spec in MODES:
    tag = spec[0]
    if only and tag not in only: continue
    print(f'>>> {tag} ({spec[2]})', flush=True)
    r = one(*spec)
    rows.append(r); print('   ', r, flush=True)
print('\n=== SUMMARY ===')
for r in rows: print(r)
