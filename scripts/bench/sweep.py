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

def one(tag, mode, ms, extra=None, ref_extra=None, window=16):
    extra = extra or []; ref_extra = ref_extra or []
    rx = f'{SC}/sweep-{tag}-rx.wav'; tx = f'{SC}/sweep-{tag}-tx.wav'
    log = open(f'{SC}/sweep-{tag}.log','wb')
    bl  = open(f'{SC}/sweep-{tag}-baresip.log','wb')
    bare = subprocess.Popen(['baresip','-f',os.path.expanduser('~/.baresip-bench')],
                            stdout=bl, stderr=subprocess.STDOUT)
    time.sleep(3)
    args = [BIN,'modem','--answer','--sip','127.0.0.1:4444','--audio-sip-loop','modec',
            '--mode',mode,'--hayes','--data-stdio','--record-rx',rx,'--record-tx',tx]+extra
    mo = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log)
    nonblock(mo.stdout)
    time.sleep(3)
    mo.stdin.write(b'ATS0=1\r'); mo.stdin.flush()
    read_for(mo.stdout, 1.5)

    result = {'tag':tag,'mode':mode,'ms':ms}
    m = AT(PORT)
    try:
        for c in ['AT&F','AT&K0','AT%C0','AT\\N0','ATX4','ATS7=40', ms] + ref_extra:
            m.cmd(c,3,quiet=True)
        r = m.cmd('ATDT2001',45,stop=[b'CONNECT',b'NO CARRIER',b'BUSY',b'NO ANSWER',b'ERROR'],quiet=True)
        rtxt = r.decode('latin1').replace('\r',' ').strip()
        result['ref'] = 'CONNECT' if b'CONNECT' in r else rtxt.split()[-1] if rtxt else '?'
        if b'CONNECT' in r:
            out = read_for(mo.stdout, 12, until=b'CONNECT')
            result['modec_connect'] = b'CONNECT' in out
            time.sleep(1.0)
            os.write(m.fd, PAY_B)                      # reference -> modec
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
