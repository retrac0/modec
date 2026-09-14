#!/usr/bin/env python3
"""The bench sweep: one call per mode between modec and the reference modem.

modec answers over SIP, driven through baresip's ctrl_tcp so the modem
starts when the call does (see docs/reference-modem.md, "0a").  The
reference -- a Conexant on /dev/ttyACM0 behind the ATA's FXS 1 -- dials
in pinned by AT+MS with error control and compression off, a known
payload goes each way, and both are compared byte for byte.

    scripts/bench/sweep.py                 # every mode in MODES
    scripts/bench/sweep.py v22bis v32b-9600
    scripts/bench/sweep.py -o v22bis       # roles reversed: modec dials
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

def rate_of(ms):
    """The bit rate a +MS pin names, for pacing what the reference is fed."""
    import re
    m = re.search(r'AT\+MS=\w+,\d,(\d+),(\d+)', ms)
    return int(m.group(2)) if m else 2400

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

def sip_options(host, port, timeout=2.0):
    """True if a SIP stack answers OPTIONS at host:port."""
    import socket, random
    b = random.randint(1, 10**9)
    msg = (f'OPTIONS sip:probe@{host}:{port} SIP/2.0\r\n'
           f'Via: SIP/2.0/UDP 192.168.30.1:5070;branch=z9hG4bK{b};rport\r\n'
           f'Max-Forwards: 70\r\nTo: <sip:probe@{host}:{port}>\r\n'
           f'From: <sip:probe@192.168.30.1>;tag={b}\r\nCall-ID: {b}@192.168.30.1\r\n'
           f'CSeq: 1 OPTIONS\r\nContent-Length: 0\r\n\r\n')
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        s.settimeout(timeout); s.sendto(msg.encode(), (host, port))
        return s.recvfrom(65535)[0].startswith(b'SIP/2.0 200')
    except OSError:
        return False
    finally:
        s.close()

def ata_address():
    """Where the ATA's FXS 1 takes SIP, as host:port.

    The HT802V2 listens on a random port unless told otherwise, and says
    nothing on 5060 -- which is what made "the ATA ignores SIP from an
    unregistered proxy" look true.  Its own INVITE names the port, so the
    reference dials out while nothing holds 5060 and the INVITE is read
    off the socket.  ATA_SIP=host:port skips all of it; the answer is
    cached in OUT/ata-sip.txt and re-checked with OPTIONS each time."""
    import socket, re
    if os.environ.get('ATA_SIP'):
        return os.environ['ATA_SIP']
    cache = f'{SC}/ata-sip.txt'
    if os.path.exists(cache):
        addr = open(cache).read().strip()
        h, p = addr.rsplit(':', 1)
        if sip_options(h, int(p)):
            return addr
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.bind(('192.168.30.1', 5060)); s.settimeout(0.5)
    m = AT(PORT)
    try:
        m.cmd('ATZ', 3, quiet=True)
        os.write(m.fd, b'ATDT2001\r')
        end = time.time() + 15; addr = None
        while time.time() < end and not addr:
            try: d, src = s.recvfrom(65535)
            except socket.timeout: continue
            c = re.search(rb'^Contact:\s*<sip:[^@>]*@([\d.]+):(\d+)', d, re.M | re.I)
            if d.startswith(b'INVITE') and c:
                addr = f'{c.group(1).decode()}:{c.group(2).decode()}'
        m.cmd('ATH', 4, quiet=True)
    finally:
        m.close(); s.close()
    if not addr:
        raise RuntimeError('the ATA never sent an INVITE: is FXS 1 pointed at 192.168.30.1?')
    open(cache, 'w').write(addr + '\n')
    time.sleep(2)
    return addr

class Stack:
    """baresip and one modec process, as a call needs them.

    one() starts and stops a stack per call; call-control tests hold one
    across several calls.  modec is started without a role -- SIP gives
    it one per call, Originate for a call it dialled and Answer for one
    it took -- but with --ans-plain for the calls it answers (see one())."""
    def __init__(self, tag, mode, extra=None):
        self.tag = tag
        self.rx = f'{SC}/sweep-{tag}-rx.wav'; self.tx = f'{SC}/sweep-{tag}-tx.wav'
        self.log = open(f'{SC}/sweep-{tag}.log','wb')
        self.bl  = open(f'{SC}/sweep-{tag}-baresip.log','wb')
        self.bare = subprocess.Popen(['baresip','-f',os.path.expanduser('~/.baresip-bench')],
                                     stdout=self.bl, stderr=subprocess.STDOUT)
        time.sleep(3)
        args = [BIN,'modem','--ans-plain','--sip','127.0.0.1:4444','--audio-sip-loop','modec',
                '--mode',mode,'--hayes','--data-stdio','--record-rx',self.rx,'--record-tx',self.tx]+(extra or [])
        self.mo = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=self.log)
        nonblock(self.mo.stdout)
        time.sleep(3)
    def send(self, data):
        self.mo.stdin.write(data); self.mo.stdin.flush()
    def read(self, secs, until=None):
        return read_for(self.mo.stdout, secs, until)
    def alive(self):
        return self.mo.poll() is None
    def close(self):
        try: self.send(b'\r')
        except Exception: pass
        self.mo.terminate(); self.bare.terminate()
        try: self.mo.wait(timeout=5)
        except Exception: self.mo.kill()
        try: self.bare.wait(timeout=5)
        except Exception: self.bare.kill()
        self.log.close(); self.bl.close()
        time.sleep(2)

def call_logs(tag):
    """The per-call logs this stack's modec wrote, oldest first.  modec
    names each call's recording on stderr; the .log beside it has the
    call's own timeline."""
    import re
    try:
        txt = open(f'{SC}/sweep-{tag}.log','rb').read().decode('latin1')
    except FileNotFoundError:
        return []
    # match the path, not the sentence: modec's stderr is unbuffered and
    # two threads write to it, so the words around the path can arrive
    # interleaved with another line
    seen = []
    for w in re.findall(r'(recordings/\d{8}T\d{6}-[^\s]*?\.wav)', txt):
        if not w.endswith('-tx.wav') and w not in seen: seen.append(w)
    if not seen:
        # garbled past reading: take the per-call logs whose stamps fall
        # inside this stack's life, which ends when its log last changed
        import glob, datetime
        end = os.path.getmtime(f'{SC}/sweep-{tag}.log')
        for lg in sorted(glob.glob('recordings/2*.log')):
            m = re.match(r'recordings/(\d{8}T\d{6})-', lg)
            if not m: continue
            t = datetime.datetime.strptime(m.group(1), '%Y%m%dT%H%M%S').timestamp()
            if end - 150 <= t <= end: seen.append(lg[:-4] + '.wav')
    return [w[:-4] + '.log' for w in seen]

def read_call(path):
    """What a per-call log says: seconds from SIP established to CONNECT,
    the rate modec connected at, retrains, and how the call ended."""
    import re
    out = {'call_log': path, 't_modec_connect': None, 'modec_rate': '', 'modec_retrains': 0, 'modec_end': ''}
    try:
        lines = open(path, 'rb').read().decode('latin1').splitlines()
    except FileNotFoundError:
        return out
    up = None
    for ln in lines:
        m = re.match(r'\s*([0-9.]+)\s+(.*)', ln)
        if not m: continue
        t, what = float(m.group(1)), m.group(2)
        if what.startswith('SIP call up') and up is None: up = t
        elif what.startswith('CONNECT') and out['t_modec_connect'] is None:
            out['t_modec_connect'] = round(t - (up or 0), 2)
            r = re.match(r'CONNECT (\S+ \d+)', what); out['modec_rate'] = r.group(1) if r else what
        elif 'retraining the link' in what: out['modec_retrains'] += 1
        elif what.startswith('call ended:'): out['modec_end'] = what[len('call ended:'):].strip()
    return out

UD_KEYS = {'20': 'ud_tx_carrier', '21': 'ud_rx_carrier', '26': 'ud_init_tx', '27': 'ud_init_rx',
           '30': 'ud_carrier_losses', '31': 'ud_renegotiations', '32': 'ud_retrains_req',
           '33': 'ud_retrains_granted', '34': 'ud_final_tx', '35': 'ud_final_rx',
           '40': 'ud_protocol', '44': 'ud_compression', '55': 'ud_rx_lost', '60': 'ud_end_cause'}

def decode_ud(compact):
    """AT#UD's key=hex pairs, as one() compacts them, to named integers.
    Key meanings are Conexant's #UD table; rates and counts read as hex."""
    out = {}
    for kv in (compact or '').split():
        k, _, v = kv.partition('=')
        if k in UD_KEYS:
            try: out[UD_KEYS[k]] = int(v, 16)
            except ValueError: pass
    return out

def decode_v1(reply):
    """AT&V1, the Conexant last-call report, to named fields."""
    import re
    names = {'TERMINATION REASON': 'v1_end', 'LAST TX rate': 'v1_tx', 'HIGHEST TX rate': 'v1_tx_high',
             'LAST RX rate': 'v1_rx', 'HIGHEST RX rate': 'v1_rx_high', 'PROTOCOL': 'v1_protocol',
             'COMPRESSION': 'v1_compression', 'Line QUALITY': 'v1_quality', 'Rx LEVEL': 'v1_rx_level',
             'EQM Sum': 'v1_eqm', 'Local Rtrn Count': 'v1_local_retrains', 'Remote Rtrn Count': 'v1_remote_retrains',
             'Rate Drop': 'v1_rate_drop'}
    out = {}
    for ln in (reply or '').splitlines():
        m = re.match(r'(.+?)\.{2,}\s*(.*)', ln.strip())
        if m and m.group(1).strip() in names:
            v = m.group(2).strip()
            out[names[m.group(1).strip()]] = v[:-4] if v.endswith(' BPS') else v
    return out

def one(tag, mode, ms, extra=None, ref_extra=None, window=16, slow=False,
        pay_a=None, pay_b=None, settle=1.0, post=None, taketurns=False,
        originate=False, after=None):
    """One call.  pay_a goes modec -> reference, pay_b the other way;
    settle is the pause after CONNECT before either is sent (a noise
    schedule may need to have started); post is a list of AT commands
    to run at the reference after the call, their replies returned in
    result['post'].  originate reverses the roles: modec dials the ATA
    and the reference answers on ring.  after is a list of AT commands
    run at the reference once it has hung up, raw replies in
    result['after'] -- AT&V1, the last call's statistics, is only
    complete then.  The defaults are the original sweep."""
    extra = extra or []; ref_extra = ref_extra or []
    dial_to = f'1001@{ata_address()}' if originate else None
    pay_a = PAY_A if pay_a is None else pay_a; pay_b = PAY_B if pay_b is None else pay_b
    rx = f'{SC}/sweep-{tag}-rx.wav'; tx = f'{SC}/sweep-{tag}-tx.wav'
    log = open(f'{SC}/sweep-{tag}.log','wb')
    bl  = open(f'{SC}/sweep-{tag}-baresip.log','wb')
    bare = subprocess.Popen(['baresip','-f',os.path.expanduser('~/.baresip-bench')],
                            stdout=bl, stderr=subprocess.STDOUT)
    time.sleep(3)
    # --ans-plain always: through the ATA the V.25 reversals on the answer
    # tone stand its echo canceller down (docs/reference-modem.md); it
    # touches nothing but the V.32 answer tone
    role = [] if originate else ['--answer','--ans-plain']
    args = [BIN,'modem']+role+['--sip','127.0.0.1:4444','--audio-sip-loop','modec',
            '--mode',mode,'--hayes','--data-stdio','--record-rx',rx,'--record-tx',tx]+extra
    mo = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log)
    nonblock(mo.stdout)
    time.sleep(3)
    result = {'tag':tag,'mode':mode,'ms':ms,'dir':'originate' if originate else 'answer'}
    m = AT(PORT)
    try:
        # inside the try: if modec has already died (an option it does not
        # know, say) this raises, and the far end and baresip still get
        # cleaned up rather than left off hook
        stops = [b'CONNECT',b'NO CARRIER',b'BUSY',b'NO ANSWER',b'ERROR']
        for c in ['AT&F','AT&K0','AT%C0','AT\\N0','ATX4','ATS7=40', ms] + ref_extra:
            m.cmd(c,3,quiet=True)
        t_dial = time.time()
        if originate:
            # the reference picks up on the first ring; modec's ATD waits
            # for the call to be established, then trains as the caller
            m.cmd('ATS0=1',3,quiet=True)
            t_dial = time.time()
            mo.stdin.write(f'ATD{dial_to}\r'.encode()); mo.stdin.flush()
            r = b''; out = b''; t0 = time.time()
            while time.time()-t0 < 60 and not any(s in r for s in stops):
                try: r += os.read(m.fd, 65536)
                except BlockingIOError: pass
                out += read_for(mo.stdout, 0.1)
        else:
            mo.stdin.write(b'ATS0=1\r'); mo.stdin.flush()
            read_for(mo.stdout, 1.5)
            r = m.cmd('ATDT2001',45,stop=stops,quiet=True)
            out = b''
        result['t_ref_result'] = round(time.time() - t_dial, 2)
        rtxt = r.decode('latin1').replace('\r',' ').strip()
        result['ref'] = 'CONNECT' if b'CONNECT' in r else rtxt.split()[-1] if rtxt else '?'
        result['ref_connect'] = next((l.strip() for l in rtxt.split('  ') if 'CONNECT' in l), '')
        if b'CONNECT' in r:
            if b'CONNECT' not in out:
                out += read_for(mo.stdout, 12, until=b'CONNECT')
            result['modec_connect'] = b'CONNECT' in out
            time.sleep(settle)
            if slow:
                # one line at a time with idle between them, so a
                # start-stop framer that lost alignment on a bad bit has
                # marks to find the next start bit against
                for ln in pay_b.split(b'\r\n'):
                    if ln: os.write(m.fd, ln + b'\r\n'); time.sleep(0.4)
            # Paced at the line rate: flow control is off at the
            # reference (&K0), and a 5 kB write at 115200 into a
            # 2400 bit/s modem is a full buffer and silent loss.
            import threading
            def feed():
                bps = rate_of(ms); step = max(16, bps // 100)
                for i in range(0, len(pay_b), step):
                    os.write(m.fd, pay_b[i:i+step]); time.sleep(step * 10.0 / bps)
            mfd = mo.stdout.fileno()
            got_ref = b''; got_modec = b''
            def pump(secs):
                """Read both ends for `secs`.  Both, always: a single read
                at the end of a non-blocking buffered pipe returns one
                chunk, which is how a 4.7 kB payload came back as 561
                bytes."""
                nonlocal got_ref, got_modec
                t0 = time.time()
                while time.time() - t0 < secs:
                    idle = True
                    try: got_ref += os.read(m.fd, 65536); idle = False
                    except BlockingIOError: pass
                    try:
                        d = os.read(mfd, 65536)
                        if d: got_modec += d; idle = False
                    except BlockingIOError: pass
                    if idle: time.sleep(0.02)
            if taketurns:
                # One direction at a time.  Both at once is a duplex load
                # the path does not carry: on two clean-line calls in a
                # row one direction came through perfectly and the other
                # was demodulated garbage, and which one flipped between
                # runs.  Taking turns also takes the near end's echo of
                # its own transmission out of the measurement.
                mo.stdin.write(pay_a); mo.stdin.flush()   # modec -> reference
                pump(window / 2)
                if slow:
                    for ln in pay_b.split(b'\r\n'):
                        if ln: os.write(m.fd, ln + b'\r\n'); time.sleep(0.4)
                else:
                    threading.Thread(target=feed, daemon=True).start()
                pump(window / 2)
            else:
                if slow:
                    for ln in pay_b.split(b'\r\n'):
                        if ln: os.write(m.fd, ln + b'\r\n'); time.sleep(0.4)
                else:
                    threading.Thread(target=feed, daemon=True).start()
                mo.stdin.write(pay_a); mo.stdin.flush()   # modec -> reference
                pump(window)
            open(f'{SC}/sweep-{tag}-at-ref.bin','wb').write(got_ref)
            open(f'{SC}/sweep-{tag}-at-modec.bin','wb').write(got_modec)
            result['ref_got'] = got_ref.count(pay_a[:40]) and pay_a in got_ref
            result['modec_got'] = pay_b in got_modec
            result['ref_bytes'] = len(got_ref); result['modec_bytes'] = len(got_modec)
            time.sleep(1.2); os.write(m.fd, b'+++'); time.sleep(1.5)
            if post:
                import re as _re
                def compact(reply):
                    pairs = _re.findall(r'DIAG <[0-9A-F]+ ([0-9A-F]+=[0-9A-F]+)>', reply)
                    return ' '.join(pairs) if pairs else reply.replace('\n', ' ').strip()
                result['post'] = {c: compact(m.cmd(c, 3, quiet=True).decode('latin1')) for c in post}
        m.cmd('ATH',4,quiet=True)
        if after:
            result['after'] = {c: m.cmd(c, 4, quiet=True).decode('latin1') for c in after}
        if post and 'post' not in result:
            import re as _re
            result['post'] = {c: ' '.join(_re.findall(r'DIAG <[0-9A-F]+ ([0-9A-F]+=[0-9A-F]+)>',
                                                     m.cmd(c, 3, quiet=True).decode('latin1'))) for c in post}
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
    logs = call_logs(tag)
    if logs: result.update(read_call(logs[-1]))
    return result

MODES = [
    # The data-mode echo canceller, on and off, at the two rates the
    # 644 ms reflection kept under a floor.  The window is long so the
    # scans have time to aim and average; the decision error's course
    # through the call, in the per-call log, is the measurement.
    ('v32b-12000-ec',   'v32bis', 'AT+MS=V32B,0,12000,12000', ['--v32-rate','12000','--line-every','25','--max-evm-v32','3'], [], 40),
    ('v32b-12000-noec', 'v32bis', 'AT+MS=V32B,0,12000,12000', ['--v32-rate','12000','--line-every','25','--max-evm-v32','3','--no-echo-data'], [], 40),
    ('v32b-14400-ec',   'v32bis', 'AT+MS=V32B,0,14400,14400', ['--v32-rate','14400','--line-every','25','--max-evm-v32','3'], [], 40),
    ('v32b-14400-noec', 'v32bis', 'AT+MS=V32B,0,14400,14400', ['--v32-rate','14400','--line-every','25','--max-evm-v32','3','--no-echo-data'], [], 40),
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
if __name__ == '__main__':
  # -o: modec places the call, the reference answers.  Tags get an -o
  # suffix so the two directions' recordings sit side by side.
  args = sys.argv[1:]
  originate = '-o' in args
  only = [a for a in args if a != '-o'] or None
  rows=[]
  for spec in MODES:
    tag = spec[0]
    if only and tag not in only: continue
    print(f'>>> {tag} ({spec[2]}){" originate" if originate else ""}', flush=True)
    if originate:
      spec = (tag + '-o',) + tuple(spec[1:])
    r = one(*spec, originate=originate)
    rows.append(r); print('   ', r, flush=True)
  print('\n=== SUMMARY ===')
  for r in rows: print(r)
