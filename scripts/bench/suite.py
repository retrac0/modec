#!/usr/bin/env python3
"""The standing bench test set: docs/bench-tests.md, as something to run.

    scripts/bench/suite.py preflight
    scripts/bench/suite.py S0                    # smoke: every mode, both ways
    scripts/bench/suite.py S1 --reps 10          # handshake reliability
    scripts/bench/suite.py S2 S3 --reps 3
    scripts/bench/suite.py S6                    # call control
    scripts/bench/suite.py S0 --only v22bis,v32b-9600 --dir originate
    scripts/bench/suite.py summary [REV]         # pass counts by test and direction

Every call is a row in recordings/bench/results.csv carrying the modem
code's revision.  A row already there for the same clean revision, suite,
test, repetition and direction is not run again, so an interrupted run
picks up where it stopped.  Pre-flight runs first and between suites, and
a bench fault stops the run rather than being recorded as a modem result.
"""
import csv, json, os, re, socket, subprocess, sys, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
os.environ.setdefault('PORT', '/dev/modem-ref')
import sweep
from sweep import SC, PORT, Stack, one, call_logs, read_call, decode_ud, decode_v1, ata_address, sip_options, read_for
from atlib import AT
from ber import score

RESULTS = f'{SC}/results.csv'
FIELDS = ['when', 'rev', 'dirty', 'suite', 'test', 'rep', 'dir', 'mode', 'ref_ms', 'modec_args',
          'ref', 'ref_connect', 'modec_connect', 't_ref_result', 't_modec_connect', 'modec_rate',
          'modec_retrains', 'modec_end', 'a_intact', 'a_lines', 'b_intact', 'b_lines',
          'ud_tx_carrier', 'ud_rx_carrier', 'ud_init_tx', 'ud_init_rx', 'ud_final_tx', 'ud_final_rx',
          'ud_carrier_losses', 'ud_renegotiations', 'ud_retrains_req', 'ud_retrains_granted',
          'ud_protocol', 'ud_compression', 'ud_rx_lost', 'ud_end_cause',
          'v1_end', 'v1_tx', 'v1_tx_high', 'v1_rx', 'v1_rx_high', 'v1_protocol', 'v1_compression',
          'v1_quality', 'v1_rx_level', 'v1_eqm', 'v1_local_retrains', 'v1_remote_retrains', 'v1_rate_drop',
          'pass', 'note', 'call_log']

class BenchFault(Exception):
    """The bench, not the modem: stop, do not record."""

# ------------------------------------------------------------------ plumbing

def revision():
    rev = subprocess.run(['git', 'rev-parse', '--short', 'HEAD'], capture_output=True, text=True).stdout.strip()
    # only the modem's own code makes a result incomparable; docs and
    # bench scripts moving do not
    dirty = subprocess.run(['git', 'status', '--porcelain', '--', 'src', 'app', 'modec.cabal'],
                           capture_output=True, text=True).stdout.strip() != ''
    return rev, dirty

def done_keys():
    if not os.path.exists(RESULTS):
        return set()
    with open(RESULTS) as f:
        return {(r['rev'], r['suite'], r['test'], r['rep'], r['dir'])
                for r in csv.DictReader(f) if r['dirty'] == 'False'}

def write_row(row):
    new = not os.path.exists(RESULTS)
    with open(RESULTS, 'a', newline='') as f:
        w = csv.DictWriter(f, fieldnames=FIELDS, extrasaction='ignore')
        if new: w.writeheader()
        w.writerow(row)
    show = {k: row.get(k) for k in ['test', 'rep', 'dir', 'ref', 'modec_rate', 't_modec_connect',
                                    'a_intact', 'a_lines', 'b_intact', 'b_lines', 'modec_end', 'pass', 'note']}
    print('   ', show, flush=True)

def preflight(verbose=True):
    """The checks in bench-tests.md.  Raises BenchFault."""
    def say(s):
        if verbose: print('  preflight:', s, flush=True)
    for name in ('baresip', 'modec', 'pw-cat'):
        if subprocess.run(['pgrep', '-x', name], capture_output=True).returncode == 0:
            raise BenchFault(f'{name} is already running')
    if subprocess.run(['pw-cli', 'info', '0'], capture_output=True).returncode != 0:
        raise BenchFault('PipeWire is not answering')
    m = AT(PORT)
    try:
        m.cmd('ATH', 4, quiet=True)
        if b'OK' not in m.cmd('AT', 3, quiet=True):
            raise BenchFault('the reference modem does not answer AT')
    finally:
        m.close()
    say('reference answers')
    host, port = ata_address().rsplit(':', 1)
    if not sip_options(host, int(port)):
        raise BenchFault(f'the ATA does not answer OPTIONS at {host}:{port}')
    say(f'ATA at {host}:{port}')
    b = subprocess.run(['cabal', 'build', 'exe:modec'], capture_output=True, text=True)
    if b.returncode != 0:
        raise BenchFault('modec does not build')
    rev, dirty = revision()
    say(f'modec built at {rev}{" (dirty)" if dirty else ""}')
    return rev, dirty

def base_row(suite, test, rep, originate, mode, ms, extra):
    rev, dirty = revision()
    return {'when': time.strftime('%Y-%m-%dT%H:%M:%S'), 'rev': rev, 'dirty': dirty, 'suite': suite,
            'test': test, 'rep': rep, 'dir': 'originate' if originate else 'answer',
            'mode': mode, 'ref_ms': ms, 'modec_args': ' '.join(extra)}

def call_row(suite, test, rep, originate, mode, ms, extra, ref_extra=None, window=16, pay_a=None, pay_b=None):
    """One call through sweep.one, scored and decoded."""
    pay_a = pay_a or sweep.PAY_A; pay_b = pay_b or sweep.PAY_B
    tag = f'suite-{suite}-{test}-{"o" if originate else "a"}'
    # ATW2: the reference's CONNECT names the line rate, not the DTE's.
    # AT#UD is empty for a call the reference answered; AT&V1 is not.
    r = one(tag, mode, ms, extra, ['ATW2'] + list(ref_extra or []), window=window, pay_a=pay_a, pay_b=pay_b,
            post=['AT#UD'], taketurns=True, originate=originate, after=['AT&V1'])
    row = base_row(suite, test, rep, originate, mode, ms, extra)
    for k in ['ref', 'ref_connect', 'modec_connect', 't_ref_result', 't_modec_connect', 'modec_rate',
              'modec_retrains', 'modec_end', 'call_log']:
        row[k] = r.get(k)
    row.update(decode_ud(r.get('post', {}).get('AT#UD')))
    row.update(decode_v1(r.get('after', {}).get('AT&V1')))
    row['a_lines'] = pay_a.count(b'\r\n'); row['b_lines'] = pay_b.count(b'\r\n')
    row['a_intact'] = row['b_intact'] = 0
    if r.get('ref') == 'CONNECT':
        try:
            row['a_intact'] = score(pay_a, open(f'{SC}/sweep-{tag}-at-ref.bin', 'rb').read(), 'A')['intact']
            row['b_intact'] = score(pay_b, open(f'{SC}/sweep-{tag}-at-modec.bin', 'rb').read(), 'B')['intact']
        except FileNotFoundError:
            pass
    return row

def payload_ok(row):
    return row['a_intact'] == row['a_lines'] and row['b_intact'] == row['b_lines']

def lines(prefix, n):
    """n indexed lines in the format ber.score reads."""
    return b''.join((f'{prefix}{i:04d} the quick brown fox jumps over the lazy dog 0123456789\r\n').encode()
                    for i in range(n))

DIRS = {'answer': [False], 'originate': [True], 'both': [False, True]}

# ------------------------------------------------------------------ S0, S1

# tag: (modec --mode, AT+MS, modec extra, window for the 496-byte pair taking turns)
MODES = {
    'bell103':    ('bell103',  'AT+MS=B103,0,300,300',     [],                     40),
    'v21':        ('v21',      'AT+MS=V21,0,300,300',      [],                     40),
    'bell212a':   ('bell212a', 'AT+MS=B212,0,1200,1200',   [],                     16),
    'v22':        ('v22',      'AT+MS=V22,0,1200,1200',    [],                     16),
    'v22bis':     ('v22bis',   'AT+MS=V22B,0,2400,2400',   [],                     16),
    'v32-4800':   ('v32',      'AT+MS=V32,0,4800,4800',    ['--v32-rate', '4800'], 16),
    'v32-9600':   ('v32',      'AT+MS=V32,0,9600,9600',    ['--v32-rate', '9600'], 16),
    'v32b-7200':  ('v32bis',   'AT+MS=V32B,0,7200,7200',   ['--v32-rate', '7200'], 16),
    'v32b-9600':  ('v32bis',   'AT+MS=V32B,0,9600,9600',   ['--v32-rate', '9600'], 16),
    'v32b-12000': ('v32bis',   'AT+MS=V32B,0,12000,12000', ['--v32-rate', '12000'], 16),
    'v32b-14400': ('v32bis',   'AT+MS=V32B,0,14400,14400', ['--v32-rate', '14400'], 16),
}

def S0(rep, originate, only):
    for tag, (mode, ms, extra, window) in MODES.items():
        if only and tag not in only: continue
        yield tag, lambda: _smoke(tag, mode, ms, extra, window, rep, originate, 'S0')

def _smoke(tag, mode, ms, extra, window, rep, originate, suite, pay=None):
    pa, pb = pay or (None, None)
    row = call_row(suite, tag, rep, originate, mode, ms, extra, window=window, pay_a=pa, pay_b=pb)
    row['pass'] = payload_ok(row)
    return row

def S1(rep, originate, only):
    # the start-up is the test: four lines each way keeps a call short
    pay = (lines('A', 4), lines('B', 4))
    for tag, (mode, ms, extra, window) in MODES.items():
        if only and tag not in only: continue
        w = 20 if window > 16 else 8
        def run(tag=tag, mode=mode, ms=ms, extra=extra, w=w):
            row = _smoke(tag, mode, ms, extra, w, rep, originate, 'S1', pay)
            # 12000 and 14400 are measured, not judged
            if tag in ('v32b-12000', 'v32b-14400'):
                row['note'] = f'pass={row["pass"]} (rate, not judged)'; row['pass'] = ''
            return row
        yield tag, run

# ------------------------------------------------------------------ S2

ALL_BUT_V32 = 'bell103,v21,v23,bell212a,v22,v22bis'
# id: (reference AT+MS, modec --mode, modec extra, expectation, what it must be)
#   expectation: a regex modec's CONNECT rate must match, or None where
#   nothing is common and both ends must clear; 'observe' rows are not judged
S2_CASES = {
    'N1':  ('AT+MS=V32B,1,4800,14400',   'v32bis',            [],                       r'^V32bis \d+$', 'must'),
    'N2':  ('AT+MS=V32,0,9600,9600',     'v32bis',            [],                       r'^V32 9600$',   'must'),
    'N3':  ('AT+MS=V32B,1,4800,14400',   'v22bis',            [],                       r'^V22bis 2400$', 'must'),
    'N4':  ('AT+MS=V22B,0,2400,2400',    'v32bis,v22bis',     [],                       r'^V22bis 2400$', 'must'),
    'N5':  ('AT+MS=V22,0,1200,1200',     'v22bis',            [],                       r'^V22 1200$',   'must'),
    'N6':  ('AT+MS=V21,0,300,300',       'v22bis,v22,v21',    [],                       r'^V21 300$',    'must'),
    'N7':  ('AT+MS=B103,0,300,300',      ALL_BUT_V32,         [],                       r'^Bell103 300$', 'must'),
    'N8':  ('AT+MS=V32B,1,4800,14400',   'v32bis',            ['--v8'],                 r'^V32bis \d+$', 'must'),
    'N9':  ('AT+MS=V32B,1,4800,14400',   'v22bis',            ['--v8'],                 r'^V22bis 2400$', 'must'),
    'N10': ('AT+MS=V32B,0,12000,12000',  'v32bis',            ['--v32-rate', '9600'],   None,            'must'),
    'N11': ('AT+MS=V22B,0,2400,2400',    'v21',               [],                       None,            'must'),
    'N12': ('AT+MS=V32B,1,4800,9600,4800,14400', 'v32bis',    [],                       r'.',            'observe'),
    'N13': ('AT+MS=B212,0,1200,1200',    'v22',               [],                       r'.',            'observe'),
}

def S2(rep, originate, only):
    pay = (lines('A', 4), lines('B', 4))
    for cid, (ms, mode, extra, want, kind) in S2_CASES.items():
        if only and cid not in only: continue
        def run(cid=cid, ms=ms, mode=mode, extra=extra, want=want, kind=kind):
            # 300 bit/s wants longer for four lines each way
            window = 24 if '300' in ms else 10
            row = call_row('S2', cid, rep, originate, mode, ms, extra, window=window, pay_a=pay[0], pay_b=pay[1])
            if want is None:
                cleared = row['ref'] != 'CONNECT' and not row['modec_rate']
                quick = row['t_ref_result'] is not None and row['t_ref_result'] <= 75
                row['pass'] = cleared and quick
                row['note'] = f'cleared={cleared} ref after {row["t_ref_result"]} s'
            else:
                landed = bool(re.search(want, row['modec_rate'] or ''))
                row['note'] = f'landed {row["modec_rate"] or "nothing"}, want {want}'
                row['pass'] = (landed and payload_ok(row)) if kind == 'must' else ''
            return row
        yield cid, run

# ------------------------------------------------------------------ S3

def S3(rep, originate, only):
    ms = 'AT+MS=V32B,1,4800,14400'
    pay = (lines('A', 8), lines('B', 8))
    tests = [('auto', [])]
    if originate:
        # transmit level in the calling role only: -3, 0 and +3 dB
        tests += [('amp0.35', ['--amp', '0.35']), ('amp0.5', ['--amp', '0.5']), ('amp0.7', ['--amp', '0.7'])]
    for name, extra in tests:
        if only and name not in only: continue
        def run(name=name, extra=extra):
            row = call_row('S3', name, rep, originate, 'v32bis', ms, extra, window=16, pay_a=pay[0], pay_b=pay[1])
            row['pass'] = ''   # a distribution, not a verdict
            row['note'] = (f'reference rx {row.get("v1_rx")} tx {row.get("v1_tx")}, quality {row.get("v1_quality")}, '
                           f'rx level {row.get("v1_rx_level")}, eqm {row.get("v1_eqm")}')
            return row
        yield name, run

# ------------------------------------------------------------------ S6

C_MODE = ('v22bis', 'AT+MS=V22B,0,2400,2400')

def ref_setup(m, extra=()):
    for c in ['AT&F', 'AT&K0', 'AT%C0', 'AT\\N0', 'ATX4', 'ATW2', 'ATS7=40', C_MODE[1]] + list(extra):
        m.cmd(c, 3, quiet=True)

def connect(st, m, originate, timeout=60):
    """Bring a call up on a running stack.  Returns (ok, reference text, modec text)."""
    stops = [b'CONNECT', b'NO CARRIER', b'BUSY', b'NO ANSWER', b'ERROR']
    if originate:
        m.cmd('ATS0=1', 3, quiet=True)
        st.send(f'ATD1001@{ata_address()}\r'.encode())
    else:
        st.send(b'ATS0=1\r'); st.read(1.5)
        os.write(m.fd, b'ATDT2001\r')
    r = b''; out = b''; t0 = time.time()
    while time.time() - t0 < timeout and not any(s in r for s in stops):
        r += m.read(0.1)
        out += st.read(0.05)
    if b'CONNECT' in r and b'CONNECT' not in out:
        out += st.read(15, until=b'CONNECT')
    return (b'CONNECT' in r and b'CONNECT' in out), r, out

def data_check(st, m, n=2):
    """n lines each way, taking turns.  True when both arrive intact."""
    time.sleep(1.0)
    a = lines('A', n); b = lines('B', n)
    st.send(a); got_ref = m.read(4)
    os.write(m.fd, b); got_modec = st.read(4)
    return score(a, got_ref, 'A')['intact'] == n and score(b, got_modec, 'B')['intact'] == n

def escape_modec(st):
    time.sleep(1.2); st.send(b'+++'); time.sleep(1.2)
    return b'OK' in st.read(2.5, until=b'OK')

def escape_ref(m):
    time.sleep(1.2); os.write(m.fd, b'+++'); time.sleep(1.2)
    return b'OK' in m.read(2.5, until=b'OK')

def baresip_command(cmd, params=''):
    """A second ctrl_tcp client, as a SIP-side event modec did not ask for."""
    body = json.dumps({'command': cmd, 'params': params, 'token': 'suite'}).encode()
    s = socket.create_connection(('127.0.0.1', 4444), timeout=3)
    try:
        s.sendall(str(len(body)).encode() + b':' + body + b',')
        try: return s.recv(4096)
        except socket.timeout: return b''
    finally:
        s.close()

def _call_control(test, originate, body):
    """Run body(st, m, row) on a fresh stack and a configured reference."""
    tag = f'suite-S6-{test}-{"o" if originate else "a"}'
    row = base_row('S6', test, 0, originate, C_MODE[0], C_MODE[1], [])
    st = Stack(tag, C_MODE[0]); m = AT(PORT)
    try:
        body(st, m, row)
    finally:
        try:
            m.dtr(True); m.cmd('ATH', 4, quiet=True)
        except Exception:
            pass
        m.close(); st.close()
    logs = call_logs(tag)
    if logs:
        c = read_call(logs[-1]); row['call_log'] = c['call_log']; row['modec_end'] = c['modec_end']
    return row

def wait_for(reader, secs, pats):
    t0 = time.time(); buf = b''
    while time.time() - t0 < secs:
        buf += reader(0.1)
        if any(p in buf for p in pats):
            return round(time.time() - t0, 2), buf
    return None, buf

def C1(st, m, row):   # the reference hangs up
    ref_setup(m); ok, _, _ = connect(st, m, row['dir'] == 'originate')
    row['modec_connect'] = ok
    if not ok: row['pass'] = False; row['note'] = 'no connection'; return
    row['a_intact'] = data_check(st, m)
    esc = escape_ref(m); os.write(m.fd, b'ATH\r')
    t, _ = wait_for(st.read, 20, [b'NO CARRIER'])
    st.send(b'AT\r'); cmd_ok = b'OK' in st.read(3, until=b'OK')
    row['pass'] = t is not None and t <= 5 and cmd_ok
    row['note'] = f'ref escaped={esc}; modec NO CARRIER after {t} s; modec command mode {cmd_ok}'

def C2(st, m, row):   # modec hangs up
    ref_setup(m); ok, _, _ = connect(st, m, row['dir'] == 'originate')
    row['modec_connect'] = ok
    if not ok: row['pass'] = False; row['note'] = 'no connection'; return
    row['a_intact'] = data_check(st, m)
    esc = escape_modec(st); st.send(b'ATH\r')
    t, _ = wait_for(m.read, 20, [b'NO CARRIER'])
    ref_ok = b'OK' in m.cmd('AT', 3, quiet=True)
    row['pass'] = esc and t is not None and t <= 5 and ref_ok
    row['note'] = f'modec escaped={esc}; ref NO CARRIER after {t} s; ref command mode {ref_ok}'

def C3(st, m, row):   # the reference drops DTR
    ref_setup(m, ['AT&D2']); ok, _, _ = connect(st, m, row['dir'] == 'originate')
    row['modec_connect'] = ok
    if not ok: row['pass'] = False; row['note'] = 'no connection'; return
    row['a_intact'] = data_check(st, m)
    m.dtr(False)
    t, _ = wait_for(st.read, 20, [b'NO CARRIER'])
    m.dtr(True); time.sleep(1.5)
    ref_ok = b'OK' in m.cmd('AT', 3, quiet=True)
    row['pass'] = t is not None and t <= 5 and ref_ok
    row['note'] = f'modec NO CARRIER after {t} s; ref command mode {ref_ok}'

def C4(st, m, row):   # the SIP side hangs up
    ref_setup(m); ok, _, _ = connect(st, m, row['dir'] == 'originate')
    row['modec_connect'] = ok
    if not ok: row['pass'] = False; row['note'] = 'no connection'; return
    row['a_intact'] = data_check(st, m)
    try:
        reply = baresip_command('hangup')
    except OSError as e:
        row['pass'] = ''; row['note'] = f'ctrl_tcp refused a second client: {e}'; return
    t, _ = wait_for(st.read, 20, [b'NO CARRIER'])
    tr, _ = wait_for(m.read, 20, [b'NO CARRIER'])
    row['pass'] = t is not None and t <= 5 and tr is not None
    row['note'] = f'baresip said {reply[:60]!r}; modec NO CARRIER after {t} s, ref after {tr} s'

def C5(st, m, row):   # nobody answers
    ref_setup(m, ['ATS0=0'])
    if row['dir'] == 'originate':
        st.send(f'ATD1001@{ata_address()}\r'.encode())
        t, buf = wait_for(st.read, 150, [b'NO ANSWER', b'NO CARRIER', b'BUSY'])
        rings = m.read(0.5).count(b'RING')
        if t is None:
            st.send(b'\r'); aborted = wait_for(st.read, 10, [b'NO CARRIER', b'OK', b'NO ANSWER'])[0]
            row['pass'] = False; row['note'] = f'modec never gave up in 150 s (a keypress ended it: {aborted is not None})'
        else:
            row['pass'] = b'NO ANSWER' in buf
            row['note'] = f'modec reported {buf.strip()[-20:]!r} after {t} s'
    else:
        st.send(b'ATS0=0\r'); st.read(1)
        os.write(m.fd, b'ATDT2001\r')
        t, rbuf = wait_for(m.read, 90, [b'NO CARRIER', b'NO ANSWER', b'BUSY'])
        rings = st.read(1).count(b'RING')
        late = st.read(6).count(b'RING')
        row['pass'] = t is not None and late == 0
        row['note'] = f'reference gave up after {t} s; RINGs at modec after it hung up: {late}'

def C6(st, m, row):   # a number nobody has
    if row['dir'] != 'originate':
        row['pass'] = ''; row['note'] = 'originate only'; return
    # Not a number the ATA lacks: the HT802V2 ignores the user part and
    # rings FXS 1 for any of them.  A SIP address nothing listens on is
    # the destination that does not exist.
    st.send(b'ATD1001@192.168.30.1:5999\r')
    t, buf = wait_for(st.read, 60, [b'NO ANSWER', b'NO CARRIER', b'BUSY', b'ERROR'])
    row['pass'] = t is not None and t <= 40
    row['note'] = f'modec said {buf.strip()[-20:]!r} after {t} s'

def C7(st, m, row):   # ring count
    if row['dir'] != 'answer':
        row['pass'] = ''; row['note'] = 'answer only'; return
    ref_setup(m)
    st.send(b'ATS0=3\r'); st.read(1)
    os.write(m.fd, b'ATDT2001\r')
    t, buf = wait_for(st.read, 30, [b'CONNECT'])
    rings = buf.count(b'RING')
    row['pass'] = rings >= 3
    row['note'] = f'{rings} RING before modec answered (S0=3)'

def C8(st, m, row):   # escape and return, at each end
    ref_setup(m); ok, _, _ = connect(st, m, row['dir'] == 'originate')
    row['modec_connect'] = ok
    if not ok: row['pass'] = False; row['note'] = 'no connection'; return
    before = data_check(st, m)
    em = escape_modec(st); st.send(b'ATO\r'); back_m = b'CONNECT' in st.read(4, until=b'CONNECT')
    after_m = data_check(st, m)
    er = escape_ref(m); os.write(m.fd, b'ATO\r'); back_r = b'CONNECT' in m.read(4, until=b'CONNECT')
    after_r = data_check(st, m)
    row['pass'] = before and em and back_m and after_m and er and back_r and after_r
    row['note'] = (f'before={before} modec: escape={em} ATO={back_m} data={after_m}; '
                   f'ref: escape={er} ATO={back_r} data={after_r}')

def C9(st, m, row):   # one modec, call after call, alternating direction
    results = []
    for i, orig in enumerate([row['dir'] == 'originate', row['dir'] != 'originate'] * 2):
        # the reference is reset every call: this test is about modec's
        # process lasting, and the CX93001 keeps something past AT&V after
        # it has answered a call that breaks its next V.22bis call (C11)
        ref_setup(m)
        ok, _, _ = connect(st, m, orig)
        good = ok and data_check(st, m)
        if ok:
            escape_ref(m); os.write(m.fd, b'ATH\r')
            wait_for(st.read, 20, [b'NO CARRIER'])
        m.cmd('ATH', 4, quiet=True); time.sleep(3)
        results.append(good)
    row['pass'] = all(results)
    row['note'] = 'calls ' + ' '.join(f'{"o" if o else "a"}={g}' for o, g in
                                      zip([row['dir'] == 'originate', row['dir'] != 'originate'] * 2, results))

def C10(st, m, row):  # the caller gives up while modec is ringing
    if row['dir'] != 'answer':
        row['pass'] = ''; row['note'] = 'answer only'; return
    ref_setup(m)
    st.send(b'ATS0=0\r'); st.read(1)
    os.write(m.fd, b'ATDT2001\r')
    rang, _ = wait_for(st.read, 20, [b'RING'])
    time.sleep(4)
    os.write(m.fd, b'\r')                       # a keypress abandons the dial
    m.read(5, until=b'NO CARRIER')
    st.read(4)                                  # RINGs already in the pipe from before it gave up
    late = st.read(6).count(b'RING')
    st.send(b'ATS0=1\r'); st.read(1)
    ok, _, _ = connect(st, m, False)
    again = ok and data_check(st, m)
    row['pass'] = rang is not None and late == 0 and again
    row['note'] = f'modec rang={rang is not None}; RINGs after the caller gave up: {late}; next call good={again}'

def C11(st, m, row):  # the reference is not reset after it has answered
    if row['dir'] != 'answer':
        row['pass'] = ''; row['note'] = 'answer only'; return
    ref_setup(m)
    ok1, _, _ = connect(st, m, True)            # modec calls, the reference answers
    first = ok1 and data_check(st, m)
    if ok1:
        escape_ref(m); os.write(m.fd, b'ATH\r'); wait_for(st.read, 20, [b'NO CARRIER'])
    m.read(3); time.sleep(3)
    ok2, r, _ = connect(st, m, False)           # then it dials modec, not reset
    second = ok2 and data_check(st, m)
    row['pass'] = ''                            # observed: a reference quirk, see bench-tests.md
    row['note'] = (f'first (reference answered) good={first}; second (reference dialled, no AT&F) '
                   f'modec connected={ok2} good={second}; reference said {r.strip()[-16:]!r}')

S6_TESTS = {'C1': C1, 'C2': C2, 'C3': C3, 'C4': C4, 'C5': C5, 'C6': C6, 'C7': C7, 'C8': C8, 'C9': C9, 'C10': C10, 'C11': C11}

def S6(rep, originate, only):
    for test, body in S6_TESTS.items():
        if only and test not in only: continue
        yield test, lambda test=test, body=body: _call_control(test, originate, body)

# ------------------------------------------------------------------ driver

SUITES = {'S0': S0, 'S1': S1, 'S2': S2, 'S3': S3, 'S6': S6}

def summary(rev=None):
    if not os.path.exists(RESULTS):
        print('no results yet'); return
    rows = list(csv.DictReader(open(RESULTS)))
    rev = rev or rows[-1]['rev']
    rows = [r for r in rows if r['rev'] == rev]
    table = {}
    for r in rows:
        k = (r['suite'], r['test'], r['dir'])
        p, n, other = table.get(k, (0, 0, 0))
        if r['pass'] == 'True': p += 1; n += 1
        elif r['pass'] == 'False': n += 1
        else: other += 1
        table[k] = (p, n, other)
    print(f'revision {rev}')
    for (s, t, d), (p, n, o) in sorted(table.items()):
        print(f'  {s:3} {t:12} {d:9} {p}/{n}' + (f'  (+{o} not judged)' if o else ''))

def main(argv):
    args = list(argv)
    def opt(name, default):
        if name in args:
            i = args.index(name); v = args[i + 1]; del args[i:i + 2]; return v
        return default
    reps = int(opt('--reps', '1')); only = opt('--only', ''); direction = opt('--dir', 'both')
    only = set(only.split(',')) if only else None
    if not args:
        print(__doc__); return
    if args[0] == 'summary':
        summary(args[1] if len(args) > 1 else None); return
    if args[0] == 'preflight':
        preflight(); return
    try:
        for name in args:
            if name not in SUITES:
                raise SystemExit(f'unknown suite {name}; have {", ".join(SUITES)}')
            rev, dirty = preflight()
            done = done_keys()
            for rep in range(reps):
                for originate in DIRS[direction]:
                    for test, run in SUITES[name](rep, originate, only):
                        key = (rev, name, test, str(rep), 'originate' if originate else 'answer')
                        if not dirty and key in done:
                            continue
                        print(f'>>> {name} {test} rep {rep} {key[4]}', flush=True)
                        row = run()
                        write_row(row)
                        # a reference that stopped answering is the bench
                        chk = AT(PORT)
                        try:
                            chk.cmd('ATH', 4, quiet=True)
                            if b'OK' not in chk.cmd('AT', 3, quiet=True):
                                raise BenchFault('the reference stopped answering AT')
                        finally:
                            chk.close()
    except BenchFault as e:
        print(f'BENCH FAULT, stopping: {e}', flush=True)
        sys.exit(2)

if __name__ == '__main__':
    main(sys.argv[1:])
