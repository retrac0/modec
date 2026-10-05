#!/usr/bin/env python3
"""Probe calls: N of the same call, outside results.csv, every log kept.

    scripts/bench/probe.py NAME N [originate|answer] [pinned|auto] [modec args...]

For taking a failure apart rather than counting it.  suite.py reuses one
tag per test, so each call overwrites the last one's stack log; here
every call has its own (recordings/bench/sweep-probe-NAME-i.log, with
modec's slow-block lines in it), its own I/O trace when IO_TRACE=1
(...-io.txt, see scripts/diag/iotrace.py), and a line of JSON on stdout:
what each end made of the call, the payload lines intact each way, how
often the cushion keeper wrote silence, the slow blocks, and the load
the machine was under.  The rate is 14400 pinned unless 'auto', which
leaves the reference in automode and modec offering all of V.32bis.
"""
import os, sys, json, re
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sweep
from sweep import one, decode_v1
from ber import score

def main(argv):
    if len(argv) < 2:
        print(__doc__); return
    name, n = argv[0], int(argv[1])
    orig = (argv[2] if len(argv) > 2 else 'originate') == 'originate'
    auto = (argv[3] if len(argv) > 3 else 'pinned') == 'auto'
    extra = argv[4:]
    ms = 'AT+MS=V32B,1,4800,14400' if auto else 'AT+MS=V32B,0,14400,14400'
    margs = extra if auto else ['--v32-rate', '14400'] + extra
    SC = sweep.SC
    for i in range(n):
        tag = f'probe-{name}-{i}'
        if os.environ.get('IO_TRACE'):
            os.environ['MODEC_IO_TRACE'] = f'{SC}/sweep-{tag}-io.txt'
        load0 = os.getloadavg()[0]
        r = one(tag, 'v32bis', ms, margs, ['ATW2'], window=16, taketurns=True, originate=orig, after=['AT&V1'])
        a = b = 0
        if r.get('ref') == 'CONNECT':
            try:
                a = score(sweep.PAY_A, open(f'{SC}/sweep-{tag}-at-ref.bin', 'rb').read(), 'A')['intact']
                b = score(sweep.PAY_B, open(f'{SC}/sweep-{tag}-at-modec.bin', 'rb').read(), 'B')['intact']
            except FileNotFoundError:
                pass
        v1 = decode_v1(r.get('after', {}).get('AT&V1'))
        try:
            lg = open(f'{SC}/sweep-{tag}.log', 'rb').read().decode('latin1')
        except FileNotFoundError:
            lg = ''
        slow = [int(x) for x in re.findall(r'slow block: (\d+)', lg)]
        print(json.dumps({
            'tag': tag, 'ref': r.get('ref'), 'rate': r.get('modec_rate'), 'a': a, 'b': b,
            'retrains': r.get('modec_retrains'), 'end': r.get('modec_end'),
            'ref_end': v1.get('v1_end'), 'ref_quality': v1.get('v1_quality'),
            'cushion': len(re.findall('transmit cushion:', lg)),
            'slow': len(slow), 'slowest_ms': max(slow) if slow else 0,
            'load': round(max(load0, os.getloadavg()[0]), 1),
            'log': r.get('call_log')}), flush=True)

if __name__ == '__main__':
    main(sys.argv[1:])
