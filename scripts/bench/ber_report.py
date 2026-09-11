#!/usr/bin/env python3
"""Tables from recordings/bench/ber.csv, with the textbook curve beside them.

    scripts/bench/ber_report.py            # every mode in the CSV
    scripts/bench/ber_report.py v22bis     # one
"""
import csv, math, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import theory

def fmt(v):
    try: v = float(v)
    except (TypeError, ValueError): return '   -   '
    if math.isnan(v): return '   -   '
    if v == 0: return '  0    '
    return f'{v:7.1e}'

rows = list(csv.DictReader(open('recordings/bench/ber.csv')))
want = sys.argv[1:] or []
modes = []
for r in rows:
    if r['mode'] not in modes and (not want or r['mode'] in want): modes.append(r['mode'])
for mode in modes:
    rs = [r for r in rows if r['mode'] == mode]
    print(f'\n=== {mode}   (Eb/N0 = SNR {10*math.log10(4000/int(rs[0]["rate"])):+.1f} dB)')
    print('  SNR   textbook BER | ref<-modec: lines intact  CER      | modec<-ref: lines intact  CER      | modec EVM  slicer SNR | outcome')
    for r in rs:
        snr = float(r['snr'])
        th = theory.ber(mode, snr)
        def cell(side):
            if r.get(f'{side}_lines'):
                return f"{r[f'{side}_intact']:>4}/{r[f'{side}_lines']:<4}  {fmt(r[f'{side}_cer'])}"
            return '     -           -    '
        evm = r.get('modec_evm')
        try:
            e = float(evm); sl = f'{e:7.4f}   {-10*math.log10(e):5.1f} dB' if e > 0 and not math.isnan(e) else '    -          -   '
        except (TypeError, ValueError): sl = '    -          -   '
        out = r['ref'] if r['ref'] != 'CONNECT' else ('ok' if r.get('modec_connect') == 'True' else 'ref only')
        print(f'  {snr:4.0f}   {fmt(th)}   | {cell("ref")} | {cell("modec")} | {sl} | {out}')
