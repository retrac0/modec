#!/usr/bin/env python3
"""The error-rate campaign as a page: measured against textbook, per mode.

Reads recordings/bench/ber.csv (and distort.csv if present) and the
offline survey's numbers, and writes one HTML file with the curves
drawn as inline SVG.  No library: the charts are a few hundred
coordinates each.

    scripts/bench/ber_html.py OUT.html
"""
import csv, math, os, re, sys, html
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import theory

SC = 'recordings/bench'
RATES = {'bell103':300,'v21':300,'bell212a':1200,'v22':1200,'v22bis':2400,'v32-4800':4800,'v32-9600':9600,
         'v32b-9600t':9600,'v32b-7200':7200,'v32b-12000':12000,'v32b-14400':14400}
NAMES = {'bell103':'Bell 103','v21':'V.21','bell212a':'Bell 212A','v22':'V.22','v22bis':'V.22bis',
         'v32-4800':'V.32 4800','v32-9600':'V.32 9600','v32b-9600t':'V.32 9600 trellis','v32b-7200':'V.32bis 7200',
         'v32b-12000':'V.32bis 12000','v32b-14400':'V.32bis 14400'}
MOD = {'bell103':'binary FSK, 300 baud, non-coherent','v21':'binary FSK, 300 baud, non-coherent',
       'bell212a':'4-DPSK, 600 baud','v22':'4-DPSK, 600 baud','v22bis':'16-QAM, 600 baud, differential quadrant',
       'v32-4800':'4 points, 2400 baud, differential','v32-9600':'16 points, 2400 baud, non-redundant',
       'v32b-9600t':'32 points, 2400 baud, 8-state trellis','v32b-7200':'16 points, 2400 baud, 8-state trellis',
       'v32b-12000':'64 points, 2400 baud, 8-state trellis','v32b-14400':'128 points, 2400 baud, 8-state trellis'}
SURVEY = {'v32-4800':'V32R4800','v32-9600':'V32R9600','v32b-9600t':'V32R9600T','v32b-7200':'V32R7200',
          'v32b-12000':'V32R12000','v32b-14400':'V32R14400'}
ORDER = ['bell103','v21','bell212a','v22','v22bis','v32-4800','v32-9600','v32b-9600t','v32b-7200','v32b-12000','v32b-14400']

def fnum(v):
    try: return float(v)
    except (TypeError, ValueError): return float('nan')

def load_csv(name, key=None):
    """Rows, with a later row for the same key replacing an earlier one:
    a re-run supersedes what it repeats."""
    p = f'{SC}/{name}'
    if not os.path.exists(p): return []
    rows = list(csv.DictReader(open(p)))
    if key is None: return rows
    seen = {}
    for r in rows: seen[tuple(r[k] for k in key)] = r
    return list(seen.values())

def survey_points():
    """(snr, ber) per rate from the offline survey: errors in 4000 bits."""
    out = {}
    p = f'{SC}/v32-survey.txt'
    if not os.path.exists(p): return out
    cur = None
    for ln in open(p):
        m = re.match(r'=== (V32R\w+)', ln)
        if m: cur = m.group(1); out[cur] = []; continue
        m = re.match(r'\s+SNR ([0-9.]+)\s+(ok|(\d+) errors)', ln)
        if m and cur:
            errs = 0 if m.group(2) == 'ok' else int(m.group(3))
            out[cur].append((float(m.group(1)), errs / 3800.0))
    return out

def cross(points, level):
    """SNR at which a falling curve of (snr, cer) crosses `level`, by
    log interpolation; None if it never does inside the points."""
    pts = sorted(points)
    for (s0, c0), (s1, c1) in zip(pts, pts[1:]):
        lo, hi = (c1, c0)
        if hi >= level > lo or (hi > level and lo == 0):
            if lo <= 0: lo = level / 10
            if hi <= 0: return None
            f = (math.log10(hi) - math.log10(level)) / (math.log10(hi) - math.log10(lo))
            return s0 + f * (s1 - s0)
    return None

def chart(mode, rows, surv):
    W, H = 640, 360; L, R, T, B = 58, 18, 18, 46
    snrs = [fnum(r['snr']) for r in rows]
    xmin = min(snrs + [theory.snr_for(mode, 1e-3) - 2]) - 1; xmax = max(s for s in snrs if s < 40) + 3 if any(s < 40 for s in snrs) else max(snrs) + 3
    ymin, ymax = 1e-4, 1.0
    def X(s): return L + (s - xmin) / (xmax - xmin) * (W - L - R)
    def Y(c): c = max(ymin, min(ymax, c)); return T + (math.log10(ymax) - math.log10(c)) / (math.log10(ymax) - math.log10(ymin)) * (H - T - B)
    g = []
    # grid: decades
    for d in range(0, 5):
        c = 10 ** (-d); y = Y(c)
        g.append(f'<line x1="{L}" y1="{y:.1f}" x2="{W-R}" y2="{y:.1f}" class="grid"/>')
        g.append(f'<text x="{L-6}" y="{y+4:.1f}" class="tick" text-anchor="end">{"1" if d==0 else "10⁻"+"¹²³⁴"[d-1]}</text>')
    step = 2 if xmax - xmin <= 14 else 4
    s = math.ceil(xmin / step) * step
    while s <= xmax:
        x = X(s); g.append(f'<line x1="{x:.1f}" y1="{T}" x2="{x:.1f}" y2="{H-B}" class="grid"/>')
        g.append(f'<text x="{x:.1f}" y="{H-B+16}" class="tick" text-anchor="middle">{s:g}</text>'); s += step
    g.append(f'<text x="{(L+W-R)/2:.0f}" y="{H-8}" class="axis" text-anchor="middle">signal to noise, dB (white noise over 0–4 kHz)</text>')
    g.append(f'<text transform="translate(14,{(T+H-B)/2:.0f}) rotate(-90)" class="axis" text-anchor="middle">character error rate</text>')
    # theory: CER ~ 10 x BER, drawn as a line
    pts = []
    s = xmin
    while s <= xmax:
        cer = min(1.0, 10 * theory.ber(mode, s))
        if cer >= ymin: pts.append(f'{X(s):.1f},{Y(cer):.1f}')
        s += 0.25
    if pts: g.append(f'<polyline points="{" ".join(pts)}" class="theory"/>')
    # survey (modec in the simulator), as a dashed line through its points
    sp = surv.get(SURVEY.get(mode, ''), [])
    if sp:
        pts = [f'{X(s):.1f},{Y(max(ymin/2, 10*b)):.1f}' for s, b in sorted(sp) if xmin <= s <= xmax]
        if len(pts) > 1: g.append(f'<polyline points="{" ".join(pts)}" class="survey"/>')
        for s_, b in sp:
            if xmin <= s_ <= xmax: g.append(f'<circle cx="{X(s_):.1f}" cy="{Y(max(ymin/2,10*b)):.1f}" r="3" class="survey-pt"/>')
    # measured
    for side, cls, shape in (('ref', 'ref', 'circle'), ('modec', 'modec', 'rect')):
        line = []
        for r in sorted(rows, key=lambda r: fnum(r['snr'])):
            s_ = fnum(r['snr']);
            if s_ >= 40: continue
            if r['ref'] != 'CONNECT' or (side == 'modec' and r.get('modec_connect') != 'True'):
                g.append(f'<text x="{X(s_):.1f}" y="{T+12}" class="{cls}-x" text-anchor="middle">×</text>'); continue
            cer = fnum(r.get(f'{side}_cer')); chars = fnum(r.get(f'{side}_chars'))
            lost = fnum(r.get(f'{side}_lost')); lines = fnum(r.get(f'{side}_lines'))
            if math.isnan(cer): continue
            if cer <= 0:
                floor = 1 / max(chars, 1)
                y = Y(max(ymin, floor))
                g.append(f'<text x="{X(s_):.1f}" y="{y+5:.1f}" class="{cls}-floor" text-anchor="middle">↓</text>')
                line.append((s_, floor))
                continue
            x, y = X(s_), Y(cer)
            if shape == 'circle': g.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="4.5" class="{cls}-pt"/>')
            else: g.append(f'<rect x="{x-4:.1f}" y="{y-4:.1f}" width="8" height="8" class="{cls}-pt"/>')
            if lost > 0:
                g.append(f'<text x="{x+7:.1f}" y="{y-6:.1f}" class="lost">{int(lost)}/{int(lines)} lost</text>')
            line.append((s_, cer))
        if len(line) > 1:
            g.append(f'<polyline points="{" ".join(f"{X(a):.1f},{Y(b):.1f}" for a,b in sorted(line))}" class="{cls}-line"/>')
    return f'<svg viewBox="0 0 {W} {H}" role="img" aria-label="error rate against signal to noise, {NAMES[mode]}">{"".join(g)}</svg>'

def main(out):
    rows = load_csv('ber.csv', ('mode', 'snr')); surv = survey_points(); dist = load_csv('distort.csv', ('mode', 'cond'))
    modes = [m for m in ORDER if any(r['mode'] == m for r in rows)]
    # summary: SNR at 1 % character errors
    summ = []
    for m in modes:
        rs = [r for r in rows if r['mode'] == m and r['ref'] == 'CONNECT']
        th = theory.snr_for(m, 1e-3)   # CER 1e-2 ~ BER 1e-3
        def meas(side):
            pts = [(fnum(r['snr']), fnum(r.get(f'{side}_cer'))) for r in rs if not math.isnan(fnum(r.get(f'{side}_cer')))]
            pts = [(s, c) for s, c in pts if s < 40]
            return cross(pts, 1e-2), pts
        rx, rpts = meas('ref'); mx, mpts = meas('modec')
        sv = surv.get(SURVEY.get(m, ''), [])
        svx = cross([(s, 10 * b) for s, b in sv], 1e-2) if sv else None
        summ.append((m, th, rx, mx, svx, rpts, mpts))
    def fmt(v, th=None):
        if v is None: return '<td class="num">—</td>'
        loss = f' <span class="loss">{v-th:+.1f}</span>' if th is not None else ''
        return f'<td class="num">{v:.1f}{loss}</td>'
    summary_rows = ''.join(
        f'<tr><th scope="row">{NAMES[m]}</th><td class="mod">{MOD[m]}</td>{fmt(th)}{fmt(rx, th)}{fmt(mx, th)}{fmt(svx, th)}</tr>'
        for m, th, rx, mx, svx, _, _ in summ)
    sections = []
    for m, th, rx, mx, svx, rpts, mpts in summ:
        rs = sorted([r for r in rows if r['mode'] == m], key=lambda r: fnum(r['snr']))
        trs = []
        for r in rs:
            s_ = fnum(r['snr'])
            def cell(side):
                if r['ref'] != 'CONNECT': return '<td class="num">—</td><td class="num">—</td>'
                if side == 'modec' and r.get('modec_connect') != 'True': return '<td class="num">no connect</td><td class="num">—</td>'
                c = fnum(r.get(f'{side}_cer')); i = r.get(f'{side}_intact'); n = r.get(f'{side}_lines')
                cs = f'{c:.1e}' if c > 0 else f'&lt;{1/max(1,fnum(r.get(f"{side}_chars"))):.0e}'
                return f'<td class="num">{i}/{n}</td><td class="num">{cs}</td>'
            evm = fnum(r.get('modec_evm'))
            ev = f'{evm:.4f} <span class="dim">({-10*math.log10(evm):.1f} dB)</span>' if evm > 0 and not math.isnan(evm) else '—'
            trs.append(f'<tr><td class="num">{s_:g}</td><td class="num">{10*theory.ber(m, s_):.1e}</td>{cell("ref")}{cell("modec")}<td class="num">{ev}</td></tr>')
        sections.append(f'''
<section class="mode" id="{m}">
  <h2>{NAMES[m]} <span class="mod">{MOD[m]}</span></h2>
  <figure>{chart(m, rs, surv)}
    <figcaption>Eb/N0 = SNR {10*math.log10(4000/RATES[m]):+.1f} dB at {RATES[m]} bit/s. Arrows mark calls with no errors at all, drawn at one over the characters received; × marks a call that did not connect or dropped.</figcaption></figure>
  <div class="tbl"><table>
    <thead><tr><th>SNR dB</th><th>textbook CER</th><th colspan="2">reference reads modec</th><th colspan="2">modec reads reference</th><th>modec decision error</th></tr>
    <tr class="sub"><th></th><th></th><th>lines intact</th><th>CER</th><th>lines intact</th><th>CER</th><th></th></tr></thead>
    <tbody>{"".join(trs)}</tbody></table></div>
</section>''')
    # distortions
    dist_html = ''
    if dist:
        dmodes = [m for m in ORDER if any(r['mode'] == m for r in dist)]
        conds = []
        for r in dist:
            if r['cond'] not in conds: conds.append(r['cond'])
        head = ''.join(f'<th colspan="2">{NAMES[m]}</th>' for m in dmodes)
        sub = ''.join('<th>ref</th><th>modec</th>' for _ in dmodes)
        body = []
        for c in conds:
            cells = []
            for m in dmodes:
                r = next((x for x in dist if x['mode'] == m and x['cond'] == c), None)
                for side in ('ref', 'modec'):
                    if r is None: cells.append('<td class="num">·</td>'); continue
                    if r['ref'] != 'CONNECT' or (side == 'modec' and r.get('modec_connect') != 'True'):
                        cells.append('<td class="num bad">no link</td>'); continue
                    cer = fnum(r.get(f'{side}_cer')); lost = fnum(r.get(f'{side}_lost'))
                    if math.isnan(cer): cells.append('<td class="num">·</td>'); continue
                    cls = 'ok' if cer == 0 and lost == 0 else ('warn' if cer < 0.01 and lost <= 1 else 'bad')
                    txt = 'clean' if cer == 0 and lost == 0 else (f'{cer:.0e}' + (f' <span class="dim">{int(lost)} lost</span>' if lost else ''))
                    cells.append(f'<td class="num {cls}">{txt}</td>')
            imp = next((x['impair'] for x in dist if x['cond'] == c), '')
            body.append(f'<tr><th scope="row">{c}<span class="dim mono"> {html.escape(imp)}</span></th>{"".join(cells)}</tr>')
        dist_html = f'''
<section id="distortion">
  <h2>Every other impairment, live</h2>
  <p>One impairment at a time on both directions, no added noise, the same payload each way. Where the offline survey sweeps the same axis the value here is the survey's. <em>clean</em> is every line intact; a rate is the character error rate among the lines that arrived, with lines that never arrived counted beside it.</p>
  <div class="tbl"><table class="dist"><thead><tr><th>condition</th>{head}</tr><tr class="sub"><th></th>{sub}</tr></thead><tbody>{"".join(body)}</tbody></table></div>
</section>'''
    n_calls = len(rows)
    page = f'''<title>Waterfall Curves, Live</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Serif:wght@500;600&family=IBM+Plex+Sans:wght@400;500&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>
:root {{ --bg:#F3F5F7; --ink:#1B2330; --muted:#5C6773; --rule:#D5DAE0; --grid:#DFE3E8; --panel:#FFFFFF;
        --theory:#6B7480; --ref:#1F7A8C; --modec:#C2731A; --survey:#8A6BB5; --ok:#2E7D4F; --warn:#9A7A12; --bad:#B23A3A; }}
@media (prefers-color-scheme: dark) {{ :root:not([data-theme="light"]) {{ --bg:#14181C; --ink:#E6EAF0; --muted:#98A3AF; --rule:#2A323C; --grid:#232B34; --panel:#1B2127;
        --theory:#9AA3AE; --ref:#4FB3C6; --modec:#E39A4A; --survey:#B79BE0; --ok:#5DBF86; --warn:#D9B23A; --bad:#E06A6A; }} }}
:root[data-theme="dark"] {{ --bg:#14181C; --ink:#E6EAF0; --muted:#98A3AF; --rule:#2A323C; --grid:#232B34; --panel:#1B2127;
        --theory:#9AA3AE; --ref:#4FB3C6; --modec:#E39A4A; --survey:#B79BE0; --ok:#5DBF86; --warn:#D9B23A; --bad:#E06A6A; }}
body {{ background:var(--bg); color:var(--ink); font-family:"IBM Plex Sans", system-ui, sans-serif; font-size:15px; line-height:1.55; margin:0; }}
main {{ max-width: 980px; margin: 0 auto; padding: 32px 20px 64px; }}
h1, h2 {{ font-family:"IBM Plex Serif", Georgia, serif; font-weight:600; text-wrap:balance; letter-spacing:-0.01em; }}
h1 {{ font-size: 2rem; margin: 0 0 6px; }}
h2 {{ font-size: 1.3rem; margin: 40px 0 10px; }}
h2 .mod {{ font-family:"IBM Plex Sans", sans-serif; font-weight:400; font-size:0.9rem; color:var(--muted); margin-left:10px; }}
p {{ max-width: 68ch; }}
.eyebrow {{ font-family:"IBM Plex Mono", monospace; font-size:0.75rem; letter-spacing:0.08em; text-transform:uppercase; color:var(--muted); margin:0 0 8px; }}
.lede {{ font-size:1.05rem; color:var(--ink); }}
.key {{ display:flex; gap:22px; flex-wrap:wrap; font-size:0.85rem; color:var(--muted); margin: 6px 0 18px; }}
.key span::before {{ content:""; display:inline-block; width:22px; height:0; border-top:2.5px solid; margin-right:8px; vertical-align:middle; }}
.key .t::before {{ border-color:var(--theory); }} .key .r::before {{ border-color:var(--ref); }} .key .m::before {{ border-color:var(--modec); }} .key .s::before {{ border-color:var(--survey); border-top-style:dashed; }}
.tbl {{ overflow-x:auto; }}
table {{ border-collapse:collapse; width:100%; font-variant-numeric: tabular-nums; }}
th, td {{ text-align:left; padding: 7px 10px; border-bottom:1px solid var(--rule); vertical-align:top; }}
thead th {{ font-weight:500; color:var(--muted); font-size:0.82rem; }}
tr.sub th {{ padding-top:0; font-weight:400; }}
th[scope="row"] {{ font-weight:500; white-space:nowrap; }}
td.num, th.num {{ font-family:"IBM Plex Mono", monospace; font-size:0.86rem; }}
td.mod {{ color:var(--muted); font-size:0.86rem; }}
.loss {{ color:var(--muted); font-size:0.8rem; }}
.dim {{ color:var(--muted); }} .mono {{ font-family:"IBM Plex Mono", monospace; font-size:0.78rem; }}
td.ok {{ color:var(--ok); }} td.warn {{ color:var(--warn); }} td.bad {{ color:var(--bad); }}
figure {{ margin: 8px 0 14px; background:var(--panel); border:1px solid var(--rule); border-radius:4px; padding:10px 8px 4px; }}
figcaption {{ font-size:0.8rem; color:var(--muted); padding: 4px 8px 6px; }}
svg {{ width:100%; height:auto; display:block; font-family:"IBM Plex Mono", monospace; }}
.grid {{ stroke:var(--grid); stroke-width:1; }} .tick {{ fill:var(--muted); font-size:11px; }} .axis {{ fill:var(--muted); font-size:11px; }}
.theory {{ fill:none; stroke:var(--theory); stroke-width:2; }}
.survey {{ fill:none; stroke:var(--survey); stroke-width:1.5; stroke-dasharray:5 4; }} .survey-pt {{ fill:var(--survey); }}
.ref-line {{ fill:none; stroke:var(--ref); stroke-width:1.5; opacity:0.7; }} .ref-pt {{ fill:var(--ref); }} .ref-floor {{ fill:var(--ref); font-size:14px; }} .ref-x {{ fill:var(--ref); font-size:16px; }}
.modec-line {{ fill:none; stroke:var(--modec); stroke-width:1.5; opacity:0.7; }} .modec-pt {{ fill:var(--modec); }} .modec-floor {{ fill:var(--modec); font-size:14px; }} .modec-x {{ fill:var(--modec); font-size:16px; }}
.lost {{ fill:var(--muted); font-size:10px; }}
.summary td.num {{ white-space:nowrap; }}
@media (prefers-reduced-motion: reduce) {{ * {{ animation:none !important; transition:none !important; }} }}
</style>
<main>
<p class="eyebrow">modec against a Conexant CX93001, through an HT802V2 and baresip · {n_calls} calls · 2026-09-11</p>
<h1>Waterfall Curves, Live</h1>
<p class="lede">Every mode both modems share, driven down through white noise on a real call, with error control off and a known text going each way. The reference modem reads what modec sends; modec reads what the reference sends. Beside each: the textbook curve for the modulation, and — for the V.32 rates — modec's own receiver on the simulator's ideal line.</p>
<div class="key"><span class="t">textbook (AWGN, ideal receiver)</span><span class="r">reference modem reading modec</span><span class="m">modec reading the reference</span><span class="s">modec, offline survey</span></div>
<h2>Where 1 % of characters go wrong</h2>
<p>The signal-to-noise ratio at which one character in a hundred arrives wrong, read off each curve; the small number is the distance from the textbook, which is the implementation loss of that direction's receiver plus what the line itself adds. A dash means the curve never crossed inside the range tried.</p>
<div class="tbl"><table class="summary"><thead><tr><th>mode</th><th>modulation</th><th>textbook</th><th>reference reads modec</th><th>modec reads reference</th><th>modec, simulator</th></tr></thead><tbody>{summary_rows}</tbody></table></div>
<p>The noise is white over 0–4 kHz and set against the signal's level, the convention <code>Modec.Channel</code> and <code>modec-bench</code> use, so Eb/N0 is the SNR plus 10 log₁₀(4000 / bit rate). Textbook CER is taken as ten times the bit error rate, one wrong bit per wrong ten-bit character; the start-stop framer makes it worse than that in practice, on both modems, because one wrong bit can misalign the characters after it.</p>
{"".join(sections)}
{dist_html}
</main>
'''
    open(out, 'w').write(page)
    print(f'wrote {out}: {len(page)} bytes, {len(modes)} modes, {len(dist)} distortion rows')

if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else 'waterfall.html')
