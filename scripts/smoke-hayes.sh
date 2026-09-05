#!/usr/bin/env bash
# Two modems in Hayes mode, cross-connected through FIFOs, each serving
# telnet.  A driver script answers on one side (ATA), dials on the other
# (ATD), waits for CONNECT on both, exchanges text, escapes with +++ and
# hangs up.  As with smoke-loopback.sh, the FIFOs pace the audio so this
# runs faster than real time.
set -euo pipefail
BIN=${BIN:-$(cabal list-bin modec)}
W=$(mktemp -d)
trap 'kill $(jobs -p) 2>/dev/null || true; rm -rf "$W"' EXIT
mkfifo "$W/a2b" "$W/b2a"
PA=${PA:-24244}
PB=${PB:-24245}

"$BIN" modem --answer --hayes --audio-in "$W/b2a" --audio-out "$W/a2b" --listen "$PA" 2> "$W/a.log" &
"$BIN" modem --originate --hayes --audio-in "$W/a2b" --audio-out "$W/b2a" --listen "$PB" 2> "$W/o.log" &
for i in $(seq 1 50); do grep -q listening "$W/a.log" && grep -q listening "$W/o.log" && break; sleep 0.1; done
grep -q listening "$W/a.log" || { echo "answerer did not start"; cat "$W/a.log"; exit 1; }

python3 - "$PA" "$PB" "$W/result.txt" <<'EOF'
import socket, sys, time, select
pa, pb, out = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
def strip(buf):
    # drop telnet negotiation
    i = 0; p = b""
    while i < len(buf):
        if buf[i] == 255 and i + 1 < len(buf) and buf[i+1] == 255: p += b"\xff"; i += 2
        elif buf[i] == 255: i += 3
        else: p += bytes([buf[i]]); i += 1
    return p
A = socket.create_connection(("127.0.0.1", pa)); B = socket.create_connection(("127.0.0.1", pb))
A.setblocking(False); B.setblocking(False)
bufA = b""; bufB = b""
def pump(timeout):
    global bufA, bufB
    r, _, _ = select.select([A, B], [], [], timeout)
    for s in r:
        d = s.recv(4096)
        if s is A: bufA += d
        else: bufB += d
def waitfor(which, token, limit):
    t0 = time.time()
    while time.time() - t0 < limit:
        pump(0.2)
        buf = strip(bufA if which == "A" else bufB)
        if token in buf: return True
    return False
res = []
A.sendall(b"AT\r"); res.append(("A AT -> OK", waitfor("A", b"OK", 5)))
B.sendall(b"ATE0\r"); res.append(("B ATE0 -> OK", waitfor("B", b"OK", 5)))
A.sendall(b"ATA\r")
time.sleep(0.3)
B.sendall(b"ATDT12\r")
res.append(("B CONNECT", waitfor("B", b"CONNECT", 90)))
res.append(("A CONNECT", waitfor("A", b"CONNECT", 90)))
bufA = b""; bufB = b""
time.sleep(1.5)
B.sendall(b"text from the caller\r\n")
res.append(("A got caller text", waitfor("A", b"text from the caller", 60)))
A.sendall(b"text from the answerer\r\n")
res.append(("B got answerer text", waitfor("B", b"text from the answerer", 60)))
time.sleep(1.5)
B.sendall(b"+++")
res.append(("B escape OK", waitfor("B", b"OK", 60)))
B.sendall(b"ATH\r")
res.append(("B ATH OK", waitfor("B", b"OK", 20)))
res.append(("A NO CARRIER", waitfor("A", b"NO CARRIER", 60)))
with open(out, "w") as f:
    for name, okk in res: f.write(f"{name}: {'ok' if okk else 'FAILED'}\n")
A.close(); B.close()
EOF
echo "--- results"; cat "$W/result.txt"
echo "--- answer log"; grep -vE "^modec: [0-9.]+ tx" "$W/a.log" | head -12
echo "--- originate log"; grep -vE "^modec: [0-9.]+ tx" "$W/o.log" | head -12
grep -q FAILED "$W/result.txt" && { echo "SMOKE-HAYES FAILED"; exit 1; } || echo "SMOKE-HAYES OK"
