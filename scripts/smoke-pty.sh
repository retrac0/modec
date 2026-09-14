#!/usr/bin/env bash
# A terminal program's view of modec: the calling modem is on a
# pseudo-terminal (--data-pty, linked to a fixed name), opened the way
# minicom or picocom opens a serial port; the answering modem is on
# telnet.  Audio goes through FIFOs, as in smoke-hayes.sh.
#
# Over the port: AT commands, +MS to hold the call to V.22, &V to read it
# back, a dial, CONNECT, text both ways, +++, and then the port is closed
# with the call still up -- DTR off, which under &D2 hangs up, so the
# answerer hears NO CARRIER.  The port is then opened again and answers AT.
set -euo pipefail
BIN=${BIN:-$(cabal list-bin modec)}
W=$(mktemp -d)
trap 'kill $(jobs -p) 2>/dev/null || true; rm -rf "$W"' EXIT
mkfifo "$W/a2b" "$W/b2a"
PA=${PA:-24254}
trap 'kill $(jobs -p) 2>/dev/null || true; rm -rf "$W"' EXIT INT TERM

"$BIN" modem --answer --hayes --audio-in "$W/b2a" --audio-out "$W/a2b" --listen "$PA" --no-record 2> "$W/a.log" &
"$BIN" modem --originate --hayes --audio-in "$W/a2b" --audio-out "$W/b2a" --pty-link "$W/ttyModem" --no-record > "$W/o.out" 2> "$W/o.log" &
for i in $(seq 1 50); do grep -q listening "$W/a.log" && [ -L "$W/ttyModem" ] && break; sleep 0.1; done
[ -L "$W/ttyModem" ] || { echo "no pty link"; cat "$W/o.log"; exit 1; }

python3 - "$PA" "$W/ttyModem" "$W/result.txt" <<'EOF'
import os, socket, sys, time, select, termios, tty
pa, link, out = int(sys.argv[1]), sys.argv[2], sys.argv[3]
def strip(buf):
    # drop telnet negotiation
    i = 0; p = b""
    while i < len(buf):
        if buf[i] == 255 and i + 1 < len(buf) and buf[i+1] == 255: p += b"\xff"; i += 2
        elif buf[i] == 255: i += 3
        else: p += bytes([buf[i]]); i += 1
    return p
def open_port():
    # as a terminal program does it: raw, and ignoring modem control lines
    fd = os.open(link, os.O_RDWR | os.O_NOCTTY)
    tty.setraw(fd)
    a = termios.tcgetattr(fd); a[2] |= termios.CLOCAL; termios.tcsetattr(fd, termios.TCSANOW, a)
    return fd
A = socket.create_connection(("127.0.0.1", pa)); A.setblocking(False)
P = open_port()
bufA = b""; bufP = b""
def pump(timeout):
    global bufA, bufP
    r, _, _ = select.select([A] + ([P] if P is not None else []), [], [], timeout)
    for s in r:
        if s is A: bufA += A.recv(4096)
        else:
            try: bufP += os.read(P, 4096)
            except OSError: pass
def waitfor(which, token, limit):
    t0 = time.time()
    while time.time() - t0 < limit:
        pump(0.2)
        buf = strip(bufA) if which == "A" else bufP
        if token in buf: return True
    return False
def send(b): os.write(P, b)
res = []
time.sleep(0.5)
send(b"AT\r"); res.append(("pty AT -> OK", waitfor("P", b"OK", 5)))
send(b"ATE0\r"); res.append(("pty ATE0 -> OK", waitfor("P", b"OK", 5)))
bufP = b""
send(b"AT+MS=V22,0\r"); res.append(("pty +MS -> OK", waitfor("P", b"OK", 5)))
bufP = b""
send(b"AT&V\r"); res.append(("pty &V shows +MS", waitfor("P", b"+MS=V22,0", 5)))
bufP = b""
send(b"ATS6=0S11=60\r"); res.append(("pty S6 S11 -> OK", waitfor("P", b"OK", 5)))
A.sendall(b"ATA\r")
time.sleep(0.3)
bufP = b""
send(b"ATDT12\r")
res.append(("pty CONNECT 1200", waitfor("P", b"CONNECT 1200", 90)))
res.append(("telnet CONNECT", waitfor("A", b"CONNECT", 90)))
bufA = b""; bufP = b""
time.sleep(1.5)
send(b"text from the pty\r\n")
res.append(("telnet got pty text", waitfor("A", b"text from the pty", 60)))
A.sendall(b"text from telnet\r\n")
res.append(("pty got telnet text", waitfor("P", b"text from telnet", 60)))
time.sleep(1.5)
bufP = b""
send(b"+++")
res.append(("pty escape OK", waitfor("P", b"OK", 60)))
bufP = b""
send(b"ATO\r")
res.append(("pty ATO -> CONNECT", waitfor("P", b"CONNECT 1200", 10)))
time.sleep(0.5)
os.close(P); P = None  # DTR off, call still up
bufA = b""
res.append(("closing the port hangs up", waitfor("A", b"NO CARRIER", 60)))
time.sleep(0.5)
P = open_port(); bufP = b""
send(b"AT\r"); res.append(("reopened port answers AT", waitfor("P", b"OK", 5)))
with open(out, "w") as f:
    for name, okk in res: f.write(f"{name}: {'ok' if okk else 'FAILED'}\n")
os.close(P); A.close()
EOF
echo "--- results"; cat "$W/result.txt"
echo "--- answer log"; grep -vE "^modec: [0-9.]+ tx" "$W/a.log" | head -12
echo "--- originate log"; grep -vE "^modec: [0-9.]+ tx" "$W/o.log" | head -16
grep -q "^/dev/pts/" "$W/o.out" || { echo "the pty path was not printed on stdout"; exit 1; }
grep -q FAILED "$W/result.txt" && { echo "SMOKE-PTY FAILED"; exit 1; } || echo "SMOKE-PTY OK"
