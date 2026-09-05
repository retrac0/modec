#!/usr/bin/env bash
# Exercises `modec modem --sip` end to end without baresip or a provider:
# a Python script plays the role of two baresip ctrl_tcp servers (one per
# modem) whose calls are joined to each other, while the audio runs
# through FIFOs as in smoke-loopback.sh.  Flow: B dials, A gets RING,
# A answers (ATA), both get CONNECT 2400, text passes both ways, B hangs
# up, A gets NO CARRIER.
set -euo pipefail
BIN=${BIN:-$(cabal list-bin modec)}
W=$(mktemp -d)
trap 'kill $(jobs -p) 2>/dev/null || true; rm -rf "$W"' EXIT
mkfifo "$W/a2b" "$W/b2a"
CA=${CA:-24446}   # fake ctrl_tcp for A
CB=${CB:-24447}   # fake ctrl_tcp for B
PA=${PA:-24448}   # telnet for A
PB=${PB:-24449}   # telnet for B

python3 - "$CA" "$CB" "$PA" "$PB" "$W/result.txt" <<'EOF' &
import socket, sys, time, select, json, re
ca, cb, pa, pb, out = int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]), sys.argv[5]

def netstring(b): return f"{len(b)}:".encode() + b + b","
def parse(buf):
    msgs = []
    while True:
        m = re.match(rb"(\d+):", buf)
        if not m: break
        n = int(m.group(1)); s = m.end()
        if len(buf) < s + n + 1: break
        msgs.append(buf[s:s+n]); buf = buf[s+n+1:]
    return msgs, buf

class Fake:
    """One fake baresip: accepts a ctrl_tcp connection from modec."""
    def __init__(self, port, name):
        self.name = name
        self.srv = socket.socket(); self.srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.srv.bind(("127.0.0.1", port)); self.srv.listen(1)
        self.conn = None; self.buf = b""; self.commands = []
    def accept(self):
        self.conn, _ = self.srv.accept(); self.conn.setblocking(False)
    def event(self, typ, **kw):
        d = {"event": True, "class": "call", "type": typ, "param": ""}; d.update(kw)
        self.conn.sendall(netstring(json.dumps(d).encode()))
    def pump(self):
        try:
            d = self.conn.recv(4096)
            if d: self.buf += d
        except BlockingIOError:
            return
        msgs, self.buf = parse(self.buf)
        for m in msgs:
            j = json.loads(m)
            self.commands.append(j)
            resp = {"response": True, "ok": True, "data": "", "token": j.get("token", "")}
            self.conn.sendall(netstring(json.dumps(resp).encode()))

A = Fake(ca, "A"); B = Fake(cb, "B")
A.accept(); B.accept()
# telnet clients to the modems' data sides
def strip(buf):
    i = 0; p = b""
    while i < len(buf):
        if buf[i] == 255 and i + 1 < len(buf) and buf[i+1] == 255: p += b"\xff"; i += 2
        elif buf[i] == 255: i += 3
        else: p += bytes([buf[i]]); i += 1
    return p
def connect_retry(port):
    for _ in range(200):
        try: return socket.create_connection(("127.0.0.1", port))
        except OSError: time.sleep(0.1)
    raise SystemExit("telnet port %d never opened" % port)
TA = connect_retry(pa); TB = connect_retry(pb)
TA.setblocking(False); TB.setblocking(False)
dta = b""; dtb = b""
def pump_all(timeout=0.05):
    global dta, dtb
    A.pump(); B.pump()
    r, _, _ = select.select([TA, TB], [], [], timeout)
    for s in r:
        d = s.recv(4096)
        if s is TA: dta += d
        else: dtb += d
def waitfor(which, token, limit, hook=None):
    t0 = time.time()
    while time.time() - t0 < limit:
        pump_all()
        if hook: hook()
        if token in strip(dta if which == "A" else dtb): return True
    return False
res = []
def cmd_seen(fake, name): return any(c.get("command") == name for c in fake.commands)
# B dials
TB.sendall(b"ATDT5551234\r")
res.append(("B dial command reached baresip", waitfor("B", b"\x00NEVER", 3, hook=lambda: None) or cmd_seen(B, "dial")))
if cmd_seen(B, "dial"):
    A.event("CALL_INCOMING", direction="incoming", peeruri="sip:5551234@test")
res.append(("A RING", waitfor("A", b"RING", 5)))
TA.sendall(b"ATA\r")
res.append(("A accept command reached baresip", waitfor("A", b"\x00NEVER", 3) or cmd_seen(A, "accept")))
if cmd_seen(A, "accept"):
    A.event("CALL_ESTABLISHED", direction="incoming", peeruri="sip:5551234@test")
    B.event("CALL_ESTABLISHED", direction="outgoing", peeruri="sip:5551234@test")
res.append(("B CONNECT", waitfor("B", b"CONNECT", 90)))
res.append(("A CONNECT", waitfor("A", b"CONNECT", 90)))
dta = b""; dtb = b""
time.sleep(1.5)
TB.sendall(b"hello from B\r\n")
res.append(("A got B text", waitfor("A", b"hello from B", 60)))
TA.sendall(b"hello from A\r\n")
res.append(("B got A text", waitfor("B", b"hello from A", 60)))
time.sleep(1.5)
TB.sendall(b"+++")
res.append(("B escape", waitfor("B", b"OK", 30)))
TB.sendall(b"ATH\r")
res.append(("B hangup reached baresip", waitfor("B", b"\x00NEVER", 3) or cmd_seen(B, "hangup")))
if cmd_seen(B, "hangup"):
    B.event("CALL_CLOSED", param="Bye"); A.event("CALL_CLOSED", param="Bye")
res.append(("A NO CARRIER", waitfor("A", b"NO CARRIER", 30)))
with open(out, "w") as f:
    for name, okk in res: f.write(f"{name}: {'ok' if okk else 'FAILED'}\n")
    f.write("A commands: " + ",".join(c.get("command","") for c in A.commands) + "\n")
    f.write("B commands: " + ",".join(c.get("command","") for c in B.commands) + "\n")
EOF
sleep 0.5
# --answer/--originate only fix the FIFO open order here; in SIP mode the
# role of each call comes from who dialled
"$BIN" modem --sip "127.0.0.1:$CA" --sip-domain test --answer --audio-in "$W/b2a" --audio-out "$W/a2b" --listen "$PA" 2> "$W/a.log" &
"$BIN" modem --sip "127.0.0.1:$CB" --sip-domain test --originate --audio-in "$W/a2b" --audio-out "$W/b2a" --listen "$PB" 2> "$W/o.log" &
wait %1 || echo "driver failed"
echo "--- results"; cat "$W/result.txt" 2>/dev/null || echo "(no results)"
echo "--- A log"; grep -vE "^modec: [0-9.]+ tx" "$W/a.log" | head -12
echo "--- B log"; grep -vE "^modec: [0-9.]+ tx" "$W/o.log" | head -12
grep -q FAILED "$W/result.txt" && { echo "SMOKE-SIP FAILED"; exit 1; } || echo "SMOKE-SIP OK"
