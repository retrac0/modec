#!/usr/bin/env bash
# Two live modems cross-connected through FIFOs carrying raw 8 kHz audio,
# an answering side serving telnet, an originating side on stdio.  Text is
# pushed both ways and compared.  No sound card, no clock: the FIFOs pace
# the loop, so this runs as fast as the CPU allows.
set -euo pipefail
BIN=${BIN:-$(cabal list-bin modec)}
W=$(mktemp -d)
trap 'kill $(jobs -p) 2>/dev/null || true; rm -rf "$W"' EXIT
mkfifo "$W/a2b" "$W/b2a"
PORT=${PORT:-24242}

printf 'From the caller: the quick brown fox. 0123456789\r\n' > "$W/o_in.txt"
ANSWER_TEXT='From the answerer: lazy dogs and IAC bytes'

# answering modem: audio a2b (out) / b2a (in), telnet server
"$BIN" modem --answer --audio-in "$W/b2a" --audio-out "$W/a2b" --listen "$PORT" 2> "$W/a.log" &
# originating modem: audio b2a (out) / a2b (in), data on stdio
"$BIN" modem --originate --audio-in "$W/a2b" --audio-out "$W/b2a" --data-stdio < "$W/o_in.txt" > "$W/o_out.txt" 2> "$W/o.log" &
# the FIFO opens block until both sides are up, only then does the answerer listen
for i in $(seq 1 50); do grep -q listening "$W/a.log" && break; sleep 0.1; done
grep -q listening "$W/a.log" || { echo "answerer did not start"; cat "$W/a.log" "$W/o.log"; exit 1; }

# telnet client to the answering modem: send text, collect what arrives
python3 - "$PORT" "$ANSWER_TEXT" "$W/a_rx.txt" <<'EOF'
import socket, sys, time
port, text, out = int(sys.argv[1]), sys.argv[2], sys.argv[3]
s = socket.create_connection(("127.0.0.1", port))
s.settimeout(0.5)
buf = b""
start = time.time()
sent = False
while time.time() - start < 60:
    try:
        d = s.recv(4096)
        if not d:
            break
        buf += d
    except socket.timeout:
        pass
    # send once the negotiation has arrived (server sends its hello immediately)
    if not sent and time.time() - start > 8:
        s.sendall(text.encode() + b"\xff\xff\r\n")   # 0xFF is escaped as IAC IAC
        sent = True
    if b"0123456789" in buf and time.time() - start > 12:
        break
# strip telnet negotiation (IAC x y triples)
i = 0; payload = b""
while i < len(buf):
    if buf[i] == 255 and i + 1 < len(buf) and buf[i+1] == 255:
        payload += b"\xff"; i += 2
    elif buf[i] == 255:
        i += 3
    else:
        payload += bytes([buf[i]]); i += 1
open(out, "wb").write(payload)
s.close()
EOF

sleep 1
echo "--- answer log";   cat "$W/a.log"
echo "--- originate log"; cat "$W/o.log"
echo "--- received by telnet client (from originate):"; cat "$W/a_rx.txt"; echo
echo "--- received by originate stdio (from answerer):"; od -c "$W/o_out.txt" | head -5
grep -q "the quick brown fox" "$W/a_rx.txt" && grep -q "lazy dogs" "$W/o_out.txt" && grep -q $'\xff' "$W/o_out.txt" && echo "SMOKE OK" || { echo "SMOKE FAILED"; exit 1; }
