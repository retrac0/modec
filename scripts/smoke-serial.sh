#!/usr/bin/env bash
# A voice-mode USB modem faked on a pseudo-terminal, with a calling modem
# behind it; modec answers on the "serial port".  Text goes both ways and
# is compared, once per sample format the dongle offers.  Everything the
# real part will exercise -- termios, the DLE shielding, the format, the
# AT dialogue, the block loop -- runs here; only the DAA is missing.
set -euo pipefail
BIN=${BIN:-$(cabal list-bin modec)}
W=$(mktemp -d)
trap 'kill $(jobs -p) 2>/dev/null || true; rm -rf "$W"' EXIT
printf 'From modec over the port: the quick brown fox. 0123456789\r\n' > "$W/in.txt"
FAR='From the far end: lazy dogs'

for FMT in ${FORMATS:-pcm14 ulaw alaw u8 s8}; do
  "$BIN" fake-dongle --format "$FMT" --say "$FAR"$'\r\n' --heard "$W/heard.$FMT" \
     --record "$W/line.$FMT.wav" --seconds 20 > "$W/pty.$FMT" 2> "$W/fake.$FMT.log" &
  for i in $(seq 1 50); do [ -s "$W/pty.$FMT" ] && break; sleep 0.1; done
  PTY=$(cat "$W/pty.$FMT")
  [ -n "$PTY" ] || { echo "no pseudo-terminal"; cat "$W/fake.$FMT.log"; exit 1; }
  timeout 120 "$BIN" modem --answer --audio-serial "$PTY" --audio-format "$FMT" --data-stdio --no-record \
     < "$W/in.txt" > "$W/out.$FMT" 2> "$W/modec.$FMT.log" || true
  wait
  if ! grep -q 0123456789 "$W/heard.$FMT"; then
    echo "FAIL $FMT: the far end did not hear modec"; cat "$W/fake.$FMT.log" "$W/modec.$FMT.log"; exit 1
  fi
  if ! grep -q "lazy dogs" "$W/out.$FMT"; then
    echo "FAIL $FMT: modec did not hear the far end"; cat "$W/fake.$FMT.log" "$W/modec.$FMT.log"; exit 1
  fi
  # the fake's recording of what modec sent, read back through the whole
  # modem as the calling side: the wire format cost nothing the far end
  # could not decode
  if ! "$BIN" replay "$W/line.$FMT.wav" 2> /dev/null | grep -q 0123456789; then
    echo "FAIL $FMT: the recording of the port does not replay"; exit 1
  fi
  echo "OK $FMT: $(grep CONNECT "$W/modec.$FMT.log" | head -1 | sed 's/modec: //'); the recording replays"
done
echo SMOKE OK
