#!/bin/bash

if [ $# -ne 1 ]; then
  cat <<EOF

  Syntax: $(basename $0) <sdl-src-root>

      e.g. $(basename $0) src/SDL
EOF
  exit 1
fi

SRCPATH="$1"

TMPFILE_RAW=$(mktemp)
TMPFILE_HS=$(mktemp)

find "$SRCPATH/Raw" -name \*.hs \
  -exec sed -n -e 's/^.*ccall.*SDL_\([[:alpha:]]\)\([^ "]\+\).*$/\L\1\E\2/p' '{}' + | \
  sort | uniq >> "$TMPFILE_RAW"

find "$SRCPATH" -name \*.hs -and -not -path \*Raw\* \
  -exec sed -n -e 's/^\s*\([^ ]\+\) ::.*$/\1/p' '{}' + | \
  sort | uniq >> "$TMPFILE_HS"

# Sorry for the python ;-)
python <<EOF

with open("$TMPFILE_RAW") as f:
  sdl_raw = f.readlines()

  with open("$TMPFILE_HS") as f:
    sdl_hs  = f.readlines()

    for sym in sorted(set(sdl_raw) - set(sdl_hs)):
      print sym.strip()

EOF

rm -f "$TMPFILE_RAW"
rm -f "$TMPFILE_HS"
