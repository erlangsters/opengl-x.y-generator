#!/usr/bin/env bash
set -euo pipefail

# Maintainer helper for the graphics-stack workspace layout.
# Run from this repository after `rebar3 escriptize`.
# It writes gl.erl, gl.hrl, and gl.c into the generator root, then copies
# them into the sibling opengl-3.3 repository.

cd "$(dirname "$0")/.."

./_build/default/bin/opengl_gen gl 3.3
cp gl.erl ../opengl-3.3/src/gl.erl
cp gl.hrl ../opengl-3.3/include/gl.hrl
cp gl.c  ../opengl-3.3/c_src/gl.c
