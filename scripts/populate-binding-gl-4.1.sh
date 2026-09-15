#!/usr/bin/env bash
set -euo pipefail

# Maintainer helper for the graphics-stack workspace layout.
# Run from this repository after `rebar3 escriptize`.
# It writes gl.erl, gl.hrl, and gl.c into the generator root, then copies
# them into the sibling opengl-4.1 repository.

cd "$(dirname "$0")/.."

./_build/default/bin/opengl_gen gl 4.1
cp gl.erl ../opengl-4.1/src/gl.erl
cp gl.hrl ../opengl-4.1/include/gl.hrl
cp gl.c  ../opengl-4.1/c_src/gl.c
