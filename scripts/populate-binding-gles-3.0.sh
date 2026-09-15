#!/usr/bin/env bash
set -euo pipefail

# Maintainer helper for the graphics-stack workspace layout.
# Run from this repository after `rebar3 escriptize`.
# It writes gl.erl, gl.hrl, and gl.c into the generator root, then copies
# them into the sibling opengl-es-3.0 repository.

cd "$(dirname "$0")/.."

./_build/default/bin/opengl_gen gles 3.0
cp gl.erl ../opengl-es-3.0/src/gl.erl
cp gl.hrl ../opengl-es-3.0/include/gl.hrl
cp gl.c  ../opengl-es-3.0/c_src/gl.c
