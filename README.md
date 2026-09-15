# OpenGL binding generator for the BEAM

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-opengl--x--y--generator-%23a90432)](https://github.com/erlangsters/opengl-x.y-generator)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-28-%23a90432)
![Current Version](https://img.shields.io/badge/version-0.1.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/opengl-x.y-generator)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/opengl-x.y-generator/build.yml)](https://github.com/erlangsters/opengl-x.y-generator/actions/workflows/build.yml)

> [!WARNING]
> This repository is still in development. Until release tags are cut, the
> `master` branch may be rewound.

The OpenGL binding generator for the Erlang and Elixir programming language. Generated bindings work exclusively with the EGL binding (and have an internal dependency on it).

The generator reads the Khronos OpenGL XML registry plus explicit binding specifications and writes:

- `gl.erl`
- `gl.hrl`
- `gl.c`

Generated target repositories are hosted separately:

- OpenGL: [3.3](https://github.com/erlangsters/opengl-3.3),
  [4.1](https://github.com/erlangsters/opengl-4.1),
  [4.6](https://github.com/erlangsters/opengl-4.6)
- OpenGL ES: [2.0](https://github.com/erlangsters/opengl-es-2.0),
  [3.0](https://github.com/erlangsters/opengl-es-3.0),
  [3.1](https://github.com/erlangsters/opengl-es-3.1),
  [3.2](https://github.com/erlangsters/opengl-es-3.2)

Generated bindings use the [EGL binding](https://github.com/erlangsters/egl-1.5) for context and surface management.

Written by the Erlangsters [community](https://about.erlangsters.org/) and released under the MIT [license](https://opensource.org/license/mit).

## Getting a binding

Build the generator:

```bash
rebar3 escriptize
```

Generate one target:

```bash
./_build/default/bin/opengl_gen gl 4.6
./_build/default/bin/opengl_gen gles 3.2
```

Supported generator targets are:

- `gl 3.3`
- `gl 4.1`
- `gl 4.6`
- `gles 2.0`
- `gles 3.0`
- `gles 3.1`
- `gles 3.2`

The generator writes `gl.erl`, `gl.hrl`, and `gl.c` in the current working directory.

## Populate a target repository

The `scripts/populate-binding-*` helpers are for the `graphics-stack` workspace layout. They generate the three files in this repository, then copy them into the sibling target clone.

```bash
./scripts/populate-binding-gl-4.6.sh
./scripts/populate-binding-gles-3.2.sh
```

Run them sequentially unless each run is isolated. Do not hand-edit generated files in the target repositories.

## Documentation

- [Documentation map](docs/README.md)
- [API mapping](docs/api-mapping.md)
- [Supported surface](docs/supported-surface.md)
- [Generator architecture](docs/generator-architecture.md)
- [Spec authoring](docs/spec-authoring.md)
- [Testing strategy](docs/testing.md)
- [Release workflow](docs/release-workflow.md)
- [Omissions and design-gated families](docs/omissions.md)

## Development rules

`binding-specs.conf` is the source of truth for generated public bindings.
Target repositories must not hand-edit generated `gl.erl`, `gl.hrl`, or
`gl.c`; fix the generator or binding specs, regenerate, and repopulate targets.
