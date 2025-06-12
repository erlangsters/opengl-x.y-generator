# OpenGL binding generator for the BEAM

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-opengl--x--y--generator-%23a90432)](https://github.com/erlangsters/opengl-x.y-generator)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-28-%23a90432)
![Current Version](https://img.shields.io/badge/version-0.1.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/opengl-4.6)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/opengl-4.6/workflow.yml)](https://github.com/erlangsters/opengl-4.6/actions/workflows/workflow.yml)

The OpenGL binding generator for the Erlang and Elixir programming language.
Generated bindings work exclusively with the
[EGL binding](https://github.com/erlangsters/egl-1-5) (and have an internal
dependency on it).

The generated bindings are hosted in those repositories for convenience.

- OpenGL: [3.3](https://github.com/erlangsters/opengl-3.3) |
  [4.1](https://github.com/erlangsters/opengl-4.1) |
  [4.6](https://github.com/erlangsters/opengl-4.6)
- OpenGL ES: [2.0](https://github.com/erlangsters/opengl-es-2.0) |
  [3.0](https://github.com/erlangsters/opengl-es-3.0) |
  [3.1](https://github.com/erlangsters/opengl-es-3.1) |
  [3.2](https://github.com/erlangsters/opengl-es-3.2)

You will need to pair up one of those bindings with the EGL binding (for a
OpenGL context and surface) and perhaps the GLFW binding (to provide a window).

Written by the Erlangsters [community](https://about.erlangsters.org/) and
released under the MIT [license](https://opensource.org/license/mit).

## Getting started

The generator is able to generate a binding for OpenGL version 3.3, 4.1 and
4.6, and all OpenGL ES versions.

> With little effort, it's technically easy to make it generate a binding for
other versions but this restriction exists for a reason: only a subset of the
available versions are relevant in today's world.

To generate a binding, run this command after it's compiled with
`rebar3 escriptize`.

```
./_build/default/bin/bin_opengl <api> <version>
```

Where `<api>` can be either `gl` or `gles`, and version is `x.z`.

It reads the `gl-specs.xml` and `binding-specs.conf` to generate `gl.h`,
`gl.erl` and `gl.c`, which can then be included in your project. Note that
in-source documentation is also generated.

For obvious reason, the API cannot be kept entirely the same and adjustments
had to be made. However, it follows a rigorous set of mapping rules which are
documented in the sections below.

## Binding specifications

The good thing about OpenGL APIs is that they're set in stone (at least at the
time of writing, in 2025) and won't change since the development has shifted
to Vulkan. It makes generating bindings for it simpler.

The strategy is to start with the official OpenGL APIs specification in the XML
format (as provided by the Khronos Group).

Snippet of the XML specifications.

```
<command>
    <proto>void <name>glProgramUniform4ui64vNV</name></proto>
    <param class="program"><ptype>GLuint</ptype> <name>program</name></param>
    <param><ptype>GLint</ptype> <name>location</name></param>
    <param><ptype>GLsizei</ptype> <name>count</name></param>
    <param kind="Vector4" len="count*4">const <ptype>GLuint64EXT</ptype> *<name>value</name></param>
</command>
```

Instead of relying on heuristic detection and guesswork to translate OpenGL functions into their BEAM-equivalent forms, the generator uses manually written 'binding specifications' to explicitly define the correct mapping.

Snippet of the binding specifications.

```
{"glHint", [ % XXX: update once final.
  {name, "hint"},
  {params, [
    {"Target", {gl_enum, "HintTarget"}},
    {"Mode", {gl_enum, "HintMode"}}
  ]},
  {return, "ok"}
]}
```

With both pieces of data combined, the generator is able to write pretty OpenGL
bindings that adhere to the specification.

## Thread safety

The generated bindings will only work with the specified EGL binding. This is
because OpenGL commands execute in a context and the API is not thread-safe.
Therefore, it relies on the EGL binding which re-arrange their execution to map
what your application is doing. xxx

Simply assume that a BEAM process is equal to a OS thread in your application
and all thread-related specifics apply in your code the exact same way. xxx

Refer to the [documentation](foobar) of the EGL binding for more information.

## API mapping (informal)

To be written.


## API mapping (formal)

To be written.
