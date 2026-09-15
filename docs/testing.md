# Testing Strategy

The test suites have two different jobs:

- generator tests prove the public mapping and emitted source contract;
- target binding tests prove representative runtime paths through a real
  EGL/OpenGL context.

Target tests are binding proofs, not OpenGL conformance tests.

## Generator Tests

Run from `opengl-x.y-generator`:

```bash
rebar3 as test eunit
rebar3 escriptize
```

Use focused modules while developing a generator change, then run the full
suite before committing generator behavior or generated surface changes.

Permanent generator test modules are grouped by responsibility:

- `generator_test_support.erl`
- `resolver_state_tests.erl`
- `resolver_object_tests.erl`
- `resolver_buffer_texture_tests.erl`
- `resolver_shader_program_tests.erl`
- `resolver_uniform_tests.erl`
- `resolver_query_readback_tests.erl`
- `resolver_draw_tests.erl`
- `resolver_pixel_framebuffer_tests.erl`
- `resolver_target_filter_tests.erl`
- `emitter_erlang_tests.erl`
- `emitter_nif_tests.erl`
- `surface_manifest_tests.erl`
- `omission_policy_tests.erl`
- `direction_checkpoint_tests.erl`
- `frontier_sweep_tests.erl`

The names are semantic on purpose. Numbered shard names may remain in test
function identifiers or comments that explain a policy decision. They are not
the navigation model for the suite.

There is no remaining command queue file. Omission and surface tests assert
the closed generated surface against `docs/omissions.md` and
`binding-specs.conf`.

## Generation Smoke

For generated surface changes, run non-populating generation smoke checks for
all supported targets in isolated temporary directories:

- `gl 3.3`
- `gl 4.1`
- `gl 4.6`
- `gles 2.0`
- `gles 3.0`
- `gles 3.1`
- `gles 3.2`

Inspect generated `gl.erl`, `gl.hrl`, and `gl.c` for at least `gl 4.6` and
`gles 3.2`. Inspect additional targets when target filtering or version
support is part of the change.

## Target Runtime Tests

Target binding repositories use capability-shaped EUnit modules. Baseline
modules are:

- `gl_test_context.erl`
- `gl_test_support.erl`
- `gl_smoke_test.erl`
- `gl_state_test.erl`

Capability modules use names such as `gl_buffer_test.erl`,
`gl_texture_test.erl`, `gl_shader_test.erl`, `gl_program_test.erl`,
`gl_uniform_test.erl`, `gl_draw_test.erl`, `gl_pixel_test.erl`,
`gl_query_test.erl`, and other target-supported capability names.

In headless or agent sessions, run target tests with:

```bash
EGL_PLATFORM=surfaceless rebar3 eunit
```

Surfaceless runtime tests prove NIF loading, dispatch, public Erlang-shaped
inputs, conversions, and representative readbacks on the local driver. If a
change depends on a specific OpenGL/OpenGL ES version, profile, extension,
shader feature, framebuffer behavior, or window-system surface, verify that
requirement explicitly.

## Docs-Only Changes

For documentation-only changes:

- do not regenerate or populate target repositories;
- do not run runtime canaries unless the docs reveal a behavioral mismatch;
- run `git diff --check` in each changed repository;
- optionally build ExDoc for one desktop target and one ES target when the
  dependencies are already available.
