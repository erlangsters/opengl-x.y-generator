# Generator Architecture

The generator turns Khronos XML plus explicit binding specs into the generated
BEAM OpenGL binding files.

## Inputs

- `gl-specs.xml` is the Khronos command, type, enum, and target metadata input.
- `binding-specs.conf` is the source of truth for the public generated surface.
- Unsupported or design-gated families are documented in `docs/omissions.md`.

The generator deliberately uses explicit binding specs instead of guessing a
public API from XML alone. OpenGL command signatures often need BEAM-facing
decisions for names, enum groups, object types, counts, byte sizes, outputs,
nullable data, and target filtering.

## Pipeline

1. XML utilities parse the Khronos registry and expose commands, enums, and
   target features.
2. Binding specs describe the public wrapper names, parameter shapes, return
   shapes, aggregate forms, raw NIF metadata, and target filters.
3. Resolver modules combine XML and specs into target-specific binding data.
4. Emitter modules generate:
   - `gl.erl` for the public Erlang API and internal raw NIF calls;
   - `gl.hrl` for generated constants and shared include data;
   - `gl.c` for NIF conversion, dispatch, and OpenGL calls.
5. Target populate scripts copy generated files into the target repositories.

## Resolver Responsibilities

Resolvers own semantic mapping decisions:

- enum and bitfield groups;
- scalar, object, binary, string, list, tuple, and count shapes;
- suffix dispatch and aggregate wrapper selection;
- target-specific inclusion and exclusion;
- hidden raw parameters such as derived counts or byte sizes;
- output allocation policy;
- BEAM-safe omissions.

Resolver tests are grouped by capability family so failures point to the
contract that changed.

## Emitter Responsibilities

Emitters own generated source shape:

- exported public wrappers and specs;
- generated enum helper functions;
- raw NIF metadata and internal raw-call names;
- Erlang-side argument validation and conversion handoff;
- C-side unpacking, allocation, OpenGL dispatch, and return packing;
- generated module documentation.

Emitter tests assert the public Erlang wrappers and raw NIF/C call shape for
representative policy families.

## Generated Output

Generator-root `gl.erl`, `gl.hrl`, and `gl.c` are scratch outputs produced by
generation and populate scripts. They should not be treated as source edits.

Target repository generated files must not be hand-edited. Change
`binding-specs.conf` or generator code, regenerate, and repopulate targets
through the scripts.
