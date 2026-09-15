# Spec Authoring

`binding-specs.conf` is the source of truth for the generated public API. A
spec change is a public API change unless it only repairs metadata for an
already exposed wrapper.

## Before Changing Specs

Confirm the intended public shape before editing:

- the Khronos command signature and target availability;
- the existing generated API around the same capability family;
- enum group names and object-name family names;
- whether counts, byte sizes, or capacities are public or derived;
- whether an output can be safely returned as a BEAM value;
- whether the command needs a focused runtime proof.

Do not add wrappers for raw pointers, callbacks, mapped memory, arbitrary
pointer-like handles, or client-owned mutable C arrays without first designing
a BEAM resource/lifetime model.

## Public Shape Rules

- Keep OpenGL command identity recognizable unless a semantic wrapper makes the
  BEAM API safer or clearer.
- Keep `glGet*` queries command-shaped. Do not add per-parameter convenience
  wrappers such as `get_shader_compile_status/1` or `get_max_texture_size/0`.
- Use atoms for enums and lists of atoms for bitfields.
- Use family-specific object-name types.
- Use `none` only for documented object-zero or nullable-data semantics.
- Derive raw counts and byte sizes from public lists or binaries when the
  derivation is unambiguous.
- Require explicit capacities or element counts for outputs that cannot be
  portably auto-sized.
- Return BEAM values instead of exposing writable host memory.
- Keep target filters precise; do not let a wrapper appear in a target that
  lacks the command or the selected public semantics.

## Tests To Add Or Update

Generator tests own the exhaustive contract:

- resolver tests for public names, parameter shapes, return shapes, hidden
  raw parameters, target filters, and omitted neighbors;
- Erlang emitter tests for public wrapper specs and raw-call wiring;
- NIF emitter tests for C conversion and OpenGL call shape;
- surface/omission tests for cross-cutting target or safety guarantees.

Target tests prove representative runtime behavior. They should exercise the
generated binding through a real EGL/OpenGL context, but they are not OpenGL
conformance tests.

## Documentation To Update

For a public surface change, update:

- `docs/api-mapping.md` when the public mapping policy changes;
- `docs/supported-surface.md` when a capability family or target boundary
  changes;
- `docs/omissions.md` when a family is intentionally excluded or becomes
  available;
- target docs when user-facing behavior changes.
