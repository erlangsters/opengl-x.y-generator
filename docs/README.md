# OpenGL Generator Documentation

This directory is the active documentation surface for the Erlangsters OpenGL
binding generator. These files describe the current contract, not the
development chronology.

## User-Facing Contract

- `api-mapping.md` explains how OpenGL commands, types, enums, bitfields,
  aggregates, binary data, object names, and return values map to Erlang and
  Elixir.
- `supported-surface.md` summarizes the generated binding surface by
  capability family and target.
- `omissions.md` records intentionally unsupported or design-gated OpenGL
  families.

## Developer-Facing Contract

- `generator-architecture.md` explains the XML/spec/resolver/emitter pipeline.
- `spec-authoring.md` describes how to change `binding-specs.conf` safely.
- `testing.md` describes generator tests, target runtime proofs, and docs-only
  verification.
- `release-workflow.md` describes generation, target propagation, verification,
  and repository checkpoints.

## Source Of Truth

`binding-specs.conf` is the source of truth for the generated public surface.
The generated target repositories must receive `gl.erl`, `gl.hrl`, and `gl.c`
only through the generator and populate scripts.

Remaining unsupported or design-gated surfaces are documented in
`omissions.md` and need explicit public API design before they can become
generated bindings.
