# Omissions And Design-Gated Families

This file records OpenGL families that are intentionally outside the current
generated public surface.

The common rule is simple: do not expose raw C pointer ownership, callback
ownership, mapped-memory lifetimes, or compatibility-profile behavior as
ordinary Erlang terms.

## Raw Pointer And Lifetime APIs

Status: omitted under the current BEAM binding model.

These APIs expose raw host pointers, opaque pointer-like handles, callbacks,
foreign-memory lifetimes, or scheduler-sensitive callback execution. They need
an explicit BEAM resource and ownership model before they can be reconsidered.

Commands:

- `glMapBuffer`
- `glMapBufferRange`
- `glMapNamedBuffer`
- `glMapNamedBufferRange`
- `glUnmapBuffer`
- `glUnmapNamedBuffer`
- `glFlushMappedBufferRange`
- `glFlushMappedNamedBufferRange`
- `glGetBufferPointerv`
- `glGetNamedBufferPointerv`
- `glGetVertexAttribPointerv`
- `glFenceSync`
- `glIsSync`
- `glDeleteSync`
- `glClientWaitSync`
- `glWaitSync`
- `glGetSynciv`
- `glGetSync`
- `glDebugMessageCallback`
- `glObjectPtrLabel`
- `glGetObjectPtrLabel`
- `glGetPointerv`

Reconsider only after designing and testing a BEAM resource/lifetime model for
mapped memory, `GLsync`, callback ownership, and arbitrary pointer values.

## Robust Target-Bound Texture Readback

Status: omitted from the ordinary generated surface until runtime behavior is
proved on another driver or accepted with an explicit waiver.

Commands:

- `glGetnTexImage`
- `glGetnCompressedTexImage`

The generated caller-sized binary shape matched the public readback policy, but
the local surfaceless Mesa OpenGL 4.5 fallback left the output buffer unwritten
without reporting a GL error. The non-robust target-bound texture readback path
is available where supported.

## Duplicate Vertex-Attribute Pointer Spellings

Status: omitted because active aggregate setters and readbacks already cover
the useful public Erlang shapes.

Families:

- `glVertexAttrib*dv`
- `glVertexAttrib*fv`
- `glVertexAttrib*sv`
- `glVertexAttribI*iv`
- `glVertexAttribI*uiv`
- `glVertexAttribL*dv`
- `glVertexAttribP*uiv`
- `glVertexAttrib4Nubv`

Do not add these as separate direct wrappers unless the public vertex-attribute
API is deliberately redesigned.

## Compatibility-Profile Commands

Status: omitted from the supported generated target set.

The generated desktop targets are core-profile bindings. Compatibility-profile
fixed-function commands and profile-specific packed fixed-function families
need an explicit target policy before any public API work.

## Automatic Allocation Conveniences

Status: design-gated.

The current public API uses explicit counts, capacities, or byte sizes for
outputs that cannot be portably auto-sized. Auto-sizing helpers can be added
later only when they have clear query rules, failure behavior, target support,
and runtime proofs.

## Client-Side Element Indexes And Indirect Payloads

Status: omitted from the ordinary generated surface.

Draw wrappers accept buffer offsets, not client-side element-index arrays or
client-side indirect command payloads. Those shapes expose caller-owned C
memory that the current BEAM binding model does not own.
