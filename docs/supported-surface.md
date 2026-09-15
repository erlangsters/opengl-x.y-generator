# Supported Surface

The generator produces bindings for these targets:

- OpenGL 3.3
- OpenGL 4.1
- OpenGL 4.6
- OpenGL ES 2.0
- OpenGL ES 3.0
- OpenGL ES 3.1
- OpenGL ES 3.2

Each target exposes the generated core surface selected from the Khronos XML
and `binding-specs.conf`. The generated API reference in each target repository
is the exact inventory for that target.

## General Boundaries

- The bindings expose core target commands, not compatibility-profile commands.
- Extension loading and ad hoc function-pointer lookup are not part of the
  public API.
- OpenGL commands execute against the current context provided by the EGL
  binding.
- Public wrappers avoid raw host pointers, callback ownership, mapped memory
  lifetimes, and arbitrary pointer-like values.

## Capability Families

All targets include the shared runtime and state foundation: error queries,
string queries, viewport/scissor state, clear state, depth/stencil/color state,
enable/disable state, object binding, and the scalar/enum/bitfield conversion
paths needed by those commands.

Object and data families cover the target-supported portions of buffers,
textures, renderbuffers, framebuffers, shaders, programs, queries, vertex
arrays, samplers, transform feedback, and program pipelines. Smaller or older
targets expose only the families present in their Khronos target surface.

Data transfer support includes buffer uploads, buffer sub-data, texture image
uploads, compressed texture uploads, framebuffer pixel readback, texture
readback where the target supports it, and clear commands with explicit
`iodata() | none` payload shapes where OpenGL accepts nullable data.

Shader and program support includes shader creation, source upload,
compilation, program linking/use, info-log readback, uniform setters,
matrix-shaped uniform setters, program-object uniform setters, reflection
queries, program binary upload/readback where supported, shader binary upload,
and SPIR-V specialization on the supported desktop target.

Draw support includes direct array and element draws, instanced/base-vertex
variants where available, element-buffer offset draws, multi-draw offset lists,
indirect draw-buffer offset commands, and OpenGL 4.6 indirect-count commands.
Client-side index arrays and client-side indirect command payloads are not
exposed.

Readback support uses explicit public storage sizes. Scalar and object-state
queries return typed lists, string-like outputs return binaries, and pixel or
texture outputs return caller-sized binaries.

Debug, copy, internal-format, multi-bind, subroutine, program-resource, and
generic state-query families are exposed only on targets whose Khronos surface
contains the corresponding commands.

## Readiness Boundary

There is no ordinary remaining command queue for the supported generated
surface. Future additions should start from an explicit public contract review,
especially when they involve raw pointers, callbacks, mapped memory, automatic
allocation, compatibility-profile commands, or runtime-specific behavior.
