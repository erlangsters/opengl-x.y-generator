# API Mapping

The generated `gl` module stays close to OpenGL command identity while using
BEAM-shaped values for the public API.

The generated API reference is the exact function/type inventory for each
target. This document explains the rules behind that inventory.

## Function Names

OpenGL command names become snake_case functions on `gl`.

Examples:

- `glCullFace` becomes `gl:cull_face/1`
- `glCreateShader` becomes `gl:create_shader/1`
- `glGetProgramInfoLog` becomes `gl:get_program_info_log/2`

The mapping preserves the original command identity whenever the OpenGL shape
is already safe and clear from Erlang.

## Enums And Bitfields

OpenGL enum constants are exposed as atoms.

Examples:

- `GL_FRONT_AND_BACK` becomes `front_and_back`
- `GL_VERTEX_SHADER` becomes `vertex_shader`
- `GL_TEXTURE_2D` becomes `texture_2d`

OpenGL bitfields are exposed as lists of atoms:

```erlang
ok = gl:clear([color_buffer_bit, depth_buffer_bit]).
```

Reserved Erlang words gain a trailing underscore where needed. For example,
`GL_AND`, `GL_OR`, and `GL_XOR` map to `and_`, `or_`, and `xor_`.

The generated module also exposes enum helper functions:

- `gl:enum_groups_/0`
- `gl:enums_/1`
- `gl:enum_value_/1`
- `gl:value_enums_/1`
- `gl:value_enum_/2`

## Scalars And Object Names

OpenGL scalar families keep named `gl:*()` types in specs while using ordinary
BEAM runtime values.

| OpenGL family | Public type | Runtime shape |
| --- | --- | --- |
| `GLboolean` | `boolean()` | `boolean()` |
| signed integers | `gl:byte()`, `gl:short()`, `gl:int()`, `gl:int64()` | `integer()` |
| unsigned integers | `gl:ubyte()`, `gl:ushort()`, `gl:uint()`, `gl:uint64()` | `non_neg_integer()` |
| sizes and offsets | `gl:sizei()`, `gl:sizeiptr()`, `gl:intptr()`, `gl:offset()` | `integer()` |
| floating point | `gl:float()`, `gl:double()` | `erlang:float()` |
| enum values | `gl:enum()` | `atom()` |
| bitfields | `gl:bitfield()` | `[atom()]` |

OpenGL object names use family-specific public types instead of a single raw
integer type. Examples include `gl:buffer()`, `gl:texture()`, `gl:shader()`,
`gl:program()`, `gl:framebuffer()`, `gl:query()`, and `gl:sampler()`.

Where OpenGL uses object name zero for unbinding, the public binding uses
`none` only on wrappers whose contract explicitly supports unbind semantics.

## Aggregates

Fixed-size numeric aggregates use tuples so the shape is visible at the call
site.

Vectors map by arity:

- `vector2(T)` is `{X, Y}`
- `vector3(T)` is `{X, Y, Z}`
- `vector4(T)` is `{X, Y, Z, W}`

Matrices are tuples of column vectors. This matches OpenGL matrix naming and
keeps non-square shapes readable.

Examples:

- `matrix2(T)` is `{{C1R1, C1R2}, {C2R1, C2R2}}`
- `matrix2x3(T)` is `{{C1R1, C1R2, C1R3}, {C2R1, C2R2, C2R3}}`
- `matrix4x2(T)` is `{{C1R1, C1R2}, {C2R1, C2R2}, {C3R1, C3R2}, {C4R1, C4R2}}`

Array forms use non-empty lists when the OpenGL call receives a counted list.
The wrapper derives the raw count from the list length.

## Binaries, Strings, Counts, And Sizes

Byte-oriented input data uses `iodata()`.

```erlang
ok = gl:buffer_data(array_buffer, VertexBytes, static_draw).
```

When OpenGL requires a writable output buffer, the binding returns BEAM values
instead of exposing writable host memory. Outputs use explicit capacities or
explicit element counts when portable automatic sizing is not part of the
public contract.

Examples:

- log and name queries accept a maximum byte length and return a binary;
- scalar readbacks accept a positive element count and return typed lists;
- pixel and texture readbacks accept a caller-sized byte capacity and return a
  binary trimmed to the data returned by OpenGL when the command reports a
  length.

Generic `glGet*` state readback stays command-shaped. The `PName` is a public
enum atom, not part of the function name:

```erlang
{ok, [Size]} = gl:get_integer(max_texture_size, 1).
{ok, [Enabled]} = gl:get_boolean(depth_writemask, 1).
```

Object-scoped `glGet*iv` query families follow the same rule. Shader, program,
and program-pipeline parameter names are enum atoms passed to the command-shaped
wrapper, and status-like values remain integer readback values:

```erlang
{ok, [1]} = gl:get_shader(Shader, compile_status, 1).
{ok, [1]} = gl:get_program(Program, link_status, 1).
{ok, [Length]} = gl:get_program(Program, info_log_length, 1).
{ok, [Status]} = gl:get_program_pipeline(Pipeline, validate_status, 1).
```

Do not add per-parameter convenience wrappers such as
`get_shader_compile_status/1`, `get_program_link_status/1`, or
`get_max_texture_size/0`. The parameter name stays a public enum argument.

The binding does not expose raw pointers, caller-owned mutable C arrays, or
long-lived mapped host memory as ordinary Erlang terms.

## Suffixed Command Families

Some OpenGL command families differ only by scalar suffix or aggregate shape.
When those commands are one logical public operation, the generated wrapper
uses a selector argument.

Example:

```erlang
ok = gl:tex_parameter(f, texture_2d, texture_lod_bias, -0.5).
ok = gl:tex_parameter(i, texture_2d, texture_min_filter, linear).
ok = gl:program_uniform(ui, Program, Location, {1, 0, 0, 1}).
```

The suffix remains in the Erlang function name when it identifies a distinct
OpenGL family rather than only a scalar selector. For example,
`tex_parameter_i/4` remains separate from `tex_parameter/4`.

## Return Values

The generated API uses a small return convention:

- commands with no result return `ok` or `{error, Reason}`;
- commands with one result return `{ok, Value}` or `{error, Reason}`;
- commands with multiple outputs return `{ok, Value1, Value2, ...}` or
  `{error, Reason}`.

The raw NIF layer stays internal. Public callers should use the documented
Erlang wrappers in `gl`.

## Context Model

The `gl` module assumes a valid OpenGL or OpenGL ES context is already current
through the EGL binding. Context creation, surface management, and current
context routing belong to EGL, not to the generated OpenGL binding.
