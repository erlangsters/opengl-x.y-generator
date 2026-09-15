-module(documentation_writer).
-export([write/1]).
-import(opengl_gen, [write/2, write/3]).

write(BindingData) ->
    write(0, "~s binding.\n\n", [maps:get(api_name, BindingData)]),

    write(0,
        "It exposes the ~s API through the `gl` module with small, explicit adjustments for BEAM use.\n\n",
        [maps:get(api_name, BindingData)]
    ),

    write(0, """
Most command names stay close to their OpenGL counterparts. The generator maps
camel-case commands to snake_case functions, exposes enums as atoms and
bitfields as lists of atoms, and uses named `gl:*()` types for OpenGL scalar
families such as `gl:int()`, `gl:sizei()`, `gl:float()`, and `gl:double()`.

For example:

```erlang
{ok, Shader} = gl:create_shader(vertex_shader).
ok = gl:line_width(3.0).
ok = gl:scissor(0, 0, 800, 600).
```

It keeps the command identity recognizable while replacing raw integer
constants with atoms and turning OpenGL object names into typed handles such as
`gl:shader()`, `gl:program()`, `gl:buffer()`, and `gl:texture()`.

When OpenGL uses fixed-size numeric aggregates, the binding prefers structured
tuple values over flat caller-managed arrays. Vectors map to tuples by arity,
and matrix families use tuples of column vectors so shapes such as
`matrix2x3(gl:float())` read as two columns of height three.

```erlang
ok = gl:program_uniform(ui, Program, Location, {0.0, 1.0, 0.0, 1.0}).
```

That keeps the aggregate shape visible at the call site.

Some OpenGL commands use a raw enum argument to choose a semantic sub-operation.
When the valid value type depends on that enum, the binding prefers explicit
semantic wrappers over exposing the raw selector directly.

```erlang
ok = gl:tex_min_filter(texture_2d, nearest).
ok = gl:tex_mag_filter(texture_2d, nearest).
ok = gl:tex_wrap_s(texture_2d, clamp_to_edge).
ok = gl:tex_wrap_t(texture_2d, clamp_to_edge).
```

Return values also follow a small BEAM-oriented convention: commands that do
not produce a value return `ok`, and commands that produce one or more values
return `{ok, ...}` tuples.

The generated module also provides enum helper functions:

- `enum_groups_/0` lists the available enum groups.
- `enums_/1` lists the atoms for one enum group.
- `enum_value_/1` maps an atom to its numeric OpenGL value.
- `value_enums_/1` maps a numeric value to the matching atoms.
- `value_enum_/2` resolves one numeric value within a specific enum group.

The module assumes a valid OpenGL context is already current before any NIF
backed command runs. That keeps context creation and current-context management
outside the `gl` module itself.
""", []),

    ok.
