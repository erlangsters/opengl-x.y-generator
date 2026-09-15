-module(emitter_erlang_tests).
-include_lib("eunit/include/eunit.hrl").

%% Erlang emitter surface contracts.

%% Historical shard 13.
s013_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s013_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s013_emitter_get_error_unchecked_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard13-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Generated} = file:read_file("gl.erl"),
        ?assertMatch(
            {_, _},
            binary:match(Generated, <<"get_error() ->\n\n    ?CALL_RAW_FUNC_UNCHECKED(glGetError_raw()).">>)
        ),
        ?assertEqual(
            nomatch,
            binary:match(Generated, <<"get_error() ->\n\n    ?CALL_RAW_FUNC(glGetError_raw()).">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s013_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_error", 0}, Functions)),

    s013_assert_enum_types(BindingData),
    s013_assert_get_error(maps:get({"get_error", 0}, Functions)).

s013_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("error_code", EnumTypes)),
    ?assert(lists:member("no_error", maps:get("error_code", EnumTypes))),
    ?assert(lists:member("invalid_enum", maps:get("error_code", EnumTypes))).

s013_assert_get_error(FunctionData) ->
    ?assertEqual("glGetError", maps:get(gl_command, FunctionData)),
    ?assertEqual([], maps:get(params_specs, FunctionData)),
    ?assertEqual({"Code", {gl_enum, "ErrorCode"}}, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_params, FunctionData)),
    ?assertEqual(
        [{"Code", {undefined, error_code, []}}],
        maps:get(specs_return, FunctionData)
    ),
    ?assertEqual(0, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([], maps:get(params, Clause)),
    ?assertEqual("glGetError", maps:get(raw_function, Clause)),
    ?assertEqual(false, maps:get(error_check, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGetError", NifFunctions),
    ?assertEqual(0, maps:get(arity, NifData)),
    ?assertEqual([], maps:get(params, NifData)),
    {glenum_to_atom, TransformMap} = maps:get(return, NifData),
    ?assert(lists:member({"GL_NO_ERROR", "no_error"}, TransformMap)),
    ?assert(lists:member({"GL_INVALID_ENUM", "invalid_enum"}, TransformMap)).

%% Historical shard 38.
s038_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s038_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s038_emitter_shader_source_normalization_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard38-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Generated} = file:read_file("gl.erl"),
        ?assertMatch(
            {_, _},
            binary:match(Generated, <<"-spec shader_source(\n    Shader :: shader(),\n    Source :: [iodata()]\n) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(Generated, <<"iolist_to_binary(SourceItem)">>)
        ),
        ?assertEqual(
            nomatch,
            binary:match(Generated, <<"-> list_to_binary(SourceItem)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(Generated, <<"?CALL_RAW_FUNC(glShaderSource_raw(Shader, SourceNew)).">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s038_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"shader_source", 2}, Functions)),
    ?assertNot(maps:is_key({"shader_source", 1}, Functions)),
    ?assertNot(maps:is_key({"shader_source", 3}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s038_assert_shader_source(maps:get({"shader_source", 2}, Functions)).

s038_assert_shader_source(FunctionData) ->
    ?assertEqual("glShaderSource", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Shader", {gl_object, shader}},
            {in, "Source", list_gl_strings}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Shader", {undefined, shader, []}},
            {"Source", {list, {undefined, iodata, []}}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Shader", do_nothing}, {"Source", normalize_list_strings_or_binary}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glShaderSource", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glShaderSource", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Shader", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Source", in_list_gl_strings}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 40.
s040_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s040_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s040_emitter_use_program_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard40-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec use_program(Program :: program() | none) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"none -> 0">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glUseProgram_raw(NewProgram)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glUseProgram(arg_0);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glUseProgram_raw\", 1, nif_glUseProgram, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s040_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"use_program", 1}, Functions)),
    ?assertNot(maps:is_key({"use_program", 0}, Functions)),
    ?assertNot(maps:is_key({"use_program", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s040_assert_use_program(maps:get({"use_program", 1}, Functions)).

s040_assert_use_program(FunctionData) ->
    ?assertEqual("glUseProgram", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Program", {gl_object, program, [none]}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Program", {set, [{undefined, program, []}, none]}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Program", {gl_object_to_uint, [{none, 0}]}}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glUseProgram", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glUseProgram", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 41.
s041_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s041_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s041_emitter_status_queries_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard41-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec get_shader(\n    Shader :: shader(),\n    ParamName :: shader_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec get_program(\n    Program :: program(),\n    ParamName :: program_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glGetShaderivValues_raw(Shader, NewParamName, Count)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glGetProgramivValues_raw(Program, NewParamName, Count)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"glGetShaderivValues_raw(_Shader, _ParamName, _Values) ->">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"glGetProgramivValues_raw(_Program, _ParamName, _Values) ->">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glGetShaderiv(arg_0, arg_1, arg_2_values);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glGetProgramiv(arg_0, arg_1, arg_2_values);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glGetShaderivValues_raw\", 3, nif_glGetShaderivValues, 0}">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glGetProgramivValues_raw\", 3, nif_glGetProgramivValues, 0}">>)
        ),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"get_shader_compile_status">>)),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"get_program_link_status">>)),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"glGetShaderivInteger_raw">>)),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"glGetProgramivInteger_raw">>)),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"enif_make_int(env, arg_2_values[i])">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s041_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_shader", 3}, Functions)),
    ?assert(maps:is_key({"get_program", 3}, Functions)),
    ?assertNot(maps:is_key({"get_shader_compile_status", 1}, Functions)),
    ?assertNot(maps:is_key({"get_program_link_status", 1}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 2}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),
    ?assertNot(maps:is_key({"get_program", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),

    s041_assert_status_query(
        maps:get({"get_shader", 3}, Functions),
        "glGetShaderiv",
        "glGetShaderivValues",
        "Shader",
        {gl_object, shader},
        shader_parameter_name,
        "compile_status"
    ),
    s041_assert_status_query(
        maps:get({"get_program", 3}, Functions),
        "glGetProgramiv",
        "glGetProgramivValues",
        "Program",
        {gl_object, program},
        program_parameter_name,
        "link_status"
    ).

s041_assert_status_query(FunctionData, Command, RawName, ObjectParamName, ObjectType, PublicEnumName, ExpectedAtom) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, ObjectParamName, ObjectType},
            {in, "ParamName", s041_expected_enum_spec(PublicEnumName)},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {ObjectParamName, s041_object_type_spec(ObjectType)},
            {"ParamName", {undefined, PublicEnumName, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {ObjectParamName, do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember(ExpectedAtom, 1, ParamNameMap)),
    ?assertEqual(RawName, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(RawName, NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {ObjectParamName, s041_object_nif_data()},
            {"ParamName", s041_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s041_expected_enum_spec(shader_parameter_name) ->
    {gl_enum, "ShaderParameterName", shader_parameter_name};
s041_expected_enum_spec(program_parameter_name) ->
    {gl_enum, ["ProgramPropertyARB", "ProgramParameterPName"], program_parameter_name}.

s041_object_type_spec({gl_object, Name}) ->
    {undefined, Name, []}.

s041_object_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s041_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 42.
s042_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s042_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s042_emitter_draw_arrays_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard42-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec draw_arrays(\n    Mode :: primitive_type(),\n    First :: gl:int(),\n    Count :: gl:sizei()\n) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"triangles -> ?GL_TRIANGLES">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glDrawArrays_raw(NewMode, First, Count)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"glDrawArrays_raw(_Mode, _First, _Count) ->">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glDrawArrays(arg_0, arg_1, arg_2);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glDrawArrays_raw\", 3, nif_glDrawArrays, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s042_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"draw_arrays", 3}, Functions)),

    s042_assert_primitive_type_enum(BindingData),
    s042_assert_draw_arrays(maps:get({"draw_arrays", 3}, Functions)).

s042_assert_primitive_type_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("primitive_type", EnumTypes)),
    PrimitiveTypes = maps:get("primitive_type", EnumTypes),
    ?assert(lists:member("points", PrimitiveTypes)),
    ?assert(lists:member("triangles", PrimitiveTypes)).

s042_assert_draw_arrays(FunctionData) ->
    ?assertEqual("glDrawArrays", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "First", gl_int},
            {in, "Count", gl_sizei}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Mode", {undefined, primitive_type, []}},
            {"First", {gl, int, []}},
            {"Count", {gl, sizei, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Mode", {gl_enum_to_uint, TransformMap}},
        {"First", do_nothing},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("points", 1, TransformMap)),
    ?assert(lists:keymember("triangles", 1, TransformMap)),
    ?assertEqual("glDrawArrays", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDrawArrays", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Mode", s042_enum_nif_data()},
            {"First", s042_int_nif_data()},
            {"Count", s042_sizei_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s042_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s042_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s042_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 45.
s045_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s045_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s045_emitter_vertex_attrib_pointer_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard45-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-export_type([offset/0]).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-type offset() :: non_neg_integer().">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec vertex_attrib_pointer(\n    Index :: gl:uint(),\n    Size :: gl:int(),\n    Type :: vertex_attrib_pointer_type(),\n    Normalized :: gl:boolean(),\n    Stride :: gl:sizei(),\n    Offset :: gl:offset()\n) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"float -> ?GL_FLOAT">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttribPointer_raw(Index, Size, NewType, Normalized, Stride, Offset)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"#include <stdint.h>">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"ErlNifUInt64 arg_5;">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"if (arg_5 > UINTPTR_MAX)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glVertexAttribPointer(arg_0, arg_1, arg_2, arg_3, arg_4, (GLvoid*)(uintptr_t)arg_5);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glVertexAttribPointer_raw\", 6, nif_glVertexAttribPointer, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s045_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"vertex_attrib_pointer", 6}, Functions)),

    s045_assert_vertex_attrib_pointer_type(BindingData),
    s045_assert_vertex_attrib_pointer(maps:get({"vertex_attrib_pointer", 6}, Functions)).

s045_assert_vertex_attrib_pointer_type(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("vertex_attrib_pointer_type", EnumTypes)),
    Types = maps:get("vertex_attrib_pointer_type", EnumTypes),
    ?assert(lists:member("float", Types)),
    ?assertNot(maps:is_key("vertex_attrib_pointer_type_arb", EnumTypes)).

s045_assert_vertex_attrib_pointer(FunctionData) ->
    ?assertEqual("glVertexAttribPointer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Index", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribPointerType"}},
            {in, "Normalized", gl_bool},
            {in, "Stride", gl_sizei},
            {in, "Offset", gl_offset}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Index", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_pointer_type, []}},
            {"Normalized", {gl, boolean, []}},
            {"Stride", {gl, sizei, []}},
            {"Offset", {gl, offset, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(6, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Index", do_nothing},
        {"Size", do_nothing},
        {"Type", {gl_enum_to_uint, TransformMap}},
        {"Normalized", do_nothing},
        {"Stride", do_nothing},
        {"Offset", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("float", 1, TransformMap)),
    ?assertEqual("glVertexAttribPointer", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glVertexAttribPointer", NifFunctions),
    ?assertEqual(6, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Index", s045_uint_nif_data()},
            {"Size", s045_int_nif_data()},
            {"Type", s045_enum_nif_data()},
            {"Normalized", boolean_to_glbool},
            {"Stride", s045_sizei_nif_data()},
            {"Offset", in_gl_offset}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s045_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s045_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s045_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s045_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 48.
s048_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s048_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s048_emitter_bind_attrib_location_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard48-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-export([bind_attrib_location/3]).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec bind_attrib_location(\n    Program :: program(),\n    Index :: gl:uint(),\n    Name :: iodata()\n) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"Name0 = iolist_to_binary(Name)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glBindAttribLocation_raw(Program, Index, Name0)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"ErlNifBinary arg_2;">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"if (!enif_inspect_binary(env, argv[2], &arg_2))">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"GLchar* arg_2_string = (GLchar*)enif_alloc(arg_2.size + 1);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"arg_2_string[arg_2.size] = '\\0';">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glBindAttribLocation(arg_0, arg_1, (const GLchar*)arg_2_string);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"enif_free(arg_2_string);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glBindAttribLocation_raw\", 3, nif_glBindAttribLocation, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s048_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"bind_attrib_location", 3}, Functions)),
    ?assert(maps:is_key({"get_program", 3}, Functions)),
    ?assertNot(maps:is_key({"get_program_validation_status", 1}, Functions)),
    ?assertNot(maps:is_key({"bind_attrib_location", 2}, Functions)),
    ?assertNot(maps:is_key({"bind_attrib_location", 4}, Functions)),

    s048_assert_bind_attrib_location(maps:get({"bind_attrib_location", 3}, Functions)).

s048_assert_bind_attrib_location(FunctionData) ->
    ?assertEqual("glBindAttribLocation", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Index", gl_uint},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Index", {gl, uint, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Index", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glBindAttribLocation", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glBindAttribLocation", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Index", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 49.
s049_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s049_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s049_emitter_get_attrib_location_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard49-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-export([get_attrib_location/2]).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec get_attrib_location(\n    Program :: program(),\n    Name :: iodata()\n) -> {ok, Location :: gl:int()} | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"Name0 = iolist_to_binary(Name)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glGetAttribLocation_raw(Program, Name0)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"ErlNifBinary arg_1;">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"GLchar* arg_1_string = (GLchar*)enif_alloc(arg_1.size + 1);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"if (arg_1.size > 0)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"arg_1_string[arg_1.size] = '\\0';">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"GLint ret = glGetAttribLocation(arg_0, (const GLchar*)arg_1_string);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"ERL_NIF_TERM ret_0 = enif_make_int(env, ret);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"enif_free(arg_1_string);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glGetAttribLocation_raw\", 2, nif_glGetAttribLocation, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s049_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_attrib_location", 2}, Functions)),
    ?assert(maps:is_key({"bind_attrib_location", 3}, Functions)),
    ?assertNot(maps:is_key({"get_attrib_location", 1}, Functions)),
    ?assertNot(maps:is_key({"get_attrib_location", 3}, Functions)),

    s049_assert_get_attrib_location(maps:get({"get_attrib_location", 2}, Functions)).

s049_assert_get_attrib_location(FunctionData) ->
    ?assertEqual("glGetAttribLocation", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"Location", gl_int}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Location", {gl, int, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetAttribLocation", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGetAttribLocation", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(glint_to_integer, maps:get(return, NifData)).

%% Historical shard 50.
s050_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s050_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s050_emitter_get_uniform_location_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard50-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-export([get_uniform_location/2]).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec get_uniform_location(\n    Program :: program(),\n    Name :: iodata()\n) -> {ok, Location :: gl:int()} | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"Name0 = iolist_to_binary(Name)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glGetUniformLocation_raw(Program, Name0)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"ErlNifBinary arg_1;">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"GLchar* arg_1_string = (GLchar*)enif_alloc(arg_1.size + 1);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"if (arg_1.size > 0)">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"arg_1_string[arg_1.size] = '\\0';">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"GLint ret = glGetUniformLocation(arg_0, (const GLchar*)arg_1_string);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"ERL_NIF_TERM ret_0 = enif_make_int(env, ret);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"enif_free(arg_1_string);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glGetUniformLocation_raw\", 2, nif_glGetUniformLocation, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s050_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_uniform_location", 2}, Functions)),
    ?assert(maps:is_key({"get_attrib_location", 2}, Functions)),
    ?assert(maps:is_key({"bind_attrib_location", 3}, Functions)),
    ?assertNot(maps:is_key({"get_uniform_location", 1}, Functions)),
    ?assertNot(maps:is_key({"get_uniform_location", 3}, Functions)),

    s050_assert_get_uniform_location(maps:get({"get_uniform_location", 2}, Functions)).

s050_assert_get_uniform_location(FunctionData) ->
    ?assertEqual("glGetUniformLocation", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"Location", gl_int}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Location", {gl, int, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetUniformLocation", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGetUniformLocation", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(glint_to_integer, maps:get(return, NifData)).

%% Historical shard 51.
s051_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s051_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s051_emitter_aggregate_scalar_uniform_setters_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard51-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-export([uniform/3]).">>)
        ),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"-export([uniform_1f/2]).">>)),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"-export([uniform_1i/2]).">>)),
        ?assertMatch({_, _}, binary:match(GeneratedErl, <<"-type uniform_value() ::">>)),
        ?assertMatch({_, _}, binary:match(GeneratedErl, <<"gl:float()">>)),
        ?assertMatch({_, _}, binary:match(GeneratedErl, <<"gl:int()">>)),
        ?assertMatch({_, _}, binary:match(GeneratedErl, <<"gl:uint()">>)),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec uniform(\n    Type :: d | f | i | ui,\n    Location :: gl:int(),\n    Value :: uniform_value()\n) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"uniform(f, Location, Value) ->">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"uniform(i, Location, Value) ->">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform1f_raw(Location, Value)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform1i_raw(Location, Value));">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glUniform1f(arg_0, arg_1);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glUniform1i(arg_0, arg_1);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glUniform1f_raw\", 2, nif_glUniform1f, 0}">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glUniform1i_raw\", 2, nif_glUniform1i, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s051_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"uniform", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_1f", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_1i", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_2f", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_2i", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_3f", 4}, Functions)),
    ?assertNot(maps:is_key({"uniform_3i", 4}, Functions)),
    ?assertNot(maps:is_key({"uniform_4f", 5}, Functions)),
    ?assertNot(maps:is_key({"uniform_4i", 5}, Functions)),

    s051_assert_uniform(maps:get({"uniform", 3}, Functions)).

s051_assert_uniform(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    ?assert(lists:member({"glUniform1i", gl_int, element}, GlCommands)),
    ?assert(lists:member({"glUniform1f", gl_float, element}, GlCommands)),
    Variants = maps:get(variants, FunctionData),
    ?assert(lists:member({gl_int, element}, Variants)),
    ?assert(lists:member({gl_float, element}, Variants)),
    {uniform_value, {set, ExtraTypeVariants}} = maps:get(extra_type, FunctionData),
    ?assert(lists:member({gl, float, []}, ExtraTypeVariants)),
    ?assert(lists:member({gl, int, []}, ExtraTypeVariants)),
    ?assertEqual(
        [
            {in, "Location", gl_int},
            {in, "Value", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    [{"Type", {set, TypeAtoms}}, {"Location", {gl, int, []}}, {"Value", {undefined, uniform_value, []}}] =
        maps:get(specs_params, FunctionData),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    Clauses = maps:get(function_clauses, FunctionData),
    s051_assert_uniform_clause("f", "glUniform1f", Clauses),
    s051_assert_uniform_clause("i", "glUniform1i", Clauses),

    NifFunctions = maps:get(nif_functions, FunctionData),
    s051_assert_uniform_nif(
        maps:get("glUniform1f", NifFunctions),
        {"GLfloat", "double", "enif_get_double", "enif_make_double"}
    ),
    s051_assert_uniform_nif(
        maps:get("glUniform1i", NifFunctions),
        {"GLint", "int", "enif_get_int", "enif_make_int"}
    ).

s051_assert_uniform_clause(TypeAtom, RawFunction, Clauses) ->
    Clause = lists:keyfind(RawFunction, 2, [
        {Clause0, maps:get(raw_function, Clause0)}
     || Clause0 <- Clauses
    ]),
    ?assertMatch({_, RawFunction}, Clause),
    {ClauseData, RawFunction} = Clause,
    ?assertEqual([], maps:get(guards, ClauseData)),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Location", do_nothing},
            {"Value", do_nothing}
        ],
        maps:get(params, ClauseData)
    ).

s051_assert_uniform_nif(NifData, ValueSpec) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
            {"Value", {gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 52.
s052_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s052_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s052_emitter_aggregate_vector_uniform_setters_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard52-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),

        s052_assert_contains(GeneratedErl, <<"-export([uniform/3]).">>),
        s052_assert_not_contains(GeneratedErl, <<"-export([uniform_2f/3]).">>),
        s052_assert_not_contains(GeneratedErl, <<"-export([uniform_3f/4]).">>),
        s052_assert_not_contains(GeneratedErl, <<"-export([uniform_4f/5]).">>),
        s052_assert_not_contains(GeneratedErl, <<"-export([uniform_2i/3]).">>),
        s052_assert_not_contains(GeneratedErl, <<"-export([uniform_3i/4]).">>),
        s052_assert_not_contains(GeneratedErl, <<"-export([uniform_4i/5]).">>),

        s052_assert_contains(GeneratedErl, <<"-type uniform_value() ::">>),
        s052_assert_contains(GeneratedErl, <<"gl:float()">>),
        s052_assert_contains(GeneratedErl, <<"vector2(gl:float())">>),
        s052_assert_contains(GeneratedErl, <<"vector3(gl:float())">>),
        s052_assert_contains(GeneratedErl, <<"vector4(gl:float())">>),
        s052_assert_contains(GeneratedErl, <<"gl:int()">>),
        s052_assert_contains(GeneratedErl, <<"vector2(gl:int())">>),
        s052_assert_contains(GeneratedErl, <<"vector3(gl:int())">>),
        s052_assert_contains(GeneratedErl, <<"vector4(gl:int())">>),

        s052_assert_contains(GeneratedErl, <<"[V1, V2] = ?GL_PACK_VECTOR_2(Value),">>),
        s052_assert_contains(GeneratedErl, <<"[V1, V2, V3] = ?GL_PACK_VECTOR_3(Value),">>),
        s052_assert_contains(GeneratedErl, <<"[V1, V2, V3, V4] = ?GL_PACK_VECTOR_4(Value),">>),
        s052_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform2f_raw(Location, V1, V2))">>),
        s052_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform3f_raw(Location, V1, V2, V3))">>),
        s052_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform4f_raw(Location, V1, V2, V3, V4))">>),
        s052_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform2i_raw(Location, V1, V2))">>),
        s052_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform3i_raw(Location, V1, V2, V3))">>),
        s052_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform4i_raw(Location, V1, V2, V3, V4))">>),

        s052_assert_contains(GeneratedC, <<"glUniform2f(arg_0, arg_1, arg_2);">>),
        s052_assert_contains(GeneratedC, <<"glUniform3f(arg_0, arg_1, arg_2, arg_3);">>),
        s052_assert_contains(GeneratedC, <<"glUniform4f(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
        s052_assert_contains(GeneratedC, <<"glUniform2i(arg_0, arg_1, arg_2);">>),
        s052_assert_contains(GeneratedC, <<"glUniform3i(arg_0, arg_1, arg_2, arg_3);">>),
        s052_assert_contains(GeneratedC, <<"glUniform4i(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
        s052_assert_contains(GeneratedC, <<"{\"glUniform2f_raw\", 3, nif_glUniform2f, 0}">>),
        s052_assert_contains(GeneratedC, <<"{\"glUniform3f_raw\", 4, nif_glUniform3f, 0}">>),
        s052_assert_contains(GeneratedC, <<"{\"glUniform4f_raw\", 5, nif_glUniform4f, 0}">>),
        s052_assert_contains(GeneratedC, <<"{\"glUniform2i_raw\", 3, nif_glUniform2i, 0}">>),
        s052_assert_contains(GeneratedC, <<"{\"glUniform3i_raw\", 4, nif_glUniform3i, 0}">>),
        s052_assert_contains(GeneratedC, <<"{\"glUniform4i_raw\", 5, nif_glUniform4i, 0}">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s052_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),

    ?assert(maps:is_key({"uniform", 3}, Functions)),
    s052_assert_direct_wrappers_absent(Functions),
    s052_assert_deferred_uniforms_absent(Functions),

    Uniform = maps:get({"uniform", 3}, Functions),
    s052_assert_uniform_commands(Uniform),
    s052_assert_uniform_specs(Uniform),
    s052_assert_uniform_clauses(Uniform),
    s052_assert_uniform_nifs(Uniform).

s052_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"uniform_1f", 2},
        {"uniform_1i", 2},
        {"uniform_2f", 3},
        {"uniform_2i", 3},
        {"uniform_3f", 4},
        {"uniform_3i", 4},
        {"uniform_4f", 5},
        {"uniform_4i", 5}
    ]).

s052_assert_deferred_uniforms_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s052_assert_uniform_commands(Uniform) ->
    Expected = [
        {"glUniform4i", {gl_vector, 4, gl_int}, element},
        {"glUniform3i", {gl_vector, 3, gl_int}, element},
        {"glUniform2i", {gl_vector, 2, gl_int}, element},
        {"glUniform1i", gl_int, element},
        {"glUniform4f", {gl_vector, 4, gl_float}, element},
        {"glUniform3f", {gl_vector, 3, gl_float}, element},
        {"glUniform2f", {gl_vector, 2, gl_float}, element},
        {"glUniform1f", gl_float, element}
    ],
    GlCommands = maps:get(gl_commands, Uniform),
    lists:foreach(fun(Command) ->
        ?assert(lists:member(Command, GlCommands))
    end, Expected),
    Variants = maps:get(variants, Uniform),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {{gl_vector, 4, gl_int}, element},
        {{gl_vector, 3, gl_int}, element},
        {{gl_vector, 2, gl_int}, element},
        {gl_int, element},
        {{gl_vector, 4, gl_float}, element},
        {{gl_vector, 3, gl_float}, element},
        {{gl_vector, 2, gl_float}, element},
        {gl_float, element}
    ]),
    ?assert(length(Expected) =< length(GlCommands)),
    ?assert(length(Expected) =< length(Variants)).

s052_assert_uniform_specs(Uniform) ->
    {uniform_value, {set, ExtraTypeVariants}} = maps:get(extra_type, Uniform),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, ExtraTypeVariants))
    end, [
        {gl, float, []},
        {undefined, vector2, [{gl, float, []}]},
        {undefined, vector3, [{gl, float, []}]},
        {undefined, vector4, [{gl, float, []}]},
        {gl, int, []},
        {undefined, vector2, [{gl, int, []}]},
        {undefined, vector3, [{gl, int, []}]},
        {undefined, vector4, [{gl, int, []}]}
    ]),
    ?assert(8 =< length(ExtraTypeVariants)),
    [{"Type", {set, TypeAtoms}}, {"Location", {gl, int, []}}, {"Value", {undefined, uniform_value, []}}] =
        maps:get(specs_params, Uniform),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, Uniform)),
    ?assertEqual(3, maps:get(function_arity, Uniform)).

s052_assert_uniform_clauses(Uniform) ->
    Clauses = maps:get(function_clauses, Uniform),
    ?assert(8 =< length(Clauses)),

    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s052_assert_before("glUniform2f", "glUniform1f", RawOrder),
    s052_assert_before("glUniform3f", "glUniform1f", RawOrder),
    s052_assert_before("glUniform4f", "glUniform1f", RawOrder),
    s052_assert_before("glUniform2i", "glUniform1i", RawOrder),
    s052_assert_before("glUniform3i", "glUniform1i", RawOrder),
    s052_assert_before("glUniform4i", "glUniform1i", RawOrder),

    s052_assert_vector_clause("f", "glUniform2f", 2, Clauses),
    s052_assert_vector_clause("f", "glUniform3f", 3, Clauses),
    s052_assert_vector_clause("f", "glUniform4f", 4, Clauses),
    s052_assert_vector_clause("i", "glUniform2i", 2, Clauses),
    s052_assert_vector_clause("i", "glUniform3i", 3, Clauses),
    s052_assert_vector_clause("i", "glUniform4i", 4, Clauses),
    s052_assert_scalar_clause("f", "glUniform1f", Clauses),
    s052_assert_scalar_clause("i", "glUniform1i", Clauses).

s052_assert_vector_clause(TypeAtom, RawFunction, VectorSize, Clauses) ->
    Clause = s052_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual(
        [{is_tuple, var}, {tuple_size, var, VectorSize}],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Location", do_nothing},
            {"Value", {gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s052_assert_scalar_clause(TypeAtom, RawFunction, Clauses) ->
    Clause = s052_find_clause(RawFunction, Clauses),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Location", do_nothing},
            {"Value", do_nothing}
        ],
        maps:get(params, Clause)
    ).

s052_assert_uniform_nifs(Uniform) ->
    NifFunctions = maps:get(nif_functions, Uniform),
    Float = {"GLfloat", "double", "enif_get_double", "enif_make_double"},
    Int = {"GLint", "int", "enif_get_int", "enif_make_int"},
    s052_assert_vector_nif(maps:get("glUniform2f", NifFunctions), 2, Float),
    s052_assert_vector_nif(maps:get("glUniform3f", NifFunctions), 3, Float),
    s052_assert_vector_nif(maps:get("glUniform4f", NifFunctions), 4, Float),
    s052_assert_vector_nif(maps:get("glUniform2i", NifFunctions), 2, Int),
    s052_assert_vector_nif(maps:get("glUniform3i", NifFunctions), 3, Int),
    s052_assert_vector_nif(maps:get("glUniform4i", NifFunctions), 4, Int).

s052_assert_vector_nif(NifData, VectorSize, ValueSpec) ->
    ?assertEqual(VectorSize + 1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}}]
            ++ [{"V" ++ integer_to_list(N), {gl_type, ValueSpec}} || N <- lists:seq(1, VectorSize)],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s052_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s052_assert_before(First, Second, Values) ->
    ?assert(s052_index_of(First, Values) < s052_index_of(Second, Values)).

s052_index_of(Value, Values) ->
    s052_index_of(Value, Values, 1).

s052_index_of(Value, [Value | _], Index) ->
    Index;
s052_index_of(Value, [_ | Rest], Index) ->
    s052_index_of(Value, Rest, Index + 1).

s052_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s052_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 64.
s064_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s064_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s064_emitter_unsigned_aggregate_uniform_setters_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard64-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),

        s064_assert_contains(GeneratedErl, <<"-export([uniform/3]).">>),
        s064_assert_not_contains(GeneratedErl, <<"-export([uniform_1ui/2]).">>),
        s064_assert_not_contains(GeneratedErl, <<"-export([uniform_2ui/3]).">>),
        s064_assert_not_contains(GeneratedErl, <<"-export([uniform_3ui/4]).">>),
        s064_assert_not_contains(GeneratedErl, <<"-export([uniform_4ui/5]).">>),
        s064_assert_contains(GeneratedErl, <<"-spec uniform(\n    Type :: d | f | i | ui,">>),
        s064_assert_contains(GeneratedErl, <<"gl:uint()">>),
        s064_assert_contains(GeneratedErl, <<"vector2(gl:uint())">>),
        s064_assert_contains(GeneratedErl, <<"vector3(gl:uint())">>),
        s064_assert_contains(GeneratedErl, <<"vector4(gl:uint())">>),
        s064_assert_contains(GeneratedErl, <<"uniform(ui, Location, Value) ->">>),
        s064_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform1ui_raw(Location, Value))">>),
        s064_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform2ui_raw(Location, V1, V2))">>),
        s064_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform3ui_raw(Location, V1, V2, V3))">>),
        s064_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glUniform4ui_raw(Location, V1, V2, V3, V4))">>),

        s064_assert_contains(GeneratedC, <<"glUniform1ui(arg_0, arg_1);">>),
        s064_assert_contains(GeneratedC, <<"glUniform2ui(arg_0, arg_1, arg_2);">>),
        s064_assert_contains(GeneratedC, <<"glUniform3ui(arg_0, arg_1, arg_2, arg_3);">>),
        s064_assert_contains(GeneratedC, <<"glUniform4ui(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
        s064_assert_contains(GeneratedC, <<"{\"glUniform1ui_raw\", 2, nif_glUniform1ui, 0}">>),
        s064_assert_contains(GeneratedC, <<"{\"glUniform2ui_raw\", 3, nif_glUniform2ui, 0}">>),
        s064_assert_contains(GeneratedC, <<"{\"glUniform3ui_raw\", 4, nif_glUniform3ui, 0}">>),
        s064_assert_contains(GeneratedC, <<"{\"glUniform4ui_raw\", 5, nif_glUniform4ui, 0}">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s064_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform", 3}, Functions)),
    s064_assert_direct_wrappers_absent(Functions),
    s064_assert_deferred_uniform_arrays_absent(Functions),

    Uniform = maps:get({"uniform", 3}, Functions),
    case s064_supports_unsigned_uniforms(Target) of
        true -> s064_assert_unsigned_uniforms_present(Uniform);
        false -> s064_assert_unsigned_uniforms_absent(Uniform, Functions)
    end.

s064_supports_unsigned_uniforms({gles, {2, 0}}) ->
    false;
s064_supports_unsigned_uniforms(_) ->
    true.

s064_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"uniform_1ui", 2},
        {"uniform_2ui", 3},
        {"uniform_3ui", 4},
        {"uniform_4ui", 5}
    ]).

s064_assert_deferred_uniform_arrays_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s064_assert_unsigned_uniforms_present(Uniform) ->
    s064_assert_type_atom(Uniform, ui),
    s064_assert_unsigned_extra_type(Uniform),
    s064_assert_unsigned_commands(Uniform),
    s064_assert_unsigned_clauses(Uniform),
    s064_assert_unsigned_nifs(Uniform).

s064_assert_type_atom(Uniform, Atom) ->
    [{"Type", {set, TypeAtoms}}, {"Location", {gl, int, []}}, {"Value", {undefined, uniform_value, []}}] =
        maps:get(specs_params, Uniform),
    ?assert(lists:member(Atom, TypeAtoms)).

s064_assert_unsigned_extra_type(Uniform) ->
    {uniform_value, {set, ExtraTypeVariants}} = maps:get(extra_type, Uniform),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, ExtraTypeVariants))
    end, [
        {gl, uint, []},
        {undefined, vector2, [{gl, uint, []}]},
        {undefined, vector3, [{gl, uint, []}]},
        {undefined, vector4, [{gl, uint, []}]}
    ]).

s064_assert_unsigned_commands(Uniform) ->
    Expected = [
        {"glUniform4ui", {gl_vector, 4, gl_uint}, element},
        {"glUniform3ui", {gl_vector, 3, gl_uint}, element},
        {"glUniform2ui", {gl_vector, 2, gl_uint}, element},
        {"glUniform1ui", gl_uint, element}
    ],
    GlCommands = maps:get(gl_commands, Uniform),
    Variants = maps:get(variants, Uniform),
    lists:foreach(fun(Command) ->
        ?assert(lists:member(Command, GlCommands))
    end, Expected),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {{gl_vector, 4, gl_uint}, element},
        {{gl_vector, 3, gl_uint}, element},
        {{gl_vector, 2, gl_uint}, element},
        {gl_uint, element}
    ]).

s064_assert_unsigned_clauses(Uniform) ->
    Clauses = maps:get(function_clauses, Uniform),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s064_assert_before("glUniform2ui", "glUniform1ui", RawOrder),
    s064_assert_before("glUniform3ui", "glUniform1ui", RawOrder),
    s064_assert_before("glUniform4ui", "glUniform1ui", RawOrder),
    s064_assert_vector_clause("glUniform2ui", 2, Clauses),
    s064_assert_vector_clause("glUniform3ui", 3, Clauses),
    s064_assert_vector_clause("glUniform4ui", 4, Clauses),
    s064_assert_scalar_clause("glUniform1ui", Clauses).

s064_assert_vector_clause(RawFunction, VectorSize, Clauses) ->
    Clause = s064_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual(
        [{is_tuple, var}, {tuple_size, var, VectorSize}],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {"ui", ignore},
            {"Location", do_nothing},
            {"Value", {gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s064_assert_scalar_clause(RawFunction, Clauses) ->
    Clause = s064_find_clause(RawFunction, Clauses),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {"ui", ignore},
            {"Location", do_nothing},
            {"Value", do_nothing}
        ],
        maps:get(params, Clause)
    ).

s064_assert_unsigned_nifs(Uniform) ->
    NifFunctions = maps:get(nif_functions, Uniform),
    Uint = {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"},
    s064_assert_scalar_nif(maps:get("glUniform1ui", NifFunctions), Uint),
    s064_assert_vector_nif(maps:get("glUniform2ui", NifFunctions), 2, Uint),
    s064_assert_vector_nif(maps:get("glUniform3ui", NifFunctions), 3, Uint),
    s064_assert_vector_nif(maps:get("glUniform4ui", NifFunctions), 4, Uint).

s064_assert_scalar_nif(NifData, ValueSpec) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
            {"Value", {gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s064_assert_vector_nif(NifData, VectorSize, ValueSpec) ->
    ?assertEqual(VectorSize + 1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}}]
            ++ [{"V" ++ integer_to_list(N), {gl_type, ValueSpec}} || N <- lists:seq(1, VectorSize)],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s064_assert_unsigned_uniforms_absent(Uniform, Functions) ->
    [{"Type", {set, TypeAtoms}}, {"Location", {gl, int, []}}, {"Value", {undefined, uniform_value, []}}] =
        maps:get(specs_params, Uniform),
    ?assertNot(lists:member(ui, TypeAtoms)),
    ?assertNot(generator_test_support:has_gl_command("glUniform1ui", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glUniform2ui", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glUniform3ui", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glUniform4ui", Functions)).

s064_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s064_assert_before(First, Second, Values) ->
    ?assert(s064_index_of(First, Values) < s064_index_of(Second, Values)).

s064_index_of(Value, Values) ->
    s064_index_of(Value, Values, 1).

s064_index_of(Value, [Value | _], Index) ->
    Index;
s064_index_of(Value, [_ | Rest], Index) ->
    s064_index_of(Value, Rest, Index + 1).

s064_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s064_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 69.
s069_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s069_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s069_emitter_numeric_aggregate_setters_test_() ->
    [
        {"gl 4.6", fun() ->
            s069_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([uniform/3]).">>,
                    <<"-export([vertex_attrib_i/3]).">>,
                    <<"-spec uniform(\n    Type :: d | f | i | ui,">>,
                    <<"-spec vertex_attrib_i(\n    Type :: ">>,
                    <<"?CALL_RAW_FUNC(glUniform4d_raw(Location, V1, V2, V3, V4))">>,
                    <<"?CALL_RAW_FUNC(glVertexAttribI4ui_raw(Index, V1, V2, V3, V4))">>
                ],
                [
                    <<"glUniform4d(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
                    <<"glVertexAttribI4ui(arg_0, arg_1, arg_2, arg_3, arg_4);">>
                ],
                [
                    <<"-export([uniform_1d/2]).">>,
                    <<"-export([vertex_attrib_i_1i/2]).">>,
                    <<"glVertexAttribI1iv_raw">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s069_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([vertex_attrib_i/3]).">>,
                    <<"-spec vertex_attrib_i(\n    Type :: i | ui,">>,
                    <<"?CALL_RAW_FUNC(glVertexAttribI4i_raw(Index, V1, V2, V3, V4))">>
                ],
                [
                    <<"glVertexAttribI4i(arg_0, arg_1, arg_2, arg_3, arg_4);">>
                ],
                [
                    <<"glUniform1d_raw">>,
                    <<"glUniform1d(">>,
                    <<"glVertexAttribI1iv_raw">>
                ]
            )
        end},
        {"gles 2.0", fun() ->
            s069_assert_emitted_surface(
                {gles, {2, 0}},
                [],
                [],
                [
                    <<"-export([vertex_attrib_i/3]).">>,
                    <<"glVertexAttribI1i_raw">>,
                    <<"glUniform1d_raw">>
                ]
            )
        end}
    ].

s069_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform", 3}, Functions)),
    s069_assert_direct_wrappers_absent(Functions),

    Uniform = maps:get({"uniform", 3}, Functions),
    case s069_supports_double_uniforms(Target) of
        true -> s069_assert_double_uniforms_present(Uniform);
        false -> s069_assert_double_uniforms_absent(Uniform, Functions)
    end,

    case s069_supports_integer_vertex_attrib(Target) of
        true ->
            ?assert(maps:is_key({"vertex_attrib_i", 3}, Functions)),
            s069_assert_integer_vertex_attrib(Target, maps:get({"vertex_attrib_i", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"vertex_attrib_i", 3}, Functions)),
            s069_assert_integer_vertex_attrib_absent(Functions)
    end,
    s069_assert_deferred_neighbors_absent(Functions).

s069_supports_double_uniforms({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s069_supports_double_uniforms({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s069_supports_double_uniforms(_) ->
    false.

s069_supports_integer_vertex_attrib({gles, {2, 0}}) ->
    false;
s069_supports_integer_vertex_attrib(_) ->
    true.

s069_assert_direct_wrappers_absent(Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- [
        {"uniform_1d", 2},
        {"uniform_2d", 3},
        {"uniform_3d", 4},
        {"uniform_4d", 5},
        {"vertex_attrib_i_1i", 2},
        {"vertex_attrib_i_1ui", 2},
        {"vertex_attrib_i_4i", 5},
        {"vertex_attrib_i_4ui", 5}
    ]].

s069_assert_double_uniforms_present(Uniform) ->
    s069_assert_type_atom(Uniform, d),
    s069_assert_extra_type_family(Uniform, uniform_value, {gl, double, []}),
    s069_assert_uniform_command_family(Uniform, d, gl_double),
    s069_assert_uniform_clause_family(Uniform, d),
    s069_assert_uniform_nif_family(Uniform, d, {"GLdouble", "double", "enif_get_double", "enif_make_double"}).

s069_assert_double_uniforms_absent(Uniform, Functions) ->
    [{"Type", {set, TypeAtoms}}, {"Location", {gl, int, []}}, {"Value", {undefined, uniform_value, []}}] =
        maps:get(specs_params, Uniform),
    ?assertNot(lists:member(d, TypeAtoms)),
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- [
        "glUniform1d",
        "glUniform2d",
        "glUniform3d",
        "glUniform4d"
    ]].

s069_assert_integer_vertex_attrib(Target, VertexAttribI) ->
    ?assertEqual(
        [
            {in, "Index", gl_uint},
            {in, "Values", gl_x}
        ],
        maps:get(params_specs, VertexAttribI)
    ),
    [{"Type", {set, TypeAtoms}}, {"Index", {gl, uint, []}}, {"Values", {undefined, vertex_attrib_i_value, []}}] =
        maps:get(specs_params, VertexAttribI),
    ?assert(lists:member(i, TypeAtoms)),
    ?assert(lists:member(ui, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, VertexAttribI)),
    ?assertEqual(3, maps:get(function_arity, VertexAttribI)),
    case s069_is_desktop(Target) of
        true ->
            s069_assert_extra_type_family(VertexAttribI, vertex_attrib_i_value, {gl, int, []}),
            s069_assert_extra_type_family(VertexAttribI, vertex_attrib_i_value, {gl, uint, []}),
            s069_assert_vertex_attrib_i_command_family(VertexAttribI, i, gl_int),
            s069_assert_vertex_attrib_i_command_family(VertexAttribI, ui, gl_uint),
            s069_assert_vertex_attrib_i_clause_family(VertexAttribI, i),
            s069_assert_vertex_attrib_i_clause_family(VertexAttribI, ui),
            s069_assert_vertex_attrib_i_nif_family(VertexAttribI, i, {"GLint", "int", "enif_get_int", "enif_make_int"}),
            s069_assert_vertex_attrib_i_nif_family(VertexAttribI, ui, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"});
        false ->
            s069_assert_vector4_only_extra_type(VertexAttribI, vertex_attrib_i_value, {gl, int, []}),
            s069_assert_vector4_only_extra_type(VertexAttribI, vertex_attrib_i_value, {gl, uint, []}),
            s069_assert_vertex_attrib_i_vector4_command(VertexAttribI, i, gl_int),
            s069_assert_vertex_attrib_i_vector4_command(VertexAttribI, ui, gl_uint),
            s069_assert_vector4_clause(maps:get(function_clauses, VertexAttribI), "glVertexAttribI4i", "Values"),
            s069_assert_vector4_clause(maps:get(function_clauses, VertexAttribI), "glVertexAttribI4ui", "Values"),
            s069_assert_vector4_nif(
                maps:get("glVertexAttribI4i", maps:get(nif_functions, VertexAttribI)),
                "Index",
                {"GLint", "int", "enif_get_int", "enif_make_int"}
            ),
            s069_assert_vector4_nif(
                maps:get("glVertexAttribI4ui", maps:get(nif_functions, VertexAttribI)),
                "Index",
                {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}
            ),
            s069_assert_es_vertex_attrib_i_smaller_arities_absent(VertexAttribI)
    end.

s069_is_desktop({gl, _Version}) ->
    true;
s069_is_desktop(_) ->
    false.

s069_assert_integer_vertex_attrib_absent(Functions) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- [
        "glVertexAttribI1i",
        "glVertexAttribI2i",
        "glVertexAttribI3i",
        "glVertexAttribI4i",
        "glVertexAttribI1ui",
        "glVertexAttribI2ui",
        "glVertexAttribI3ui",
        "glVertexAttribI4ui"
    ]].

s069_assert_deferred_neighbors_absent(Functions) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- [
        "glVertexAttribI1iv",
        "glVertexAttribI2iv",
        "glVertexAttribI3iv",
        "glVertexAttribI4iv",
        "glVertexAttribI1uiv",
        "glVertexAttribI2uiv",
        "glVertexAttribI3uiv",
        "glVertexAttribI4uiv"
    ]].

s069_assert_type_atom(FunctionData, Atom) ->
    [{"Type", {set, TypeAtoms}} | _] = maps:get(specs_params, FunctionData),
    ?assert(lists:member(Atom, TypeAtoms)).

s069_assert_extra_type_family(FunctionData, ExtraTypeName, ScalarSpec) ->
    {ExtraTypeName, {set, ExtraTypeVariants}} = maps:get(extra_type, FunctionData),
    [?assert(lists:member(TypeSpec, ExtraTypeVariants)) || TypeSpec <- [
        ScalarSpec,
        {undefined, vector2, [ScalarSpec]},
        {undefined, vector3, [ScalarSpec]},
        {undefined, vector4, [ScalarSpec]}
    ]].

s069_assert_vector4_only_extra_type(FunctionData, ExtraTypeName, ScalarSpec) ->
    {ExtraTypeName, {set, ExtraTypeVariants}} = maps:get(extra_type, FunctionData),
    ?assert(lists:member({undefined, vector4, [ScalarSpec]}, ExtraTypeVariants)),
    ?assertNot(lists:member(ScalarSpec, ExtraTypeVariants)),
    ?assertNot(lists:member({undefined, vector2, [ScalarSpec]}, ExtraTypeVariants)),
    ?assertNot(lists:member({undefined, vector3, [ScalarSpec]}, ExtraTypeVariants)).

s069_assert_uniform_command_family(Uniform, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    s069_assert_command_family(Uniform, "glUniform", Suffix, GlType),
    s069_assert_variant_family(Uniform, GlType).

s069_assert_vertex_attrib_i_command_family(VertexAttribI, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    s069_assert_command_family(VertexAttribI, "glVertexAttribI", Suffix, GlType),
    s069_assert_variant_family(VertexAttribI, GlType).

s069_assert_vertex_attrib_i_vector4_command(VertexAttribI, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    GlCommands = maps:get(gl_commands, VertexAttribI),
    ?assert(lists:member({"glVertexAttribI4" ++ Suffix, {gl_vector, 4, GlType}, element}, GlCommands)),
    ?assertNot(lists:member({"glVertexAttribI1" ++ Suffix, GlType, element}, GlCommands)),
    ?assertNot(lists:member({"glVertexAttribI2" ++ Suffix, {gl_vector, 2, GlType}, element}, GlCommands)),
    ?assertNot(lists:member({"glVertexAttribI3" ++ Suffix, {gl_vector, 3, GlType}, element}, GlCommands)),
    Variants = maps:get(variants, VertexAttribI),
    ?assert(lists:member({{gl_vector, 4, GlType}, element}, Variants)).

s069_assert_command_family(FunctionData, Prefix, Suffix, GlType) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    [?assert(lists:member(Command, GlCommands)) || Command <- [
        {Prefix ++ "1" ++ Suffix, GlType, element},
        {Prefix ++ "2" ++ Suffix, {gl_vector, 2, GlType}, element},
        {Prefix ++ "3" ++ Suffix, {gl_vector, 3, GlType}, element},
        {Prefix ++ "4" ++ Suffix, {gl_vector, 4, GlType}, element}
    ]].

s069_assert_variant_family(FunctionData, GlType) ->
    Variants = maps:get(variants, FunctionData),
    [?assert(lists:member(Variant, Variants)) || Variant <- [
        {GlType, element},
        {{gl_vector, 2, GlType}, element},
        {{gl_vector, 3, GlType}, element},
        {{gl_vector, 4, GlType}, element}
    ]].

s069_assert_uniform_clause_family(Uniform, TypeAtom) ->
    s069_assert_clause_family(maps:get(function_clauses, Uniform), "glUniform", atom_to_list(TypeAtom), "Value").

s069_assert_vertex_attrib_i_clause_family(VertexAttribI, TypeAtom) ->
    s069_assert_clause_family(maps:get(function_clauses, VertexAttribI), "glVertexAttribI", atom_to_list(TypeAtom), "Values").

s069_assert_clause_family(Clauses, Prefix, Suffix, ValueName) ->
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s069_assert_before(Prefix ++ "2" ++ Suffix, Prefix ++ "1" ++ Suffix, RawOrder),
    s069_assert_before(Prefix ++ "3" ++ Suffix, Prefix ++ "1" ++ Suffix, RawOrder),
    s069_assert_before(Prefix ++ "4" ++ Suffix, Prefix ++ "1" ++ Suffix, RawOrder),
    s069_assert_vector_clause(Prefix ++ "2" ++ Suffix, 2, ValueName, Clauses),
    s069_assert_vector_clause(Prefix ++ "3" ++ Suffix, 3, ValueName, Clauses),
    s069_assert_vector_clause(Prefix ++ "4" ++ Suffix, 4, ValueName, Clauses),
    s069_assert_scalar_clause(Prefix ++ "1" ++ Suffix, ValueName, Clauses).

s069_assert_vector_clause(RawFunction, VectorSize, ValueName, Clauses) ->
    Clause = s069_find_clause(RawFunction, Clauses),
    ?assertEqual(ValueName, maps:get(guard_var, Clause)),
    ?assertEqual(
        [{is_tuple, var}, {tuple_size, var, VectorSize}],
        maps:get(guards, Clause)
    ),
    ?assert(lists:member({ValueName, {gl_vector_to_list, VectorSize}}, maps:get(params, Clause))).

s069_assert_vector4_clause(Clauses, RawFunction, ValueName) ->
    s069_assert_vector_clause(RawFunction, 4, ValueName, Clauses).

s069_assert_scalar_clause(RawFunction, ValueName, Clauses) ->
    Clause = s069_find_clause(RawFunction, Clauses),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assert(lists:member({ValueName, do_nothing}, maps:get(params, Clause))).

s069_assert_uniform_nif_family(Uniform, TypeAtom, ValueSpec) ->
    s069_assert_nif_family(maps:get(nif_functions, Uniform), "glUniform", atom_to_list(TypeAtom), "Location", ValueSpec).

s069_assert_vertex_attrib_i_nif_family(VertexAttribI, TypeAtom, ValueSpec) ->
    s069_assert_nif_family(maps:get(nif_functions, VertexAttribI), "glVertexAttribI", atom_to_list(TypeAtom), "Index", ValueSpec).

s069_assert_nif_family(NifFunctions, Prefix, Suffix, FirstParam, ValueSpec) ->
    s069_assert_scalar_nif(maps:get(Prefix ++ "1" ++ Suffix, NifFunctions), FirstParam, ValueSpec),
    s069_assert_vector_nif(maps:get(Prefix ++ "2" ++ Suffix, NifFunctions), 2, FirstParam, ValueSpec),
    s069_assert_vector_nif(maps:get(Prefix ++ "3" ++ Suffix, NifFunctions), 3, FirstParam, ValueSpec),
    s069_assert_vector_nif(maps:get(Prefix ++ "4" ++ Suffix, NifFunctions), 4, FirstParam, ValueSpec).

s069_assert_scalar_nif(NifData, FirstParam, ValueSpec) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {FirstParam, {gl_type, s069_first_param_nif_data(FirstParam)}},
            {"Values", {gl_type, ValueSpec}}
        ],
        s069_normalize_nif_params(maps:get(params, NifData))
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s069_assert_vector_nif(NifData, VectorSize, FirstParam, ValueSpec) ->
    ?assertEqual(VectorSize + 1, maps:get(arity, NifData)),
    ?assertEqual(
        [{FirstParam, {gl_type, s069_first_param_nif_data(FirstParam)}}]
            ++ [{"V" ++ integer_to_list(N), {gl_type, ValueSpec}} || N <- lists:seq(1, VectorSize)],
        s069_normalize_nif_params(maps:get(params, NifData))
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s069_assert_vector4_nif(NifData, FirstParam, ValueSpec) ->
    s069_assert_vector_nif(NifData, 4, FirstParam, ValueSpec).

s069_assert_es_vertex_attrib_i_smaller_arities_absent(VertexAttribI) ->
    NifFunctions = maps:get(nif_functions, VertexAttribI),
    [?assertNot(maps:is_key(Command, NifFunctions)) || Command <- [
        "glVertexAttribI1i",
        "glVertexAttribI2i",
        "glVertexAttribI3i",
        "glVertexAttribI1ui",
        "glVertexAttribI2ui",
        "glVertexAttribI3ui"
    ]].

s069_first_param_nif_data("Location") ->
    {"GLint", "int", "enif_get_int", "enif_make_int"};
s069_first_param_nif_data("Index") ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s069_normalize_nif_params(Params) ->
    [{s069_normalize_value_name(Name), Spec} || {Name, Spec} <- Params].

s069_normalize_value_name("Value") ->
    "Values";
s069_normalize_value_name(Name) ->
    Name.

s069_assert_emitted_surface(Target, ErlIncludes, CIncludes, Excludes) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard69-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        [s069_assert_contains(GeneratedErl, Needle) || Needle <- ErlIncludes],
        [s069_assert_contains(GeneratedC, Needle) || Needle <- CIncludes],
        [s069_assert_not_contains(GeneratedErl, Needle) || Needle <- Excludes],
        [s069_assert_not_contains(GeneratedC, Needle) || Needle <- Excludes]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s069_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s069_assert_before(First, Second, Values) ->
    ?assert(s069_index_of(First, Values) < s069_index_of(Second, Values)).

s069_index_of(Value, Values) ->
    s069_index_of(Value, Values, 1).

s069_index_of(Value, [Value | _], Index) ->
    Index;
s069_index_of(Value, [_ | Rest], Index) ->
    s069_index_of(Value, Rest, Index + 1).

s069_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s069_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 71.
s071_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s071_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s071_emitter_point_parameter_test_() ->
    [
        {"gl 4.6", fun() ->
            s071_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([point_parameter/3]).">>,
                    <<"-type point_parameter_value() ::">>,
                    <<"gl:float()">>,
                    <<"[gl:float()]">>,
                    <<"gl:int()">>,
                    <<"[gl:int()]">>,
                    <<"-spec point_parameter(\n    Type :: f | i,">>,
                    <<"point_parameter(f, ParamName, Param) when is_list(Param) ->">>,
                    <<"point_parameter(f, ParamName, Param) ->">>,
                    <<"point_parameter(i, ParamName, Param) when is_list(Param) ->">>,
                    <<"point_parameter(i, ParamName, Param) ->">>,
                    <<"?CALL_RAW_FUNC(glPointParameterfv_raw(NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glPointParameterf_raw(NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glPointParameteriv_raw(NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glPointParameteri_raw(NewParamName, Param))">>
                ],
                [
                    <<"glPointParameterfv(arg_0, arg_1_array);">>,
                    <<"glPointParameterf(arg_0, arg_1);">>,
                    <<"glPointParameteriv(arg_0, arg_1_array);">>,
                    <<"glPointParameteri(arg_0, arg_1);">>
                ],
                [
                    <<"point_parameter_name_arb">>,
                    <<"get_point_parameter">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s071_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [],
                [],
                [
                    <<"-export([point_parameter/3]).">>,
                    <<"glPointParameterf_raw">>,
                    <<"glPointParameterfv_raw">>,
                    <<"glPointParameteri_raw">>,
                    <<"glPointParameteriv_raw">>,
                    <<"point_parameter_name_arb">>
                ]
            )
        end}
    ].

s071_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case Target of
        {gl, _} ->
            ?assert(maps:is_key({"point_parameter", 3}, Functions)),
            ?assertNot(maps:is_key({"point_parameter", 2}, Functions)),
            s071_assert_point_parameter(BindingData, maps:get({"point_parameter", 3}, Functions), Target);
        {gles, _} ->
            ?assertNot(maps:is_key({"point_parameter", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glPointParameterf", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glPointParameterfv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glPointParameteri", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glPointParameteriv", Functions))
    end.

s071_assert_point_parameter(BindingData, FunctionData, Target) ->
    s071_assert_enum_contains(BindingData, "point_parameter_name", "point_fade_threshold_size"),
    ?assertNot(lists:member("point_distance_attenuation", maps:get("point_parameter_name", maps:get(enum_types, BindingData)))),
    ?assertNot(maps:is_key("point_parameter_name_arb", maps:get(enum_types, BindingData))),
    ?assertEqual(
        [
            {in, "ParamName", {gl_enum, "PointParameterNameARB", point_parameter_name}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    [{"Type", {set, TypeAtoms}}, {"ParamName", {undefined, point_parameter_name, []}}, {"Param", {undefined, point_parameter_value, []}}] =
        maps:get(specs_params, FunctionData),
    ?assertEqual(s071_expected_type_atoms(Target), TypeAtoms),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s071_assert_extra_type(FunctionData, Target),
    s071_assert_commands(FunctionData, Target),
    s071_assert_clauses(FunctionData, Target),
    s071_assert_nifs(FunctionData, Target).

s071_expected_type_atoms({gl, _}) ->
    [f, i].

s071_assert_extra_type(FunctionData, Target) ->
    {point_parameter_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    Expected = case Target of
        {gl, _} ->
            [{gl, float, []}, {list, {gl, float, []}}, {gl, int, []}, {list, {gl, int, []}}]
    end,
    ?assertEqual(lists:sort(Expected), lists:sort(Variants)).

s071_assert_commands(FunctionData, Target) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    ?assert(lists:member({"glPointParameterfv", gl_float, array}, GlCommands)),
    ?assert(lists:member({"glPointParameterf", gl_float, element}, GlCommands)),
    case Target of
        {gl, _} ->
            ?assert(lists:member({"glPointParameteriv", gl_int, array}, GlCommands)),
            ?assert(lists:member({"glPointParameteri", gl_int, element}, GlCommands)),
            ?assertEqual(
                lists:sort([{gl_float, array}, {gl_float, element}, {gl_int, array}, {gl_int, element}]),
                lists:sort(maps:get(variants, FunctionData))
            )
    end.

s071_assert_clauses(FunctionData, Target) ->
    Clauses = maps:get(function_clauses, FunctionData),
    s071_assert_array_clause("glPointParameterfv", "f", Clauses),
    s071_assert_element_clause("glPointParameterf", "f", Clauses),
    case Target of
        {gl, _} ->
            s071_assert_array_clause("glPointParameteriv", "i", Clauses),
            s071_assert_element_clause("glPointParameteri", "i", Clauses)
    end.

s071_assert_array_clause(RawFunction, Suffix, Clauses) ->
    Clause = s071_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    s071_assert_clause_params(Suffix, Clause).

s071_assert_element_clause(RawFunction, Suffix, Clauses) ->
    Clause = s071_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([], maps:get(guards, Clause)),
    s071_assert_clause_params(Suffix, Clause).

s071_assert_clause_params(Suffix, Clause) ->
    [
        {Suffix, ignore},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("point_fade_threshold_size", 1, ParamNameTransformMap)),
    ?assertNot(lists:keymember("point_distance_attenuation", 1, ParamNameTransformMap)).

s071_assert_nifs(FunctionData, Target) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    Enum = s071_enum_nif_data(),
    Float = s071_float_nif_data(),
    RawFloat = s071_float_raw_nif_data(),
    Int = s071_int_nif_data(),
    RawInt = s071_int_raw_nif_data(),
    s071_assert_nif(maps:get("glPointParameterfv", NifFunctions), [{"ParamName", Enum}, {"Param", {list_gl_type, RawFloat}}]),
    s071_assert_nif(maps:get("glPointParameterf", NifFunctions), [{"ParamName", Enum}, {"Param", Float}]),
    case Target of
        {gl, _} ->
            s071_assert_nif(maps:get("glPointParameteriv", NifFunctions), [{"ParamName", Enum}, {"Param", {list_gl_type, RawInt}}]),
            s071_assert_nif(maps:get("glPointParameteri", NifFunctions), [{"ParamName", Enum}, {"Param", Int}])
    end.

s071_assert_nif(NifData, Params) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s071_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s071_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s071_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard71-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s071_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s071_assert_contains(C, Needle) || Needle <- RequiredC],
        [s071_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s071_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s071_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s071_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s071_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s071_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

s071_float_raw_nif_data() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s071_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s071_int_raw_nif_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

%% Historical shard 72.
s072_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s072_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s072_emitter_draw_read_buffer_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s072_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([draw_buffer/1]).">>,
                    <<"-export([read_buffer/1]).">>,
                    <<"-type draw_buffer_mode() ::">>,
                    <<"-type read_buffer_mode() ::">>,
                    <<"-spec draw_buffer(Buffer :: draw_buffer_mode()) -> ok | {error, atom()}.">>,
                    <<"-spec read_buffer(Source :: read_buffer_mode()) -> ok | {error, atom()}.">>,
                    <<"draw_buffer(Buffer) ->">>,
                    <<"read_buffer(Source) ->">>,
                    <<"?CALL_RAW_FUNC(glDrawBuffer_raw(NewBuffer))">>,
                    <<"?CALL_RAW_FUNC(glReadBuffer_raw(NewSource))">>
                ],
                [
                    <<"glDrawBuffer(arg_0);">>,
                    <<"glReadBuffer(arg_0);">>
                ],
                []
            )
        end},
        {"gles 3.2", fun() ->
            s072_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([read_buffer/1]).">>,
                    <<"-type read_buffer_mode() ::">>,
                    <<"-spec read_buffer(Source :: read_buffer_mode()) -> ok | {error, atom()}.">>,
                    <<"read_buffer(Source) ->">>,
                    <<"?CALL_RAW_FUNC(glReadBuffer_raw(NewSource))">>
                ],
                [
                    <<"glReadBuffer(arg_0);">>
                ],
                [
                    <<"-export([draw_buffer/1]).">>,
                    <<"draw_buffer(Buffer) ->">>,
                    <<"glDrawBuffer_raw">>,
                    <<"glNamedFramebufferDrawBuffer_raw">>
                ]
            )
        end},
        {"gles 2.0", fun() ->
            s072_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [],
                [],
                [
                    <<"-export([draw_buffer/1]).">>,
                    <<"-export([read_buffer/1]).">>,
                    <<"glDrawBuffer_raw">>,
                    <<"glReadBuffer_raw">>,
                    <<"draw_buffer_mode()">>,
                    <<"read_buffer_mode()">>
                ]
            )
        end}
    ].

s072_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s072_assert_presence(Target, Functions),
    s072_assert_deferred_neighbors_absent(Functions),
    s072_assert_present_paths(Target, BindingData, Functions).

s072_assert_presence({gl, _}, Functions) ->
    ?assert(maps:is_key({"draw_buffer", 1}, Functions)),
    ?assert(maps:is_key({"read_buffer", 1}, Functions));
s072_assert_presence({gles, {2, 0}}, Functions) ->
    ?assertNot(maps:is_key({"draw_buffer", 1}, Functions)),
    ?assertNot(maps:is_key({"read_buffer", 1}, Functions));
s072_assert_presence({gles, _}, Functions) ->
    ?assertNot(maps:is_key({"draw_buffer", 1}, Functions)),
    ?assert(maps:is_key({"read_buffer", 1}, Functions)).

s072_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s072_assert_present_paths({gl, _}, BindingData, Functions) ->
    s072_assert_draw_buffer(BindingData, maps:get({"draw_buffer", 1}, Functions)),
    s072_assert_read_buffer(BindingData, maps:get({"read_buffer", 1}, Functions));
s072_assert_present_paths({gles, {2, 0}}, _BindingData, _Functions) ->
    ok;
s072_assert_present_paths({gles, _}, BindingData, Functions) ->
    s072_assert_read_buffer(BindingData, maps:get({"read_buffer", 1}, Functions)).

s072_assert_draw_buffer(BindingData, FunctionData) ->
    s072_assert_enum_contains(BindingData, "draw_buffer_mode", "color_attachment0"),
    s072_assert_enum_contains(BindingData, "draw_buffer_mode", "none"),
    s072_assert_direct_enum(
        FunctionData,
        "glDrawBuffer",
        {in, "Buffer", {gl_enum, "DrawBufferMode"}},
        {"Buffer", {undefined, draw_buffer_mode, []}},
        "Buffer",
        ["color_attachment0", "back", "none"]
    ).

s072_assert_read_buffer(BindingData, FunctionData) ->
    s072_assert_enum_contains(BindingData, "read_buffer_mode", "color_attachment0"),
    s072_assert_enum_contains(BindingData, "read_buffer_mode", "none"),
    s072_assert_direct_enum(
        FunctionData,
        "glReadBuffer",
        {in, "Source", {gl_enum, "ReadBufferMode"}},
        {"Source", {undefined, read_buffer_mode, []}},
        "Source",
        ["color_attachment0", "back", "none"]
    ).

s072_assert_direct_enum(FunctionData, GlCommand, ParamSpec, SpecParam, ParamName, ExpectedAtoms) ->
    ?assertEqual([ParamSpec], maps:get(params_specs, FunctionData)),
    ?assertEqual([SpecParam], maps:get(specs_params, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    [{ParamName, {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- ExpectedAtoms],

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(GlCommand, NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    ?assertEqual([{ParamName, s072_enum_nif_data()}], maps:get(params, NifData)).

s072_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s072_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard72-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s072_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s072_assert_contains(C, Needle) || Needle <- RequiredC],
        [s072_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s072_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s072_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s072_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s072_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 75.
s075_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s075_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s075_emitter_typed_integer_sampler_parameter_test_() ->
    [
        {"gl 4.6", fun() ->
            s075_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([sampler_parameter_i/4]).">>,
                    <<"-type sampler_parameter_i_name() ::">>,
                    <<"-type sampler_parameter_i_value() ::">>,
                    <<"-spec sampler_parameter_i(\n    Type :: i | ui,">>,
                    <<"sampler_parameter_i(i, Sampler, ParamName, Param) when is_list(Param) ->">>,
                    <<"sampler_parameter_i(ui, Sampler, ParamName, Param) when is_list(Param) ->">>,
                    <<"?CALL_RAW_FUNC(glSamplerParameterIiv_raw(Sampler, NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glSamplerParameterIuiv_raw(Sampler, NewParamName, Param))">>
                ],
                [
                    <<"glSamplerParameterIiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glSamplerParameterIuiv(arg_0, arg_1, arg_2_array);">>,
                    <<"{\"glSamplerParameterIiv_raw\", 3, nif_glSamplerParameterIiv, 0}">>,
                    <<"{\"glSamplerParameterIuiv_raw\", 3, nif_glSamplerParameterIuiv, 0}">>
                ],
                [
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s075_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([sampler_parameter_i/4]).">>,
                    <<"sampler_parameter_i_name()">>,
                    <<"texture_wrap_s">>,
                    <<"glSamplerParameterIiv_raw">>,
                    <<"glSamplerParameterIuiv_raw">>
                ],
                [
                    <<"glSamplerParameterIiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glSamplerParameterIuiv(arg_0, arg_1, arg_2_array);">>
                ],
                [
                ]
            )
        end},
        {"gles 3.1", fun() ->
            s075_assert_emitted_surface(
                {gles, {3, 1}},
                "OpenGL ES 3.1",
                [],
                [],
                [
                    <<"-export([sampler_parameter_i/4]).">>,
                    <<"glSamplerParameterIiv">>,
                    <<"glSamplerParameterIuiv">>
                ]
            )
        end}
    ].

s075_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s075_supports_sampler_parameter_i(Target) of
        true ->
            ?assert(maps:is_key({"sampler_parameter_i", 4}, Functions)),
            ?assertNot(maps:is_key({"sampler_parameter_i", 3}, Functions)),
            s075_assert_sampler_parameter_i(BindingData, maps:get({"sampler_parameter_i", 4}, Functions));
        false ->
            ?assertNot(maps:is_key({"sampler_parameter_i", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glSamplerParameterIiv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glSamplerParameterIuiv", Functions))
    end,
    s075_assert_deferred_sampler_parameter_neighbors_absent(Functions).

s075_supports_sampler_parameter_i({gl, _Version}) ->
    true;
s075_supports_sampler_parameter_i({gles, {3, 2}}) ->
    true;
s075_supports_sampler_parameter_i(_) ->
    false.

s075_assert_deferred_sampler_parameter_neighbors_absent(Functions) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- [
    ]].

s075_assert_sampler_parameter_i(BindingData, FunctionData) ->
    s075_assert_enum_contains(BindingData, "sampler_parameter_i_name", "texture_wrap_s"),
    s075_assert_enum_contains(BindingData, "sampler_parameter_i_name", "texture_min_filter"),
    s075_assert_enum_not_contains(BindingData, "sampler_parameter_i_name", "texture_border_color"),
    ?assertEqual(
        [
            {in, "Sampler", {gl_object, sampler}},
            {in, "ParamName", {gl_enum, "SamplerParameterI", sampler_parameter_i_name}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, ui]}},
            {"Sampler", {undefined, sampler, []}},
            {"ParamName", {undefined, sampler_parameter_i_name, []}},
            {"Param", {undefined, sampler_parameter_i_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s075_assert_extra_type(FunctionData),
    s075_assert_commands(FunctionData),
    s075_assert_clauses(FunctionData),
    s075_assert_nifs(FunctionData).

s075_assert_extra_type(FunctionData) ->
    {sampler_parameter_i_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assertEqual([{list, {gl, int, []}}, {list, {gl, uint, []}}], Variants).

s075_assert_commands(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    ?assert(lists:member({"glSamplerParameterIiv", gl_int, array}, GlCommands)),
    ?assert(lists:member({"glSamplerParameterIuiv", gl_uint, array}, GlCommands)),
    ?assertEqual(lists:sort([{gl_int, array}, {gl_uint, array}]), lists:sort(maps:get(variants, FunctionData))).

s075_assert_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    s075_assert_array_clause("glSamplerParameterIiv", "i", Clauses),
    s075_assert_array_clause("glSamplerParameterIuiv", "ui", Clauses).

s075_assert_array_clause(RawFunction, Suffix, Clauses) ->
    Clause = s075_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    [
        {Suffix, ignore},
        {"Sampler", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_min_filter", 1, ParamNameTransformMap)),
    ?assertNot(lists:keymember("texture_border_color", 1, ParamNameTransformMap)).

s075_assert_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s075_assert_nif(
        maps:get("glSamplerParameterIiv", NifFunctions),
        [{"Sampler", s075_uint_nif_data()}, {"ParamName", s075_enum_nif_data()}, {"Param", {list_gl_type, s075_int_raw_nif_data()}}]
    ),
    s075_assert_nif(
        maps:get("glSamplerParameterIuiv", NifFunctions),
        [{"Sampler", s075_uint_nif_data()}, {"ParamName", s075_enum_nif_data()}, {"Param", {list_gl_type, s075_uint_raw_nif_data()}}]
    ).

s075_assert_nif(NifData, Params) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s075_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s075_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s075_assert_enum_not_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assertNot(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s075_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard75-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s075_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s075_assert_contains(C, Needle) || Needle <- RequiredC],
        [s075_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s075_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s075_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s075_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s075_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s075_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s075_int_raw_nif_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s075_uint_raw_nif_data() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

%% Historical shard 81.
s081_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s081_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s081_emitter_image_compute_controls_test_() ->
    [
        {"gl 4.6", fun() ->
            s081_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([bind_image_texture/7]).">>,
                    <<"-export([dispatch_compute/3]).">>,
                    <<"-export_type([buffer_access/0]).">>,
                    <<"-type buffer_access() ::">>,
                    <<"read_write">>,
                    <<"-spec bind_image_texture(\n    Unit :: gl:uint(),\n    Texture :: texture(),\n    Level :: gl:int(),\n    Layered :: gl:boolean(),\n    Layer :: gl:int(),\n    Access :: buffer_access(),\n    Format :: internal_format()">>,
                    <<"-spec dispatch_compute(\n    NumGroupsX :: gl:uint(),\n    NumGroupsY :: gl:uint(),\n    NumGroupsZ :: gl:uint()">>,
                    <<"gl:bind_image_texture(0, Texture, 0, false, 0, read_write, rgba8).">>,
                    <<"gl:dispatch_compute(0, 0, 0).">>,
                    <<"?CALL_RAW_FUNC(glBindImageTexture_raw(Unit, Texture, Level, Layered, Layer, NewAccess, NewFormat))">>,
                    <<"?CALL_RAW_FUNC(glDispatchCompute_raw(NumGroupsX, NumGroupsY, NumGroupsZ))">>
                ],
                [
                    <<"glBindImageTexture(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>,
                    <<"glDispatchCompute(arg_0, arg_1, arg_2);">>,
                    <<"{\"glBindImageTexture_raw\", 7, nif_glBindImageTexture, 0}">>,
                    <<"{\"glDispatchCompute_raw\", 3, nif_glDispatchCompute, 0}">>
                ],
                s081_deferred_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s081_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([bind_image_texture/7]).">>,
                    <<"-export([dispatch_compute/3]).">>,
                    <<"glBindImageTexture_raw">>,
                    <<"glDispatchCompute_raw">>
                ],
                [
                    <<"glBindImageTexture(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>,
                    <<"glDispatchCompute(arg_0, arg_1, arg_2);">>
                ],
                s081_deferred_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s081_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                [
                    <<"-export([bind_image_texture/7]).">>,
                    <<"-export([dispatch_compute/3]).">>,
                    <<"glBindImageTexture_raw">>,
                    <<"glDispatchCompute_raw">>
                ]
            )
        end}
    ].

s081_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s081_supports_image_compute(Target) of
        true ->
            ?assert(maps:is_key({"bind_image_texture", 7}, Functions)),
            ?assert(maps:is_key({"dispatch_compute", 3}, Functions)),
            s081_assert_buffer_access_enum(BindingData),
            s081_assert_bind_image_texture(BindingData, maps:get({"bind_image_texture", 7}, Functions)),
            s081_assert_dispatch_compute(maps:get({"dispatch_compute", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"bind_image_texture", 7}, Functions)),
            ?assertNot(maps:is_key({"dispatch_compute", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glBindImageTexture", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glDispatchCompute", Functions))
    end,
    s081_assert_deferred_neighbors_absent(Functions).

s081_supports_image_compute({gl, {4, 6}}) ->
    true;
s081_supports_image_compute({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s081_supports_image_compute(_) ->
    false.

s081_assert_buffer_access_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("buffer_access", EnumTypes)),
    ?assertNot(maps:is_key("buffer_access_arb", EnumTypes)),
    Access = maps:get("buffer_access", EnumTypes),
    ?assert(lists:member("read_only", Access)),
    ?assert(lists:member("write_only", Access)),
    ?assert(lists:member("read_write", Access)).

s081_assert_bind_image_texture(BindingData, FunctionData) ->
    s081_assert_enum_contains(BindingData, "internal_format", "rgba8"),
    ?assertEqual("glBindImageTexture", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Unit", gl_uint},
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "Layered", gl_bool},
            {in, "Layer", gl_int},
            {in, "Access", {gl_enum, "BufferAccessARB", buffer_access}},
            {in, "Format", {gl_enum, "InternalFormat"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Unit", {gl, uint, []}},
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"Layered", {gl, boolean, []}},
            {"Layer", {gl, int, []}},
            {"Access", {undefined, buffer_access, []}},
            {"Format", {undefined, internal_format, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glBindImageTexture", maps:get(raw_function, Clause)),
    Params = maps:get(params, Clause),
    ?assert(lists:member({"Unit", do_nothing}, Params)),
    ?assert(lists:member({"Texture", do_nothing}, Params)),
    ?assert(lists:member({"Level", do_nothing}, Params)),
    ?assert(lists:member({"Layered", do_nothing}, Params)),
    ?assert(lists:member({"Layer", do_nothing}, Params)),
    s081_assert_enum_param("Access", "read_write", "GL_READ_WRITE", Params),
    s081_assert_enum_param("Format", "rgba8", "GL_RGBA8", Params),
    s081_assert_nif_params(
        FunctionData,
        "glBindImageTexture",
        [
            s081_uint_nif_data(),
            s081_uint_nif_data(),
            s081_int_nif_data(),
            boolean_to_glbool,
            s081_int_nif_data(),
            s081_enum_nif_data(),
            s081_enum_nif_data()
        ]
    ).

s081_assert_dispatch_compute(FunctionData) ->
    ?assertEqual("glDispatchCompute", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "NumGroupsX", gl_uint},
            {in, "NumGroupsY", gl_uint},
            {in, "NumGroupsZ", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"NumGroupsX", {gl, uint, []}},
            {"NumGroupsY", {gl, uint, []}},
            {"NumGroupsZ", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glDispatchCompute", maps:get(raw_function, Clause)),
    ?assertEqual(
        [
            {"NumGroupsX", do_nothing},
            {"NumGroupsY", do_nothing},
            {"NumGroupsZ", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    s081_assert_nif_params(
        FunctionData,
        "glDispatchCompute",
        [s081_uint_nif_data(), s081_uint_nif_data(), s081_uint_nif_data()]
    ).

s081_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s081_deferred_needles() ->
    [].

s081_assert_nif_params(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(ExpectedParams), maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s081_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s081_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).

s081_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard81-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s081_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s081_assert_contains(C, Needle) || Needle <- RequiredC],
        [s081_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s081_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s081_assert_contains(Haystack, Needle) ->
    ?assertNotEqual({Needle, nomatch}, {Needle, binary:match(Haystack, Needle)}).

s081_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s081_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s081_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s081_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 83.
s083_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s083_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s083_emitter_indirect_compute_dispatch_test_() ->
    [
        {"gl 4.6", fun() ->
            s083_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([dispatch_compute_indirect/1]).">>,
                    <<"-spec dispatch_compute_indirect(Indirect :: gl:intptr())">>,
                    <<"gl:dispatch_compute_indirect(0).">>,
                    <<"?CALL_RAW_FUNC(glDispatchComputeIndirect_raw(Indirect))">>
                ],
                [
                    <<"glDispatchComputeIndirect(arg_0);">>,
                    <<"{\"glDispatchComputeIndirect_raw\", 1, nif_glDispatchComputeIndirect, 0}">>
                ],
                s083_deferred_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s083_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([dispatch_compute_indirect/1]).">>,
                    <<"glDispatchComputeIndirect_raw">>
                ],
                [
                    <<"glDispatchComputeIndirect(arg_0);">>
                ],
                s083_deferred_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s083_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                [
                    <<"-export([dispatch_compute_indirect/1]).">>,
                    <<"glDispatchComputeIndirect_raw">>,
                    <<"glDispatchComputeIndirect(arg_0);">>
                ]
            )
        end}
    ].

s083_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s083_supports_indirect_compute(Target) of
        true ->
            ?assert(maps:is_key({"dispatch_compute_indirect", 1}, Functions)),
            s083_assert_dispatch_compute_indirect(maps:get({"dispatch_compute_indirect", 1}, Functions));
        false ->
            ?assertNot(maps:is_key({"dispatch_compute_indirect", 1}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glDispatchComputeIndirect", Functions))
    end,
    s083_assert_deferred_neighbors_absent(Functions).

s083_supports_indirect_compute({gl, {4, 6}}) ->
    true;
s083_supports_indirect_compute({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s083_supports_indirect_compute(_) ->
    false.

s083_assert_dispatch_compute_indirect(FunctionData) ->
    ?assertEqual("glDispatchComputeIndirect", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Indirect", gl_intptr}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Indirect", {gl, intptr, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glDispatchComputeIndirect", maps:get(raw_function, Clause)),
    ?assertEqual(
        [
            {"Indirect", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    s083_assert_nif_params(
        FunctionData,
        "glDispatchComputeIndirect",
        [s083_intptr_nif_data()]
    ).

s083_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s083_deferred_needles() ->
    [].

s083_assert_nif_params(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(ExpectedParams), maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s083_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard83-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s083_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s083_assert_contains(C, Needle) || Needle <- RequiredC],
        [s083_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s083_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s083_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s083_assert_contains(Haystack, Needle) ->
    ?assertNotEqual({Needle, nomatch}, {Needle, binary:match(Haystack, Needle)}).

s083_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 84.
s084_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s084_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s084_emitter_shader_storage_block_binding_test_() ->
    [
        {"gl 4.6", fun() ->
            s084_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([shader_storage_block_binding/3]).">>,
                    <<"-spec shader_storage_block_binding(\n    Program :: program(),\n    StorageBlockIndex :: gl:uint(),\n    StorageBlockBinding :: gl:uint()">>,
                    <<"gl:shader_storage_block_binding(Program, 0, 0).">>,
                    <<"?CALL_RAW_FUNC(glShaderStorageBlockBinding_raw(Program, StorageBlockIndex, StorageBlockBinding))">>
                ],
                [
                    <<"glShaderStorageBlockBinding(arg_0, arg_1, arg_2);">>,
                    <<"{\"glShaderStorageBlockBinding_raw\", 3, nif_glShaderStorageBlockBinding, 0}">>
                ],
                s084_deferred_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s084_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                [
                    <<"-export([shader_storage_block_binding/3]).">>,
                    <<"glShaderStorageBlockBinding_raw">>,
                    <<"glShaderStorageBlockBinding(arg_0, arg_1, arg_2);">>
                ]
            )
        end},
        {"gl 4.1", fun() ->
            s084_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                [
                    <<"-export([shader_storage_block_binding/3]).">>,
                    <<"glShaderStorageBlockBinding_raw">>,
                    <<"glShaderStorageBlockBinding(arg_0, arg_1, arg_2);">>
                ]
            )
        end}
    ].

s084_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s084_supports_shader_storage_block_binding(Target) of
        true ->
            ?assert(maps:is_key({"shader_storage_block_binding", 3}, Functions)),
            s084_assert_shader_storage_block_binding(maps:get({"shader_storage_block_binding", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"shader_storage_block_binding", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glShaderStorageBlockBinding", Functions))
    end,
    s084_assert_deferred_neighbors_absent(Functions).

s084_supports_shader_storage_block_binding({gl, {4, 6}}) ->
    true;
s084_supports_shader_storage_block_binding(_) ->
    false.

s084_assert_shader_storage_block_binding(FunctionData) ->
    ?assertEqual("glShaderStorageBlockBinding", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "StorageBlockIndex", gl_uint},
            {in, "StorageBlockBinding", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"StorageBlockIndex", {gl, uint, []}},
            {"StorageBlockBinding", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glShaderStorageBlockBinding", maps:get(raw_function, Clause)),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"StorageBlockIndex", do_nothing},
            {"StorageBlockBinding", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    s084_assert_nif_params(
        FunctionData,
        "glShaderStorageBlockBinding",
        [s084_uint_nif_data(), s084_uint_nif_data(), s084_uint_nif_data()]
    ).

s084_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s084_deferred_needles() ->
    [].

s084_assert_nif_params(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(ExpectedParams), maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s084_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard84-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s084_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s084_assert_contains(C, Needle) || Needle <- RequiredC],
        [s084_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s084_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s084_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s084_assert_contains(Haystack, Needle) ->
    ?assertNotEqual({Needle, nomatch}, {Needle, binary:match(Haystack, Needle)}).

s084_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 90.
s090_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s090_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s090_emitter_uniform_block_test_() ->
    [
        {"gl 4.6", fun() ->
            s090_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([get_uniform_block_index/2]).">>,
                    <<"-export([uniform_block_binding/3]).">>,
                    <<"Index :: gl:uint()">>,
                    <<"Name0 = iolist_to_binary(Name)">>,
                    <<"?CALL_RAW_FUNC(glGetUniformBlockIndex_raw(Program, Name0)).">>,
                    <<"uniform_block_binding(Program, UniformBlockIndex, UniformBlockBinding) ->">>
                ],
                [
                    <<"GLuint ret = glGetUniformBlockIndex(arg_0, (const GLchar*)arg_1_string);">>,
                    <<"ERL_NIF_TERM ret_0 = enif_make_uint(env, ret);">>,
                    <<"glUniformBlockBinding(arg_0, arg_1, arg_2);">>
                ],
                s090_deferred_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s090_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([get_uniform_block_index/2]).">>,
                    <<"-export([uniform_block_binding/3]).">>,
                    <<"glGetUniformBlockIndex_raw(Program, Name0)">>,
                    <<"uniform_block_binding(Program, UniformBlockIndex, UniformBlockBinding) ->">>
                ],
                [
                    <<"GLuint ret = glGetUniformBlockIndex(arg_0, (const GLchar*)arg_1_string);">>,
                    <<"glUniformBlockBinding(arg_0, arg_1, arg_2);">>
                ],
                s090_deferred_needles()
            )
        end},
        {"gles 2.0", fun() ->
            s090_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [],
                [],
                [
                    <<"-export([get_uniform_block_index/2]).">>,
                    <<"-export([uniform_block_binding/3]).">>,
                    <<"glGetUniformBlockIndex(">>,
                    <<"glUniformBlockBinding(">>
                ]
            )
        end}
    ].

s090_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s090_supports(Target) of
        true ->
            ?assert(maps:is_key({"get_uniform_block_index", 2}, Functions)),
            ?assert(maps:is_key({"uniform_block_binding", 3}, Functions)),
            s090_assert_get_uniform_block_index(maps:get({"get_uniform_block_index", 2}, Functions)),
            s090_assert_uniform_block_binding(maps:get({"uniform_block_binding", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"get_uniform_block_index", 2}, Functions)),
            ?assertNot(maps:is_key({"uniform_block_binding", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetUniformBlockIndex", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glUniformBlockBinding", Functions))
    end,
    s090_assert_deferred_neighbors_absent(Functions).

s090_supports({gles, {2, 0}}) ->
    false;
s090_supports(_) ->
    true.

s090_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s090_assert_get_uniform_block_index(FunctionData) ->
    ?assertEqual("glGetUniformBlockIndex", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"Index", gl_uint}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Index", {gl, uint, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetUniformBlockIndex", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetUniformBlockIndex", maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s090_uint_nif_data()},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(gluint_to_uint, maps:get(return, NifData)).

s090_assert_uniform_block_binding(FunctionData) ->
    ?assertEqual("glUniformBlockBinding", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "UniformBlockIndex", gl_uint},
            {in, "UniformBlockBinding", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"UniformBlockIndex", {gl, uint, []}},
            {"UniformBlockBinding", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"UniformBlockIndex", do_nothing},
            {"UniformBlockBinding", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glUniformBlockBinding", maps:get(raw_function, Clause)),

    NifData = maps:get("glUniformBlockBinding", maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s090_uint_nif_data()},
            {"UniformBlockIndex", s090_uint_nif_data()},
            {"UniformBlockBinding", s090_uint_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s090_assert_emitted_surface(Target, ApiName, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard90-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [?assertMatch({_, _}, binary:match(Erl, Expected)) || Expected <- ExpectedErl],
        [?assertMatch({_, _}, binary:match(C, Expected)) || Expected <- ExpectedC],
        [?assertEqual(nomatch, binary:match(Erl, ForbiddenItem)) || ForbiddenItem <- Forbidden],
        [?assertEqual(nomatch, binary:match(C, ForbiddenItem)) || ForbiddenItem <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s090_deferred_needles() ->
    [].

s090_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 91.
s091_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s091_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s091_emitter_fragment_data_location_test_() ->
    [
        {"gl 4.6", fun() ->
            s091_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([bind_frag_data_location/3]).">>,
                    <<"-export([bind_frag_data_location/4]).">>,
                    <<"-export([get_frag_data_location/2]).">>,
                    <<"-export([get_frag_data_index/2]).">>,
                    <<"Name0 = iolist_to_binary(Name)">>,
                    <<"?CALL_RAW_FUNC(glBindFragDataLocation_raw(Program, Color, Name0)).">>,
                    <<"?CALL_RAW_FUNC(glBindFragDataLocationIndexed_raw(Program, ColorNumber, Index, Name0)).">>,
                    <<"?CALL_RAW_FUNC(glGetFragDataLocation_raw(Program, Name0)).">>,
                    <<"?CALL_RAW_FUNC(glGetFragDataIndex_raw(Program, Name0)).">>
                ],
                [
                    <<"glBindFragDataLocation(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"glBindFragDataLocationIndexed(arg_0, arg_1, arg_2, (const GLchar*)arg_3_string);">>,
                    <<"GLint ret = glGetFragDataLocation(arg_0, (const GLchar*)arg_1_string);">>,
                    <<"GLint ret = glGetFragDataIndex(arg_0, (const GLchar*)arg_1_string);">>,
                    <<"ERL_NIF_TERM ret_0 = enif_make_int(env, ret);">>
                ],
                s091_deferred_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s091_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([get_frag_data_location/2]).">>,
                    <<"?CALL_RAW_FUNC(glGetFragDataLocation_raw(Program, Name0)).">>
                ],
                [
                    <<"GLint ret = glGetFragDataLocation(arg_0, (const GLchar*)arg_1_string);">>
                ],
                [
                    <<"-export([bind_frag_data_location/3]).">>,
                    <<"-export([bind_frag_data_location/4]).">>,
                    <<"-export([get_frag_data_index/2]).">>,
                    <<"glBindFragDataLocation(">>,
                    <<"glBindFragDataLocationIndexed(">>,
                    <<"glGetFragDataIndex(">>
                ] ++ s091_deferred_needles()
            )
        end},
        {"gles 2.0", fun() ->
            s091_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [],
                [],
                [
                    <<"-export([bind_frag_data_location/3]).">>,
                    <<"-export([bind_frag_data_location/4]).">>,
                    <<"-export([get_frag_data_location/2]).">>,
                    <<"-export([get_frag_data_index/2]).">>,
                    <<"glBindFragDataLocation(">>,
                    <<"glBindFragDataLocationIndexed(">>,
                    <<"glGetFragDataLocation(">>,
                    <<"glGetFragDataIndex(">>
                ]
            )
        end}
    ].

s091_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    case s091_supports_frag_data_binding(Target) of
        true ->
            ?assert(maps:is_key({"bind_frag_data_location", 3}, Functions)),
            s091_assert_bind_frag_data_location(maps:get({"bind_frag_data_location", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"bind_frag_data_location", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glBindFragDataLocation", Functions))
    end,

    case s091_supports_frag_data_indexed(Target) of
        true ->
            ?assert(maps:is_key({"bind_frag_data_location", 4}, Functions)),
            ?assert(maps:is_key({"get_frag_data_index", 2}, Functions)),
            s091_assert_bind_frag_data_location_indexed(maps:get({"bind_frag_data_location", 4}, Functions)),
            s091_assert_get_frag_data_index(maps:get({"get_frag_data_index", 2}, Functions));
        false ->
            ?assertNot(maps:is_key({"bind_frag_data_location", 4}, Functions)),
            ?assertNot(maps:is_key({"get_frag_data_index", 2}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glBindFragDataLocationIndexed", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetFragDataIndex", Functions))
    end,

    case s091_supports_frag_data_location(Target) of
        true ->
            ?assert(maps:is_key({"get_frag_data_location", 2}, Functions)),
            s091_assert_get_frag_data_location(maps:get({"get_frag_data_location", 2}, Functions));
        false ->
            ?assertNot(maps:is_key({"get_frag_data_location", 2}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetFragDataLocation", Functions))
    end,

    s091_assert_deferred_neighbors_absent(Functions).

s091_supports_frag_data_binding({gl, _}) ->
    true;
s091_supports_frag_data_binding(_) ->
    false.

s091_supports_frag_data_indexed({gl, {Major, Minor}}) ->
    Major > 3 orelse (Major =:= 3 andalso Minor >= 3);
s091_supports_frag_data_indexed(_) ->
    false.

s091_supports_frag_data_location({gl, _}) ->
    true;
s091_supports_frag_data_location({gles, {Major, _}}) when Major >= 3 ->
    true;
s091_supports_frag_data_location(_) ->
    false.

s091_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glBindFragDataLocationEXT",
        "glBindFragDataLocationIndexedEXT",
        "glGetFragDataLocationEXT",
        "glGetFragDataIndexEXT"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s091_assert_bind_frag_data_location(FunctionData) ->
    ?assertEqual("glBindFragDataLocation", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Color", gl_uint},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Color", {gl, uint, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Color", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glBindFragDataLocation", maps:get(raw_function, Clause)),

    NifData = maps:get("glBindFragDataLocation", maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s091_uint_nif_data()},
            {"Color", s091_uint_nif_data()},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s091_assert_bind_frag_data_location_indexed(FunctionData) ->
    ?assertEqual("glBindFragDataLocationIndexed", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ColorNumber", gl_uint},
            {in, "Index", gl_uint},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ColorNumber", {gl, uint, []}},
            {"Index", {gl, uint, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"ColorNumber", do_nothing},
            {"Index", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glBindFragDataLocationIndexed", maps:get(raw_function, Clause)),

    NifData = maps:get("glBindFragDataLocationIndexed", maps:get(nif_functions, FunctionData)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s091_uint_nif_data()},
            {"ColorNumber", s091_uint_nif_data()},
            {"Index", s091_uint_nif_data()},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s091_assert_get_frag_data_location(FunctionData) ->
    s091_assert_frag_data_integer_query(
        FunctionData,
        "glGetFragDataLocation",
        "Location",
        "get_frag_data_location"
    ).

s091_assert_get_frag_data_index(FunctionData) ->
    s091_assert_frag_data_integer_query(
        FunctionData,
        "glGetFragDataIndex",
        "Index",
        "get_frag_data_index"
    ).

s091_assert_frag_data_integer_query(FunctionData, GlCommand, ReturnName, _PublicName) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Name", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({ReturnName, gl_int}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Name", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{ReturnName, {gl, int, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Name", normalize_gl_string}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s091_uint_nif_data()},
            {"Name", in_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(glint_to_integer, maps:get(return, NifData)).

s091_assert_emitted_surface(Target, ApiName, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard91-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [?assertMatch({_, _}, binary:match(Erl, Expected)) || Expected <- ExpectedErl],
        [?assertMatch({_, _}, binary:match(C, Expected)) || Expected <- ExpectedC],
        [?assertEqual(nomatch, binary:match(Erl, ForbiddenItem)) || ForbiddenItem <- Forbidden],
        [?assertEqual(nomatch, binary:match(C, ForbiddenItem)) || ForbiddenItem <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s091_deferred_needles() ->
    [
        <<"glBindFragDataLocationEXT">>,
        <<"glBindFragDataLocationIndexedEXT">>,
        <<"glGetFragDataLocationEXT">>,
        <<"glGetFragDataIndexEXT">>
    ].

s091_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 92.
s092_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s092_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s092_emitter_string_output_test_() ->
    [
        {"gl 4.6", fun() ->
            s092_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([get_string/1]).">>,
                    <<"-export([get_shader_source/2]).">>,
                    <<"-export([get_shader_info_log/2]).">>,
                    <<"-export([get_program_info_log/2]).">>,
                    <<"-export([get_program_pipeline_info_log/2]).">>,
                    <<"-spec get_string(Name :: string_name()) -> {ok, String :: binary()} | {error, atom()}.">>,
                    <<"MaxLength :: pos_integer()">>,
                    <<"Source :: binary()">>,
                    <<"?CALL_RAW_FUNC(glGetString_raw(NewName)).">>,
                    <<"?CALL_RAW_FUNC(glGetShaderSource_raw(Shader, MaxLength)).">>
                ],
                [
                    <<"const GLubyte* ret = glGetString(arg_0);">>,
                    <<"size_t ret_len = ret ? strlen((const char*)ret) : 0;">>,
                    <<"if (!enif_alloc_binary(ret_len, &ret_tmp)) {">>,
                    <<"if (ret_len > 0) {">>,
                    <<"if (arg_1_max_length_tmp == 0) {">>,
                    <<"GLchar* arg_1_info_log = (GLchar*)enif_alloc(arg_1_max_length_tmp);">>,
                    <<"if (!arg_1_info_log) {">>,
                    <<"glGetShaderSource(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>,
                    <<"glGetShaderInfoLog(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>,
                    <<"glGetProgramInfoLog(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>,
                    <<"glGetProgramPipelineInfoLog(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>,
                    <<"unsigned char* arg_1_bin = enif_make_new_binary(env, arg_1_length, &arg_1_result);">>
                ],
                []
            )
        end},
        {"gles 2.0", fun() ->
            s092_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [
                    <<"-export([get_string/1]).">>,
                    <<"-export([get_shader_source/2]).">>,
                    <<"-export([get_shader_info_log/2]).">>,
                    <<"-export([get_program_info_log/2]).">>
                ],
                [
                    <<"glGetString(arg_0);">>,
                    <<"glGetShaderSource(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>,
                    <<"glGetShaderInfoLog(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>,
                    <<"glGetProgramInfoLog(arg_0, arg_1_max_length, &arg_1_length, arg_1_info_log);">>
                ],
                [
                    <<"-export([get_program_pipeline_info_log/2]).">>,
                    <<"glGetProgramPipelineInfoLog(">>
                ]
            )
        end}
    ].

s092_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_string", 1}, Functions)),
    ?assert(maps:is_key({"get_shader_source", 2}, Functions)),
    ?assert(maps:is_key({"get_shader_info_log", 2}, Functions)),
    ?assert(maps:is_key({"get_program_info_log", 2}, Functions)),

    s092_assert_pipeline_info_log_presence(Target, Functions),
    s092_assert_deferred_neighbors_absent(Functions),

    s092_assert_enum_contains(BindingData, "string_name", "version"),
    s092_assert_get_string(maps:get({"get_string", 1}, Functions)),
    s092_assert_out_string(
        maps:get({"get_shader_source", 2}, Functions),
        "glGetShaderSource",
        {in, "Shader", {gl_object, shader}},
        {"Shader", {undefined, shader, []}},
        "Source"
    ),
    s092_assert_out_string(
        maps:get({"get_shader_info_log", 2}, Functions),
        "glGetShaderInfoLog",
        {in, "Shader", {gl_object, shader}},
        {"Shader", {undefined, shader, []}},
        "InfoLog"
    ),
    s092_assert_out_string(
        maps:get({"get_program_info_log", 2}, Functions),
        "glGetProgramInfoLog",
        {in, "Program", {gl_object, program}},
        {"Program", {undefined, program, []}},
        "InfoLog"
    ),

    case s092_supports_program_pipeline_info_log(Target) of
        true ->
            s092_assert_out_string(
                maps:get({"get_program_pipeline_info_log", 2}, Functions),
                "glGetProgramPipelineInfoLog",
                {in, "Pipeline", {gl_object, program_pipeline}},
                {"Pipeline", {undefined, program_pipeline, []}},
                "InfoLog"
            );
        false ->
            ok
    end.

s092_supports_program_pipeline_info_log({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s092_supports_program_pipeline_info_log({gl, {Major, _}}) when Major > 4 ->
    true;
s092_supports_program_pipeline_info_log({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s092_supports_program_pipeline_info_log({gles, {Major, _}}) when Major > 3 ->
    true;
s092_supports_program_pipeline_info_log(_) ->
    false.

s092_assert_pipeline_info_log_presence(Target, Functions) ->
    case s092_supports_program_pipeline_info_log(Target) of
        true ->
            ?assert(maps:is_key({"get_program_pipeline_info_log", 2}, Functions));
        false ->
            ?assertNot(maps:is_key({"get_program_pipeline_info_log", 2}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetProgramPipelineInfoLog", Functions))
    end.

s092_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glMapBuffer",
        "glFenceSync"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s092_assert_get_string(FunctionData) ->
    ?assertEqual("glGetString", maps:get(gl_command, FunctionData)),
    ?assertEqual([{in, "Name", {gl_enum, "StringName"}}], maps:get(params_specs, FunctionData)),
    ?assertEqual({"String", {gl_string, glubyte}}, maps:get(return_specs, FunctionData)),
    ?assertEqual([{"Name", {undefined, string_name, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual([{"String", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Name", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(lists:keymember("version", 1, TransformMap)),
    ?assertEqual("glGetString", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetString", maps:get(nif_functions, FunctionData)),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual([{"Name", s092_enum_nif_data()}], maps:get(params, NifData)),
    ?assertEqual(const_glubyte_to_string, maps:get(return, NifData)).

s092_assert_out_string(FunctionData, Command, ParamSpec, PublicParamSpec, ReturnName) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual([ParamSpec, {out, ReturnName, gl_string}], maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [PublicParamSpec, {"MaxLength", {undefined, pos_integer, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{ReturnName, {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [{in, ParamName, _}] = [ParamSpec],
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{ParamName, do_nothing}, {"MaxLength", do_nothing}], maps:get(params, Clause)),
    ?assertEqual(Command, maps:get(raw_function, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {ParamName, s092_uint_nif_data()},
            {"MaxLength", out_gl_string}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s092_assert_enum_contains(BindingData, EnumName, Value) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumName, EnumTypes)),
    ?assert(lists:member(Value, maps:get(EnumName, EnumTypes))).

s092_assert_emitted_surface(Target, ApiName, ExpectedErl, ExpectedC, ForbiddenNeedles) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard92-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [?assertMatch({_, _}, binary:match(Erl, Needle)) || Needle <- ExpectedErl],
        [?assertMatch({_, _}, binary:match(C, Needle)) || Needle <- ExpectedC],
        [?assertEqual(nomatch, binary:match(Erl, Needle)) || Needle <- ForbiddenNeedles],
        [?assertEqual(nomatch, binary:match(C, Needle)) || Needle <- ForbiddenNeedles]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s092_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s092_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 93.
s093_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s093_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s093_emitter_debug_label_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s093_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([debug_message_insert/5]).">>,
                    <<"-export([push_debug_group/3]).">>,
                    <<"-export([pop_debug_group/0]).">>,
                    <<"-export([object_label/3]).">>,
                    <<"-export([get_object_label/3]).">>,
                    <<"Message :: iodata()">>,
                    <<"Label :: iodata()">>,
                    <<"MaxLength :: pos_integer()">>,
                    <<"Label :: binary()">>,
                    <<"Message0 = iolist_to_binary(Message)">>,
                    <<"Label0 = iolist_to_binary(Label)">>,
                    <<"?CALL_RAW_FUNC(glDebugMessageInsert_raw(NewSource, NewType, Id, NewSeverity, Message0)).">>,
                    <<"?CALL_RAW_FUNC(glGetObjectLabel_raw(NewIdentifier, Name, MaxLength)).">>
                ],
                [
                    <<"char* arg_4_msg = enif_alloc(arg_4.size + 1);">>,
                    <<"if (!arg_4_msg) {">>,
                    <<"glDebugMessageInsert(arg_0, arg_1, arg_2, arg_3, (GLsizei)arg_4.size, arg_4_msg);">>,
                    <<"glPushDebugGroup(arg_0, arg_1, (GLsizei)arg_2.size, arg_2_msg);">>,
                    <<"glPopDebugGroup();">>,
                    <<"glObjectLabel(arg_0, arg_1, (GLsizei)arg_2.size, arg_2_msg);">>,
                    <<"glGetObjectLabel(arg_0, arg_1, arg_2_max_length, &arg_2_length, arg_2_info_log);">>,
                    <<"unsigned char* arg_2_bin = enif_make_new_binary(env, arg_2_length, &arg_2_result);">>
                ],
                s093_forbidden_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s093_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([debug_message_insert/5]).">>,
                    <<"-export([push_debug_group/3]).">>,
                    <<"-export([pop_debug_group/0]).">>,
                    <<"-export([object_label/3]).">>,
                    <<"-export([get_object_label/3]).">>
                ],
                [
                    <<"glDebugMessageInsert(arg_0, arg_1, arg_2, arg_3, (GLsizei)arg_4.size, arg_4_msg);">>,
                    <<"glObjectLabel(arg_0, arg_1, (GLsizei)arg_2.size, arg_2_msg);">>
                ],
                s093_forbidden_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s093_assert_emitted_surface(
                {gl, {4, 1}},
                "OpenGL 4.1",
                [],
                [],
                s093_needles()
            )
        end}
    ].

s093_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    case s093_supports_debug_labels(Target) of
        true ->
            [?assert(maps:is_key(Function, Functions)) || Function <- s093_functions()],
            s093_assert_enum_contains(BindingData, "debug_source", "debug_source_application"),
            s093_assert_enum_contains(BindingData, "debug_type", "debug_type_marker"),
            s093_assert_enum_contains(BindingData, "debug_severity", "debug_severity_notification"),
            s093_assert_enum_contains(BindingData, "object_identifier", "texture"),
            s093_assert_debug_message_insert(maps:get({"debug_message_insert", 5}, Functions)),
            s093_assert_push_debug_group(maps:get({"push_debug_group", 3}, Functions)),
            s093_assert_pop_debug_group(maps:get({"pop_debug_group", 0}, Functions)),
            s093_assert_object_label(maps:get({"object_label", 3}, Functions)),
            s093_assert_get_object_label(maps:get({"get_object_label", 3}, Functions));
        false ->
            [?assertNot(maps:is_key(Function, Functions)) || Function <- s093_functions()],
            [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- s093_commands()]
    end,

    s093_assert_deferred_neighbors_absent(Functions).

s093_supports_debug_labels({gl, {Major, Minor}}) ->
    Major > 4 orelse (Major =:= 4 andalso Minor >= 3);
s093_supports_debug_labels({gles, {Major, Minor}}) ->
    Major > 3 orelse (Major =:= 3 andalso Minor >= 2);
s093_supports_debug_labels(_) ->
    false.

s093_functions() ->
    [
        {"debug_message_insert", 5},
        {"push_debug_group", 3},
        {"pop_debug_group", 0},
        {"object_label", 3},
        {"get_object_label", 3}
    ].

s093_commands() ->
    [
        "glDebugMessageInsert",
        "glPushDebugGroup",
        "glPopDebugGroup",
        "glObjectLabel",
        "glGetObjectLabel"
    ].

s093_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glDebugMessageCallback",
        "glObjectPtrLabel",
        "glGetObjectPtrLabel",
        "glGetPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s093_assert_debug_message_insert(FunctionData) ->
    ?assertEqual("glDebugMessageInsert", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Source", {gl_enum, "DebugSource"}},
            {in, "Type", {gl_enum, "DebugType"}},
            {in, "Id", gl_uint},
            {in, "Severity", {gl_enum, "DebugSeverity"}},
            {in, "Message", {gl_string, char}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertNot(lists:keymember("Length", 2, maps:get(params_specs, FunctionData))),
    ?assertEqual(
        [
            {"Source", {undefined, debug_source, []}},
            {"Type", {undefined, debug_type, []}},
            {"Id", {gl, uint, []}},
            {"Severity", {undefined, debug_severity, []}},
            {"Message", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    s093_assert_enum_param("Source", "debug_source_application", maps:get(params, Clause)),
    s093_assert_enum_param("Type", "debug_type_marker", maps:get(params, Clause)),
    s093_assert_enum_param("Severity", "debug_severity_notification", maps:get(params, Clause)),
    ?assert(lists:member({"Id", do_nothing}, maps:get(params, Clause))),
    ?assert(lists:member({"Message", normalize_gl_string}, maps:get(params, Clause))),
    s093_assert_nif(
        FunctionData,
        "glDebugMessageInsert",
        [s093_enum_nif_data(), s093_enum_nif_data(), s093_uint_nif_data(), s093_enum_nif_data(), binary_to_gl_string_char]
    ).

s093_assert_push_debug_group(FunctionData) ->
    ?assertEqual("glPushDebugGroup", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Source", {gl_enum, "DebugSource"}},
            {in, "Id", gl_uint},
            {in, "Message", {gl_string, char}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Source", {undefined, debug_source, []}},
            {"Id", {gl, uint, []}},
            {"Message", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    s093_assert_enum_param("Source", "debug_source_application", maps:get(params, Clause)),
    ?assert(lists:member({"Id", do_nothing}, maps:get(params, Clause))),
    ?assert(lists:member({"Message", normalize_gl_string}, maps:get(params, Clause))),
    s093_assert_nif(FunctionData, "glPushDebugGroup", [s093_enum_nif_data(), s093_uint_nif_data(), binary_to_gl_string_char]).

s093_assert_pop_debug_group(FunctionData) ->
    ?assertEqual("glPopDebugGroup", maps:get(gl_command, FunctionData)),
    ?assertEqual([], maps:get(params_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_params, FunctionData)),
    s093_assert_nif(FunctionData, "glPopDebugGroup", []).

s093_assert_object_label(FunctionData) ->
    ?assertEqual("glObjectLabel", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Identifier", {gl_enum, "ObjectIdentifier"}},
            {in, "Name", gl_uint},
            {in, "Label", {gl_string, char}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Identifier", {undefined, object_identifier, []}},
            {"Name", {gl, uint, []}},
            {"Label", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    s093_assert_enum_param("Identifier", "texture", maps:get(params, Clause)),
    ?assert(lists:member({"Name", do_nothing}, maps:get(params, Clause))),
    ?assert(lists:member({"Label", normalize_gl_string}, maps:get(params, Clause))),
    s093_assert_nif(FunctionData, "glObjectLabel", [s093_enum_nif_data(), s093_uint_nif_data(), binary_to_gl_string_char]).

s093_assert_get_object_label(FunctionData) ->
    ?assertEqual("glGetObjectLabel", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Identifier", {gl_enum, "ObjectIdentifier"}},
            {in, "Name", gl_uint},
            {out, "Label", gl_string}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Identifier", {undefined, object_identifier, []}},
            {"Name", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Label", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s093_assert_enum_param("Identifier", "texture", maps:get(params, Clause)),
    ?assert(lists:member({"Name", do_nothing}, maps:get(params, Clause))),
    ?assert(lists:member({"MaxLength", do_nothing}, maps:get(params, Clause))),
    s093_assert_nif(FunctionData, "glGetObjectLabel", [s093_enum_nif_data(), s093_uint_nif_data(), out_gl_string]).

s093_assert_nif(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(ExpectedParams), maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s093_assert_enum_param(ParamName, Atom, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:keymember(Atom, 1, TransformMap)).

s093_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s093_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard93-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [?assertMatch({_, _}, binary:match(Erl, Needle)) || Needle <- RequiredErl],
        [?assertMatch({_, _}, binary:match(C, Needle)) || Needle <- RequiredC],
        [?assertEqual(nomatch, binary:match(Erl, Needle)) || Needle <- Forbidden],
        [?assertEqual(nomatch, binary:match(C, Needle)) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s093_needles() ->
    [
        <<"debug_message_insert">>,
        <<"push_debug_group">>,
        <<"pop_debug_group">>,
        <<"object_label">>,
        <<"get_object_label">>,
        <<"glDebugMessageInsert">>,
        <<"glPushDebugGroup">>,
        <<"glPopDebugGroup">>,
        <<"glObjectLabel">>,
        <<"glGetObjectLabel">>
    ].

s093_forbidden_needles() ->
    [
        <<"debug_message_callback">>,
        <<"object_ptr_label">>,
        <<"get_object_ptr_label">>,
        <<"glDebugMessageCallback">>,
        <<"glObjectPtrLabel">>,
        <<"glGetObjectPtrLabel">>
    ].

s093_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s093_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 94.
s094_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s094_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s094_emitter_framebuffer_clear_values_test_() ->
    [
        {"gl 4.6", fun() ->
            s094_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([clear_buffer/4]).">>,
                    <<"-export([clear_named_framebuffer/5]).">>,
                    <<"-type clear_buffer() ::">>,
                    <<"-type clear_buffer_value() ::">>,
                    <<"-type clear_named_framebuffer_value() ::">>,
                    <<"-spec clear_buffer(\n    Type :: f | i | ui,">>,
                    <<"-spec clear_named_framebuffer(\n    Type :: f | i | ui,">>,
                    <<"clear_buffer(f, Buffer, DrawBuffer, Value) when is_list(Value) ->">>,
                    <<"clear_buffer(i, Buffer, DrawBuffer, Value) when is_list(Value) ->">>,
                    <<"clear_buffer(ui, Buffer, DrawBuffer, Value) when is_list(Value) ->">>,
                    <<"clear_named_framebuffer(f, Framebuffer, Buffer, DrawBuffer, Value) when is_list(Value) ->">>,
                    <<"?CALL_RAW_FUNC(glClearBufferfv_raw(NewBuffer, DrawBuffer, Value))">>,
                    <<"?CALL_RAW_FUNC(glClearNamedFramebufferfv_raw(Framebuffer, NewBuffer, DrawBuffer, Value))">>
                ],
                [
                    <<"glClearBufferfv(arg_0, arg_1, arg_2_array);">>,
                    <<"glClearBufferiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glClearBufferuiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glClearNamedFramebufferfv(arg_0, arg_1, arg_2, arg_3_array);">>,
                    <<"glClearNamedFramebufferiv(arg_0, arg_1, arg_2, arg_3_array);">>,
                    <<"glClearNamedFramebufferuiv(arg_0, arg_1, arg_2, arg_3_array);">>
                ],
                s094_forbidden_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s094_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([clear_buffer/4]).">>,
                    <<"-type clear_buffer_value() ::">>,
                    <<"glClearBufferfv_raw">>,
                    <<"glClearBufferiv_raw">>,
                    <<"glClearBufferuiv_raw">>
                ],
                [
                    <<"glClearBufferfv(arg_0, arg_1, arg_2_array);">>,
                    <<"glClearBufferiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glClearBufferuiv(arg_0, arg_1, arg_2_array);">>
                ],
                [<<"clear_named_framebuffer/5">>, <<"glClearNamedFramebuffer">>] ++ s094_forbidden_needles()
            )
        end},
        {"gles 2.0", fun() ->
            s094_assert_emitted_surface(
                {gles, {2, 0}},
                [],
                [],
                [
                    <<"-export([clear_buffer/4]).">>,
                    <<"-export([clear_named_framebuffer/5]).">>,
                    <<"glClearBufferfv">>,
                    <<"glClearNamedFramebufferfv">>
                ]
            )
        end}
    ].

s094_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s094_assert_presence(Target, Functions),
    s094_assert_deferred_neighbors_absent(Functions),
    [s094_assert_path(Function, BindingData, maps:get(Function, Functions))
     || Function <- s094_present_functions(Target)].

s094_assert_presence(Target, Functions) ->
    Present = s094_present_functions(Target),
    Absent = s094_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s094_present_functions({gles, {2, 0}}) ->
    [];
s094_present_functions({gl, {4, 6}}) ->
    s094_all_functions();
s094_present_functions(_) ->
    [{"clear_buffer", 4}].

s094_all_functions() ->
    [{"clear_buffer", 4}, {"clear_named_framebuffer", 5}].

s094_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands],
    ?assertNot(maps:is_key({"clear_buffer", 5}, Functions)),
    ?assertNot(maps:is_key({"clear_named_framebuffer", 6}, Functions)).

s094_assert_path({"clear_buffer", 4}, BindingData, FunctionData) ->
    s094_assert_enum(BindingData),
    ?assertEqual(
        [
            {in, "Buffer", {gl_enum, "Buffer", clear_buffer}},
            {in, "DrawBuffer", gl_int},
            {in, "Value", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i, ui]}},
            {"Buffer", {undefined, clear_buffer, []}},
            {"DrawBuffer", {gl, int, []}},
            {"Value", {undefined, clear_buffer_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s094_assert_extra_type(clear_buffer_value, FunctionData),
    s094_assert_commands(
        FunctionData,
        [
            {"glClearBufferfv", gl_float, array},
            {"glClearBufferiv", gl_int, array},
            {"glClearBufferuiv", gl_uint, array}
        ]
    ),
    s094_assert_clear_buffer_clauses(FunctionData),
    s094_assert_clear_buffer_nifs(FunctionData);
s094_assert_path({"clear_named_framebuffer", 5}, BindingData, FunctionData) ->
    s094_assert_enum(BindingData),
    ?assertEqual(
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Buffer", {gl_enum, "Buffer", clear_buffer}},
            {in, "DrawBuffer", gl_int},
            {in, "Value", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i, ui]}},
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Buffer", {undefined, clear_buffer, []}},
            {"DrawBuffer", {gl, int, []}},
            {"Value", {undefined, clear_named_framebuffer_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(5, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s094_assert_extra_type(clear_named_framebuffer_value, FunctionData),
    s094_assert_commands(
        FunctionData,
        [
            {"glClearNamedFramebufferfv", gl_float, array},
            {"glClearNamedFramebufferiv", gl_int, array},
            {"glClearNamedFramebufferuiv", gl_uint, array}
        ]
    ),
    s094_assert_clear_named_framebuffer_clauses(FunctionData),
    s094_assert_clear_named_framebuffer_nifs(FunctionData).

s094_assert_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("clear_buffer", EnumTypes)),
    ?assertNot(maps:is_key("buffer", EnumTypes)),
    ClearBuffer = maps:get("clear_buffer", EnumTypes),
    [?assert(lists:member(Atom, ClearBuffer)) || Atom <- ["color", "depth", "stencil"]].

s094_assert_extra_type(TypeName, FunctionData) ->
    {TypeName, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assertEqual(
        lists:sort([{list, {gl, float, []}}, {list, {gl, int, []}}, {list, {gl, uint, []}}]),
        lists:sort(Variants)
    ).

s094_assert_commands(FunctionData, Commands) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    [?assert(lists:member(Command, GlCommands)) || Command <- Commands],
    ?assertEqual(
        lists:sort([{gl_float, array}, {gl_int, array}, {gl_uint, array}]),
        lists:sort(maps:get(variants, FunctionData))
    ).

s094_assert_clear_buffer_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    s094_assert_array_clause("glClearBufferfv", "f", Clauses, ["Buffer", "DrawBuffer", "Value"]),
    s094_assert_array_clause("glClearBufferiv", "i", Clauses, ["Buffer", "DrawBuffer", "Value"]),
    s094_assert_array_clause("glClearBufferuiv", "ui", Clauses, ["Buffer", "DrawBuffer", "Value"]).

s094_assert_clear_named_framebuffer_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    s094_assert_array_clause("glClearNamedFramebufferfv", "f", Clauses, ["Framebuffer", "Buffer", "DrawBuffer", "Value"]),
    s094_assert_array_clause("glClearNamedFramebufferiv", "i", Clauses, ["Framebuffer", "Buffer", "DrawBuffer", "Value"]),
    s094_assert_array_clause("glClearNamedFramebufferuiv", "ui", Clauses, ["Framebuffer", "Buffer", "DrawBuffer", "Value"]).

s094_assert_array_clause(RawFunction, Suffix, Clauses, ParamNames) ->
    Clause = s094_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    Params = maps:get(params, Clause),
    ?assertEqual({Suffix, ignore}, hd(Params)),
    s094_assert_clause_param_names(ParamNames, tl(Params)),
    {"Buffer", {gl_enum_to_uint, TransformMap}} = lists:keyfind("Buffer", 1, Params),
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- ["color", "depth", "stencil"]].

s094_assert_clause_param_names(ExpectedNames, Params) ->
    ?assertEqual(ExpectedNames, [Name || {Name, _Transform} <- Params]).

s094_assert_clear_buffer_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s094_assert_nif(
        maps:get("glClearBufferfv", NifFunctions),
        [
            {"Buffer", s094_enum_nif_data()},
            {"DrawBuffer", s094_int_nif_data()},
            {"Value", {list_gl_type, s094_float_raw_nif_data()}}
        ]
    ),
    s094_assert_nif(
        maps:get("glClearBufferiv", NifFunctions),
        [
            {"Buffer", s094_enum_nif_data()},
            {"DrawBuffer", s094_int_nif_data()},
            {"Value", {list_gl_type, s094_int_raw_nif_data()}}
        ]
    ),
    s094_assert_nif(
        maps:get("glClearBufferuiv", NifFunctions),
        [
            {"Buffer", s094_enum_nif_data()},
            {"DrawBuffer", s094_int_nif_data()},
            {"Value", {list_gl_type, s094_uint_raw_nif_data()}}
        ]
    ).

s094_assert_clear_named_framebuffer_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s094_assert_nif(
        maps:get("glClearNamedFramebufferfv", NifFunctions),
        [
            {"Framebuffer", s094_uint_nif_data()},
            {"Buffer", s094_enum_nif_data()},
            {"DrawBuffer", s094_int_nif_data()},
            {"Value", {list_gl_type, s094_float_raw_nif_data()}}
        ]
    ),
    s094_assert_nif(
        maps:get("glClearNamedFramebufferiv", NifFunctions),
        [
            {"Framebuffer", s094_uint_nif_data()},
            {"Buffer", s094_enum_nif_data()},
            {"DrawBuffer", s094_int_nif_data()},
            {"Value", {list_gl_type, s094_int_raw_nif_data()}}
        ]
    ),
    s094_assert_nif(
        maps:get("glClearNamedFramebufferuiv", NifFunctions),
        [
            {"Framebuffer", s094_uint_nif_data()},
            {"Buffer", s094_enum_nif_data()},
            {"DrawBuffer", s094_int_nif_data()},
            {"Value", {list_gl_type, s094_uint_raw_nif_data()}}
        ]
    ).

s094_assert_nif(NifData, Params) ->
    ?assertEqual(length(Params), maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s094_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s094_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard94-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s094_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s094_assert_contains(C, Needle) || Needle <- RequiredC],
        [s094_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s094_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s094_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s094_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s094_forbidden_needles() ->
    [
        <<"-export([clear_buffer/5]).">>,
        <<"-export([clear_named_framebuffer/6]).">>
    ].

s094_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s094_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s094_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s094_float_raw_nif_data() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s094_int_raw_nif_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s094_uint_raw_nif_data() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.


%% Historical shard 108.
s108_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s108_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s108_emitter_gles32_direct_controls_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s108_assert_emitted_surface(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s108_functions() ->
    [
        {"blend_barrier", 0},
        {"primitive_bounding_box", 8}
    ].

s108_supported({gles, {3, 2}}) ->
    true;
s108_supported(_) ->
    false.

s108_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    [
        s108_assert_presence(s108_supported(Target), Function, Functions)
     || Function <- s108_functions()
    ],
    case s108_supported(Target) of
        true ->
            s108_assert_blend_barrier(maps:get({"blend_barrier", 0}, Functions)),
            s108_assert_primitive_bounding_box(maps:get({"primitive_bounding_box", 8}, Functions));
        false ->
            ok
    end,
    s108_assert_deferred_neighbors_absent(Functions).

s108_assert_presence(true, Function, Functions) ->
    ?assert(maps:is_key(Function, Functions));
s108_assert_presence(false, Function, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)).

s108_assert_blend_barrier(FunctionData) ->
    s108_assert_direct(
        FunctionData,
        "glBlendBarrier",
        [],
        [],
        [],
        [],
        gl_void
    ).

s108_assert_primitive_bounding_box(FunctionData) ->
    Params = [
        {"MinX", gl_float},
        {"MinY", gl_float},
        {"MinZ", gl_float},
        {"MinW", gl_float},
        {"MaxX", gl_float},
        {"MaxY", gl_float},
        {"MaxZ", gl_float},
        {"MaxW", gl_float}
    ],
    s108_assert_direct(
        FunctionData,
        "glPrimitiveBoundingBox",
        [{in, Name, Type} || {Name, Type} <- Params],
        [{Name, {gl, float, []}} || {Name, _Type} <- Params],
        [{Name, do_nothing} || {Name, _Type} <- Params],
        [{Name, s108_float_nif_data()} || {Name, _Type} <- Params],
        gl_void
    ).

s108_assert_direct(
    FunctionData,
    GlCommand,
    ParamsSpecs,
    SpecsParams,
    ClauseParams,
    NifParams,
    ReturnSpecs
) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(ReturnSpecs, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    ?assertEqual(ClauseParams, maps:get(params, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s108_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glPrimitiveBoundingBoxEXT",
        "glGetnTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s108_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard108-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case s108_supported(Target) of
            true ->
                s108_assert_contains(Erl, <<"-export([blend_barrier/0]).">>),
                s108_assert_contains(Erl, <<"-export([primitive_bounding_box/8]).">>),
                s108_assert_contains(Erl, <<"-spec blend_barrier() -> ok | {error, atom()}.">>),
                s108_assert_contains(Erl, <<"-spec primitive_bounding_box(">>),
                s108_assert_contains(C, <<"glBlendBarrier();">>),
                s108_assert_contains(C, <<"glPrimitiveBoundingBox(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>);
            false ->
                s108_assert_not_contains(Erl, <<"-export([blend_barrier/0]).">>),
                s108_assert_not_contains(Erl, <<"-export([primitive_bounding_box/8]).">>),
                s108_assert_not_contains(C, <<"glBlendBarrier(">>),
                s108_assert_not_contains(C, <<"glPrimitiveBoundingBox(">>)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s108_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

s108_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s108_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).


%% Historical shard 112.
s112_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s112_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s112_emitter_depth_stencil_clear_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s112_assert_emitted_surface(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s112_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assertEqual(
        s112_supports_clear_buffer_depth_stencil(Target),
        maps:is_key({"clear_buffer_depth_stencil", 3}, Functions)
    ),
    ?assertEqual(
        s112_supports_named_clear_buffer_depth_stencil(Target),
        maps:is_key({"clear_named_framebuffer_depth_stencil", 4}, Functions)
    ),
    ?assertNot(maps:is_key({"clear_buffer", 5}, Functions)),
    ?assertNot(maps:is_key({"clear_named_framebuffer", 6}, Functions)),
    case s112_supports_clear_buffer_depth_stencil(Target) of
        true ->
            s112_assert_clear_buffer_depth_stencil(
                maps:get({"clear_buffer_depth_stencil", 3}, Functions)
            );
        false ->
            ok
    end,
    case s112_supports_named_clear_buffer_depth_stencil(Target) of
        true ->
            s112_assert_clear_named_framebuffer_depth_stencil(
                maps:get({"clear_named_framebuffer_depth_stencil", 4}, Functions)
            );
        false ->
            ok
    end.

s112_supports_clear_buffer_depth_stencil({gles, {2, 0}}) ->
    false;
s112_supports_clear_buffer_depth_stencil(_Target) ->
    true.

s112_supports_named_clear_buffer_depth_stencil({gl, {4, 6}}) ->
    true;
s112_supports_named_clear_buffer_depth_stencil(_Target) ->
    false.

s112_assert_clear_buffer_depth_stencil(FunctionData) ->
    ?assertEqual("glClearBufferfi", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {const, "Buffer", {gl_enum_constant, "GL_DEPTH_STENCIL"}},
            {in, "DrawBuffer", gl_int},
            {in, "Depth", gl_float},
            {in, "Stencil", gl_int}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"DrawBuffer", {gl, int, []}},
            {"Depth", {gl, float, []}},
            {"Stencil", {gl, int, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Buffer", {gl_enum_constant, "GL_DEPTH_STENCIL"}},
            {"DrawBuffer", do_nothing},
            {"Depth", do_nothing},
            {"Stencil", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glClearBufferfi", maps:get(raw_function, Clause)),
    s112_assert_nif(
        maps:get("glClearBufferfi", maps:get(nif_functions, FunctionData)),
        [
            {"Buffer", s112_enum_nif_data()},
            {"DrawBuffer", s112_int_nif_data()},
            {"Depth", s112_float_nif_data()},
            {"Stencil", s112_int_nif_data()}
        ]
    ).

s112_assert_clear_named_framebuffer_depth_stencil(FunctionData) ->
    ?assertEqual("glClearNamedFramebufferfi", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {const, "Buffer", {gl_enum_constant, "GL_DEPTH_STENCIL"}},
            {in, "DrawBuffer", gl_int},
            {in, "Depth", gl_float},
            {in, "Stencil", gl_int}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"DrawBuffer", {gl, int, []}},
            {"Depth", {gl, float, []}},
            {"Stencil", {gl, int, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Framebuffer", do_nothing},
            {"Buffer", {gl_enum_constant, "GL_DEPTH_STENCIL"}},
            {"DrawBuffer", do_nothing},
            {"Depth", do_nothing},
            {"Stencil", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glClearNamedFramebufferfi", maps:get(raw_function, Clause)),
    s112_assert_nif(
        maps:get("glClearNamedFramebufferfi", maps:get(nif_functions, FunctionData)),
        [
            {"Framebuffer", s112_uint_nif_data()},
            {"Buffer", s112_enum_nif_data()},
            {"DrawBuffer", s112_int_nif_data()},
            {"Depth", s112_float_nif_data()},
            {"Stencil", s112_int_nif_data()}
        ]
    ).

s112_assert_nif(NifData, Params) ->
    ?assertEqual(length(Params), maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s112_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard112-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        s112_assert_emitted_presence(Target, Erl, C)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s112_assert_emitted_presence({gles, {2, 0}}, Erl, C) ->
    s112_assert_not_contains(Erl, <<"-export([clear_buffer_depth_stencil/3]).">>),
    s112_assert_not_contains(Erl, <<"-export([clear_named_framebuffer_depth_stencil/4]).">>),
    s112_assert_not_contains(C, <<"glClearBufferfi(">>),
    s112_assert_not_contains(C, <<"glClearNamedFramebufferfi(">>);
s112_assert_emitted_presence({gl, {4, 6}}, Erl, C) ->
    s112_assert_contains(Erl, <<"-export([clear_buffer_depth_stencil/3]).">>),
    s112_assert_contains(Erl, <<"-export([clear_named_framebuffer_depth_stencil/4]).">>),
    s112_assert_contains(Erl, <<"?CALL_RAW_FUNC(glClearBufferfi_raw(?GL_DEPTH_STENCIL, DrawBuffer, Depth, Stencil)).">>),
    s112_assert_contains(Erl, <<"?CALL_RAW_FUNC(glClearNamedFramebufferfi_raw(Framebuffer, ?GL_DEPTH_STENCIL, DrawBuffer, Depth, Stencil)).">>),
    s112_assert_contains(C, <<"glClearBufferfi(arg_0, arg_1, arg_2, arg_3);">>),
    s112_assert_contains(C, <<"glClearNamedFramebufferfi(arg_0, arg_1, arg_2, arg_3, arg_4);">>);
s112_assert_emitted_presence(_Target, Erl, C) ->
    s112_assert_contains(Erl, <<"-export([clear_buffer_depth_stencil/3]).">>),
    s112_assert_not_contains(Erl, <<"-export([clear_named_framebuffer_depth_stencil/4]).">>),
    s112_assert_contains(C, <<"glClearBufferfi(arg_0, arg_1, arg_2, arg_3);">>),
    s112_assert_not_contains(C, <<"glClearNamedFramebufferfi(">>).

s112_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s112_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s112_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s112_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

s112_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s112_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).


%% Historical shard 119.
s119_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s119_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s119_emitter_uniform_array_setters_test_() ->
    [
        {"gl 4.6", fun() ->
            s119_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([uniform/3]).">>,
                    <<"[gl:float()]">>,
                    <<"[vector4(gl:double())]">>,
                    <<"uniform(f, Location, Value) when is_list(Value) ->">>,
                    <<"Count = length(Value),">>,
                    <<"?CALL_RAW_FUNC(glUniform1fv_raw(Location, Count, Value))">>,
                    <<"?CALL_RAW_FUNC(glUniform4dv_raw(Location, Count, NewValue))">>
                ],
                [
                    <<"glUniform1fv(arg_0, arg_1, arg_2_array);">>,
                    <<"glUniform4dv(arg_0, arg_1, arg_2_array);">>
                ],
                [
                    <<"-export([uniform_1fv/3]).">>,
                    <<"-export([uniform_matrix/4]).">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s119_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([uniform/3]).">>,
                    <<"[gl:float()]">>,
                    <<"[vector4(gl:uint())]">>,
                    <<"uniform(ui, Location, Value) when is_list(Value) ->">>,
                    <<"?CALL_RAW_FUNC(glUniform4uiv_raw(Location, Count, NewValue))">>
                ],
                [
                    <<"glUniform1fv(arg_0, arg_1, arg_2_array);">>,
                    <<"glUniform4uiv(arg_0, arg_1, arg_2_array);">>
                ],
                [
                    <<"glUniform1dv_raw">>
                ]
            )
        end},
        {"gles 2.0", fun() ->
            s119_assert_emitted_surface(
                {gles, {2, 0}},
                [
                    <<"-export([uniform/3]).">>,
                    <<"[gl:float()]">>,
                    <<"[vector4(gl:int())]">>,
                    <<"glUniform1fv_raw/3">>
                ],
                [
                    <<"glUniform1fv(arg_0, arg_1, arg_2_array);">>,
                    <<"glUniform4iv(arg_0, arg_1, arg_2_array);">>
                ],
                [
                    <<"glUniform1uiv_raw">>,
                    <<"glUniform1dv_raw">>,
                    <<"glUniformMatrix2x3fv_raw">>
                ]
            )
        end}
    ].

s119_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform", 3}, Functions)),
    s119_assert_direct_wrappers_absent(Functions),
    s119_assert_deferred_neighbors_absent(Functions),

    Uniform = maps:get({"uniform", 3}, Functions),
    ?assertEqual(
        [
            {in, "Location", gl_int},
            {in, "Value", gl_x}
        ],
        maps:get(params_specs, Uniform)
    ),
    s119_assert_specs(Target, Uniform),
    s119_assert_family(Target, Uniform, f, gl_float),
    s119_assert_family(Target, Uniform, i, gl_int),
    case s119_supports_unsigned_uniforms(Target) of
        true -> s119_assert_family(Target, Uniform, ui, gl_uint);
        false -> s119_assert_family_absent(Functions, Uniform, ui)
    end,
    case s119_supports_double_uniforms(Target) of
        true -> s119_assert_family(Target, Uniform, d, gl_double);
        false -> s119_assert_family_absent(Functions, Uniform, d)
    end.

s119_supports_unsigned_uniforms({gles, {2, 0}}) ->
    false;
s119_supports_unsigned_uniforms(_) ->
    true.

s119_supports_double_uniforms({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s119_supports_double_uniforms({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s119_supports_double_uniforms(_) ->
    false.

s119_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"uniform_1fv", 3},
        {"uniform_2fv", 3},
        {"uniform_3fv", 3},
        {"uniform_4fv", 3},
        {"uniform_1iv", 3},
        {"uniform_2iv", 3},
        {"uniform_3iv", 3},
        {"uniform_4iv", 3},
        {"uniform_1uiv", 3},
        {"uniform_2uiv", 3},
        {"uniform_3uiv", 3},
        {"uniform_4uiv", 3},
        {"uniform_1dv", 3},
        {"uniform_2dv", 3},
        {"uniform_3dv", 3},
        {"uniform_4dv", 3}
    ]).

s119_assert_deferred_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s119_assert_specs(Target, Uniform) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, uniform_value, []}}
    ] = maps:get(specs_params, Uniform),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    ?assertEqual(s119_supports_unsigned_uniforms(Target), lists:member(ui, TypeAtoms)),
    ?assertEqual(s119_supports_double_uniforms(Target), lists:member(d, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, Uniform)),
    ?assertEqual(3, maps:get(function_arity, Uniform)).

s119_assert_family(Target, Uniform, TypeAtom, GlType) ->
    s119_assert_type_specs(Uniform, GlType),
    s119_assert_commands(Uniform, TypeAtom, GlType),
    s119_assert_clauses(Uniform, TypeAtom, GlType),
    s119_assert_nifs(Uniform, TypeAtom, GlType),
    case {TypeAtom, Target} of
        {ui, {gles, {2, 0}}} -> error(unexpected_unsigned_uniforms);
        {d, _} ->
            ?assert(s119_supports_double_uniforms(Target));
        _ ->
            ok
    end.

s119_assert_family_absent(Functions, Uniform, TypeAtom) ->
    Suffix = atom_to_list(TypeAtom),
    [{"Type", {set, TypeAtoms}} | _] = maps:get(specs_params, Uniform),
    ?assertNot(lists:member(TypeAtom, TypeAtoms)),
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, [
        "glUniform1" ++ Suffix,
        "glUniform2" ++ Suffix,
        "glUniform3" ++ Suffix,
        "glUniform4" ++ Suffix,
        "glUniform1" ++ Suffix ++ "v",
        "glUniform2" ++ Suffix ++ "v",
        "glUniform3" ++ Suffix ++ "v",
        "glUniform4" ++ Suffix ++ "v"
    ]).

s119_assert_type_specs(Uniform, GlType) ->
    ScalarSpec = s119_scalar_spec(GlType),
    {uniform_value, {set, ExtraTypeVariants}} = maps:get(extra_type, Uniform),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, ExtraTypeVariants))
    end, [
        ScalarSpec,
        {list, ScalarSpec},
        {undefined, vector2, [ScalarSpec]},
        {undefined, vector3, [ScalarSpec]},
        {undefined, vector4, [ScalarSpec]},
        {list, {undefined, vector2, [ScalarSpec]}},
        {list, {undefined, vector3, [ScalarSpec]}},
        {list, {undefined, vector4, [ScalarSpec]}}
    ]).

s119_assert_commands(Uniform, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    GlCommands = maps:get(gl_commands, Uniform),
    Variants = maps:get(variants, Uniform),
    lists:foreach(fun(Command) ->
        ?assert(lists:member(Command, GlCommands))
    end, [
        {"glUniform1" ++ Suffix ++ "v", GlType, counted_array},
        {"glUniform2" ++ Suffix ++ "v", {gl_vector, 2, GlType}, counted_array},
        {"glUniform3" ++ Suffix ++ "v", {gl_vector, 3, GlType}, counted_array},
        {"glUniform4" ++ Suffix ++ "v", {gl_vector, 4, GlType}, counted_array}
    ]),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {GlType, counted_array},
        {{gl_vector, 2, GlType}, counted_array},
        {{gl_vector, 3, GlType}, counted_array},
        {{gl_vector, 4, GlType}, counted_array}
    ]).

s119_assert_clauses(Uniform, TypeAtom, _GlType) ->
    Suffix = atom_to_list(TypeAtom),
    Clauses = maps:get(function_clauses, Uniform),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s119_assert_before("glUniform1" ++ Suffix ++ "v", "glUniform1" ++ Suffix, RawOrder),
    s119_assert_array_clause(Clauses, Suffix, "glUniform1" ++ Suffix ++ "v", scalar),
    s119_assert_array_clause(Clauses, Suffix, "glUniform2" ++ Suffix ++ "v", 2),
    s119_assert_array_clause(Clauses, Suffix, "glUniform3" ++ Suffix ++ "v", 3),
    s119_assert_array_clause(Clauses, Suffix, "glUniform4" ++ Suffix ++ "v", 4).

s119_assert_array_clause(Clauses, Suffix, RawFunction, scalar) ->
    Clause = s119_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {Suffix, ignore},
            {"Location", do_nothing},
            {"Count", {derived_count, "Value"}},
            {"Value", do_nothing}
        ],
        maps:get(params, Clause)
    );
s119_assert_array_clause(Clauses, Suffix, RawFunction, VectorSize) ->
    Clause = s119_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual(
        [
            {is_list, var},
            {is_tuple, head_var},
            {tuple_size, head_var, VectorSize}
        ],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {Suffix, ignore},
            {"Location", do_nothing},
            {"Count", {derived_count, "Value"}},
            {"Value", {list_gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s119_assert_nifs(Uniform, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    NifFunctions = maps:get(nif_functions, Uniform),
    ValueSpec = s119_convert_spec(GlType),
    s119_assert_array_nif(maps:get("glUniform1" ++ Suffix ++ "v", NifFunctions), ValueSpec),
    s119_assert_array_nif(maps:get("glUniform2" ++ Suffix ++ "v", NifFunctions), ValueSpec),
    s119_assert_array_nif(maps:get("glUniform3" ++ Suffix ++ "v", NifFunctions), ValueSpec),
    s119_assert_array_nif(maps:get("glUniform4" ++ Suffix ++ "v", NifFunctions), ValueSpec).

s119_assert_array_nif(NifData, ValueSpec) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
            {"Count", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {"Value", {list_gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s119_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s119_assert_before(First, Second, Values) ->
    ?assert(s119_index_of(First, Values) < s119_index_of(Second, Values)).

s119_index_of(Value, Values) ->
    s119_index_of(Value, Values, 1).

s119_index_of(Value, [Value | _], Index) ->
    Index;
s119_index_of(Value, [_ | Rest], Index) ->
    s119_index_of(Value, Rest, Index + 1).

s119_scalar_spec(gl_float) -> {gl, float, []};
s119_scalar_spec(gl_int) -> {gl, int, []};
s119_scalar_spec(gl_uint) -> {gl, uint, []};
s119_scalar_spec(gl_double) -> {gl, double, []}.

s119_convert_spec(gl_float) -> {"GLfloat", "double", "enif_get_double", "enif_make_double"};
s119_convert_spec(gl_int) -> {"GLint", "int", "enif_get_int", "enif_make_int"};
s119_convert_spec(gl_uint) -> {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"};
s119_convert_spec(gl_double) -> {"GLdouble", "double", "enif_get_double", "enif_make_double"}.

s119_assert_emitted_surface(Target, PresentErl, PresentC, Absent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard119-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        lists:foreach(fun(Needle) -> s119_assert_contains(Erl, Needle) end, PresentErl),
        lists:foreach(fun(Needle) -> s119_assert_contains(C, Needle) end, PresentC),
        lists:foreach(fun(Needle) ->
            s119_assert_not_contains(Erl, Needle),
            s119_assert_not_contains(C, Needle)
        end, Absent)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s119_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s119_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 120.
s120_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s120_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s120_emitter_program_uniform_array_setters_test_() ->
    [
        {"gl 4.6", fun() ->
            s120_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([program_uniform/4]).">>,
                    <<"[gl:float()]">>,
                    <<"[vector4(gl:double())]">>,
                    <<"program_uniform(f, Program, Location, Value) when is_list(Value) ->">>,
                    <<"Count = length(Value),">>,
                    <<"?CALL_RAW_FUNC(glProgramUniform1fv_raw(Program, Location, Count, Value))">>,
                    <<"?CALL_RAW_FUNC(glProgramUniform4dv_raw(Program, Location, Count, NewValue))">>
                ],
                [
                    <<"glProgramUniform1fv(arg_0, arg_1, arg_2, arg_3_array);">>,
                    <<"glProgramUniform4dv(arg_0, arg_1, arg_2, arg_3_array);">>
                ],
                [
                    <<"-export([program_uniform_1fv/4]).">>,
                    <<"-export([program_uniform_matrix_4fv/5]).">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s120_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([program_uniform/4]).">>,
                    <<"[gl:float()]">>,
                    <<"[vector4(gl:uint())]">>,
                    <<"program_uniform(ui, Program, Location, Value) when is_list(Value) ->">>,
                    <<"?CALL_RAW_FUNC(glProgramUniform4uiv_raw(Program, Location, Count, NewValue))">>
                ],
                [
                    <<"glProgramUniform1fv(arg_0, arg_1, arg_2, arg_3_array);">>,
                    <<"glProgramUniform4uiv(arg_0, arg_1, arg_2, arg_3_array);">>
                ],
                [
                    <<"glProgramUniform1dv_raw">>
                ]
            )
        end},
        {"gles 3.0", fun() ->
            s120_assert_emitted_surface(
                {gles, {3, 0}},
                [],
                [],
                [
                    <<"-export([program_uniform/4]).">>,
                    <<"glProgramUniform1fv_raw">>,
                    <<"glProgramUniform1f(">>,
                    <<"glProgramUniform1fv(">>
                ]
            )
        end}
    ].

s120_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s120_supports_program_uniform(Target) of
        true ->
            ?assert(maps:is_key({"program_uniform", 4}, Functions)),
            s120_assert_direct_wrappers_absent(Functions),
            ProgramUniform = maps:get({"program_uniform", 4}, Functions),
            s120_assert_specs(Target, ProgramUniform),
            s120_assert_array_family(Target, ProgramUniform, f, gl_float),
            s120_assert_array_family(Target, ProgramUniform, i, gl_int),
            s120_assert_array_family(Target, ProgramUniform, ui, gl_uint),
            case s120_supports_double_program_uniform(Target) of
                true -> s120_assert_array_family(Target, ProgramUniform, d, gl_double);
                false -> s120_assert_array_family_absent(Functions, ProgramUniform, d)
            end,
            s120_assert_deferred_neighbors_absent(Functions);
        false ->
            ?assertNot(maps:is_key({"program_uniform", 4}, Functions)),
            s120_assert_no_array_commands(Functions)
    end.

s120_supports_program_uniform({gl, {4, 1}}) -> true;
s120_supports_program_uniform({gl, {4, 6}}) -> true;
s120_supports_program_uniform({gles, {3, 1}}) -> true;
s120_supports_program_uniform({gles, {3, 2}}) -> true;
s120_supports_program_uniform(_) -> false.

s120_supports_double_program_uniform({gl, _}) -> true;
s120_supports_double_program_uniform({gles, _}) -> false.

s120_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"program_uniform_1fv", 4},
        {"program_uniform_2fv", 4},
        {"program_uniform_3fv", 4},
        {"program_uniform_4fv", 4},
        {"program_uniform_1iv", 4},
        {"program_uniform_2iv", 4},
        {"program_uniform_3iv", 4},
        {"program_uniform_4iv", 4},
        {"program_uniform_1uiv", 4},
        {"program_uniform_2uiv", 4},
        {"program_uniform_3uiv", 4},
        {"program_uniform_4uiv", 4},
        {"program_uniform_1dv", 4},
        {"program_uniform_2dv", 4},
        {"program_uniform_3dv", 4},
        {"program_uniform_4dv", 4}
    ]).

s120_assert_specs(Target, ProgramUniform) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, program_uniform_value, []}}
    ] = maps:get(specs_params, ProgramUniform),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    ?assert(lists:member(ui, TypeAtoms)),
    ?assertEqual(s120_supports_double_program_uniform(Target), lists:member(d, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, ProgramUniform)),
    ?assertEqual(4, maps:get(function_arity, ProgramUniform)).

s120_assert_array_family(Target, ProgramUniform, TypeAtom, GlType) ->
    s120_assert_type_specs(ProgramUniform, GlType),
    s120_assert_commands(ProgramUniform, TypeAtom, GlType),
    s120_assert_clauses(ProgramUniform, TypeAtom),
    s120_assert_nifs(ProgramUniform, TypeAtom, GlType),
    case {TypeAtom, Target} of
        {d, _} -> ?assert(s120_supports_double_program_uniform(Target));
        _ -> ok
    end.

s120_assert_array_family_absent(Functions, ProgramUniform, TypeAtom) ->
    Suffix = atom_to_list(TypeAtom),
    [{"Type", {set, TypeAtoms}} | _] = maps:get(specs_params, ProgramUniform),
    ?assertNot(lists:member(TypeAtom, TypeAtoms)),
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, [
        "glProgramUniform1" ++ Suffix ++ "v",
        "glProgramUniform2" ++ Suffix ++ "v",
        "glProgramUniform3" ++ Suffix ++ "v",
        "glProgramUniform4" ++ Suffix ++ "v"
    ]).

s120_assert_type_specs(ProgramUniform, GlType) ->
    ScalarSpec = s120_scalar_spec(GlType),
    {program_uniform_value, {set, ExtraTypeVariants}} = maps:get(extra_type, ProgramUniform),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, ExtraTypeVariants))
    end, [
        ScalarSpec,
        {list, ScalarSpec},
        {undefined, vector2, [ScalarSpec]},
        {undefined, vector3, [ScalarSpec]},
        {undefined, vector4, [ScalarSpec]},
        {list, {undefined, vector2, [ScalarSpec]}},
        {list, {undefined, vector3, [ScalarSpec]}},
        {list, {undefined, vector4, [ScalarSpec]}}
    ]).

s120_assert_commands(ProgramUniform, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    GlCommands = maps:get(gl_commands, ProgramUniform),
    Variants = maps:get(variants, ProgramUniform),
    lists:foreach(fun(Command) ->
        ?assert(lists:member(Command, GlCommands))
    end, [
        {"glProgramUniform1" ++ Suffix ++ "v", GlType, counted_array},
        {"glProgramUniform2" ++ Suffix ++ "v", {gl_vector, 2, GlType}, counted_array},
        {"glProgramUniform3" ++ Suffix ++ "v", {gl_vector, 3, GlType}, counted_array},
        {"glProgramUniform4" ++ Suffix ++ "v", {gl_vector, 4, GlType}, counted_array}
    ]),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {GlType, counted_array},
        {{gl_vector, 2, GlType}, counted_array},
        {{gl_vector, 3, GlType}, counted_array},
        {{gl_vector, 4, GlType}, counted_array}
    ]).

s120_assert_clauses(ProgramUniform, TypeAtom) ->
    Suffix = atom_to_list(TypeAtom),
    Clauses = maps:get(function_clauses, ProgramUniform),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s120_assert_before("glProgramUniform1" ++ Suffix ++ "v", "glProgramUniform1" ++ Suffix, RawOrder),
    s120_assert_array_clause(Clauses, Suffix, "glProgramUniform1" ++ Suffix ++ "v", scalar),
    s120_assert_array_clause(Clauses, Suffix, "glProgramUniform2" ++ Suffix ++ "v", 2),
    s120_assert_array_clause(Clauses, Suffix, "glProgramUniform3" ++ Suffix ++ "v", 3),
    s120_assert_array_clause(Clauses, Suffix, "glProgramUniform4" ++ Suffix ++ "v", 4).

s120_assert_array_clause(Clauses, Suffix, RawFunction, scalar) ->
    Clause = s120_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {Suffix, ignore},
            {"Program", do_nothing},
            {"Location", do_nothing},
            {"Count", {derived_count, "Value"}},
            {"Value", do_nothing}
        ],
        maps:get(params, Clause)
    );
s120_assert_array_clause(Clauses, Suffix, RawFunction, VectorSize) ->
    Clause = s120_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual(
        [
            {is_list, var},
            {is_tuple, head_var},
            {tuple_size, head_var, VectorSize}
        ],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {Suffix, ignore},
            {"Program", do_nothing},
            {"Location", do_nothing},
            {"Count", {derived_count, "Value"}},
            {"Value", {list_gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s120_assert_nifs(ProgramUniform, TypeAtom, GlType) ->
    Suffix = atom_to_list(TypeAtom),
    NifFunctions = maps:get(nif_functions, ProgramUniform),
    ValueSpec = s120_convert_spec(GlType),
    s120_assert_array_nif(maps:get("glProgramUniform1" ++ Suffix ++ "v", NifFunctions), ValueSpec),
    s120_assert_array_nif(maps:get("glProgramUniform2" ++ Suffix ++ "v", NifFunctions), ValueSpec),
    s120_assert_array_nif(maps:get("glProgramUniform3" ++ Suffix ++ "v", NifFunctions), ValueSpec),
    s120_assert_array_nif(maps:get("glProgramUniform4" ++ Suffix ++ "v", NifFunctions), ValueSpec).

s120_assert_array_nif(NifData, ValueSpec) ->
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, s120_convert_spec(gl_uint)}},
            {"Location", {gl_type, s120_convert_spec(gl_int)}},
            {"Count", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {"Value", {list_gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s120_assert_deferred_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s120_assert_no_array_commands(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, s120_array_commands()).

s120_array_commands() ->
    [
        "glProgramUniform1fv",
        "glProgramUniform2fv",
        "glProgramUniform3fv",
        "glProgramUniform4fv",
        "glProgramUniform1iv",
        "glProgramUniform2iv",
        "glProgramUniform3iv",
        "glProgramUniform4iv",
        "glProgramUniform1uiv",
        "glProgramUniform2uiv",
        "glProgramUniform3uiv",
        "glProgramUniform4uiv",
        "glProgramUniform1dv",
        "glProgramUniform2dv",
        "glProgramUniform3dv",
        "glProgramUniform4dv"
    ].

s120_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s120_assert_before(First, Second, Values) ->
    ?assert(s120_index_of(First, Values) < s120_index_of(Second, Values)).

s120_index_of(Value, Values) ->
    s120_index_of(Value, Values, 1).

s120_index_of(Value, [Value | _], Index) ->
    Index;
s120_index_of(Value, [_ | Rest], Index) ->
    s120_index_of(Value, Rest, Index + 1).

s120_scalar_spec(gl_float) -> {gl, float, []};
s120_scalar_spec(gl_int) -> {gl, int, []};
s120_scalar_spec(gl_uint) -> {gl, uint, []};
s120_scalar_spec(gl_double) -> {gl, double, []}.

s120_convert_spec(gl_float) -> {"GLfloat", "double", "enif_get_double", "enif_make_double"};
s120_convert_spec(gl_int) -> {"GLint", "int", "enif_get_int", "enif_make_int"};
s120_convert_spec(gl_uint) -> {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"};
s120_convert_spec(gl_double) -> {"GLdouble", "double", "enif_get_double", "enif_make_double"}.

s120_assert_emitted_surface(Target, PresentErl, PresentC, Absent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard120-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        lists:foreach(fun(Needle) -> s120_assert_contains(Erl, Needle) end, PresentErl),
        lists:foreach(fun(Needle) -> s120_assert_contains(C, Needle) end, PresentC),
        lists:foreach(fun(Needle) ->
            s120_assert_not_contains(Erl, Needle),
            s120_assert_not_contains(C, Needle)
        end, Absent)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s120_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s120_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 121.
s121_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s121_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s121_emitter_uniform_matrix_test_() ->
    [
        {"gl 4.6", fun() -> s121_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s121_assert_emitted_surface({gles, {3, 2}}) end}
    ].

s121_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform_matrix", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_matrix_2fv", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_matrix_3fv", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_matrix_4fv", 2}, Functions)),

    UniformMatrix = maps:get({"uniform_matrix", 3}, Functions),
    s121_assert_specs(UniformMatrix),
    s121_assert_commands(UniformMatrix),
    s121_assert_clauses(UniformMatrix),
    s121_assert_nifs(UniformMatrix),
    s121_assert_deferred_matrix_neighbors_absent(Functions).

s121_assert_specs(UniformMatrix) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, uniform_matrix_value, []}}
    ] = maps:get(specs_params, UniformMatrix),
    ?assert(lists:member(f, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, UniformMatrix)),
    ?assertEqual(3, maps:get(function_arity, UniformMatrix)),

    {uniform_matrix_value, {set, TypeVariants}} = maps:get(extra_type, UniformMatrix),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, TypeVariants))
    end, [
        {undefined, matrix2, [{gl, float, []}]},
        {undefined, matrix3, [{gl, float, []}]},
        {undefined, matrix4, [{gl, float, []}]}
    ]).

s121_assert_commands(UniformMatrix) ->
    GlCommands = maps:get(gl_commands, UniformMatrix),
    Variants = maps:get(variants, UniformMatrix),
    lists:foreach(fun(Size) ->
        Command = "glUniformMatrix" ++ integer_to_list(Size) ++ "fv",
        Variant = {{gl_matrix, Size, Size, gl_float}, element},
        ?assert(lists:member({Command, {gl_matrix, Size, Size, gl_float}, element}, GlCommands)),
        ?assert(lists:member(Variant, Variants))
    end, [2, 3, 4]).

s121_assert_clauses(UniformMatrix) ->
    Clauses = maps:get(function_clauses, UniformMatrix),
    lists:foreach(fun(Size) ->
        RawFunction = "glUniformMatrix" ++ integer_to_list(Size) ++ "fv",
        Clause = s121_find_clause(RawFunction, Clauses),
        ?assertEqual("Value", maps:get(guard_var, Clause)),
        ?assertEqual(
            [
                {is_tuple, var},
                {tuple_size, var, Size},
                {is_tuple, {element, 1, var}},
                {tuple_size, {element, 1, var}, Size}
            ],
            maps:get(guards, Clause)
        ),
        ?assertEqual(
            [
                {"f", ignore},
                {"Location", do_nothing},
                {"Count", {gl_sizei_constant, 1}},
                {"Transpose", {gl_bool_constant, false}},
                {"Value", {gl_matrix_to_list, Size, Size}}
            ],
            maps:get(params, Clause)
        )
    end, [2, 3, 4]).

s121_assert_nifs(UniformMatrix) ->
    NifFunctions = maps:get(nif_functions, UniformMatrix),
    lists:foreach(fun(Size) ->
        RawFunction = "glUniformMatrix" ++ integer_to_list(Size) ++ "fv",
        NifData = maps:get(RawFunction, NifFunctions),
        ?assertEqual(4, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
                {"Count", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
                {"Transpose", boolean_to_glbool},
                {"Value", {list_gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}}
            ],
            maps:get(params, NifData)
        ),
        ?assertEqual(void, maps:get(return, NifData))
    end, [2, 3, 4]).

s121_assert_deferred_matrix_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s121_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard121-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        lists:foreach(fun(Needle) -> s121_assert_contains(Erl, Needle) end, [
            <<"-export([uniform_matrix/3]).">>,
            <<"matrix2(gl:float())">>,
            <<"matrix3(gl:float())">>,
            <<"matrix4(gl:float())">>,
            <<"uniform_matrix(f, Location, Value) when">>,
            <<"?CALL_RAW_FUNC(glUniformMatrix2fv_raw(Location, 1, false, NewValue))">>,
            <<"?CALL_RAW_FUNC(glUniformMatrix3fv_raw(Location, 1, false, NewValue))">>,
            <<"?CALL_RAW_FUNC(glUniformMatrix4fv_raw(Location, 1, false, NewValue))">>
        ]),
        lists:foreach(fun(Needle) -> s121_assert_contains(C, Needle) end, [
            <<"glUniformMatrix2fv(arg_0, arg_1, arg_2, arg_3_array);">>,
            <<"glUniformMatrix4fv(arg_0, arg_1, arg_2, arg_3_array);">>
        ]),
        lists:foreach(fun(Needle) ->
            s121_assert_not_contains(Erl, Needle),
            s121_assert_not_contains(C, Needle)
        end, [
            <<"-export([uniform_matrix_2fv/2]).">>,
            <<"-export([program_uniform_matrix_2fv/5]).">>
        ])
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s121_find_clause(RawFunction, Clauses) ->
    [Clause] = [
        Clause0
     || Clause0 <- Clauses,
        maps:get(raw_function, Clause0) =:= RawFunction,
        s121_has_matrix_element_transform(Clause0)
    ],
    Clause.

s121_has_matrix_element_transform(Clause) ->
    lists:any(fun
        ({"Value", {gl_matrix_to_list, _M, _N}}) -> true;
        (_) -> false
    end, maps:get(params, Clause)).

s121_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s121_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 122.
s122_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s122_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s122_emitter_pixel_storef_test_() ->
    [
        {"gl 4.6", fun() -> s122_assert_emitted_surface({gl, {4, 6}}, desktop) end},
        {"gles 3.2", fun() -> s122_assert_emitted_surface({gles, {3, 2}}, es) end}
    ].

s122_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    ?assert(maps:is_key({"pixel_store", 2}, Functions)),
    ?assertNot(maps:is_key({"pixel_store", 3}, Functions)),
    ?assertNot(maps:is_key({"pixel_store_f", 2}, Functions)),
    PixelStore = maps:get({"pixel_store", 2}, Functions),
    s122_assert_pixel_store_parameter_enum(BindingData),
    case Target of
        {gl, _} ->
            s122_assert_desktop_pixel_store(PixelStore, Functions);
        {gles, _} ->
            s122_assert_es_pixel_store(PixelStore, Functions)
    end.

s122_assert_pixel_store_parameter_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("pixel_store_parameter", EnumTypes)),
    PixelStoreParameters = maps:get("pixel_store_parameter", EnumTypes),
    ?assert(lists:member("pack_alignment", PixelStoreParameters)),
    ?assert(lists:member("unpack_alignment", PixelStoreParameters)).

s122_assert_desktop_pixel_store(PixelStore, Functions) ->
    ?assert(generator_test_support:has_gl_command("glPixelStorei", Functions)),
    ?assert(generator_test_support:has_gl_command("glPixelStoref", Functions)),
    ?assertEqual(2, maps:get(function_arity, PixelStore)),
    ?assertEqual(
        [
            {"Name", {undefined, pixel_store_parameter, []}},
            {"Param", {set, [{gl, int, []}, {gl, float, []}]}}
        ],
        maps:get(specs_params, PixelStore)
    ),
    ?assertEqual([], maps:get(specs_return, PixelStore)),
    ?assert(lists:member("glPixelStorei", maps:get(gl_commands, PixelStore))),
    ?assert(lists:member("glPixelStoref", maps:get(gl_commands, PixelStore))),
    s122_assert_desktop_clauses(PixelStore),
    s122_assert_desktop_nifs(PixelStore).

s122_assert_es_pixel_store(PixelStore, Functions) ->
    ?assert(generator_test_support:has_gl_command("glPixelStorei", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glPixelStoref", Functions)),
    ?assertEqual(2, maps:get(function_arity, PixelStore)),
    ?assertEqual(
        [
            {"Name", {undefined, pixel_store_parameter, []}},
            {"Param", {gl, int, []}}
        ],
        maps:get(specs_params, PixelStore)
    ),
    ?assertEqual([], maps:get(specs_return, PixelStore)),
    [Clause] = maps:get(function_clauses, PixelStore),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assertEqual("glPixelStorei", maps:get(raw_function, Clause)),
    NifFunctions = maps:get(nif_functions, PixelStore),
    ?assert(maps:is_key("glPixelStorei", NifFunctions)),
    ?assertNot(maps:is_key("glPixelStoref", NifFunctions)),
    s122_assert_nif(
        maps:get("glPixelStorei", NifFunctions),
        [{"Name", s122_enum_nif_data()}, {"Param", s122_int_nif_data()}]
    ).

s122_assert_desktop_clauses(PixelStore) ->
    Clauses = maps:get(function_clauses, PixelStore),
    FloatClause = s122_find_clause("glPixelStoref", Clauses),
    IntClause = s122_find_clause("glPixelStorei", Clauses),
    s122_assert_clause(FloatClause, [{is_float, var}]),
    s122_assert_clause(IntClause, [{is_integer, var}]).

s122_assert_clause(Clause, Guards) ->
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual(Guards, maps:get(guards, Clause)),
    [
        {"Name", {gl_enum_to_uint, NameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("pack_alignment", 1, NameTransformMap)),
    ?assert(lists:keymember("unpack_alignment", 1, NameTransformMap)).

s122_assert_desktop_nifs(PixelStore) ->
    NifFunctions = maps:get(nif_functions, PixelStore),
    ?assert(maps:is_key("glPixelStorei", NifFunctions)),
    ?assert(maps:is_key("glPixelStoref", NifFunctions)),
    s122_assert_nif(
        maps:get("glPixelStorei", NifFunctions),
        [{"Name", s122_enum_nif_data()}, {"Param", s122_int_nif_data()}]
    ),
    s122_assert_nif(
        maps:get("glPixelStoref", NifFunctions),
        [{"Name", s122_enum_nif_data()}, {"Param", s122_float_nif_data()}]
    ).

s122_assert_nif(NifData, Params) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s122_assert_emitted_surface(Target, Expected) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard122-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case Expected of
            desktop ->
                s122_assert_contains(Erl, <<"-export([pixel_store/2]).">>),
                s122_assert_contains(Erl, <<"Param :: gl:int() | gl:float()">>),
                s122_assert_contains(Erl, <<"pixel_store(Name, Param) when is_float(Param) ->">>),
                s122_assert_contains(Erl, <<"pixel_store(Name, Param) when is_integer(Param) ->">>),
                s122_assert_contains(Erl, <<"?CALL_RAW_FUNC(glPixelStoref_raw(NewName, Param))">>),
                s122_assert_contains(Erl, <<"?CALL_RAW_FUNC(glPixelStorei_raw(NewName, Param))">>),
                s122_assert_contains(C, <<"glPixelStoref(arg_0, arg_1);">>),
                s122_assert_contains(C, <<"glPixelStorei(arg_0, arg_1);">>);
            es ->
                s122_assert_contains(Erl, <<"-export([pixel_store/2]).">>),
                s122_assert_contains(Erl, <<"Param :: gl:int()">>),
                s122_assert_contains(Erl, <<"?CALL_RAW_FUNC(glPixelStorei_raw(NewName, Param))">>),
                s122_assert_contains(C, <<"glPixelStorei(arg_0, arg_1);">>),
                s122_assert_not_contains(Erl, <<"Param :: gl:int() | gl:float()">>),
                s122_assert_not_contains(Erl, <<"glPixelStoref_raw">>),
                s122_assert_not_contains(C, <<"glPixelStoref">>)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s122_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s122_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s122_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s122_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s122_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s122_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 123.
s123_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s123_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s123_emitter_uniform_matrix_test_() ->
    [
        {"gl 4.6", fun() -> s123_assert_emitted_surface({gl, {4, 6}}, supported) end},
        {"gles 3.2", fun() -> s123_assert_emitted_surface({gles, {3, 2}}, supported) end},
        {"gles 2.0", fun() -> s123_assert_emitted_surface({gles, {2, 0}}, unsupported) end}
    ].

s123_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform_matrix", 3}, Functions)),
    s123_assert_direct_wrappers_absent(Functions),

    UniformMatrix = maps:get({"uniform_matrix", 3}, Functions),
    case s123_supports_non_square_uniform_matrix(Target) of
        true ->
            s123_assert_non_square_specs(UniformMatrix),
            s123_assert_non_square_commands(UniformMatrix),
            s123_assert_non_square_clauses(UniformMatrix),
            s123_assert_non_square_nifs(UniformMatrix);
        false ->
            s123_assert_non_square_absent(UniformMatrix, Functions)
    end,
    s123_assert_deferred_matrix_neighbors_absent(Functions).

s123_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"uniform_matrix_2x3fv", 2},
        {"uniform_matrix_3x2fv", 2},
        {"uniform_matrix_2x4fv", 2},
        {"uniform_matrix_4x2fv", 2},
        {"uniform_matrix_3x4fv", 2},
        {"uniform_matrix_4x3fv", 2}
    ]).

s123_assert_non_square_specs(UniformMatrix) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, uniform_matrix_value, []}}
    ] = maps:get(specs_params, UniformMatrix),
    ?assert(lists:member(f, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, UniformMatrix)),

    {uniform_matrix_value, {set, TypeVariants}} = maps:get(extra_type, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        TypeSpec = {undefined, s123_matrix_type(M, N), [{gl, float, []}]},
        ?assert(lists:member(TypeSpec, TypeVariants))
    end, s123_non_square_shapes()).

s123_assert_non_square_commands(UniformMatrix) ->
    GlCommands = maps:get(gl_commands, UniformMatrix),
    Variants = maps:get(variants, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        Command = s123_matrix_command(M, N),
        Variant = {{gl_matrix, M, N, gl_float}, element},
        ?assert(lists:member({Command, {gl_matrix, M, N, gl_float}, element}, GlCommands)),
        ?assert(lists:member(Variant, Variants))
    end, s123_non_square_shapes()).

s123_assert_non_square_clauses(UniformMatrix) ->
    Clauses = maps:get(function_clauses, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s123_matrix_command(M, N),
        Clause = s123_find_clause(RawFunction, Clauses),
        ?assertEqual("Value", maps:get(guard_var, Clause)),
        ?assertEqual(
            [
                {is_tuple, var},
                {tuple_size, var, M},
                {is_tuple, {element, 1, var}},
                {tuple_size, {element, 1, var}, N}
            ],
            maps:get(guards, Clause)
        ),
        ?assertEqual(
            [
                {"f", ignore},
                {"Location", do_nothing},
                {"Count", {gl_sizei_constant, 1}},
                {"Transpose", {gl_bool_constant, false}},
                {"Value", {gl_matrix_to_list, M, N}}
            ],
            maps:get(params, Clause)
        )
    end, s123_non_square_shapes()).

s123_assert_non_square_nifs(UniformMatrix) ->
    NifFunctions = maps:get(nif_functions, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s123_matrix_command(M, N),
        NifData = maps:get(RawFunction, NifFunctions),
        ?assertEqual(4, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
                {"Count", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
                {"Transpose", boolean_to_glbool},
                {"Value", {list_gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}}
            ],
            maps:get(params, NifData)
        ),
        ?assertEqual(void, maps:get(return, NifData))
    end, s123_non_square_shapes()).

s123_assert_non_square_absent(UniformMatrix, Functions) ->
    {uniform_matrix_value, {set, TypeVariants}} = maps:get(extra_type, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        ?assertNot(lists:member({undefined, s123_matrix_type(M, N), [{gl, float, []}]}, TypeVariants)),
        ?assertNot(generator_test_support:has_gl_command(s123_matrix_command(M, N), Functions))
    end, s123_non_square_shapes()).

s123_assert_deferred_matrix_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s123_assert_emitted_surface(Target, Support) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard123-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case Support of
            supported ->
                s123_assert_supported_emission(Erl, C);
            unsupported ->
                s123_assert_unsupported_emission(Erl, C)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s123_assert_supported_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s123_assert_contains(Erl, Needle) end, [
        <<"-export([uniform_matrix/3]).">>,
        <<"matrix2x3(gl:float())">>,
        <<"matrix4x3(gl:float())">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix2x3fv_raw(Location, 1, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix4x3fv_raw(Location, 1, false, NewValue))">>
    ]),
    lists:foreach(fun(Needle) -> s123_assert_contains(C, Needle) end, [
        <<"glUniformMatrix2x3fv(arg_0, arg_1, arg_2, arg_3_array);">>,
        <<"glUniformMatrix4x3fv(arg_0, arg_1, arg_2, arg_3_array);">>
    ]),
    s123_assert_not_contains(Erl, <<"-export([uniform_matrix_2x3fv/2]).">>).

s123_assert_unsupported_emission(Erl, C) ->
    lists:foreach(fun(Needle) ->
        s123_assert_not_contains(Erl, Needle),
        s123_assert_not_contains(C, Needle)
    end, [
        <<"glUniformMatrix2x3fv_raw">>,
        <<"glUniformMatrix4x3fv_raw">>,
        <<"glUniformMatrix2x3fv(arg_0, arg_1, arg_2, arg_3_array);">>,
        <<"glUniformMatrix4x3fv(arg_0, arg_1, arg_2, arg_3_array);">>
    ]).

s123_find_clause(RawFunction, Clauses) ->
    [Clause] = [
        Clause0
     || Clause0 <- Clauses,
        maps:get(raw_function, Clause0) =:= RawFunction,
        s123_has_matrix_element_transform(Clause0)
    ],
    Clause.

s123_has_matrix_element_transform(Clause) ->
    lists:any(fun
        ({"Value", {gl_matrix_to_list, _M, _N}}) -> true;
        (_) -> false
    end, maps:get(params, Clause)).

s123_supports_non_square_uniform_matrix({gles, {2, 0}}) -> false;
s123_supports_non_square_uniform_matrix(_) -> true.

s123_non_square_shapes() ->
    [{2, 3}, {3, 2}, {2, 4}, {4, 2}, {3, 4}, {4, 3}].

s123_matrix_command(M, N) ->
    "glUniformMatrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N) ++ "fv".

s123_matrix_type(M, N) ->
    list_to_atom("matrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N)).

s123_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s123_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 124.
s124_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s124_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s124_emitter_uniform_matrix_test_() ->
    [
        {"gl 4.6", fun() -> s124_assert_emitted_surface({gl, {4, 6}}, supported) end},
        {"gl 3.3", fun() -> s124_assert_emitted_surface({gl, {3, 3}}, unsupported) end},
        {"gles 3.2", fun() -> s124_assert_emitted_surface({gles, {3, 2}}, unsupported) end}
    ].

s124_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform_matrix", 3}, Functions)),
    s124_assert_direct_wrappers_absent(Functions),

    UniformMatrix = maps:get({"uniform_matrix", 3}, Functions),
    case s124_supports_double_uniform_matrix(Target) of
        true ->
            s124_assert_double_specs(UniformMatrix),
            s124_assert_double_commands(UniformMatrix),
            s124_assert_double_clauses(UniformMatrix),
            s124_assert_double_nifs(UniformMatrix);
        false ->
            s124_assert_double_absent(UniformMatrix, Functions)
    end,
    s124_assert_program_matrix_neighbors_absent(Functions).

s124_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"uniform_matrix_2dv", 2},
        {"uniform_matrix_3dv", 2},
        {"uniform_matrix_4dv", 2},
        {"uniform_matrix_2x3dv", 2},
        {"uniform_matrix_3x2dv", 2},
        {"uniform_matrix_2x4dv", 2},
        {"uniform_matrix_4x2dv", 2},
        {"uniform_matrix_3x4dv", 2},
        {"uniform_matrix_4x3dv", 2}
    ]).

s124_assert_double_specs(UniformMatrix) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, uniform_matrix_value, []}}
    ] = maps:get(specs_params, UniformMatrix),
    ?assert(lists:member(d, TypeAtoms)),
    ?assert(lists:member(f, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, UniformMatrix)),

    {uniform_matrix_value, {set, TypeVariants}} = maps:get(extra_type, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        TypeSpec = {undefined, s124_matrix_type(M, N), [{gl, double, []}]},
        ?assert(lists:member(TypeSpec, TypeVariants))
    end, s124_matrix_shapes()).

s124_assert_double_commands(UniformMatrix) ->
    GlCommands = maps:get(gl_commands, UniformMatrix),
    Variants = maps:get(variants, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        Command = s124_matrix_command(M, N),
        Variant = {{gl_matrix, M, N, gl_double}, element},
        ?assert(lists:member({Command, {gl_matrix, M, N, gl_double}, element}, GlCommands)),
        ?assert(lists:member(Variant, Variants))
    end, s124_matrix_shapes()).

s124_assert_double_clauses(UniformMatrix) ->
    Clauses = maps:get(function_clauses, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s124_matrix_command(M, N),
        Clause = s124_find_clause(RawFunction, Clauses),
        ?assertEqual("Value", maps:get(guard_var, Clause)),
        ?assertEqual(
            [
                {is_tuple, var},
                {tuple_size, var, M},
                {is_tuple, {element, 1, var}},
                {tuple_size, {element, 1, var}, N}
            ],
            maps:get(guards, Clause)
        ),
        ?assertEqual(
            [
                {"d", ignore},
                {"Location", do_nothing},
                {"Count", {gl_sizei_constant, 1}},
                {"Transpose", {gl_bool_constant, false}},
                {"Value", {gl_matrix_to_list, M, N}}
            ],
            maps:get(params, Clause)
        )
    end, s124_matrix_shapes()).

s124_assert_double_nifs(UniformMatrix) ->
    NifFunctions = maps:get(nif_functions, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s124_matrix_command(M, N),
        NifData = maps:get(RawFunction, NifFunctions),
        ?assertEqual(4, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Location", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
                {"Count", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
                {"Transpose", boolean_to_glbool},
                {"Value", {list_gl_type, {"GLdouble", "double", "enif_get_double", "enif_make_double"}}}
            ],
            maps:get(params, NifData)
        ),
        ?assertEqual(void, maps:get(return, NifData))
    end, s124_matrix_shapes()).

s124_assert_double_absent(UniformMatrix, Functions) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, uniform_matrix_value, []}}
    ] = maps:get(specs_params, UniformMatrix),
    ?assertNot(lists:member(d, TypeAtoms)),

    {uniform_matrix_value, {set, TypeVariants}} = maps:get(extra_type, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        ?assertNot(lists:member({undefined, s124_matrix_type(M, N), [{gl, double, []}]}, TypeVariants)),
        ?assertNot(generator_test_support:has_gl_command(s124_matrix_command(M, N), Functions))
    end, s124_matrix_shapes()).

s124_assert_program_matrix_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s124_assert_emitted_surface(Target, Support) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard124-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case Support of
            supported ->
                s124_assert_supported_emission(Erl, C);
            unsupported ->
                s124_assert_unsupported_emission(Erl, C)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s124_assert_supported_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s124_assert_contains(Erl, Needle) end, [
        <<"-export([uniform_matrix/3]).">>,
        <<"Type :: d | f">>,
        <<"matrix2(gl:double())">>,
        <<"matrix2x3(gl:double())">>,
        <<"matrix4x3(gl:double())">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix2dv_raw(Location, 1, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix4x3dv_raw(Location, 1, false, NewValue))">>
    ]),
    lists:foreach(fun(Needle) -> s124_assert_contains(C, Needle) end, [
        <<"glUniformMatrix2dv(arg_0, arg_1, arg_2, arg_3_array);">>,
        <<"glUniformMatrix4x3dv(arg_0, arg_1, arg_2, arg_3_array);">>,
        <<"{\"glUniformMatrix2dv_raw\", 4, nif_glUniformMatrix2dv, 0}">>,
        <<"{\"glUniformMatrix4x3dv_raw\", 4, nif_glUniformMatrix4x3dv, 0}">>
    ]),
    s124_assert_not_contains(Erl, <<"-export([uniform_matrix_2dv/2]).">>),
    s124_assert_not_contains(Erl, <<"-export([program_uniform_matrix_2dv/5]).">>).

s124_assert_unsupported_emission(Erl, C) ->
    lists:foreach(fun(Needle) ->
        s124_assert_not_contains(Erl, Needle),
        s124_assert_not_contains(C, Needle)
    end, [
        <<"glUniformMatrix2dv_raw">>,
        <<"glUniformMatrix4x3dv_raw">>,
        <<"glUniformMatrix2dv(arg_0, arg_1, arg_2, arg_3_array);">>,
        <<"glUniformMatrix4x3dv(arg_0, arg_1, arg_2, arg_3_array);">>
    ]),
    s124_assert_not_contains(Erl, <<"-spec uniform_matrix(\n    Type :: d | f">>),
    s124_assert_not_contains(Erl, <<"matrix2(gl:double())">>).

s124_find_clause(RawFunction, Clauses) ->
    [Clause] = [
        Clause0
     || Clause0 <- Clauses,
        maps:get(raw_function, Clause0) =:= RawFunction,
        s124_has_matrix_element_transform(Clause0)
    ],
    Clause.

s124_has_matrix_element_transform(Clause) ->
    lists:any(fun
        ({"Value", {gl_matrix_to_list, _M, _N}}) -> true;
        (_) -> false
    end, maps:get(params, Clause)).

s124_supports_double_uniform_matrix({gl, {4, _}}) -> true;
s124_supports_double_uniform_matrix(_) -> false.

s124_matrix_shapes() ->
    [{2, 2}, {3, 3}, {4, 4}, {2, 3}, {3, 2}, {2, 4}, {4, 2}, {3, 4}, {4, 3}].

s124_matrix_command(N, N) ->
    "glUniformMatrix" ++ integer_to_list(N) ++ "dv";
s124_matrix_command(M, N) ->
    "glUniformMatrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N) ++ "dv".

s124_matrix_type(N, N) ->
    list_to_atom("matrix" ++ integer_to_list(N));
s124_matrix_type(M, N) ->
    list_to_atom("matrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N)).

s124_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s124_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 125.
s125_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s125_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s125_emitter_program_uniform_matrix_test_() ->
    [
        {"gl 4.6", fun() -> s125_assert_emitted_surface({gl, {4, 6}}, full) end},
        {"gles 3.2", fun() -> s125_assert_emitted_surface({gles, {3, 2}}, float_only) end},
        {"gl 3.3", fun() -> s125_assert_emitted_surface({gl, {3, 3}}, unsupported) end}
    ].

s125_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    s125_assert_direct_wrappers_absent(Functions),
    case s125_supports_program_uniform_matrix(Target) of
        true ->
            ?assert(maps:is_key({"program_uniform_matrix", 4}, Functions)),
            ProgramUniformMatrix = maps:get({"program_uniform_matrix", 4}, Functions),
            s125_assert_specs(Target, ProgramUniformMatrix),
            s125_assert_matrix_family(ProgramUniformMatrix, f, gl_float),
            case s125_supports_double_program_uniform_matrix(Target) of
                true -> s125_assert_matrix_family(ProgramUniformMatrix, d, gl_double);
                false -> s125_assert_matrix_family_absent(Functions, ProgramUniformMatrix, d)
            end,
            s125_assert_deferred_neighbors_absent(Functions);
        false ->
            ?assertNot(maps:is_key({"program_uniform_matrix", 4}, Functions)),
            s125_assert_no_program_matrix_commands(Functions)
    end.

s125_supports_program_uniform_matrix({gl, {4, 1}}) -> true;
s125_supports_program_uniform_matrix({gl, {4, 6}}) -> true;
s125_supports_program_uniform_matrix({gles, {3, 1}}) -> true;
s125_supports_program_uniform_matrix({gles, {3, 2}}) -> true;
s125_supports_program_uniform_matrix(_) -> false.

s125_supports_double_program_uniform_matrix({gl, {4, _}}) -> true;
s125_supports_double_program_uniform_matrix(_) -> false.

s125_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun(Name) ->
        lists:foreach(fun(Arity) ->
            ?assertNot(maps:is_key({Name, Arity}, Functions))
        end, [2, 3, 4, 5])
    end, [
        "program_uniform_matrix_2fv",
        "program_uniform_matrix_3fv",
        "program_uniform_matrix_4fv",
        "program_uniform_matrix_2x3fv",
        "program_uniform_matrix_3x2fv",
        "program_uniform_matrix_2x4fv",
        "program_uniform_matrix_4x2fv",
        "program_uniform_matrix_3x4fv",
        "program_uniform_matrix_4x3fv",
        "program_uniform_matrix_2dv",
        "program_uniform_matrix_3dv",
        "program_uniform_matrix_4dv",
        "program_uniform_matrix_2x3dv",
        "program_uniform_matrix_3x2dv",
        "program_uniform_matrix_2x4dv",
        "program_uniform_matrix_4x2dv",
        "program_uniform_matrix_3x4dv",
        "program_uniform_matrix_4x3dv"
    ]).

s125_assert_specs(Target, ProgramUniformMatrix) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, program_uniform_matrix_value, []}}
    ] = maps:get(specs_params, ProgramUniformMatrix),
    ?assert(lists:member(f, TypeAtoms)),
    ?assertEqual(s125_supports_double_program_uniform_matrix(Target), lists:member(d, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, ProgramUniformMatrix)),
    ?assertEqual(4, maps:get(function_arity, ProgramUniformMatrix)),

    {program_uniform_matrix_value, {set, TypeVariants}} = maps:get(extra_type, ProgramUniformMatrix),
    lists:foreach(fun({M, N}) ->
        ?assert(lists:member({undefined, s125_matrix_type(M, N), [{gl, float, []}]}, TypeVariants))
    end, s125_matrix_shapes()),
    case s125_supports_double_program_uniform_matrix(Target) of
        true ->
            lists:foreach(fun({M, N}) ->
                ?assert(lists:member({undefined, s125_matrix_type(M, N), [{gl, double, []}]}, TypeVariants))
            end, s125_matrix_shapes());
        false ->
            lists:foreach(fun({M, N}) ->
                ?assertNot(lists:member({undefined, s125_matrix_type(M, N), [{gl, double, []}]}, TypeVariants))
            end, s125_matrix_shapes())
    end.

s125_assert_matrix_family(ProgramUniformMatrix, TypeAtom, GlType) ->
    s125_assert_commands(ProgramUniformMatrix, TypeAtom, GlType),
    s125_assert_clauses(ProgramUniformMatrix, TypeAtom),
    s125_assert_nifs(ProgramUniformMatrix, TypeAtom, GlType).

s125_assert_matrix_family_absent(Functions, ProgramUniformMatrix, TypeAtom) ->
    [{"Type", {set, TypeAtoms}} | _] = maps:get(specs_params, ProgramUniformMatrix),
    ?assertNot(lists:member(TypeAtom, TypeAtoms)),
    lists:foreach(fun({M, N}) ->
        ?assertNot(generator_test_support:has_gl_command(s125_matrix_command(TypeAtom, M, N), Functions))
    end, s125_matrix_shapes()).

s125_assert_commands(ProgramUniformMatrix, TypeAtom, GlType) ->
    GlCommands = maps:get(gl_commands, ProgramUniformMatrix),
    Variants = maps:get(variants, ProgramUniformMatrix),
    lists:foreach(fun({M, N}) ->
        Command = s125_matrix_command(TypeAtom, M, N),
        Variant = {{gl_matrix, M, N, GlType}, element},
        ?assert(lists:member({Command, {gl_matrix, M, N, GlType}, element}, GlCommands)),
        ?assert(lists:member(Variant, Variants))
    end, s125_matrix_shapes()).

s125_assert_clauses(ProgramUniformMatrix, TypeAtom) ->
    Clauses = maps:get(function_clauses, ProgramUniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s125_matrix_command(TypeAtom, M, N),
        Clause = s125_find_clause(RawFunction, Clauses),
        ?assertEqual("Value", maps:get(guard_var, Clause)),
        ?assertEqual(
            [
                {is_tuple, var},
                {tuple_size, var, M},
                {is_tuple, {element, 1, var}},
                {tuple_size, {element, 1, var}, N}
            ],
            maps:get(guards, Clause)
        ),
        ?assertEqual(
            [
                {atom_to_list(TypeAtom), ignore},
                {"Program", do_nothing},
                {"Location", do_nothing},
                {"Count", {gl_sizei_constant, 1}},
                {"Transpose", {gl_bool_constant, false}},
                {"Value", {gl_matrix_to_list, M, N}}
            ],
            maps:get(params, Clause)
        )
    end, s125_matrix_shapes()).

s125_assert_nifs(ProgramUniformMatrix, TypeAtom, GlType) ->
    NifFunctions = maps:get(nif_functions, ProgramUniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s125_matrix_command(TypeAtom, M, N),
        NifData = maps:get(RawFunction, NifFunctions),
        ?assertEqual(5, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Program", {gl_type, s125_convert_spec(gl_uint)}},
                {"Location", {gl_type, s125_convert_spec(gl_int)}},
                {"Count", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
                {"Transpose", boolean_to_glbool},
                {"Value", {list_gl_type, s125_convert_spec(GlType)}}
            ],
            maps:get(params, NifData)
        ),
        ?assertEqual(void, maps:get(return, NifData))
    end, s125_matrix_shapes()).

s125_assert_deferred_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s125_assert_no_program_matrix_commands(Functions) ->
    lists:foreach(fun({TypeAtom, _GlType}) ->
        lists:foreach(fun({M, N}) ->
            ?assertNot(generator_test_support:has_gl_command(s125_matrix_command(TypeAtom, M, N), Functions))
        end, s125_matrix_shapes())
    end, [{f, gl_float}, {d, gl_double}]).

s125_assert_emitted_surface(Target, Support) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard125-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case Support of
            full -> s125_assert_full_emission(Erl, C);
            float_only -> s125_assert_float_only_emission(Erl, C);
            unsupported -> s125_assert_unsupported_emission(Erl, C)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s125_assert_full_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s125_assert_contains(Erl, Needle) end, [
        <<"-export([program_uniform_matrix/4]).">>,
        <<"Type :: d | f">>,
        <<"matrix2(gl:double())">>,
        <<"matrix4x3(gl:double())">>,
        <<"?CALL_RAW_FUNC(glProgramUniformMatrix2fv_raw(Program, Location, 1, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glProgramUniformMatrix4x3dv_raw(Program, Location, 1, false, NewValue))">>
    ]),
    lists:foreach(fun(Needle) -> s125_assert_contains(C, Needle) end, [
        <<"glProgramUniformMatrix2fv(arg_0, arg_1, arg_2, arg_3, arg_4_array);">>,
        <<"glProgramUniformMatrix4x3dv(arg_0, arg_1, arg_2, arg_3, arg_4_array);">>,
        <<"{\"glProgramUniformMatrix2fv_raw\", 5, nif_glProgramUniformMatrix2fv, 0}">>,
        <<"{\"glProgramUniformMatrix4x3dv_raw\", 5, nif_glProgramUniformMatrix4x3dv, 0}">>
    ]),
    s125_assert_not_contains(Erl, <<"-export([program_uniform_matrix_2fv/5]).">>).

s125_assert_float_only_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s125_assert_contains(Erl, Needle) end, [
        <<"-export([program_uniform_matrix/4]).">>,
        <<"program_uniform_matrix(f, Program, Location, Value) when">>,
        <<"matrix4x3(gl:float())">>,
        <<"?CALL_RAW_FUNC(glProgramUniformMatrix4x3fv_raw(Program, Location, 1, false, NewValue))">>
    ]),
    s125_assert_contains(C, <<"glProgramUniformMatrix4x3fv(arg_0, arg_1, arg_2, arg_3, arg_4_array);">>),
    s125_assert_not_contains(Erl, <<"program_uniform_matrix(d, Program, Location, Value) when">>),
    s125_assert_not_contains(Erl, <<"matrix2(gl:double())">>),
    s125_assert_not_contains(C, <<"glProgramUniformMatrix2dv(">>).

s125_assert_unsupported_emission(Erl, C) ->
    lists:foreach(fun(Needle) ->
        s125_assert_not_contains(Erl, Needle),
        s125_assert_not_contains(C, Needle)
    end, [
        <<"-export([program_uniform_matrix/4]).">>,
        <<"glProgramUniformMatrix2fv_raw">>,
        <<"glProgramUniformMatrix4x3fv_raw">>,
        <<"glProgramUniformMatrix2dv_raw">>
    ]).

s125_find_clause(RawFunction, Clauses) ->
    [Clause] = [
        Clause0
     || Clause0 <- Clauses,
        maps:get(raw_function, Clause0) =:= RawFunction,
        s125_has_matrix_element_transform(Clause0)
    ],
    Clause.

s125_has_matrix_element_transform(Clause) ->
    lists:any(fun
        ({"Value", {gl_matrix_to_list, _M, _N}}) -> true;
        (_) -> false
    end, maps:get(params, Clause)).

s125_matrix_shapes() ->
    [{2, 2}, {3, 3}, {4, 4}, {2, 3}, {3, 2}, {2, 4}, {4, 2}, {3, 4}, {4, 3}].

s125_matrix_command(TypeAtom, N, N) ->
    "glProgramUniformMatrix" ++ integer_to_list(N) ++ atom_to_list(TypeAtom) ++ "v";
s125_matrix_command(TypeAtom, M, N) ->
    "glProgramUniformMatrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N) ++ atom_to_list(TypeAtom) ++ "v".

s125_matrix_type(N, N) ->
    list_to_atom("matrix" ++ integer_to_list(N));
s125_matrix_type(M, N) ->
    list_to_atom("matrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N)).

s125_convert_spec(gl_float) -> {"GLfloat", "double", "enif_get_double", "enif_make_double"};
s125_convert_spec(gl_int) -> {"GLint", "int", "enif_get_int", "enif_make_int"};
s125_convert_spec(gl_uint) -> {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"};
s125_convert_spec(gl_double) -> {"GLdouble", "double", "enif_get_double", "enif_make_double"}.

s125_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s125_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
