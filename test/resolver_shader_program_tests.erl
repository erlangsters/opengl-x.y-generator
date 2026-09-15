-module(resolver_shader_program_tests).
-include_lib("eunit/include/eunit.hrl").

%% Shader, program, reflection, debug, and pipeline resolver contracts.

%% Historical shard 27.
s027_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s027_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s027_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"hint", 2}, Functions)),

    ExpectedTarget = s027_expected_hint_target(Target),
    s027_assert_hint_target_enum(ExpectedTarget, BindingData),
    s027_assert_hint_mode_enum(BindingData),
    s027_assert_hint_path(ExpectedTarget, maps:get({"hint", 2}, Functions)).

s027_expected_hint_target({gl, _}) ->
    "fragment_shader_derivative_hint";
s027_expected_hint_target({gles, {2, 0}}) ->
    "generate_mipmap_hint";
s027_expected_hint_target({gles, {3, 0}}) ->
    "generate_mipmap_hint";
s027_expected_hint_target({gles, _}) ->
    "fragment_shader_derivative_hint".

s027_assert_hint_target_enum(ExpectedTarget, BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("hint_target", EnumTypes)),
    HintTargets = maps:get("hint_target", EnumTypes),
    ?assert(lists:member(ExpectedTarget, HintTargets)).

s027_assert_hint_mode_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("hint_mode", EnumTypes)),
    HintModes = maps:get("hint_mode", EnumTypes),
    ?assert(lists:member("dont_care", HintModes)),
    ?assert(lists:member("fastest", HintModes)),
    ?assert(lists:member("nicest", HintModes)).

s027_assert_hint_path(ExpectedTarget, FunctionData) ->
    ?assertEqual("glHint", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "HintTarget"}},
            {in, "Mode", {gl_enum, "HintMode"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Target", {undefined, hint_target, []}},
            {"Mode", {undefined, hint_mode, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"Mode", {gl_enum_to_uint, ModeTransformMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember(ExpectedTarget, 1, TargetTransformMap)),
    ?assert(lists:keymember("dont_care", 1, ModeTransformMap)),
    ?assert(lists:keymember("fastest", 1, ModeTransformMap)),
    ?assert(lists:keymember("nicest", 1, ModeTransformMap)),
    ?assertEqual("glHint", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glHint", NifFunctions)),
    NifData = maps:get("glHint", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s027_enum_nif_data()},
            {"Mode", s027_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s027_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 34.
s034_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s034_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s034_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"create_shader", 1}, Functions)),
    ?assert(maps:is_key({"is_shader", 1}, Functions)),
    ?assert(maps:is_key({"delete_shader", 1}, Functions)),
    ?assertNot(maps:is_key({"create_shader", 0}, Functions)),
    ?assertNot(maps:is_key({"delete_shader", 2}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s034_assert_enum_types(BindingData),
    s034_assert_create_shader(maps:get({"create_shader", 1}, Functions)),
    s034_assert_is_shader(maps:get({"is_shader", 1}, Functions)),
    s034_assert_delete_shader(maps:get({"delete_shader", 1}, Functions)).

s034_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("shader_type", EnumTypes)),
    ShaderTypes = maps:get("shader_type", EnumTypes),
    ?assert(lists:member("vertex_shader", ShaderTypes)),
    ?assert(lists:member("fragment_shader", ShaderTypes)).

s034_assert_create_shader(FunctionData) ->
    ?assertEqual("glCreateShader", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Type", {gl_enum, "ShaderType"}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Type", {undefined, shader_type, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"Shader", {undefined, shader, []}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Type", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(lists:keymember("vertex_shader", 1, TransformMap)),
    ?assertEqual("glCreateShader", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glCreateShader", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Type", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(gluint_to_integer, maps:get(return, NifData)).

s034_assert_is_shader(FunctionData) ->
    ?assertEqual("glIsShader", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Shader", {gl_object, shader}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Shader", {undefined, shader, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"IsShader", {gl, boolean, []}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Shader", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glIsShader", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glIsShader", NifFunctions),
    ?assertEqual(
        [{"Shader", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s034_assert_delete_shader(FunctionData) ->
    ?assertEqual("glDeleteShader", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Shader", {gl_object, shader}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Shader", {undefined, shader, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Shader", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glDeleteShader", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDeleteShader", NifFunctions),
    ?assertEqual(
        [{"Shader", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 35.
s035_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s035_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s035_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"create_program", 0}, Functions)),
    ?assert(maps:is_key({"is_program", 1}, Functions)),
    ?assert(maps:is_key({"delete_program", 1}, Functions)),
    ?assertNot(maps:is_key({"create_program", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_program", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s035_assert_create_program(maps:get({"create_program", 0}, Functions)),
    s035_assert_is_program(maps:get({"is_program", 1}, Functions)),
    s035_assert_delete_program(maps:get({"delete_program", 1}, Functions)).

s035_assert_create_program(FunctionData) ->
    ?assertEqual("glCreateProgram", maps:get(gl_command, FunctionData)),
    ?assertEqual([], maps:get(params_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_params, FunctionData)),
    ?assertEqual(
        [{"Program", {undefined, program, []}}],
        maps:get(specs_return, FunctionData)
    ),
    ?assertEqual(0, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([], maps:get(params, Clause)),
    ?assertEqual("glCreateProgram", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glCreateProgram", NifFunctions),
    ?assertEqual(0, maps:get(arity, NifData)),
    ?assertEqual([], maps:get(params, NifData)),
    ?assertEqual(gluint_to_integer, maps:get(return, NifData)).

s035_assert_is_program(FunctionData) ->
    ?assertEqual("glIsProgram", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Program", {gl_object, program}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Program", {undefined, program, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"IsProgram", {gl, boolean, []}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Program", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glIsProgram", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glIsProgram", NifFunctions),
    ?assertEqual(
        [{"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s035_assert_delete_program(FunctionData) ->
    ?assertEqual("glDeleteProgram", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Program", {gl_object, program}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Program", {undefined, program, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Program", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glDeleteProgram", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDeleteProgram", NifFunctions),
    ?assertEqual(
        [{"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 36.
s036_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s036_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s036_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"attach_shader", 2}, Functions)),
    ?assert(maps:is_key({"detach_shader", 2}, Functions)),
    ?assertNot(maps:is_key({"attach_shader", 3}, Functions)),
    ?assertNot(maps:is_key({"detach_shader", 3}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s036_assert_attach_or_detach_shader(
        maps:get({"attach_shader", 2}, Functions),
        "glAttachShader"
    ),
    s036_assert_attach_or_detach_shader(
        maps:get({"detach_shader", 2}, Functions),
        "glDetachShader"
    ).

s036_assert_attach_or_detach_shader(FunctionData, RawCommand) ->
    ?assertEqual(RawCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Shader", {gl_object, shader}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Shader", {undefined, shader, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Program", do_nothing}, {"Shader", do_nothing}],
        maps:get(params, Clause)
    ),
    ?assertEqual(RawCommand, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(RawCommand, NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Shader", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 37.
s037_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s037_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s037_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"compile_shader", 1}, Functions)),
    ?assertNot(maps:is_key({"compile_shader", 0}, Functions)),
    ?assertNot(maps:is_key({"compile_shader", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s037_assert_compile_shader(maps:get({"compile_shader", 1}, Functions)).

s037_assert_compile_shader(FunctionData) ->
    ?assertEqual("glCompileShader", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Shader", {gl_object, shader}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Shader", {undefined, shader, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Shader", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glCompileShader", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glCompileShader", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Shader", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 39.
s039_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s039_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s039_emitter_link_program_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard39-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
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
            binary:match(GeneratedErl, <<"-spec link_program(Program :: program()) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glLinkProgram_raw(Program)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glLinkProgram(arg_0);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glLinkProgram_raw\", 1, nif_glLinkProgram, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s039_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"link_program", 1}, Functions)),
    ?assertNot(maps:is_key({"link_program", 0}, Functions)),
    ?assertNot(maps:is_key({"link_program", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),

    s039_assert_link_program(maps:get({"link_program", 1}, Functions)).

s039_assert_link_program(FunctionData) ->
    ?assertEqual("glLinkProgram", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Program", {gl_object, program}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Program", {undefined, program, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Program", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glLinkProgram", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glLinkProgram", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 46.
s046_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s046_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s046_emitter_validate_program_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard46-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
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
            binary:match(GeneratedErl, <<"-export([validate_program/1]).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec validate_program(Program :: program()) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glValidateProgram_raw(Program)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glValidateProgram(arg_0);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glValidateProgram_raw\", 1, nif_glValidateProgram, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s046_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"validate_program", 1}, Functions)),
    ?assertNot(maps:is_key({"validate_program", 0}, Functions)),
    ?assertNot(maps:is_key({"validate_program", 2}, Functions)),

    s046_assert_validate_program(maps:get({"validate_program", 1}, Functions)).

s046_assert_validate_program(FunctionData) ->
    ?assertEqual("glValidateProgram", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Program", {gl_object, program}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Program", {undefined, program, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Program", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glValidateProgram", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glValidateProgram", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 47.
s047_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s047_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s047_emitter_validation_status_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard47-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
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
            binary:match(GeneratedErl, <<"-export([get_program/3]).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec get_program(\n    Program :: program(),\n    ParamName :: program_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glGetProgramivValues_raw(Program, NewParamName, Count)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"validate_status -> ?GL_VALIDATE_STATUS">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glGetProgramiv(arg_0, arg_1, arg_2_values);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glGetProgramivValues_raw\", 3, nif_glGetProgramivValues, 0}">>)
        ),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"get_program_validation_status">>)),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"glGetProgramivInteger_raw">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s047_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_program", 3}, Functions)),
    ?assertNot(maps:is_key({"get_program_validation_status", 1}, Functions)),
    ?assertNot(maps:is_key({"get_program_link_status", 1}, Functions)),
    ?assertNot(maps:is_key({"get_program_validation_status", 0}, Functions)),
    ?assertNot(maps:is_key({"get_program_validation_status", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),

    s047_assert_program_query(maps:get({"get_program", 3}, Functions)).

s047_assert_program_query(FunctionData) ->
    ?assertEqual("glGetProgramiv", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ParamName", {gl_enum, ["ProgramPropertyARB", "ProgramParameterPName"], program_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ParamName", {undefined, program_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Program", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("validate_status", 1, ParamNameMap)),
    ?assertEqual("glGetProgramivValues", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGetProgramivValues", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"ParamName", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 56.
s056_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s056_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s056_emitter_transform_feedback_and_pipeline_surface_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard56-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([transform_feedback/0]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([program_pipeline/0]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([transform_feedback_target/0]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"bind_transform_feedback_target()">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([gen_transform_feedbacks/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([delete_transform_feedbacks/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([bind_transform_feedback/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_transform_feedbacks/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([transform_feedback_buffer_base/3]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([transform_feedback_buffer_range/5]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([gen_program_pipelines/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([delete_program_pipelines/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([bind_program_pipeline/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_program_pipelines/1]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([delete_transform_feedbacks/2]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([delete_program_pipelines/2]).">>)),

        ?assertMatch({_, _}, binary:match(C, <<"glBindTransformFeedback(arg_0, arg_1);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glDeleteTransformFeedbacks(arg_0, (void*)arg_1.data);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glCreateTransformFeedbacks(arg_0_n, arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glTransformFeedbackBufferBase(arg_0, arg_1, arg_2);">>)),
        ?assertMatch(
            {_, _},
            binary:match(C, <<"glTransformFeedbackBufferRange(arg_0, arg_1, arg_2, arg_3, arg_4);">>)
        ),
        ?assertMatch({_, _}, binary:match(C, <<"glBindProgramPipeline(arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glDeleteProgramPipelines(arg_0, (void*)arg_1.data);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glCreateProgramPipelines(arg_0_n, arg_0);">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s056_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s056_assert_presence(Target, Functions),
    s056_assert_deferred_neighbors_absent(Functions),
    s056_assert_present_paths(Target, BindingData, Functions).

s056_assert_presence(Target, Functions) ->
    Present = s056_present_functions(Target),
    Absent = s056_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"delete_transform_feedbacks", 2}, Functions)),
    ?assertNot(maps:is_key({"delete_program_pipelines", 2}, Functions)).

s056_present_functions({gl, {3, 3}}) ->
    [];
s056_present_functions({gl, {4, 1}}) ->
    s056_transform_feedback_lifecycle() ++ s056_program_pipeline_lifecycle();
s056_present_functions({gl, {4, 6}}) ->
    s056_all_functions();
s056_present_functions({gles, {2, 0}}) ->
    [];
s056_present_functions({gles, {3, 0}}) ->
    s056_transform_feedback_lifecycle();
s056_present_functions({gles, {3, 1}}) ->
    s056_transform_feedback_lifecycle() ++ s056_program_pipeline_lifecycle();
s056_present_functions({gles, {3, 2}}) ->
    s056_transform_feedback_lifecycle() ++ s056_program_pipeline_lifecycle().

s056_all_functions() ->
    s056_transform_feedback_lifecycle() ++
        s056_transform_feedback_dsa() ++
        s056_program_pipeline_lifecycle() ++
        s056_program_pipeline_dsa().

s056_transform_feedback_lifecycle() ->
    [
        {"gen_transform_feedbacks", 1},
        {"delete_transform_feedbacks", 1},
        {"is_transform_feedback", 1},
        {"bind_transform_feedback", 2}
    ].

s056_transform_feedback_dsa() ->
    [
        {"create_transform_feedbacks", 1},
        {"transform_feedback_buffer_base", 3},
        {"transform_feedback_buffer_range", 5}
    ].

s056_program_pipeline_lifecycle() ->
    [
        {"gen_program_pipelines", 1},
        {"delete_program_pipelines", 1},
        {"is_program_pipeline", 1},
        {"bind_program_pipeline", 1}
    ].

s056_program_pipeline_dsa() ->
    [{"create_program_pipelines", 1}].

s056_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s056_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s056_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s056_present_functions(Target)
    ).

s056_assert_path({"gen_transform_feedbacks", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_gen_object(FunctionData, "glGenTransformFeedbacks", "Feedbacks", transform_feedback);
s056_assert_path({"delete_transform_feedbacks", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_delete_object_list(FunctionData, "glDeleteTransformFeedbacks", "Feedbacks", transform_feedback);
s056_assert_path({"is_transform_feedback", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_is_object(
        FunctionData,
        "glIsTransformFeedback",
        "Feedback",
        "IsFeedback",
        transform_feedback
    );
s056_assert_path({"bind_transform_feedback", 2}, _Target, BindingData, FunctionData) ->
    s056_assert_enum_contains(BindingData, "transform_feedback_target", "transform_feedback"),
    s056_assert_direct(
        FunctionData,
        "glBindTransformFeedback",
        [
            {in, "Target", {gl_enum, "BindTransformFeedbackTarget", transform_feedback_target}},
            {in, "Feedback", {gl_object, transform_feedback}}
        ],
        [
            {"Target", {undefined, transform_feedback_target, []}},
            {"Feedback", {undefined, transform_feedback, []}}
        ],
        [{"Target", {gl_enum_to_uint, ["transform_feedback"]}}, {"Feedback", do_nothing}],
        [{"Target", s056_enum_nif_data()}, {"Feedback", s056_uint_nif_data()}]
    );
s056_assert_path({"create_transform_feedbacks", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_gen_object(FunctionData, "glCreateTransformFeedbacks", "Feedbacks", transform_feedback);
s056_assert_path({"transform_feedback_buffer_base", 3}, _Target, _BindingData, FunctionData) ->
    s056_assert_direct(
        FunctionData,
        "glTransformFeedbackBufferBase",
        [
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "Index", gl_uint},
            {in, "Buffer", {gl_object, buffer}}
        ],
        [
            {"Feedback", {undefined, transform_feedback, []}},
            {"Index", {gl, uint, []}},
            {"Buffer", {undefined, buffer, []}}
        ],
        [{"Feedback", do_nothing}, {"Index", do_nothing}, {"Buffer", do_nothing}],
        [{"Feedback", s056_uint_nif_data()}, {"Index", s056_uint_nif_data()}, {"Buffer", s056_uint_nif_data()}]
    );
s056_assert_path({"transform_feedback_buffer_range", 5}, _Target, _BindingData, FunctionData) ->
    s056_assert_direct(
        FunctionData,
        "glTransformFeedbackBufferRange",
        [
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "Index", gl_uint},
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Size", gl_sizeiptr}
        ],
        [
            {"Feedback", {undefined, transform_feedback, []}},
            {"Index", {gl, uint, []}},
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Size", {gl, sizeiptr, []}}
        ],
        [
            {"Feedback", do_nothing},
            {"Index", do_nothing},
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"Feedback", s056_uint_nif_data()},
            {"Index", s056_uint_nif_data()},
            {"Buffer", s056_uint_nif_data()},
            {"Offset", s056_intptr_nif_data()},
            {"Size", s056_sizeiptr_nif_data()}
        ]
    );
s056_assert_path({"gen_program_pipelines", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_gen_object(FunctionData, "glGenProgramPipelines", "Pipelines", program_pipeline);
s056_assert_path({"delete_program_pipelines", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_delete_object_list(FunctionData, "glDeleteProgramPipelines", "Pipelines", program_pipeline);
s056_assert_path({"is_program_pipeline", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_is_object(
        FunctionData,
        "glIsProgramPipeline",
        "Pipeline",
        "IsPipeline",
        program_pipeline
    );
s056_assert_path({"bind_program_pipeline", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_direct(
        FunctionData,
        "glBindProgramPipeline",
        [{in, "Pipeline", {gl_object, program_pipeline}}],
        [{"Pipeline", {undefined, program_pipeline, []}}],
        [{"Pipeline", do_nothing}],
        [{"Pipeline", s056_uint_nif_data()}]
    );
s056_assert_path({"create_program_pipelines", 1}, _Target, _BindingData, FunctionData) ->
    s056_assert_gen_object(FunctionData, "glCreateProgramPipelines", "Pipelines", program_pipeline).

s056_assert_gen_object(FunctionData, GlCommand, ParamName, ObjectType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{out, ParamName, {{list, 2, "N"}, {gl_object, ObjectType}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual([{"N", {undefined, pos_integer, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual([{ParamName, {list, {undefined, ObjectType, []}}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assert(
        lists:member(
            {ParamName, {return_list_terms_alloc, "GLuint", "enif_make_uint"}},
            maps:get(params, NifData)
        )
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s056_assert_delete_object_list(FunctionData, GlCommand, ParamName, ObjectType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, ParamName, {counted_list, "N", {gl_object, ObjectType}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual([{ParamName, {list, {undefined, ObjectType, []}}}], maps:get(specs_params, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{ParamName, {counted_list_gl_objects_to_binary, "N"}}], maps:get(params, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"N", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {ParamName, binary_to_glbinary}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s056_assert_is_object(FunctionData, GlCommand, ParamName, ReturnName, ObjectType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual([{in, ParamName, {gl_object, ObjectType}}], maps:get(params_specs, FunctionData)),
    ?assertEqual([{ParamName, {undefined, ObjectType, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual([{ReturnName, {gl, boolean, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{ParamName, do_nothing}], maps:get(params, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s056_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s056_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s056_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s056_assert_clause_param/1, lists:zip(Expected, Actual)).

s056_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s056_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s056_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s056_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s056_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s056_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s056_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 61.
s061_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s061_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s061_emitter_pipeline_control_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s061_assert_emitted_surface({gl, {4, 6}}, "OpenGL 4.6", s061_full_exports(), s061_full_c_calls())
        end},
        {"gles 3.2", fun() ->
            s061_assert_emitted_surface({gles, {3, 2}}, "OpenGL ES 3.2", s061_full_exports(), s061_full_c_calls())
        end},
        {"gles 3.0", fun() ->
            s061_assert_emitted_surface(
                {gles, {3, 0}},
                "OpenGL ES 3.0",
                [<<"-export([program_parameter/3]).">>],
                [<<"glProgramParameteri(arg_0, arg_1, arg_2);">>]
            )
        end}
    ].

s061_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s061_assert_presence(Target, Functions),
    s061_assert_deferred_neighbors_absent(Functions),
    s061_assert_present_paths(Target, BindingData, Functions).

s061_assert_presence(Target, Functions) ->
    Present = s061_present_functions(Target),
    Absent = s061_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s061_present_functions({gl, {3, 3}}) ->
    [];
s061_present_functions({gl, {4, 1}}) ->
    s061_all_functions();
s061_present_functions({gl, {4, 6}}) ->
    s061_all_functions();
s061_present_functions({gles, {2, 0}}) ->
    [];
s061_present_functions({gles, {3, 0}}) ->
    [{"program_parameter", 3}];
s061_present_functions({gles, {3, 1}}) ->
    s061_all_functions();
s061_present_functions({gles, {3, 2}}) ->
    s061_all_functions().

s061_all_functions() ->
    [
        {"program_parameter", 3},
        {"use_program_stages", 3},
        {"active_shader_program", 2},
        {"validate_program_pipeline", 1}
    ].

s061_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s061_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s061_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s061_present_functions(Target)
    ).

s061_assert_path({"program_parameter", 3}, Target, BindingData, FunctionData) ->
    RequiredParamName =
        case Target of
            {gles, {3, 0}} -> "program_binary_retrievable_hint";
            _ -> "program_separable"
        end,
    s061_assert_enum_contains(BindingData, "program_parameter_p_name", RequiredParamName),
    s061_assert_direct(
        FunctionData,
        "glProgramParameteri",
        [
            {in, "Program", {gl_object, program}},
            {in, "ParamName", {gl_enum, "ProgramParameterPName"}},
            {in, "Value", gl_int}
        ],
        [
            {"Program", {undefined, program, []}},
            {"ParamName", {undefined, program_parameter_p_name, []}},
            {"Value", {gl, int, []}}
        ],
        [
            {"Program", do_nothing},
            {"ParamName", {gl_enum_to_uint, [RequiredParamName]}},
            {"Value", do_nothing}
        ],
        [
            {"Program", s061_uint_nif_data()},
            {"ParamName", s061_enum_nif_data()},
            {"Value", s061_int_nif_data()}
        ]
    );
s061_assert_path({"use_program_stages", 3}, _Target, BindingData, FunctionData) ->
    s061_assert_bitfield_contains(BindingData, "use_program_stage_mask", "vertex_shader_bit"),
    s061_assert_bitfield_contains(BindingData, "use_program_stage_mask", "fragment_shader_bit"),
    s061_assert_direct(
        FunctionData,
        "glUseProgramStages",
        [
            {in, "Pipeline", {gl_object, program_pipeline}},
            {in, "Stages", {gl_bitfield, "UseProgramStageMask"}},
            {in, "Program", {gl_object, program}}
        ],
        [
            {"Pipeline", {undefined, program_pipeline, []}},
            {"Stages", {undefined, use_program_stage_mask, []}},
            {"Program", {undefined, program, []}}
        ],
        [
            {"Pipeline", do_nothing},
            {"Stages", {gl_bitfield_to_uint, ["vertex_shader_bit", "fragment_shader_bit"]}},
            {"Program", do_nothing}
        ],
        [
            {"Pipeline", s061_uint_nif_data()},
            {"Stages", s061_bitfield_nif_data()},
            {"Program", s061_uint_nif_data()}
        ]
    );
s061_assert_path({"active_shader_program", 2}, _Target, _BindingData, FunctionData) ->
    s061_assert_direct(
        FunctionData,
        "glActiveShaderProgram",
        [
            {in, "Pipeline", {gl_object, program_pipeline}},
            {in, "Program", {gl_object, program}}
        ],
        [
            {"Pipeline", {undefined, program_pipeline, []}},
            {"Program", {undefined, program, []}}
        ],
        [{"Pipeline", do_nothing}, {"Program", do_nothing}],
        [{"Pipeline", s061_uint_nif_data()}, {"Program", s061_uint_nif_data()}]
    );
s061_assert_path({"validate_program_pipeline", 1}, _Target, _BindingData, FunctionData) ->
    s061_assert_direct(
        FunctionData,
        "glValidateProgramPipeline",
        [{in, "Pipeline", {gl_object, program_pipeline}}],
        [{"Pipeline", {undefined, program_pipeline, []}}],
        [{"Pipeline", do_nothing}],
        [{"Pipeline", s061_uint_nif_data()}]
    ).

s061_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s061_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s061_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s061_assert_clause_param/1, lists:zip(Expected, Actual)).

s061_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s061_assert_clause_param({{Name, {gl_bitfield_to_uint, RequiredAtoms}}, {Name, {gl_bitfield_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s061_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s061_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s061_assert_bitfield_contains(BindingData, BitfieldType, Atom) ->
    BitfieldTypes = maps:get(bitfield_types, BindingData),
    ?assert(lists:member(Atom, maps:get(BitfieldType, BitfieldTypes))).

s061_assert_emitted_surface(Target, ApiName, ExpectedExports, ExpectedCalls) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard61-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [?assertMatch({_, _}, binary:match(Erl, Export)) || Export <- ExpectedExports],
        [?assertMatch({_, _}, binary:match(C, Call)) || Call <- ExpectedCalls],
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s061_deferred_exports()],
        [?assertEqual(nomatch, binary:match(C, Call)) || Call <- s061_deferred_c_calls()]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s061_full_exports() ->
    [
        <<"-export([program_parameter/3]).">>,
        <<"-export([use_program_stages/3]).">>,
        <<"-export([active_shader_program/2]).">>,
        <<"-export([validate_program_pipeline/1]).">>
    ].

s061_full_c_calls() ->
    [
        <<"glProgramParameteri(arg_0, arg_1, arg_2);">>,
        <<"glUseProgramStages(arg_0, arg_1, arg_2);">>,
        <<"glActiveShaderProgram(arg_0, arg_1);">>,
        <<"glValidateProgramPipeline(arg_0);">>
    ].

s061_deferred_exports() ->
    [
        <<"-export([get_program_resource/4]).">>
    ].

s061_deferred_c_calls() ->
    [].

s061_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s061_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s061_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s061_bitfield_nif_data() ->
    {gl_type, {"GLbitfield", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 62.
s062_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s062_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s062_emitter_direct_control_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s062_assert_emitted_surface({gl, {4, 6}}, "OpenGL 4.6", s062_gl_46_exports(), s062_gl_46_c_calls(), [])
        end},
        {"gles 3.2", fun() ->
            s062_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                s062_gles_32_exports(),
                s062_gles_32_c_calls(),
                s062_gl_only_exports()
            )
        end},
        {"gles 2.0", fun() ->
            s062_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [<<"-export([release_shader_compiler/0]).">>],
                [<<"glReleaseShaderCompiler();">>],
                [{"bind_sampler", 2}, {"vertex_attrib_divisor", 2}, {"memory_barrier", 1}]
            )
        end}
    ].

s062_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s062_assert_presence(Target, Functions),
    s062_assert_deferred_neighbors_absent(Functions),
    s062_assert_present_paths(Target, BindingData, Functions).

s062_assert_presence(Target, Functions) ->
    Present = s062_present_functions(Target),
    Absent = s062_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s062_present_functions({gl, {3, 3}}) ->
    s062_desktop_33_controls();
s062_present_functions({gl, {4, 1}}) ->
    s062_desktop_41_controls();
s062_present_functions({gl, {4, 6}}) ->
    s062_all_functions();
s062_present_functions({gles, {2, 0}}) ->
    [{"release_shader_compiler", 0}];
s062_present_functions({gles, {3, 0}}) ->
    s062_es_30_controls();
s062_present_functions({gles, {3, 1}}) ->
    s062_es_30_controls() ++ s062_memory_barriers();
s062_present_functions({gles, {3, 2}}) ->
    s062_es_30_controls() ++ s062_memory_barriers().

s062_all_functions() ->
    s062_desktop_41_controls() ++ s062_gl_46_controls() ++ s062_memory_barriers() ++ [{"texture_barrier", 0}].

s062_desktop_33_controls() ->
    [
        {"provoking_vertex", 1},
        {"bind_sampler", 2},
        {"vertex_attrib_divisor", 2}
    ].

s062_desktop_41_controls() ->
    s062_desktop_33_controls() ++
        [
            {"release_shader_compiler", 0},
            {"scissor_indexed", 5},
            {"depth_range_indexed", 3}
        ].

s062_gl_46_controls() ->
    [{"clip_control", 2}].

s062_es_30_controls() ->
    [
        {"bind_sampler", 2},
        {"vertex_attrib_divisor", 2},
        {"release_shader_compiler", 0}
    ].

s062_memory_barriers() ->
    [
        {"memory_barrier", 1},
        {"memory_barrier_by_region", 1}
    ].

s062_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glFenceSync",
        "glIsSync",
        "glDeleteSync",
        "glClientWaitSync",
        "glWaitSync",
        "glGetSynciv"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s062_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s062_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s062_present_functions(Target)
    ).

s062_assert_path({"provoking_vertex", 1}, _Target, BindingData, FunctionData) ->
    s062_assert_enum_contains(BindingData, "vertex_provoking_mode", "last_vertex_convention"),
    s062_assert_direct(
        FunctionData,
        "glProvokingVertex",
        [{in, "Mode", {gl_enum, "VertexProvokingMode"}}],
        [{"Mode", {undefined, vertex_provoking_mode, []}}],
        [{"Mode", {gl_enum_to_uint, ["last_vertex_convention"]}}],
        [{"Mode", s062_enum_nif_data()}]
    );
s062_assert_path({"bind_sampler", 2}, _Target, _BindingData, FunctionData) ->
    s062_assert_bind_sampler(FunctionData);
s062_assert_path({"vertex_attrib_divisor", 2}, _Target, _BindingData, FunctionData) ->
    s062_assert_direct(
        FunctionData,
        "glVertexAttribDivisor",
        [{in, "Index", gl_uint}, {in, "Divisor", gl_uint}],
        [{"Index", {gl, uint, []}}, {"Divisor", {gl, uint, []}}],
        [{"Index", do_nothing}, {"Divisor", do_nothing}],
        [{"Index", s062_uint_nif_data()}, {"Divisor", s062_uint_nif_data()}]
    );
s062_assert_path({"release_shader_compiler", 0}, _Target, _BindingData, FunctionData) ->
    s062_assert_direct(FunctionData, "glReleaseShaderCompiler", [], [], [], []);
s062_assert_path({"scissor_indexed", 5}, _Target, _BindingData, FunctionData) ->
    s062_assert_direct(
        FunctionData,
        "glScissorIndexed",
        [
            {in, "Index", gl_uint},
            {in, "Left", gl_int},
            {in, "Bottom", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Index", {gl, uint, []}},
            {"Left", {gl, int, []}},
            {"Bottom", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        [
            {"Index", do_nothing},
            {"Left", do_nothing},
            {"Bottom", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        [
            {"Index", s062_uint_nif_data()},
            {"Left", s062_int_nif_data()},
            {"Bottom", s062_int_nif_data()},
            {"Width", s062_sizei_nif_data()},
            {"Height", s062_sizei_nif_data()}
        ]
    );
s062_assert_path({"depth_range_indexed", 3}, _Target, _BindingData, FunctionData) ->
    s062_assert_direct(
        FunctionData,
        "glDepthRangeIndexed",
        [{in, "Index", gl_uint}, {in, "Near", gl_double}, {in, "Far", gl_double}],
        [{"Index", {gl, uint, []}}, {"Near", {gl, double, []}}, {"Far", {gl, double, []}}],
        [{"Index", do_nothing}, {"Near", do_nothing}, {"Far", do_nothing}],
        [{"Index", s062_uint_nif_data()}, {"Near", s062_double_nif_data()}, {"Far", s062_double_nif_data()}]
    );
s062_assert_path({"memory_barrier", 1}, _Target, BindingData, FunctionData) ->
    s062_assert_bitfield_contains(BindingData, "memory_barrier_mask", "buffer_update_barrier_bit"),
    s062_assert_memory_barrier(FunctionData, "glMemoryBarrier");
s062_assert_path({"clip_control", 2}, _Target, BindingData, FunctionData) ->
    s062_assert_enum_contains(BindingData, "clip_control_origin", "lower_left"),
    s062_assert_enum_contains(BindingData, "clip_control_depth", "negative_one_to_one"),
    s062_assert_direct(
        FunctionData,
        "glClipControl",
        [
            {in, "Origin", {gl_enum, "ClipControlOrigin"}},
            {in, "Depth", {gl_enum, "ClipControlDepth"}}
        ],
        [
            {"Origin", {undefined, clip_control_origin, []}},
            {"Depth", {undefined, clip_control_depth, []}}
        ],
        [
            {"Origin", {gl_enum_to_uint, ["lower_left"]}},
            {"Depth", {gl_enum_to_uint, ["negative_one_to_one"]}}
        ],
        [{"Origin", s062_enum_nif_data()}, {"Depth", s062_enum_nif_data()}]
    );
s062_assert_path({"memory_barrier_by_region", 1}, _Target, BindingData, FunctionData) ->
    s062_assert_bitfield_contains(BindingData, "memory_barrier_mask", "framebuffer_barrier_bit"),
    s062_assert_memory_barrier(FunctionData, "glMemoryBarrierByRegion");
s062_assert_path({"texture_barrier", 0}, _Target, _BindingData, FunctionData) ->
    s062_assert_direct(FunctionData, "glTextureBarrier", [], [], [], []).

s062_assert_bind_sampler(FunctionData) ->
    ?assertEqual("glBindSampler", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Unit", gl_uint},
            {in, "Sampler", {gl_object, sampler, [none]}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Unit", {gl, uint, []}},
            {"Sampler", {set, [{undefined, sampler, []}, none]}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Unit", do_nothing},
            {"Sampler", {gl_object_to_uint, [{none, 0}]}}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glBindSampler", maps:get(raw_function, Clause)),
    NifData = maps:get("glBindSampler", maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Unit", s062_uint_nif_data()},
            {"Sampler", s062_uint_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s062_assert_memory_barrier(FunctionData, GlCommand) ->
    s062_assert_direct(
        FunctionData,
        GlCommand,
        [{in, "Barriers", {gl_bitfield, "MemoryBarrierMask"}}],
        [{"Barriers", {undefined, memory_barrier_mask, []}}],
        [{"Barriers", {gl_bitfield_to_uint, ["buffer_update_barrier_bit", "framebuffer_barrier_bit"]}}],
        [{"Barriers", s062_bitfield_nif_data()}]
    ).

s062_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s062_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s062_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s062_assert_clause_param/1, lists:zip(Expected, Actual)).

s062_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s062_assert_clause_param({{Name, {gl_bitfield_to_uint, RequiredAtoms}}, {Name, {gl_bitfield_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s062_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s062_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s062_assert_bitfield_contains(BindingData, BitfieldType, Atom) ->
    BitfieldTypes = maps:get(bitfield_types, BindingData),
    ?assert(lists:member(Atom, maps:get(BitfieldType, BitfieldTypes))).

s062_assert_emitted_surface(Target, ApiName, ExpectedExports, ExpectedCalls, ExtraAbsentFunctions) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard62-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [?assertMatch({_, _}, binary:match(Erl, Export)) || Export <- ExpectedExports],
        [?assertMatch({_, _}, binary:match(C, Call)) || Call <- ExpectedCalls],
        [
            ?assertEqual(nomatch, binary:match(Erl, s062_export_binary(Function, Arity)))
         || {Function, Arity} <- ExtraAbsentFunctions
        ],
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s062_deferred_exports()],
        [?assertEqual(nomatch, binary:match(C, Call)) || Call <- s062_deferred_c_calls()]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s062_gl_46_exports() ->
    [
        <<"-export([provoking_vertex/1]).">>,
        <<"-export([bind_sampler/2]).">>,
        <<"-export([vertex_attrib_divisor/2]).">>,
        <<"-export([release_shader_compiler/0]).">>,
        <<"-export([scissor_indexed/5]).">>,
        <<"-export([depth_range_indexed/3]).">>,
        <<"-export([memory_barrier/1]).">>,
        <<"-export([clip_control/2]).">>,
        <<"-export([memory_barrier_by_region/1]).">>,
        <<"-export([texture_barrier/0]).">>
    ].

s062_gles_32_exports() ->
    [
        <<"-export([bind_sampler/2]).">>,
        <<"-export([vertex_attrib_divisor/2]).">>,
        <<"-export([release_shader_compiler/0]).">>,
        <<"-export([memory_barrier/1]).">>,
        <<"-export([memory_barrier_by_region/1]).">>
    ].

s062_gl_only_exports() ->
    [
        {"provoking_vertex", 1},
        {"scissor_indexed", 5},
        {"depth_range_indexed", 3},
        {"clip_control", 2},
        {"texture_barrier", 0}
    ].

s062_gl_46_c_calls() ->
    [
        <<"glProvokingVertex(arg_0);">>,
        <<"glBindSampler(arg_0, arg_1);">>,
        <<"glVertexAttribDivisor(arg_0, arg_1);">>,
        <<"glReleaseShaderCompiler();">>,
        <<"glScissorIndexed(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glDepthRangeIndexed(arg_0, arg_1, arg_2);">>,
        <<"glMemoryBarrier(arg_0);">>,
        <<"glClipControl(arg_0, arg_1);">>,
        <<"glMemoryBarrierByRegion(arg_0);">>,
        <<"glTextureBarrier();">>
    ].

s062_gles_32_c_calls() ->
    [
        <<"glBindSampler(arg_0, arg_1);">>,
        <<"glVertexAttribDivisor(arg_0, arg_1);">>,
        <<"glReleaseShaderCompiler();">>,
        <<"glMemoryBarrier(arg_0);">>,
        <<"glMemoryBarrierByRegion(arg_0);">>
    ].

s062_deferred_exports() ->
    [
        <<"-export([fence_sync/2]).">>,
        <<"-export([client_wait_sync/3]).">>,
        <<"-export([wait_sync/3]).">>,
        <<"-export([sampler_parameter/3]).">>,
        <<"-export([get_sampler_parameter/3]).">>,
        <<"-export([get_query_indexed/3]).">>
    ].

s062_deferred_c_calls() ->
    [
        <<"glFenceSync(">>,
        <<"glClientWaitSync(">>,
        <<"glWaitSync(">>
    ].

s062_export_binary(Function, Arity) ->
    list_to_binary(io_lib:format("-export([~s/~p]).", [Function, Arity])).

s062_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s062_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s062_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s062_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s062_double_nif_data() ->
    {gl_type, {"GLdouble", "double", "enif_get_double", "enif_make_double"}}.

s062_bitfield_nif_data() ->
    {gl_type, {"GLbitfield", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 150.
s150_pointer_lifetime_omission_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s150_assert_omitted(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s150_emitter_omits_pointer_lifetime_surface_test_() ->
    [
        {"gl 4.6", fun() -> s150_assert_emitted_surface_omits({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s150_assert_emitted_surface_omits({gl, {4, 1}}) end},
        {"gles 3.2", fun() -> s150_assert_emitted_surface_omits({gles, {3, 2}}) end}
    ].

s150_assert_omitted(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- s150_omitted_commands()].

s150_assert_emitted_surface_omits(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard150-emitter-" ++
            integer_to_list(erlang:system_time(nanosecond)) ++
            "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s150_assert_not_contains(Erl, Pattern) || Pattern <- s150_omitted_erlang_patterns()],
        [s150_assert_not_contains(C, Pattern) || Pattern <- s150_omitted_c_patterns()]
    after
        ok = file:set_cwd(Cwd)
    end.

s150_omitted_commands() ->
    s150_mapped_buffer_commands() ++ s150_sync_commands() ++ s150_callback_and_pointer_commands().

s150_mapped_buffer_commands() ->
    [
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glUnmapBuffer",
        "glUnmapNamedBuffer",
        "glFlushMappedBufferRange",
        "glFlushMappedNamedBufferRange",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv"
    ].

s150_sync_commands() ->
    [
        "glFenceSync",
        "glClientWaitSync",
        "glWaitSync",
        "glDeleteSync",
        "glIsSync",
        "glGetSynciv"
    ].

s150_callback_and_pointer_commands() ->
    [
        "glDebugMessageCallback",
        "glObjectPtrLabel",
        "glGetObjectPtrLabel",
        "glGetPointerv"
    ].

s150_omitted_erlang_patterns() ->
    [
        <<"-export([map_buffer/">>,
        <<"-export([map_buffer_range/">>,
        <<"-export([map_named_buffer/">>,
        <<"-export([map_named_buffer_range/">>,
        <<"-export([unmap_buffer/">>,
        <<"-export([unmap_named_buffer/">>,
        <<"-export([flush_mapped_buffer_range/">>,
        <<"-export([flush_mapped_named_buffer_range/">>,
        <<"-export([fence_sync/">>,
        <<"-export([client_wait_sync/">>,
        <<"-export([wait_sync/">>,
        <<"-export([delete_sync/">>,
        <<"-export([is_sync/">>,
        <<"-export([debug_message_callback/">>,
        <<"-export([object_ptr_label/">>,
        <<"-export([get_object_ptr_label/">>,
        <<"-export([get_pointer/">>
    ].

s150_omitted_c_patterns() ->
    [
        <<"glMapBuffer(">>,
        <<"glMapBufferRange(">>,
        <<"glMapNamedBuffer(">>,
        <<"glMapNamedBufferRange(">>,
        <<"glUnmapBuffer(">>,
        <<"glUnmapNamedBuffer(">>,
        <<"glFlushMappedBufferRange(">>,
        <<"glFlushMappedNamedBufferRange(">>,
        <<"glGetBufferPointerv(">>,
        <<"glGetNamedBufferPointerv(">>,
        <<"glGetVertexAttribPointerv(">>,
        <<"glFenceSync(">>,
        <<"glClientWaitSync(">>,
        <<"glWaitSync(">>,
        <<"glDeleteSync(">>,
        <<"glIsSync(">>,
        <<"glGetSynciv(">>,
        <<"glDebugMessageCallback(">>,
        <<"glObjectPtrLabel(">>,
        <<"glGetObjectPtrLabel(">>,
        <<"glGetPointerv(">>
    ].

s150_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 155.
s155_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s155_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s155_emitter_program_binary_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s155_assert_emitted_surface({gl, {4, 6}}, s155_expected_erlang(), s155_expected_c(), s155_forbidden_surface())
        end},
        {"gles 3.2", fun() ->
            s155_assert_emitted_surface({gles, {3, 2}}, s155_expected_erlang(), s155_expected_c(), s155_forbidden_surface())
        end},
        {"gl 3.3", fun() ->
            s155_assert_emitted_surface({gl, {3, 3}}, [], [], s155_absent_surface())
        end},
        {"gles 2.0", fun() ->
            s155_assert_emitted_surface({gles, {2, 0}}, [], [], s155_absent_surface())
        end}
    ].

s155_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s155_present_functions(Target),

    s155_assert_presence(Present, Functions),
    s155_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s155_assert_get_program_binary(maps:get({"get_program_binary", 2}, Functions)),
            s155_assert_program_binary(maps:get({"program_binary", 3}, Functions))
    end.

s155_present_functions({gl, {4, 1}}) ->
    s155_all_functions();
s155_present_functions({gl, {4, 6}}) ->
    s155_all_functions();
s155_present_functions({gles, {3, 0}}) ->
    s155_all_functions();
s155_present_functions({gles, {3, 1}}) ->
    s155_all_functions();
s155_present_functions({gles, {3, 2}}) ->
    s155_all_functions();
s155_present_functions(_) ->
    [].

s155_all_functions() ->
    [
        {"get_program_binary", 2},
        {"program_binary", 3}
    ].

s155_assert_presence(Present, Functions) ->
    Absent = s155_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_program_binary", 1}, Functions)),
    ?assertNot(maps:is_key({"get_program_binary", 3}, Functions)),
    ?assertNot(maps:is_key({"program_binary", 2}, Functions)),
    ?assertNot(maps:is_key({"program_binary", 4}, Functions)).

s155_assert_get_program_binary(FunctionData) ->
    ?assertEqual("glGetProgramBinary", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {out, "Binary", {program_binary, "BinarySize"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"BinarySize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [
            {"BinaryFormat", {gl, uint, []}},
            {"Binary", {undefined, binary, []}}
        ],
        maps:get(specs_return, FunctionData)
    ),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"BinarySize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetProgramBinary", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetProgramBinary", maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s155_uint_nif_data()},
            {"Binary", out_program_binary}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s155_assert_program_binary(FunctionData) ->
    ?assertEqual("glProgramBinary", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "BinaryFormat", gl_enum_value},
            {in, "Binary", {byte_data_with_trailing_size, "Length", gl_sizei}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"BinaryFormat", {gl, uint, []}},
            {"Binary", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"BinaryFormat", do_nothing},
            {"Binary", {byte_data_with_trailing_size, "Length", gl_sizei}}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glProgramBinary", maps:get(raw_function, Clause)),

    NifData = maps:get("glProgramBinary", maps:get(nif_functions, FunctionData)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s155_uint_nif_data()},
            {"BinaryFormat", s155_enum_value_nif_data()},
            {"Binary", s155_binary_nif_data()},
            {"Length", s155_sizei_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s155_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glProgramBinaryOES",
        "glShaderBinaryOES"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s155_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard155-emitter-" ++
            integer_to_list(erlang:system_time(nanosecond)) ++
            "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s155_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s155_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s155_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s155_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s155_expected_erlang() ->
    [
        <<"-export([get_program_binary/2]).">>,
        <<"-export([program_binary/3]).">>,
        <<"-spec get_program_binary(\n    Program :: program(),\n    BinarySize :: non_neg_integer()\n) -> {ok, BinaryFormat :: gl:uint(), Binary :: binary()} | {error, atom()}.">>,
        <<"-spec program_binary(\n    Program :: program(),\n    BinaryFormat :: gl:uint(),\n    Binary :: iodata()\n) -> ok | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetProgramBinary_raw(Program, BinarySize)).">>,
        <<"Binary0 = iolist_to_binary(Binary),\n    Length = byte_size(Binary0),">>,
        <<"?CALL_RAW_FUNC(glProgramBinary_raw(Program, BinaryFormat, Binary0, Length)).">>
    ].

s155_expected_c() ->
    [
        <<"ErlNifUInt64 arg_1_size;">>,
        <<"if (arg_1_size > (ErlNifUInt64)INT_MAX) {">>,
        <<"unsigned char* arg_1_data = NULL;">>,
        <<"GLsizei arg_1_length = 0;">>,
        <<"GLenum arg_1_format = 0;">>,
        <<"glGetProgramBinary(arg_0, (GLsizei)arg_1_size, &arg_1_length, &arg_1_format, arg_1_data);">>,
        <<"ERL_NIF_TERM arg_1_format_ret = enif_make_uint(env, arg_1_format);">>,
        <<"memcpy(arg_1_bin, arg_1_data, (size_t)arg_1_length);">>,
        <<"glProgramBinary(arg_0, arg_1, (void*)arg_2.data, arg_3);">>,
        <<"{\"glGetProgramBinary_raw\", 2, nif_glGetProgramBinary, 0}">>,
        <<"{\"glProgramBinary_raw\", 4, nif_glProgramBinary, 0}">>
    ].

s155_forbidden_surface() ->
    [
        <<"glProgramBinaryOES(">>,
        <<"glShaderBinaryOES(">>
    ].

s155_absent_surface() ->
    [
        <<"-export([get_program_binary/2]).">>,
        <<"-export([program_binary/3]).">>,
        <<"-spec get_program_binary(">>,
        <<"-spec program_binary(">>,
        <<"?CALL_RAW_FUNC(glGetProgramBinary_raw(Program, BinarySize)).">>,
        <<"?CALL_RAW_FUNC(glProgramBinary_raw(Program, BinaryFormat, Binary0, Length)).">>,
        <<"glGetProgramBinary(">>,
        <<"glProgramBinary(">>,
        <<"{\"glGetProgramBinary_raw\", 2, nif_glGetProgramBinary, 0}">>,
        <<"{\"glProgramBinary_raw\", 4, nif_glProgramBinary, 0}">>
    ].

s155_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s155_enum_value_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s155_binary_nif_data() ->
    binary_to_glbinary.

s155_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s155_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s155_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 174.

s174_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s174_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s174_emitter_transform_feedback_varying_test_() ->
    [
        {"gl 4.6", fun() -> s174_assert_emitted_present({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s174_assert_emitted_present({gles, {3, 2}}) end},
        {"gles 2.0", fun() -> s174_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s174_assert_target({gles, {2, 0}} = Target) ->
    Functions = generator_test_support:functions(Target),
    ?assertNot(maps:is_key({"get_transform_feedback_varying", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetTransformFeedbackVarying", Functions)),
    s174_assert_deferred_neighbors_absent(Functions);
s174_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    ?assert(maps:is_key({"get_transform_feedback_varying", 3}, Functions)),
    Function = maps:get({"get_transform_feedback_varying", 3}, Functions),
    s174_assert_transform_feedback_varying(Function),
    s174_assert_reflection_still_uses_glint(Functions),
    s174_assert_enum_contains(BindingData, "attribute_type", "float"),
    s174_assert_deferred_neighbors_absent(Functions).

s174_assert_transform_feedback_varying(Function) ->
    ?assertEqual("glGetTransformFeedbackVarying", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Index", gl_uint},
            {out, "Info", {active_reflection_info, gl_sizei, {gl_enum, "AttributeType"}}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Index", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual(
        [
            {"Size", {gl, sizei, []}},
            {"Type", {undefined, attribute_type, []}},
            {"Name", {undefined, binary, []}}
        ],
        maps:get(specs_return, Function)
    ),
    ?assertEqual(3, maps:get(function_arity, Function)),

    [Clause] = maps:get(function_clauses, Function),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Index", do_nothing},
            {"MaxLength", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetTransformFeedbackVarying", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetTransformFeedbackVarying", maps:get(nif_functions, Function)),
    ?assertEqual(3, maps:get(arity, NifData)),
    UintNifData = s174_uint_nif_data(),
    [
        {"Program", UintNifData},
        {"Index", UintNifData},
        {"MaxLength", {out_active_reflection_info, "GLsizei", TypeMap}}
    ] = maps:get(params, NifData),
    ?assert(lists:keymember("GL_FLOAT", 1, TypeMap)),
    ?assert(lists:keymember("float", 2, TypeMap)),
    ?assertEqual(void, maps:get(return, NifData)).

s174_assert_reflection_still_uses_glint(Functions) ->
    [
        begin
            Function = maps:get(FunctionKey, Functions),
            NifData = maps:get(GlCommand, maps:get(nif_functions, Function)),
            {"MaxLength", {out_active_reflection_info, "GLint", _TypeMap}} =
                lists:last(maps:get(params, NifData))
        end
     || {FunctionKey, GlCommand} <- [
            {{"get_active_attrib", 3}, "glGetActiveAttrib"},
            {{"get_active_uniform", 3}, "glGetActiveUniform"}
        ]
    ].

s174_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s174_deferred_neighbor_commands()
    ].

s174_assert_emitted_present(Target) ->
    s174_assert_emitted_surface(
        Target,
        [
            <<"-export([get_transform_feedback_varying/3]).">>,
            <<"-export_type([attribute_type/0]).">>,
            <<"-spec get_transform_feedback_varying(\n    Program :: program(),\n    Index :: gl:uint(),\n    MaxLength :: pos_integer()\n) -> {ok, Size :: gl:sizei(), Type :: attribute_type(), Name :: binary()} | {error, atom()}.">>,
            <<"?CALL_RAW_FUNC(glGetTransformFeedbackVarying_raw(Program, Index, MaxLength)).">>
        ],
        [
            <<"GLsizei arg_2_size;">>,
            <<"glGetTransformFeedbackVarying(arg_0, arg_1, arg_2_max_length, &arg_2_length, &arg_2_size, &arg_2_type, arg_2_name);">>,
            <<"case GL_FLOAT: arg_2_type_ret = beam_atom_float; break;">>,
            <<"{\"glGetTransformFeedbackVarying_raw\", 3, nif_glGetTransformFeedbackVarying, 0}">>,
            <<"glGetActiveAttrib(arg_0, arg_1, arg_2_max_length, &arg_2_length, &arg_2_size, &arg_2_type, arg_2_name);">>,
            <<"glGetActiveUniform(arg_0, arg_1, arg_2_max_length, &arg_2_length, &arg_2_size, &arg_2_type, arg_2_name);">>,
            <<"GLint arg_2_size;">>
        ],
        s174_forbidden_needles()
    ).

s174_assert_emitted_absent(Target) ->
    s174_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_transform_feedback_varying">>,
            <<"glGetTransformFeedbackVarying">>
            | s174_forbidden_needles()
        ]
    ).

s174_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard174-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(
        api_name,
        generator_test_support:target_name(Target),
        generator_test_support:resolve_target(Target)
    ),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s174_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s174_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s174_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s174_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s174_deferred_neighbor_commands() ->
    [
        "glGetSynciv",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv",
        "glGetPointerv",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange"
    ].

s174_forbidden_needles() ->
    [
        <<"glGetSynciv">>,
        <<"glGetBufferPointerv">>,
        <<"glGetNamedBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>,
        <<"glGetPointerv">>,
        <<"glMapBuffer">>,
        <<"glMapBufferRange">>,
        <<"glMapNamedBuffer">>,
        <<"glMapNamedBufferRange">>
    ].

s174_assert_enum_contains(BindingData, EnumType, Atom) ->
    ?assert(lists:member(Atom, maps:get(EnumType, maps:get(enum_types, BindingData)))).

s174_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s174_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s174_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s174_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 175.

s175_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s175_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s175_emitter_uniform_reflection_test_() ->
    [
        {"gl 4.6", fun() -> s175_assert_emitted_present({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s175_assert_emitted_present({gles, {3, 2}}) end},
        {"gles 2.0", fun() -> s175_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s175_assert_target({gles, {2, 0}} = Target) ->
    Functions = generator_test_support:functions(Target),
    [
        ?assertNot(maps:is_key(Key, Functions))
     || Key <- [
            {"get_uniform_indices", 2},
            {"get_active_uniform_name", 3},
            {"get_active_uniforms", 3},
            {"get_active_uniform_block_name", 3},
            {"get_active_uniform_block", 4}
        ]
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s175_commands()
    ],
    s175_assert_deferred_neighbors_absent(Functions);
s175_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s175_assert_uniform_indices(maps:get({"get_uniform_indices", 2}, Functions)),
    s175_assert_active_uniform_name_presence(Target, Functions),
    s175_assert_active_uniforms(maps:get({"get_active_uniforms", 3}, Functions)),
    s175_assert_active_uniform_block_name(maps:get({"get_active_uniform_block_name", 3}, Functions)),
    s175_assert_active_uniform_block(maps:get({"get_active_uniform_block", 4}, Functions)),
    s175_assert_enum_contains(BindingData, active_uniform_parameter_name, "uniform_size"),
    s175_assert_enum_contains(BindingData, active_uniform_block_parameter_name, "uniform_block_active_uniforms"),
    s175_assert_deferred_neighbors_absent(Functions).

s175_assert_active_uniform_name_presence({gl, _Version}, Functions) ->
    s175_assert_active_uniform_name(maps:get({"get_active_uniform_name", 3}, Functions));
s175_assert_active_uniform_name_presence({gles, _Version}, Functions) ->
    ?assertNot(maps:is_key({"get_active_uniform_name", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetActiveUniformName", Functions)).

s175_assert_uniform_indices(Function) ->
    ?assertEqual("glGetUniformIndices", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Names", list_gl_strings_null_terminated},
            {out, "Indices", {typed_value_list_from_counted_input, "Names", gl_uint}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Names", {list, {undefined, iodata, []}}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Indices", {list, {gl, uint, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(2, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Names", normalize_list_strings_or_binary}
        ],
        maps:get(params, Clause)
    ),
    s175_assert_nif(
        Function,
        "glGetUniformIndices",
        2,
        [
            {"Program", s175_uint_nif_data()},
            {"Names", in_list_gl_strings_null_terminated},
            {"Indices", {out_typed_value_list_from_counted_input, "Names", "GLuint", "enif_make_uint"}}
        ]
    ).

s175_assert_active_uniform_name(Function) ->
    s175_assert_string_output_function(
        Function,
        "glGetActiveUniformName",
        [
            {in, "Program", {gl_object, program}},
            {in, "UniformIndex", gl_uint},
            {out, "Name", gl_string}
        ],
        [
            {"Program", {undefined, program, []}},
            {"UniformIndex", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        [
            {"Program", s175_uint_nif_data()},
            {"UniformIndex", s175_uint_nif_data()},
            {"MaxLength", out_gl_string}
        ]
    ).

s175_assert_active_uniforms(Function) ->
    ?assertEqual("glGetActiveUniformsiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "UniformIndices", gl_uint_list_with_count},
            {in, "ParamName", {gl_enum, "UniformPName", active_uniform_parameter_name}},
            {out, "Values", {typed_value_list_from_counted_input, "UniformIndices", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"UniformIndices", {list, {gl, uint, []}}},
            {"ParamName", {undefined, active_uniform_parameter_name, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(3, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"UniformIndices", gl_uint_list_with_count},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("uniform_size", 1, ParamNameMap)),
    s175_assert_nif(
        Function,
        "glGetActiveUniformsiv",
        3,
        [
            {"Program", s175_uint_nif_data()},
            {"UniformIndices", in_gl_uint_list_with_count},
            {"ParamName", s175_enum_nif_data()},
            {"Values", {out_typed_value_list_from_counted_input, "UniformIndices", "GLint", "enif_make_int"}}
        ]
    ).

s175_assert_active_uniform_block_name(Function) ->
    s175_assert_string_output_function(
        Function,
        "glGetActiveUniformBlockName",
        [
            {in, "Program", {gl_object, program}},
            {in, "UniformBlockIndex", gl_uint},
            {out, "Name", gl_string}
        ],
        [
            {"Program", {undefined, program, []}},
            {"UniformBlockIndex", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        [
            {"Program", s175_uint_nif_data()},
            {"UniformBlockIndex", s175_uint_nif_data()},
            {"MaxLength", out_gl_string}
        ]
    ).

s175_assert_active_uniform_block(Function) ->
    ?assertEqual("glGetActiveUniformBlockiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "UniformBlockIndex", gl_uint},
            {in, "ParamName", {gl_enum, "UniformBlockPName", active_uniform_block_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"UniformBlockIndex", {gl, uint, []}},
            {"ParamName", {undefined, active_uniform_block_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(4, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"UniformBlockIndex", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("uniform_block_active_uniforms", 1, ParamNameMap)),
    s175_assert_nif(
        Function,
        "glGetActiveUniformBlockiv",
        4,
        [
            {"Program", s175_uint_nif_data()},
            {"UniformBlockIndex", s175_uint_nif_data()},
            {"ParamName", s175_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ]
    ).

s175_assert_string_output_function(Function, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, Function)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, Function)),
    ?assertEqual(SpecsParams, maps:get(specs_params, Function)),
    ?assertEqual([{"Name", {undefined, binary, []}}], maps:get(specs_return, Function)),
    ?assertEqual(3, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {lists:nth(2, [Name || {_, Name, _} <- ParamsSpecs]), do_nothing},
            {"MaxLength", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    s175_assert_nif(Function, Command, 3, NifParams).

s175_assert_nif(Function, Command, Arity, Params) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s175_assert_emitted_present(Target) ->
    {ExpectedActiveNameErl, ExpectedActiveNameC, ForbiddenActiveName} =
        s175_active_uniform_name_emitter_expectations(Target),
    s175_assert_emitted_surface(
        Target,
        [
            <<"-export([get_uniform_indices/2]).">>,
            <<"-export([get_active_uniforms/3]).">>,
            <<"-export([get_active_uniform_block_name/3]).">>,
            <<"-export([get_active_uniform_block/4]).">>,
            <<"-export_type([active_uniform_parameter_name/0]).">>,
            <<"-export_type([active_uniform_block_parameter_name/0]).">>,
            <<"get_uniform_indices(Program, Names) ->">>,
            <<"get_active_uniforms(Program, UniformIndices, ParamName) ->">>,
            <<"get_active_uniform_block(Program, UniformBlockIndex, ParamName, Count) ->">>
        ] ++ ExpectedActiveNameErl,
        [
            <<"glGetUniformIndices(arg_0, (GLsizei)arg_1_count, (const GLchar* const*)arg_1_strings, out_2_values);">>,
            <<"GLsizei out_2_count = (GLsizei)arg_1_count;">>,
            <<"GLuint* out_2_values = enif_alloc(sizeof(GLuint) * (size_t)out_2_count);">>,
            <<"glGetActiveUniformsiv(arg_0, arg_1_count, (const GLuint*)arg_1.data, arg_2, out_3_values);">>,
            <<"GLint* out_3_values = enif_alloc(sizeof(GLint) * (size_t)out_3_count);">>,
            <<"glGetActiveUniformBlockName(arg_0, arg_1, arg_2_max_length, &arg_2_length, arg_2_info_log);">>,
            <<"glGetActiveUniformBlockiv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetUniformIndices_raw\", 2, nif_glGetUniformIndices, 0}">>,
            <<"{\"glGetActiveUniformsiv_raw\", 3, nif_glGetActiveUniformsiv, 0}">>,
            <<"{\"glGetActiveUniformBlockiv_raw\", 4, nif_glGetActiveUniformBlockiv, 0}">>
        ] ++ ExpectedActiveNameC,
        ForbiddenActiveName ++ s175_forbidden_needles()
    ).

s175_active_uniform_name_emitter_expectations({gl, _Version}) ->
    {
        [
            <<"-export([get_active_uniform_name/3]).">>,
            <<"get_active_uniform_name(Program, UniformIndex, MaxLength) ->">>
        ],
        [
            <<"glGetActiveUniformName(arg_0, arg_1, arg_2_max_length, &arg_2_length, arg_2_info_log);">>,
            <<"{\"glGetActiveUniformName_raw\", 3, nif_glGetActiveUniformName, 0}">>
        ],
        []
    };
s175_active_uniform_name_emitter_expectations({gles, _Version}) ->
    {[], [], [<<"get_active_uniform_name">>, <<"glGetActiveUniformName">>]}.

s175_assert_emitted_absent(Target) ->
    s175_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_uniform_indices">>,
            <<"get_active_uniform_name">>,
            <<"get_active_uniforms">>,
            <<"get_active_uniform_block_name">>,
            <<"get_active_uniform_block">>
            | [list_to_binary(Command) || Command <- s175_commands()]
        ] ++ s175_forbidden_needles()
    ).

s175_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard175-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(
        api_name,
        generator_test_support:target_name(Target),
        generator_test_support:resolve_target(Target)
    ),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s175_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s175_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s175_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s175_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s175_commands() ->
    [
        "glGetUniformIndices",
        "glGetActiveUniformName",
        "glGetActiveUniformsiv",
        "glGetActiveUniformBlockName",
        "glGetActiveUniformBlockiv"
    ].

s175_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s175_deferred_neighbor_commands()
    ].

s175_deferred_neighbor_commands() ->
    [
        "glGetSynciv",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv",
        "glGetPointerv",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange"
    ].

s175_forbidden_needles() ->
    [list_to_binary(Command) || Command <- s175_deferred_neighbor_commands()].

s175_assert_enum_contains(BindingData, EnumType, Atom) ->
    ?assert(lists:member(Atom, maps:get(atom_to_list(EnumType), maps:get(enum_types, BindingData)))).

s175_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s175_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s175_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s175_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s175_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 176.

s176_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s176_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s176_emitter_program_resource_reflection_test_() ->
    [
        {"gl 4.6", fun() -> s176_assert_emitted_present({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s176_assert_emitted_present({gles, {3, 2}}) end},
        {"gl 4.1", fun() -> s176_assert_emitted_absent({gl, {4, 1}}) end},
        {"gles 3.0", fun() -> s176_assert_emitted_absent({gles, {3, 0}}) end}
    ].

s176_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s176_has_surface(Target) of
        true ->
            s176_assert_program_interface(maps:get({"get_program_interface", 4}, Functions)),
            s176_assert_program_resource_name(maps:get({"get_program_resource_name", 4}, Functions)),
            s176_assert_program_resource(maps:get({"get_program_resource", 5}, Functions)),
            s176_assert_enum_contains(BindingData, program_interface, "uniform"),
            s176_assert_enum_contains(BindingData, program_interface_parameter_name, "active_resources"),
            s176_assert_enum_contains(BindingData, program_resource_property, "type"),
            s176_assert_enum_contains(BindingData, program_resource_property, "array_size"),
            s176_assert_enum_contains(BindingData, program_resource_property, "num_active_variables"),
            s176_assert_enum_contains(BindingData, program_resource_property, "active_variables");
        false ->
            s176_assert_absent(Functions)
    end,
    s176_assert_deferred_neighbors_absent(Functions).

s176_has_surface({gl, {4, 6}}) -> true;
s176_has_surface({gles, {3, 1}}) -> true;
s176_has_surface({gles, {3, 2}}) -> true;
s176_has_surface(_) -> false.

s176_assert_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Key, Functions))
     || Key <- [
            {"get_program_interface", 4},
            {"get_program_resource_name", 4},
            {"get_program_resource", 5}
        ]
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- s176_commands()].

s176_assert_program_interface(Function) ->
    ?assertEqual("glGetProgramInterfaceiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ProgramInterface", {gl_enum, "ProgramInterface"}},
            {in, "ParamName", {gl_enum, "ProgramInterfacePName", program_interface_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ProgramInterface", {undefined, program_interface, []}},
            {"ParamName", {undefined, program_interface_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(4, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"ProgramInterface", {gl_enum_to_uint, InterfaceMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("uniform_block", 1, InterfaceMap)),
    ?assert(lists:keymember("active_resources", 1, ParamNameMap)),
    s176_assert_nif(
        Function,
        "glGetProgramInterfaceiv",
        4,
        [
            {"Program", s176_uint_nif_data()},
            {"ProgramInterface", s176_enum_nif_data()},
            {"ParamName", s176_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ]
    ).

s176_assert_program_resource_name(Function) ->
    ?assertEqual("glGetProgramResourceName", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ProgramInterface", {gl_enum, "ProgramInterface"}},
            {in, "Index", gl_uint},
            {out, "Name", gl_string}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ProgramInterface", {undefined, program_interface, []}},
            {"Index", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Name", {undefined, binary, []}}], maps:get(specs_return, Function)),
    ?assertEqual(4, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"ProgramInterface", {gl_enum_to_uint, InterfaceMap}},
        {"Index", do_nothing},
        {"MaxLength", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("uniform", 1, InterfaceMap)),
    s176_assert_nif(
        Function,
        "glGetProgramResourceName",
        4,
        [
            {"Program", s176_uint_nif_data()},
            {"ProgramInterface", s176_enum_nif_data()},
            {"Index", s176_uint_nif_data()},
            {"MaxLength", out_gl_string}
        ]
    ).

s176_assert_program_resource(Function) ->
    ?assertEqual("glGetProgramResourceiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ProgramInterface", {gl_enum, "ProgramInterface"}},
            {in, "Index", gl_uint},
            {in, "Properties", {gl_enum_list_with_count, "ProgramResourceProperty", program_resource_property}},
            {out, "Values", {caller_sized_typed_value_list, "Count", "Length", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ProgramInterface", {undefined, program_interface, []}},
            {"Index", {gl, uint, []}},
            {"Properties", {list, {undefined, program_resource_property, []}}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(5, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"ProgramInterface", {gl_enum_to_uint, InterfaceMap}},
        {"Index", do_nothing},
        {"Properties", {gl_enum_list_with_count, PropertyMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("uniform", 1, InterfaceMap)),
    ?assert(lists:keymember("type", 1, PropertyMap)),
    ?assert(lists:keymember("array_size", 1, PropertyMap)),
    ?assert(lists:keymember("active_variables", 1, PropertyMap)),
    s176_assert_nif(
        Function,
        "glGetProgramResourceiv",
        5,
        [
            {"Program", s176_uint_nif_data()},
            {"ProgramInterface", s176_enum_nif_data()},
            {"Index", s176_uint_nif_data()},
            {"Properties", in_gl_enum_list_with_count},
            {"Values", {caller_sized_typed_value_list, "Count", "Length", "GLint", "enif_make_int"}}
        ]
    ).

s176_assert_nif(Function, Command, Arity, Params) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s176_assert_emitted_present(Target) ->
    s176_assert_emitted_surface(
        Target,
        [
            <<"-export([get_program_interface/4]).">>,
            <<"-export([get_program_resource_name/4]).">>,
            <<"-export([get_program_resource/5]).">>,
            <<"-export_type([program_interface_parameter_name/0]).">>,
            <<"-export_type([program_resource_property/0]).">>,
            <<"get_program_interface(Program, ProgramInterface, ParamName, Count) ->">>,
            <<"get_program_resource_name(Program, ProgramInterface, Index, MaxLength) ->">>,
            <<"get_program_resource(Program, ProgramInterface, Index, Properties, Count) ->">>,
            <<"NewProperties = case Properties of">>,
            <<"?CALL_RAW_FUNC(glGetProgramResourceiv_raw(Program, NewProgramInterface, Index, NewProperties, Count)).">>
        ],
        [
            <<"glGetProgramInterfaceiv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"glGetProgramResourceName(arg_0, arg_1, arg_2, arg_3_max_length, &arg_3_length, arg_3_info_log);">>,
            <<"if (arg_3.size == 0 || arg_3.size % sizeof(GLenum) != 0) {">>,
            <<"GLsizei arg_3_count = (GLsizei)arg_3_count_tmp;">>,
            <<"glGetProgramResourceiv(arg_0, arg_1, arg_2, arg_3_count, (const GLenum*)arg_3.data, arg_4_count, &arg_4_length, arg_4_values);">>,
            <<"if (arg_4_length > arg_4_count) {">>,
            <<"for (int i = arg_4_length-1; i >= 0; i--) {">>,
            <<"{\"glGetProgramInterfaceiv_raw\", 4, nif_glGetProgramInterfaceiv, 0}">>,
            <<"{\"glGetProgramResourceName_raw\", 4, nif_glGetProgramResourceName, 0}">>,
            <<"{\"glGetProgramResourceiv_raw\", 5, nif_glGetProgramResourceiv, 0}">>
        ],
        s176_forbidden_needles()
    ).

s176_assert_emitted_absent(Target) ->
    s176_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_program_interface">>,
            <<"get_program_resource_name">>,
            <<"get_program_resource(">>
            | [list_to_binary(Command) || Command <- s176_commands()]
        ] ++ s176_forbidden_needles()
    ).

s176_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard176-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(
        api_name,
        generator_test_support:target_name(Target),
        generator_test_support:resolve_target(Target)
    ),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s176_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s176_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s176_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s176_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s176_commands() ->
    [
        "glGetProgramInterfaceiv",
        "glGetProgramResourceName",
        "glGetProgramResourceiv"
    ].

s176_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s176_deferred_neighbor_commands()
    ].

s176_deferred_neighbor_commands() ->
    [
        "glGetSynciv",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glGetPointerv",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv"
    ].

s176_forbidden_needles() ->
    [list_to_binary(Command) || Command <- s176_deferred_neighbor_commands()].

s176_assert_enum_contains(BindingData, EnumType, Atom) ->
    ?assert(lists:member(Atom, maps:get(atom_to_list(EnumType), maps:get(enum_types, BindingData)))).

s176_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s176_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s176_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s176_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s176_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 178.

s178_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s178_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s178_emitter_subroutine_reflection_test_() ->
    [
        {"gl 4.6", fun() -> s178_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s178_assert_emitted_present({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s178_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s178_assert_emitted_absent({gles, {3, 2}}) end}
    ].

s178_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s178_supports(Target) of
        true ->
            s178_assert_active_subroutine_uniform(maps:get({"get_active_subroutine_uniform", 5}, Functions)),
            s178_assert_subroutine_name(
                maps:get({"get_active_subroutine_uniform_name", 4}, Functions),
                "glGetActiveSubroutineUniformName"
            ),
            s178_assert_subroutine_name(
                maps:get({"get_active_subroutine_name", 4}, Functions),
                "glGetActiveSubroutineName"
            ),
            s178_assert_uniform_subroutines(maps:get({"uniform_subroutines", 2}, Functions)),
            s178_assert_get_uniform_subroutine(maps:get({"get_uniform_subroutine", 3}, Functions)),
            s178_assert_get_program_stage(maps:get({"get_program_stage", 4}, Functions)),
            s178_assert_enum_contains(BindingData, shader_type, "fragment_shader"),
            s178_assert_enum_contains(BindingData, subroutine_parameter_name, "uniform_size"),
            s178_assert_enum_contains(BindingData, subroutine_parameter_name, "uniform_name_length"),
            s178_assert_enum_contains(BindingData, subroutine_parameter_name, "num_compatible_subroutines"),
            s178_assert_enum_contains(BindingData, subroutine_parameter_name, "compatible_subroutines"),
            s178_assert_enum_contains(BindingData, program_stage_parameter_name, "active_subroutines"),
            s178_assert_enum_contains(BindingData, program_stage_parameter_name, "active_subroutine_uniforms"),
            s178_assert_enum_contains(BindingData, program_stage_parameter_name, "active_subroutine_uniform_locations");
        false ->
            s178_assert_absent(Functions)
    end,
    s178_assert_deferred_neighbors_absent(Functions).

s178_supports({gl, {4, 1}}) -> true;
s178_supports({gl, {4, 6}}) -> true;
s178_supports(_) -> false.

s178_assert_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Key, Functions))
     || Key <- [
            {"get_active_subroutine_uniform", 5},
            {"get_active_subroutine_uniform_name", 4},
            {"get_active_subroutine_name", 4},
            {"uniform_subroutines", 2},
            {"get_uniform_subroutine", 3},
            {"get_program_stage", 4}
        ]
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- s178_commands()].

s178_assert_active_subroutine_uniform(Function) ->
    ?assertEqual("glGetActiveSubroutineUniformiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "Index", gl_uint},
            {in, "ParamName", {gl_enum, "SubroutineParameterName", subroutine_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ShaderType", {undefined, shader_type, []}},
            {"Index", {gl, uint, []}},
            {"ParamName", {undefined, subroutine_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(5, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"Index", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("fragment_shader", 1, ShaderTypeMap)),
    ?assert(lists:keymember("num_compatible_subroutines", 1, ParamNameMap)),
    ?assert(lists:keymember("compatible_subroutines", 1, ParamNameMap)),
    s178_assert_nif(
        Function,
        "glGetActiveSubroutineUniformiv",
        5,
        [
            {"Program", s178_uint_nif_data()},
            {"ShaderType", s178_enum_nif_data()},
            {"Index", s178_uint_nif_data()},
            {"ParamName", s178_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ]
    ).

s178_assert_subroutine_name(Function, Command) ->
    ?assertEqual(Command, maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "Index", gl_uint},
            {out, "Name", gl_string}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ShaderType", {undefined, shader_type, []}},
            {"Index", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Name", {undefined, binary, []}}], maps:get(specs_return, Function)),
    ?assertEqual(4, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"Index", do_nothing},
        {"MaxLength", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("fragment_shader", 1, ShaderTypeMap)),
    s178_assert_nif(
        Function,
        Command,
        4,
        [
            {"Program", s178_uint_nif_data()},
            {"ShaderType", s178_enum_nif_data()},
            {"Index", s178_uint_nif_data()},
            {"MaxLength", out_gl_string}
        ]
    ).

s178_assert_uniform_subroutines(Function) ->
    ?assertEqual("glUniformSubroutinesuiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "Indices", gl_uint_list_with_count}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"ShaderType", {undefined, shader_type, []}},
            {"Indices", {list, {gl, uint, []}}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([], maps:get(specs_return, Function)),
    ?assertEqual(2, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"Indices", gl_uint_list_with_count}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("fragment_shader", 1, ShaderTypeMap)),
    s178_assert_nif(
        Function,
        "glUniformSubroutinesuiv",
        2,
        [
            {"ShaderType", s178_enum_nif_data()},
            {"Indices", in_gl_uint_list_with_count}
        ]
    ).

s178_assert_get_uniform_subroutine(Function) ->
    ?assertEqual("glGetUniformSubroutineuiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "Location", gl_int},
            {out, "Values", {typed_value_list, "Count", gl_uint}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"ShaderType", {undefined, shader_type, []}},
            {"Location", {gl, int, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, uint, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(3, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"Location", do_nothing},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("fragment_shader", 1, ShaderTypeMap)),
    s178_assert_nif(
        Function,
        "glGetUniformSubroutineuiv",
        3,
        [
            {"ShaderType", s178_enum_nif_data()},
            {"Location", s178_int_nif_data()},
            {"Values", {out_typed_value_list, "GLuint", "enif_make_uint"}}
        ]
    ).

s178_assert_get_program_stage(Function) ->
    ?assertEqual("glGetProgramStageiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "ParamName", {gl_enum, "ProgramStagePName", program_stage_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"ShaderType", {undefined, shader_type, []}},
            {"ParamName", {undefined, program_stage_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(4, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("fragment_shader", 1, ShaderTypeMap)),
    ?assert(lists:keymember("active_subroutine_uniforms", 1, ParamNameMap)),
    s178_assert_nif(
        Function,
        "glGetProgramStageiv",
        4,
        [
            {"Program", s178_uint_nif_data()},
            {"ShaderType", s178_enum_nif_data()},
            {"ParamName", s178_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ]
    ).

s178_assert_nif(Function, Command, Arity, Params) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s178_assert_emitted_present(Target) ->
    s178_assert_emitted_surface(
        Target,
        [
            <<"-export([get_active_subroutine_uniform/5]).">>,
            <<"-export([get_active_subroutine_uniform_name/4]).">>,
            <<"-export([get_active_subroutine_name/4]).">>,
            <<"-export([uniform_subroutines/2]).">>,
            <<"-export([get_uniform_subroutine/3]).">>,
            <<"-export([get_program_stage/4]).">>,
            <<"-export_type([subroutine_parameter_name/0]).">>,
            <<"-export_type([program_stage_parameter_name/0]).">>,
            <<"get_active_subroutine_uniform(Program, ShaderType, Index, ParamName, Count) ->">>,
            <<"get_active_subroutine_uniform_name(Program, ShaderType, Index, MaxLength) ->">>,
            <<"get_active_subroutine_name(Program, ShaderType, Index, MaxLength) ->">>,
            <<"uniform_subroutines(ShaderType, Indices) ->">>,
            <<"get_uniform_subroutine(ShaderType, Location, Count) ->">>,
            <<"get_program_stage(Program, ShaderType, ParamName, Count) ->">>,
            <<"NewIndices = case Indices of">>,
            <<"?CALL_RAW_FUNC(glUniformSubroutinesuiv_raw(NewShaderType, NewIndices)).">>
        ],
        [
            <<"glGetActiveSubroutineUniformiv(arg_0, arg_1, arg_2, arg_3, arg_4_values);">>,
            <<"glGetActiveSubroutineUniformName(arg_0, arg_1, arg_2, arg_3_max_length, &arg_3_length, arg_3_info_log);">>,
            <<"glGetActiveSubroutineName(arg_0, arg_1, arg_2, arg_3_max_length, &arg_3_length, arg_3_info_log);">>,
            <<"GLsizei arg_1_count = (GLsizei)arg_1_count_tmp;">>,
            <<"glUniformSubroutinesuiv(arg_0, arg_1_count, (const GLuint*)arg_1.data);">>,
            <<"glGetUniformSubroutineuiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetProgramStageiv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetActiveSubroutineUniformiv_raw\", 5, nif_glGetActiveSubroutineUniformiv, 0}">>,
            <<"{\"glGetActiveSubroutineUniformName_raw\", 4, nif_glGetActiveSubroutineUniformName, 0}">>,
            <<"{\"glGetActiveSubroutineName_raw\", 4, nif_glGetActiveSubroutineName, 0}">>,
            <<"{\"glUniformSubroutinesuiv_raw\", 2, nif_glUniformSubroutinesuiv, 0}">>,
            <<"{\"glGetUniformSubroutineuiv_raw\", 3, nif_glGetUniformSubroutineuiv, 0}">>,
            <<"{\"glGetProgramStageiv_raw\", 4, nif_glGetProgramStageiv, 0}">>
        ],
        s178_forbidden_needles()
    ).

s178_assert_emitted_absent(Target) ->
    s178_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_active_subroutine_uniform">>,
            <<"get_active_subroutine_uniform_name">>,
            <<"get_active_subroutine_name">>,
            <<"uniform_subroutines">>,
            <<"get_uniform_subroutine">>,
            <<"get_program_stage">>
            | [list_to_binary(Command) || Command <- s178_commands()]
        ] ++ s178_forbidden_needles()
    ).

s178_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard178-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(
        api_name,
        generator_test_support:target_name(Target),
        generator_test_support:resolve_target(Target)
    ),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s178_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s178_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s178_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s178_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s178_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s178_deferred_neighbor_commands()
    ].

s178_deferred_neighbor_commands() ->
    [
        "glGetSynciv",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glGetPointerv",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv"
    ].

s178_commands() ->
    [
        "glGetActiveSubroutineUniformiv",
        "glGetActiveSubroutineUniformName",
        "glGetActiveSubroutineName",
        "glUniformSubroutinesuiv",
        "glGetUniformSubroutineuiv",
        "glGetProgramStageiv"
    ].

s178_forbidden_needles() ->
    [list_to_binary(Command) || Command <- s178_deferred_neighbor_commands()].

s178_assert_enum_contains(BindingData, EnumType, Atom) ->
    ?assert(lists:member(Atom, maps:get(atom_to_list(EnumType), maps:get(enum_types, BindingData)))).

s178_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s178_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s178_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s178_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s178_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s178_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 182.

s182_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s182_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s182_emitter_shader_utility_test_() ->
    [
        {"gl 4.6", fun() -> s182_assert_emitted_gl46() end},
        {"gl 4.1", fun() -> s182_assert_emitted_gl41() end},
        {"gl 3.3", fun() -> s182_assert_emitted_gl33() end},
        {"gles 3.2", fun() -> s182_assert_emitted_es32() end},
        {"gles 3.0", fun() -> s182_assert_emitted_es30() end},
        {"gles 2.0", fun() -> s182_assert_emitted_es20() end}
    ].

s182_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s182_supports_shader_precision(Target) of
        true ->
            s182_assert_shader_precision_format(maps:get({"get_shader_precision_format", 2}, Functions)),
            s182_assert_enum_contains(BindingData, shader_precision_type, "high_float"),
            s182_assert_enum_contains(BindingData, shader_precision_type, "medium_int");
        false ->
            s182_assert_shader_precision_format_absent(Functions)
    end,
    case s182_supports_create_shader_program(Target) of
        true ->
            s182_assert_create_shader_program(maps:get({"create_shader_program", 2}, Functions));
        false ->
            s182_assert_create_shader_program_absent(Functions)
    end,
    case s182_supports_shader_precision(Target) orelse s182_supports_create_shader_program(Target) of
        true -> s182_assert_enum_contains(BindingData, shader_type, "fragment_shader");
        false -> ok
    end,
    s182_assert_deferred_neighbors_absent(Functions).

s182_supports_shader_precision({gl, {4, Minor}}) when Minor >= 1 -> true;
s182_supports_shader_precision({gl, {Major, _Minor}}) when Major > 4 -> true;
s182_supports_shader_precision({gles, _Version}) -> true;
s182_supports_shader_precision(_) -> false.

s182_supports_create_shader_program({gl, {4, Minor}}) when Minor >= 1 -> true;
s182_supports_create_shader_program({gl, {Major, _Minor}}) when Major > 4 -> true;
s182_supports_create_shader_program({gles, {3, Minor}}) when Minor >= 1 -> true;
s182_supports_create_shader_program(_) -> false.

s182_assert_shader_precision_format(Function) ->
    ?assertEqual("glGetShaderPrecisionFormat", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "PrecisionType", {gl_enum, "PrecisionType", shader_precision_type}},
            {out, "Precision", shader_precision_format}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"ShaderType", {undefined, shader_type, []}},
            {"PrecisionType", {undefined, shader_precision_type, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual(
        [
            {"RangeMin", {gl, int, []}},
            {"RangeMax", {gl, int, []}},
            {"Precision", {gl, int, []}}
        ],
        maps:get(specs_return, Function)
    ),
    ?assertEqual(2, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"PrecisionType", {gl_enum_to_uint, PrecisionTypeMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("fragment_shader", 1, ShaderTypeMap)),
    ?assert(lists:keymember("high_float", 1, PrecisionTypeMap)),
    s182_assert_nif(
        Function,
        "glGetShaderPrecisionFormat",
        2,
        [
            {"ShaderType", s182_enum_nif_data()},
            {"PrecisionType", s182_enum_nif_data()},
            {"Precision", out_shader_precision_format}
        ],
        void
    ).

s182_assert_shader_precision_format_absent(Functions) ->
    ?assertNot(maps:is_key({"get_shader_precision_format", 2}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetShaderPrecisionFormat", Functions)).

s182_assert_create_shader_program(Function) ->
    ?assertEqual("glCreateShaderProgramv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "ShaderType", {gl_enum, "ShaderType"}},
            {in, "Sources", list_gl_strings_null_terminated}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"ShaderType", {undefined, shader_type, []}},
            {"Sources", {list, {undefined, iodata, []}}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Program", {undefined, program, []}}], maps:get(specs_return, Function)),
    ?assertEqual(2, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"ShaderType", {gl_enum_to_uint, ShaderTypeMap}},
        {"Sources", normalize_list_strings_or_binary}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("vertex_shader", 1, ShaderTypeMap)),
    s182_assert_nif(
        Function,
        "glCreateShaderProgramv",
        2,
        [
            {"ShaderType", s182_enum_nif_data()},
            {"Sources", in_list_gl_strings_null_terminated}
        ],
        gluint_to_integer
    ).

s182_assert_create_shader_program_absent(Functions) ->
    ?assertNot(maps:is_key({"create_shader_program", 2}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glCreateShaderProgramv", Functions)).

s182_assert_nif(Function, Command, Arity, Params, Return) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(Return, maps:get(return, NifData)).

s182_assert_emitted_gl46() ->
    s182_assert_emitted_present({gl, {4, 6}}).

s182_assert_emitted_gl41() ->
    s182_assert_emitted_present({gl, {4, 1}}).

s182_assert_emitted_present(Target) ->
    s182_assert_emitted_surface(
        Target,
        s182_shader_precision_needles() ++ s182_create_shader_program_needles(),
        s182_shader_precision_c_needles() ++ s182_create_shader_program_c_needles(),
        s182_forbidden_needles()
    ).

s182_assert_emitted_gl33() ->
    s182_assert_emitted_surface(
        {gl, {3, 3}},
        [],
        [],
        s182_shader_precision_absent_needles() ++ s182_create_shader_program_absent_needles() ++ s182_forbidden_needles()
    ).

s182_assert_emitted_es32() ->
    s182_assert_emitted_present({gles, {3, 2}}).

s182_assert_emitted_es30() ->
    s182_assert_emitted_precision_only({gles, {3, 0}}).

s182_assert_emitted_es20() ->
    s182_assert_emitted_precision_only({gles, {2, 0}}).

s182_assert_emitted_precision_only(Target) ->
    s182_assert_emitted_surface(
        Target,
        s182_shader_precision_needles(),
        s182_shader_precision_c_needles(),
        s182_create_shader_program_absent_needles() ++ s182_forbidden_needles()
    ).

s182_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard182-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s182_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s182_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s182_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s182_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s182_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s182_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s182_shader_precision_needles() ->
    [
        <<"-export([get_shader_precision_format/2]).">>,
        <<"-export_type([shader_precision_type/0]).">>,
        <<"-spec get_shader_precision_format(\n    ShaderType :: shader_type(),\n    PrecisionType :: shader_precision_type()\n) -> {ok, RangeMin :: gl:int(), RangeMax :: gl:int(), Precision :: gl:int()} | {error, atom()}.">>,
        <<"get_shader_precision_format(ShaderType, PrecisionType) ->">>,
        <<"?CALL_RAW_FUNC(glGetShaderPrecisionFormat_raw(NewShaderType, NewPrecisionType)).">>
    ].

s182_create_shader_program_needles() ->
    [
        <<"-export([create_shader_program/2]).">>,
        <<"-spec create_shader_program(\n    ShaderType :: shader_type(),\n    Sources :: [iodata()]\n) -> {ok, Program :: program()} | {error, atom()}.">>,
        <<"create_shader_program(ShaderType, Sources) ->">>,
        <<"SourcesNew = lists:map(fun">>,
        <<"?CALL_RAW_FUNC(glCreateShaderProgramv_raw(NewShaderType, SourcesNew)).">>
    ].

s182_shader_precision_c_needles() ->
    [
        <<"GLint arg_2_range[2] = {0, 0};">>,
        <<"GLint arg_2_precision = 0;">>,
        <<"glGetShaderPrecisionFormat(arg_0, arg_1, arg_2_range, &arg_2_precision);">>,
        <<"ERL_NIF_TERM arg_2_range_min_ret = enif_make_int(env, arg_2_range[0]);">>,
        <<"{\"glGetShaderPrecisionFormat_raw\", 2, nif_glGetShaderPrecisionFormat, 0}">>
    ].

s182_create_shader_program_c_needles() ->
    [
        <<"GLchar** arg_1_strings = enif_alloc(sizeof(GLchar*) * arg_1_count);">>,
        <<"GLuint ret = glCreateShaderProgramv(arg_0, (GLsizei)arg_1_count, (const GLchar* const*)arg_1_strings);">>,
        <<"enif_free(arg_1_strings);">>,
        <<"{\"glCreateShaderProgramv_raw\", 2, nif_glCreateShaderProgramv, 0}">>
    ].

s182_shader_precision_absent_needles() ->
    [
        <<"-export([get_shader_precision_format/2]).">>,
        <<"glGetShaderPrecisionFormat(">>,
        <<"{\"glGetShaderPrecisionFormat_raw\", 2, nif_glGetShaderPrecisionFormat, 0}">>
    ].

s182_create_shader_program_absent_needles() ->
    [
        <<"-export([create_shader_program/2]).">>,
        <<"glCreateShaderProgramv(">>,
        <<"{\"glCreateShaderProgramv_raw\", 2, nif_glCreateShaderProgramv, 0}">>
    ].

s182_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glCopyImageSubDataEXT",
            "glCopyImageSubDataNV",
            "glCopyImageSubDataOES"
        ]
    ].

s182_forbidden_needles() ->
    [
        <<"glCopyImageSubDataEXT(">>,
        <<"glCopyImageSubDataNV(">>,
        <<"glCopyImageSubDataOES(">>
    ].

s182_assert_enum_contains(BindingData, TypeName, AtomName) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(atom_to_list(TypeName), EnumTypes)),
    ?assert(lists:member(AtomName, maps:get(atom_to_list(TypeName), EnumTypes))).

s182_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s182_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s182_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s182_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 183.

s183_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s183_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s183_emitter_program_pipeline_query_test_() ->
    [
        {"gl 4.6", fun() -> s183_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s183_assert_emitted_present({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s183_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s183_assert_emitted_present({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s183_assert_emitted_present({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s183_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s183_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s183_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s183_supports_program_pipeline_query(Target) of
        true ->
            s183_assert_program_pipeline_query(maps:get({"get_program_pipeline", 3}, Functions)),
            s183_assert_enum_contains(BindingData, program_pipeline_parameter_name, "active_program"),
            s183_assert_enum_contains(BindingData, program_pipeline_parameter_name, "info_log_length"),
            s183_assert_enum_contains(BindingData, program_pipeline_parameter_name, "validate_status");
        false ->
            s183_assert_program_pipeline_query_absent(Functions)
    end,
    s183_assert_fixed_semantic_wrappers_removed(Functions),
    ?assertNot(maps:is_key({"get_parameter", 3}, Functions)),
    s183_assert_deferred_neighbors_absent(Functions).

s183_supports_program_pipeline_query({gl, {4, Minor}}) when Minor >= 1 -> true;
s183_supports_program_pipeline_query({gl, {Major, _Minor}}) when Major > 4 -> true;
s183_supports_program_pipeline_query({gles, {3, Minor}}) when Minor >= 1 -> true;
s183_supports_program_pipeline_query({gles, {Major, _Minor}}) when Major > 3 -> true;
s183_supports_program_pipeline_query(_) -> false.

s183_assert_program_pipeline_query(Function) ->
    ?assertEqual("glGetProgramPipelineiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Pipeline", {gl_object, program_pipeline}},
            {in, "ParamName", {gl_enum, "PipelineParameterName", program_pipeline_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Pipeline", {undefined, program_pipeline, []}},
            {"ParamName", {undefined, program_pipeline_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(3, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    ?assertEqual("glGetProgramPipelineivValues", maps:get(raw_function, Clause)),
    [
        {"Pipeline", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("active_program", 1, ParamNameMap)),
    ?assert(lists:keymember("info_log_length", 1, ParamNameMap)),
    s183_assert_nif(
        maps:get("glGetProgramPipelineivValues", maps:get(nif_functions, Function)),
        "glGetProgramPipelineiv",
        3,
        [
            {"Pipeline", s183_uint_nif_data()},
            {"ParamName", s183_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ]
    ).

s183_assert_program_pipeline_query_absent(Functions) ->
    ?assertNot(maps:is_key({"get_program_pipeline", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetProgramPipelineiv", Functions)).

s183_assert_fixed_semantic_wrappers_removed(Functions) ->
    ?assertNot(maps:is_key({"get_program_pipeline_validation_status", 1}, Functions)),
    ?assertNot(maps:is_key({"get_program_pipeline_info_log_length", 1}, Functions)).

s183_assert_nif(NifData, Command, Arity, Params) ->
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s183_assert_emitted_present(Target) ->
    s183_assert_emitted_surface(
        Target,
        [
            <<"-export([get_program_pipeline/3]).">>,
            <<"-export_type([program_pipeline_parameter_name/0]).">>,
            <<"-spec get_program_pipeline(\n    Pipeline :: program_pipeline(),\n    ParamName :: program_pipeline_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>,
            <<"get_program_pipeline(Pipeline, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetProgramPipelineivValues_raw(Pipeline, NewParamName, Count)).">>,
            <<"validate_status -> ?GL_VALIDATE_STATUS">>
        ],
        [
            <<"glGetProgramPipelineiv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetProgramPipelineivValues_raw\", 3, nif_glGetProgramPipelineivValues, 0}">>
        ],
        [
            <<"-export([get_program_pipeline_validation_status/1]).">>,
            <<"-export([get_program_pipeline_info_log_length/1]).">>,
            <<"glGetProgramPipelineivInteger_raw">>
         | s183_forbidden_needles()
        ]
    ).

s183_assert_emitted_absent(Target) ->
    s183_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"-export([get_program_pipeline/3]).">>,
            <<"-export_type([program_pipeline_parameter_name/0]).">>,
            <<"glGetProgramPipelineivValues">>,
            <<"glGetProgramPipelineiv(arg_0, arg_1, arg_2_values);">>
         | s183_forbidden_needles()
        ]
    ).

s183_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard183-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s183_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s183_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s183_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s183_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s183_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s183_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s183_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glCopyImageSubDataEXT",
            "glCopyImageSubDataNV",
            "glCopyImageSubDataOES"
        ]
    ].

s183_forbidden_needles() ->
    [
        <<"glCopyImageSubDataEXT(">>,
        <<"glCopyImageSubDataNV(">>,
        <<"glCopyImageSubDataOES(">>,
        <<"get_parameter(">>
    ].

s183_assert_enum_contains(BindingData, TypeName, AtomName) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(atom_to_list(TypeName), EnumTypes)),
    ?assert(lists:member(AtomName, maps:get(atom_to_list(TypeName), EnumTypes))).

s183_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s183_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s183_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s183_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s183_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 192.

s192_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s192_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s192_emitter_debug_log_test_() ->
    [
        {"gl 4.6", fun() -> s192_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s192_assert_emitted_absent({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s192_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s192_assert_emitted_present({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s192_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s192_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s192_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s192_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s192_supports_debug_log(Target) of
        true ->
            s192_assert_enum_contains(BindingData, "debug_source", "debug_source_application"),
            s192_assert_enum_contains(BindingData, "debug_type", "debug_type_marker"),
            s192_assert_enum_contains(BindingData, "debug_severity", "debug_severity_notification"),
            s192_assert_path(maps:get({"get_debug_message_log", 2}, Functions));
        false ->
            ?assertNot(maps:is_key({"get_debug_message_log", 2}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetDebugMessageLog", Functions))
    end,
    s192_assert_deferred_neighbors_absent(Functions).

s192_supports_debug_log({gl, {4, 6}}) ->
    true;
s192_supports_debug_log({gles, {3, 2}}) ->
    true;
s192_supports_debug_log(_) ->
    false.

s192_assert_path(FunctionData) ->
    ?assertEqual("glGetDebugMessageLog", maps:get(gl_command, FunctionData)),
    ?assertEqual(s192_params_specs(), maps:get(params_specs, FunctionData)),
    ?assertEqual({"MessagesRead", debug_message_log_count}, maps:get(return_specs, FunctionData)),
    ?assertEqual(s192_specs_params(), maps:get(specs_params, FunctionData)),
    ?assertEqual(s192_specs_return(), maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glGetDebugMessageLog", maps:get(raw_function, Clause)),
    ?assertEqual(
        [{"MaxMessages", do_nothing}, {"MessageLogSize", do_nothing}],
        maps:get(params, Clause)
    ),

    NifData = maps:get("glGetDebugMessageLog", maps:get(nif_functions, FunctionData)),
    ?assertEqual("glGetDebugMessageLog", maps:get(gl_command, NifData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(debug_message_log_count, maps:get(return, NifData)),
    s192_assert_nif_params(maps:get(params, NifData)).

s192_params_specs() ->
    [
        {in, "MaxMessages", gl_uint_positive},
        {out, "Messages", {debug_message_log, "MaxMessages", "MessageLogSize"}}
    ].

s192_specs_params() ->
    [
        {"MaxMessages", {undefined, pos_integer, []}},
        {"MessageLogSize", {undefined, pos_integer, []}}
    ].

s192_specs_return() ->
    [
        {
            "Messages",
            {
                list,
                {tuple, [
                    {undefined, debug_source, []},
                    {undefined, debug_type, []},
                    {gl, uint, []},
                    {undefined, debug_severity, []},
                    {undefined, binary, []}
                ]}
            }
        }
    ].

s192_assert_nif_params([
    {"MaxMessages", {"gl_type", _}},
    {"Messages", {
        out_debug_message_log,
        "MaxMessages",
        SourceMap,
        TypeMap,
        SeverityMap
    }}
]) ->
    ?assert(lists:keymember("GL_DEBUG_SOURCE_APPLICATION", 1, SourceMap)),
    ?assert(lists:keymember("GL_DEBUG_TYPE_MARKER", 1, TypeMap)),
    ?assert(lists:keymember("GL_DEBUG_SEVERITY_NOTIFICATION", 1, SeverityMap));
s192_assert_nif_params([
    {"MaxMessages", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
    {"Messages", {
        out_debug_message_log,
        "MaxMessages",
        SourceMap,
        TypeMap,
        SeverityMap
    }}
]) ->
    ?assert(lists:keymember("GL_DEBUG_SOURCE_APPLICATION", 1, SourceMap)),
    ?assert(lists:keymember("GL_DEBUG_TYPE_MARKER", 1, TypeMap)),
    ?assert(lists:keymember("GL_DEBUG_SEVERITY_NOTIFICATION", 1, SeverityMap)).

s192_assert_emitted_present(Target) ->
    {Erl, C} = s192_generate_surface(Target),
    s192_assert_contains(Erl, <<"-export([get_debug_message_log/2]).">>),
    s192_assert_contains(Erl, <<"MaxMessages :: pos_integer()">>),
    s192_assert_contains(Erl, <<"MessageLogSize :: pos_integer()">>),
    s192_assert_contains(
        Erl,
        <<"Messages :: [{debug_source(), debug_type(), gl:uint(), debug_severity(), binary()}]">>
    ),
    s192_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetDebugMessageLog_raw(MaxMessages, MessageLogSize))">>),
    s192_assert_contains(
        C,
        <<"GLuint ret = glGetDebugMessageLog(arg_0, arg_1_size, arg_1_sources, arg_1_types, arg_1_ids, arg_1_severities, arg_1_lengths, arg_1_message_log);">>
    ),
    s192_assert_contains(C, <<"case GL_DEBUG_SOURCE_APPLICATION: arg_1_source_ret = beam_atom_debug_source_application; break;">>),
    s192_assert_contains(C, <<"case GL_DEBUG_TYPE_MARKER: arg_1_type_ret = beam_atom_debug_type_marker; break;">>),
    s192_assert_contains(C, <<"case GL_DEBUG_SEVERITY_NOTIFICATION: arg_1_severity_ret = beam_atom_debug_severity_notification; break;">>),
    s192_assert_contains(C, <<"arg_1_message_log[arg_1_offset + arg_1_message_length - 1] == '\\0'">>),
    s192_assert_contains(C, <<"{\"glGetDebugMessageLog_raw\", 2, nif_glGetDebugMessageLog, 0}">>),
    s192_assert_deferred_emitted_absent(Erl, C).

s192_assert_emitted_absent(Target) ->
    {Erl, C} = s192_generate_surface(Target),
    s192_assert_not_contains(Erl, <<"-export([get_debug_message_log/2]).">>),
    s192_assert_not_contains(C, <<"glGetDebugMessageLog(">>),
    s192_assert_deferred_emitted_absent(Erl, C).

s192_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard192-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s192_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        {Erl, C}
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s192_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s192_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s192_deferred_neighbor_commands()
    ].

s192_assert_deferred_emitted_absent(Erl, C) ->
    [
        s192_assert_not_contains(Erl, Needle)
     || Needle <- [
            <<"-export([debug_message_callback/">>
        ]
    ],
    [
        s192_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glDebugMessageCallback(">>,
            <<"glObjectPtrLabel(">>,
            <<"glGetObjectPtrLabel(">>,
            <<"glGetPointerv(">>
        ]
    ].

s192_deferred_neighbor_commands() ->
    [
        "glDebugMessageCallback",
        "glObjectPtrLabel",
        "glGetObjectPtrLabel",
        "glGetPointerv"
    ].

s192_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s192_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s192_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s192_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
