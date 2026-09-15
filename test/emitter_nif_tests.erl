-module(emitter_nif_tests).
-include_lib("eunit/include/eunit.hrl").

%% NIF and C emitter surface contracts.

egl_nif_loader_emitter_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-egl-loader-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(
        api_name,
        "OpenGL ES 3.1",
        generator_test_support:resolve_target({gles, {3, 1}})
    ),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gles, {3, 1}}, BindingData),
        gl_nif_module_generator:generate({gles, {3, 1}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"EGLNifLocation = filename:join(EGLPrivDir, \"beam-egl\") ++">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"{win32, _} -> \".dll\";">>)
        ),
        ?assertEqual(nomatch, binary:match(GeneratedErl, <<"\"beam-egl\") ++ \".so\"">>)),
        ?assertMatch({_, _}, binary:match(GeneratedC, <<"#if defined(_WIN32)">>)),
        ?assertMatch({_, _}, binary:match(GeneratedC, <<"#include <windows.h>">>)),
        ?assertMatch({_, _}, binary:match(GeneratedC, <<"LoadLibraryA(beam_egl_so_path)">>)),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"GetProcAddress(\n    (HMODULE)egl_nif_lib_handle, \"egl_execute_command\")">>)
        ),
        ?assertMatch({_, _}, binary:match(GeneratedC, <<"const ERL_NIF_TERM argv[]">>)),
        ?assertEqual(nomatch, binary:match(GeneratedC, <<"#include <pthread.h>">>)),
        ?assertEqual(nomatch, binary:match(GeneratedC, <<"ERL_NIF_TERM* []">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

%% Historical shard 130.
s130_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s130_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s130_emitter_buffer_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s130_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([get_buffer_sub_data/3]).">>,
                    <<"-export([get_named_buffer_sub_data/3]).">>,
                    <<"-spec get_buffer_sub_data(\n    Target :: buffer_target(),\n    Offset :: gl:intptr(),\n    Size :: non_neg_integer()\n) -> {ok, Data :: binary()} | {error, atom()}.">>,
                    <<"-spec get_named_buffer_sub_data(\n    Buffer :: buffer(),\n    Offset :: gl:intptr(),\n    Size :: non_neg_integer()\n) -> {ok, Data :: binary()} | {error, atom()}.">>,
                    <<"?CALL_RAW_FUNC(glGetBufferSubData_raw(NewTarget, Offset, Size)).">>,
                    <<"?CALL_RAW_FUNC(glGetNamedBufferSubData_raw(Buffer, Offset, Size)).">>
                ],
                [
                    <<"if (arg_2_size > (ErlNifUInt64)PTRDIFF_MAX) {">>,
                    <<"unsigned char* arg_2_bin = enif_make_new_binary(env, arg_2_size, &arg_2_term);">>,
                    <<"if (arg_2_bin == NULL && arg_2_size > 0) {">>,
                    <<"glGetBufferSubData(arg_0, arg_1, (GLsizeiptr)arg_2_size, arg_2_bin);">>,
                    <<"glGetNamedBufferSubData(arg_0, arg_1, (GLsizeiptr)arg_2_size, arg_2_bin);">>,
                    <<"return enif_make_tuple(env, 1,\n        arg_2_term\n    );">>
                ],
                [
                    <<"-export([map_buffer/2]).">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s130_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [],
                [],
                [
                    <<"-export([get_buffer_sub_data/3]).">>,
                    <<"-export([get_named_buffer_sub_data/3]).">>,
                    <<"glGetBufferSubData(">>,
                    <<"glGetNamedBufferSubData(">>
                ]
            )
        end}
    ].

s130_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s130_assert_presence(s130_supports_get_buffer_sub_data(Target), {"get_buffer_sub_data", 3}, Functions),
    s130_assert_presence(s130_supports_get_named_buffer_sub_data(Target), {"get_named_buffer_sub_data", 3}, Functions),

    case s130_supports_get_buffer_sub_data(Target) of
        true ->
            s130_assert_enum_contains(BindingData, "buffer_target", "array_buffer"),
            s130_assert_get_buffer_sub_data(maps:get({"get_buffer_sub_data", 3}, Functions));
        false ->
            ?assertNot(generator_test_support:has_gl_command("glGetBufferSubData", Functions))
    end,

    case s130_supports_get_named_buffer_sub_data(Target) of
        true ->
            s130_assert_get_named_buffer_sub_data(maps:get({"get_named_buffer_sub_data", 3}, Functions));
        false ->
            ?assertNot(generator_test_support:has_gl_command("glGetNamedBufferSubData", Functions))
    end,

    s130_assert_deferred_neighbors_absent(Functions).

s130_supports_get_buffer_sub_data({gl, _Version}) ->
    true;
s130_supports_get_buffer_sub_data(_) ->
    false.

s130_supports_get_named_buffer_sub_data({gl, {4, Minor}}) when Minor >= 5 ->
    true;
s130_supports_get_named_buffer_sub_data({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s130_supports_get_named_buffer_sub_data(_) ->
    false.

s130_assert_presence(true, Function, Functions) ->
    ?assert(maps:is_key(Function, Functions));
s130_assert_presence(false, Function, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)).

s130_assert_get_buffer_sub_data(FunctionData) ->
    s130_assert_binary_readback(
        FunctionData,
        "glGetBufferSubData",
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "Offset", gl_intptr},
            {out, "Data", {gl_binary, {explicit, "Size"}}}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"Offset", {gl, intptr, []}},
            {"Size", {undefined, non_neg_integer, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["array_buffer"]}},
            {"Offset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"Target", s130_enum_nif_data()},
            {"Offset", s130_intptr_nif_data()},
            {"Data", out_binary_explicit}
        ]
    ).

s130_assert_get_named_buffer_sub_data(FunctionData) ->
    s130_assert_binary_readback(
        FunctionData,
        "glGetNamedBufferSubData",
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {out, "Data", {gl_binary, {explicit, "Size"}}}
        ],
        [
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Size", {undefined, non_neg_integer, []}}
        ],
        [
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"Buffer", s130_uint_nif_data()},
            {"Offset", s130_intptr_nif_data()},
            {"Data", out_binary_explicit}
        ]
    ).

s130_assert_binary_readback(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([{"Data", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s130_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s130_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s130_assert_clause_param/1, lists:zip(Expected, Actual)).

s130_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s130_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s130_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glMapBuffer",
        "glMapBufferRange",
        "glGetBufferPointerv",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glGetNamedBufferPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s130_assert_enum_contains(BindingData, EnumName, Value) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumName, EnumTypes)),
    ?assert(lists:member(Value, maps:get(EnumName, EnumTypes))).

s130_assert_emitted_surface(Target, ApiName, ExpectedErl, ExpectedC, ForbiddenNeedles) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard130-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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

s130_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s130_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s130_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 131.
s131_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s131_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s131_emitter_debug_control_test_() ->
    [
        {"gl 4.6", fun() ->
            s131_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([debug_message_control/5]).">>,
                    <<"Ids :: all | [gl:uint()]">>,
                    <<"{Count, NewIds} = case Ids of">>,
                    <<"all ->">>,
                    <<"{0, undefined}">>,
                    <<"[_ | _] ->">>,
                    <<"?CALL_RAW_FUNC(glDebugMessageControl_raw(NewSource, NewType, NewSeverity, Count, NewIds, Enabled)).">>
                ],
                [
                    <<"const void* arg_4 = NULL;">>,
                    <<"if (enif_is_identical(argv[4], enif_make_atom(env, \"undefined\"))) {">>,
                    <<"else if (enif_inspect_binary(env, argv[4], &arg_4_bin)) {">>,
                    <<"glDebugMessageControl(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>
                ],
                s131_forbidden_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s131_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([debug_message_control/5]).">>,
                    <<"glDebugMessageControl_raw(NewSource, NewType, NewSeverity, Count, NewIds, Enabled)">>
                ],
                [
                    <<"glDebugMessageControl(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>
                ],
                s131_forbidden_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s131_assert_emitted_surface(
                {gl, {4, 1}},
                "OpenGL 4.1",
                [],
                [],
                [<<"debug_message_control">>, <<"glDebugMessageControl">>]
            )
        end},
        {"gles 3.1", fun() ->
            s131_assert_emitted_surface(
                {gles, {3, 1}},
                "OpenGL ES 3.1",
                [],
                [],
                [<<"debug_message_control">>, <<"glDebugMessageControl">>]
            )
        end}
    ].

s131_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    case s131_supports_debug_message_control(Target) of
        true ->
            ?assert(maps:is_key({"debug_message_control", 5}, Functions)),
            s131_assert_enum_contains(BindingData, "debug_source", "debug_source_application"),
            s131_assert_enum_contains(BindingData, "debug_source", "dont_care"),
            s131_assert_enum_contains(BindingData, "debug_type", "debug_type_marker"),
            s131_assert_enum_contains(BindingData, "debug_type", "dont_care"),
            s131_assert_enum_contains(BindingData, "debug_severity", "debug_severity_notification"),
            s131_assert_enum_contains(BindingData, "debug_severity", "dont_care"),
            s131_assert_debug_message_control(maps:get({"debug_message_control", 5}, Functions));
        false ->
            ?assertNot(maps:is_key({"debug_message_control", 5}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glDebugMessageControl", Functions))
    end,

    ?assertNot(maps:is_key({"debug_message_control", 6}, Functions)),
    s131_assert_deferred_neighbors_absent(Functions).

s131_supports_debug_message_control({gl, {4, 6}}) ->
    true;
s131_supports_debug_message_control({gles, {3, 2}}) ->
    true;
s131_supports_debug_message_control(_) ->
    false.

s131_assert_debug_message_control(FunctionData) ->
    ?assertEqual("glDebugMessageControl", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Source", {gl_enum, "DebugSource"}},
            {in, "Type", {gl_enum, "DebugType"}},
            {in, "Severity", {gl_enum, "DebugSeverity"}},
            {in, "Ids", {counted_list_or_all, "Count", gl_uint}},
            {in, "Enabled", gl_bool}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Source", {undefined, debug_source, []}},
            {"Type", {undefined, debug_type, []}},
            {"Severity", {undefined, debug_severity, []}},
            {"Ids", {set, [all, {list, {gl, uint, []}}]}},
            {"Enabled", {gl, boolean, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(5, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s131_assert_enum_param("Source", ["debug_source_application", "dont_care"], Params),
    s131_assert_enum_param("Type", ["debug_type_marker", "dont_care"], Params),
    s131_assert_enum_param("Severity", ["debug_severity_notification", "dont_care"], Params),
    ?assert(lists:member({"Ids", {counted_list_or_all_gl_uints_to_binary, "Count"}}, Params)),
    ?assert(lists:member({"Enabled", do_nothing}, Params)),
    ?assertEqual("glDebugMessageControl", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDebugMessageControl", NifFunctions),
    ?assertEqual(6, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Source", s131_enum_nif_data()},
            {"Type", s131_enum_nif_data()},
            {"Severity", s131_enum_nif_data()},
            {"Count", s131_sizei_nif_data()},
            {"Ids", in_gl_binary_or_null},
            {"Enabled", boolean_to_glbool}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s131_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glDebugMessageCallback",
        "glObjectPtrLabel",
        "glGetObjectPtrLabel",
        "glGetPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s131_assert_enum_param(ParamName, Atoms, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- Atoms].

s131_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s131_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard131-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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

s131_forbidden_needles() ->
    [
        <<"debug_message_callback">>,
        <<"object_ptr_label">>,
        <<"get_object_ptr_label">>,
        <<"glDebugMessageCallback">>,
        <<"glObjectPtrLabel">>,
        <<"glGetObjectPtrLabel">>
    ].

s131_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s131_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 132.
s132_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s132_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s132_emitter_transform_feedback_varyings_test_() ->
    [
        {"gl 4.6", fun() ->
            s132_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([transform_feedback_varyings/3]).">>,
                    <<"Varyings :: [iodata()]">>,
                    <<"BufferMode :: transform_feedback_buffer_mode()">>,
                    <<"VaryingsNew = lists:map(fun">>,
                    <<"iolist_to_binary(VaryingsItem)">>,
                    <<"glTransformFeedbackVaryings_raw(Program, VaryingsNew, NewBufferMode)">>
                ],
                [
                    <<"GLchar** arg_1_strings = enif_alloc(sizeof(GLchar*) * arg_1_count);">>,
                    <<"arg_1_string[arg_1_binary.size] = '\\0';">>,
                    <<"glTransformFeedbackVaryings(arg_0, (GLsizei)arg_1_count, (const GLchar* const*)arg_1_strings, arg_2);">>,
                    <<"enif_free(arg_1_strings[arg_1_j]);">>
                ],
                s132_unsupported_needles() ++ s132_no_lengths_call_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s132_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([transform_feedback_varyings/3]).">>,
                    <<"glTransformFeedbackVaryings_raw(Program, VaryingsNew, NewBufferMode)">>
                ],
                [
                    <<"glTransformFeedbackVaryings(arg_0, (GLsizei)arg_1_count, (const GLchar* const*)arg_1_strings, arg_2);">>
                ],
                s132_unsupported_needles() ++ s132_no_lengths_call_needles()
            )
        end},
        {"gles 2.0", fun() ->
            s132_assert_emitted_surface(
                {gles, {2, 0}},
                [],
                [],
                [<<"transform_feedback_varyings">>, <<"glTransformFeedbackVaryings">>]
            )
        end}
    ].

s132_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    case s132_supports_transform_feedback_varyings(Target) of
        true ->
            ?assert(maps:is_key({"transform_feedback_varyings", 3}, Functions)),
            ?assertNot(maps:is_key({"transform_feedback_varyings", 4}, Functions)),
            s132_assert_enum_contains(BindingData, "transform_feedback_buffer_mode", "interleaved_attribs"),
            s132_assert_enum_contains(BindingData, "transform_feedback_buffer_mode", "separate_attribs"),
            s132_assert_transform_feedback_varyings(maps:get({"transform_feedback_varyings", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"transform_feedback_varyings", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glTransformFeedbackVaryings", Functions))
    end,

    s132_assert_deferred_neighbors_absent(Functions).

s132_supports_transform_feedback_varyings({gles, {2, 0}}) ->
    false;
s132_supports_transform_feedback_varyings(_) ->
    true.

s132_assert_transform_feedback_varyings(FunctionData) ->
    ?assertEqual("glTransformFeedbackVaryings", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Varyings", list_gl_strings_null_terminated},
            {in, "BufferMode", {gl_enum, "TransformFeedbackBufferMode"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Varyings", {list, {undefined, iodata, []}}},
            {"BufferMode", {undefined, transform_feedback_buffer_mode, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assert(lists:member({"Program", do_nothing}, Params)),
    ?assert(lists:member({"Varyings", normalize_list_strings_or_binary}, Params)),
    s132_assert_enum_param("BufferMode", ["interleaved_attribs", "separate_attribs"], Params),
    ?assertEqual("glTransformFeedbackVaryings", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glTransformFeedbackVaryings", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s132_uint_nif_data()},
            {"Varyings", in_list_gl_strings_null_terminated},
            {"BufferMode", s132_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s132_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s132_assert_enum_param(ParamName, Atoms, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- Atoms].

s132_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s132_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard132-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s132_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s132_assert_contains(C, Needle) || Needle <- RequiredC],
        [s132_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s132_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s132_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s132_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s132_unsupported_needles() ->
    [].

s132_no_lengths_call_needles() ->
    [
        <<"glTransformFeedbackVaryings(arg_0, (GLsizei)arg_1_count, arg_1_strings, arg_1_lengths, arg_2);">>,
        <<"glTransformFeedbackVaryings(arg_0, (GLsizei)arg_1_count, (const GLchar* const*)arg_1_strings, arg_1_lengths, arg_2);">>
    ].

s132_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s132_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 133.
s133_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s133_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s133_emitter_attached_shader_list_readback_test_() ->
    [
        {"gl 4.6", fun() -> s133_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s133_assert_emitted_surface({gles, {3, 2}}) end},
        {"gles 2.0", fun() -> s133_assert_emitted_surface({gles, {2, 0}}) end}
    ].

s133_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_attached_shaders", 2}, Functions)),
    ?assertNot(maps:is_key({"get_attached_shaders", 3}, Functions)),
    s133_assert_get_attached_shaders(maps:get({"get_attached_shaders", 2}, Functions)),
    s133_assert_deferred_neighbors_absent(Functions).

s133_assert_get_attached_shaders(FunctionData) ->
    ?assertEqual("glGetAttachedShaders", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {out, "Shaders", {caller_sized_list, "MaxCount", "Count", {gl_object, shader}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"MaxCount", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Shaders", {list, {undefined, shader, []}}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Program", do_nothing}, {"MaxCount", do_nothing}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetAttachedShaders", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGetAttachedShaders", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s133_uint_nif_data()},
            {"MaxCount", {caller_sized_list, "GLuint", "enif_make_uint"}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s133_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s133_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard133-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        s133_assert_contains(Erl, <<"-export([get_attached_shaders/2]).">>),
        s133_assert_contains(Erl, <<"-spec get_attached_shaders(\n    Program :: program(),\n    MaxCount :: non_neg_integer()\n) -> {ok, Shaders :: [shader()]} | {error, atom()}.">>),
        s133_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetAttachedShaders_raw(Program, MaxCount)).">>),
        s133_assert_contains(C, <<"GLsizei arg_1_count = 0;">>),
        s133_assert_contains(C, <<"GLuint* arg_1 = NULL;">>),
        s133_assert_contains(C, <<"if (arg_1_max > 0) {">>),
        s133_assert_contains(C, <<"glGetAttachedShaders(arg_0, arg_1_max, &arg_1_count, arg_1);">>),
        s133_assert_contains(C, <<"if (arg_1_count > arg_1_max) {">>),
        s133_assert_contains(C, <<"arg_1_ret = enif_make_list_cell(env, enif_make_uint(env, arg_1[i]), arg_1_ret);">>),
        s133_assert_contains(C, <<"{\"glGetAttachedShaders_raw\", 2, nif_glGetAttachedShaders, 0}">>),
        s133_assert_not_contains(Erl, <<"get_attached_shaders/3">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s133_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s133_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s133_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 134.
s134_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s134_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s134_emitter_reflection_metadata_test_() ->
    [
        {"gl 4.6", fun() -> s134_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s134_assert_emitted_surface({gles, {3, 2}}) end},
        {"gles 2.0", fun() -> s134_assert_emitted_surface({gles, {2, 0}}) end}
    ].

s134_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_active_attrib", 3}, Functions)),
    ?assert(maps:is_key({"get_active_uniform", 3}, Functions)),
    ?assertNot(maps:is_key({"get_active_attrib", 4}, Functions)),
    ?assertNot(maps:is_key({"get_active_uniform", 4}, Functions)),
    s134_assert_reflection_function(
        maps:get({"get_active_attrib", 3}, Functions),
        "glGetActiveAttrib",
        "AttributeType",
        attribute_type
    ),
    s134_assert_reflection_function(
        maps:get({"get_active_uniform", 3}, Functions),
        "glGetActiveUniform",
        "UniformType",
        uniform_type
    ),
    s134_assert_enum_contains(BindingData, "attribute_type", "float_vec4"),
    s134_assert_enum_contains(BindingData, "uniform_type", "float_vec4"),
    s134_assert_deferred_neighbors_absent(Functions).

s134_assert_reflection_function(FunctionData, GlCommand, EnumGroup, PublicType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Index", gl_uint},
            {out, "Info", {active_reflection_info, {gl_enum, EnumGroup}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"Index", {gl, uint, []}},
            {"MaxLength", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [
            {"Size", {gl, int, []}},
            {"Type", {undefined, PublicType, []}},
            {"Name", {undefined, binary, []}}
        ],
        maps:get(specs_return, FunctionData)
    ),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Program", do_nothing},
            {"Index", do_nothing},
            {"MaxLength", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    UintNifData = s134_uint_nif_data(),
        [
            {"Program", UintNifData},
            {"Index", UintNifData},
            {"MaxLength", {out_active_reflection_info, "GLint", TypeMap}}
        ] = maps:get(params, NifData),
    ?assert(lists:keymember("GL_FLOAT_VEC4", 1, TypeMap)),
    ?assert(lists:keymember("float_vec4", 2, TypeMap)),
    ?assertEqual(void, maps:get(return, NifData)).

s134_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s134_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard134-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        s134_assert_contains(Erl, <<"-export([get_active_attrib/3]).">>),
        s134_assert_contains(Erl, <<"-export([get_active_uniform/3]).">>),
        s134_assert_contains(Erl, <<"-spec get_active_attrib(\n    Program :: program(),\n    Index :: gl:uint(),\n    MaxLength :: pos_integer()\n) -> {ok, Size :: gl:int(), Type :: attribute_type(), Name :: binary()} | {error, atom()}.">>),
        s134_assert_contains(Erl, <<"-spec get_active_uniform(\n    Program :: program(),\n    Index :: gl:uint(),\n    MaxLength :: pos_integer()\n) -> {ok, Size :: gl:int(), Type :: uniform_type(), Name :: binary()} | {error, atom()}.">>),
        s134_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetActiveAttrib_raw(Program, Index, MaxLength)).">>),
        s134_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetActiveUniform_raw(Program, Index, MaxLength)).">>),
        s134_assert_not_contains(Erl, <<"get_active_attrib/4">>),
        s134_assert_not_contains(Erl, <<"get_active_uniform/4">>),

        s134_assert_contains(C, <<"GLint arg_2_size;">>),
        s134_assert_contains(C, <<"GLenum arg_2_type;">>),
        s134_assert_contains(C, <<"glGetActiveAttrib(arg_0, arg_1, arg_2_max_length, &arg_2_length, &arg_2_size, &arg_2_type, arg_2_name);">>),
        s134_assert_contains(C, <<"glGetActiveUniform(arg_0, arg_1, arg_2_max_length, &arg_2_length, &arg_2_size, &arg_2_type, arg_2_name);">>),
        s134_assert_contains(C, <<"ERL_NIF_TERM arg_2_size_ret = enif_make_int(env, arg_2_size);">>),
        s134_assert_contains(C, <<"ERL_NIF_TERM arg_2_type_ret;">>),
        s134_assert_contains(C, <<"case GL_FLOAT_VEC4: arg_2_type_ret = beam_atom_float_vec4; break;">>),
        s134_assert_contains(C, <<"memcpy(arg_2_bin, arg_2_name, arg_2_length);">>),
        s134_assert_contains(C, <<"return enif_make_tuple(env, 3,">>),
        s134_assert_contains(C, <<"{\"glGetActiveAttrib_raw\", 3, nif_glGetActiveAttrib, 0}">>),
        s134_assert_contains(C, <<"{\"glGetActiveUniform_raw\", 3, nif_glGetActiveUniform, 0}">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s134_assert_enum_contains(BindingData, EnumType, Atom) ->
    ?assert(lists:member(Atom, maps:get(EnumType, maps:get(enum_types, BindingData)))).

s134_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s134_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s134_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 135.
s135_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s135_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s135_emitter_read_pixels_test_() ->
    [
        {"gl 4.6", fun() -> s135_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s135_assert_emitted_surface({gles, {3, 2}}) end}
    ].

s135_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"read_pixels", 7}, Functions)),
    ?assertNot(maps:is_key({"read_pixels", 6}, Functions)),
    ?assertNot(maps:is_key({"read_pixels", 8}, Functions)),
    s135_assert_read_pixels(maps:get({"read_pixels", 7}, Functions)),
    s135_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s135_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte"),
    s135_assert_deferred_neighbors_absent(Functions).

s135_assert_read_pixels(FunctionData) ->
    ?assertEqual("glReadPixels", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "X", gl_int},
            {in, "Y", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {out, "Pixels", {gl_binary, {implicit, "PixelsSize"}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"X", {gl, int, []}},
            {"Y", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}},
            {"Format", {undefined, pixel_format, []}},
            {"Type", {undefined, pixel_type, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(7, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s135_assert_clause_params(
        [
            {"X", do_nothing},
            {"Y", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing},
            {"Format", {gl_enum_to_uint, ["rgba"]}},
            {"Type", {gl_enum_to_uint, ["unsigned_byte"]}},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glReadPixels", maps:get(raw_function, Clause)),

    NifData = maps:get("glReadPixels", maps:get(nif_functions, FunctionData)),
    ?assertEqual(7, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"X", s135_int_nif_data()},
            {"Y", s135_int_nif_data()},
            {"Width", s135_sizei_nif_data()},
            {"Height", s135_sizei_nif_data()},
            {"Format", s135_enum_nif_data()},
            {"Type", s135_enum_nif_data()},
            {"Pixels", out_binary_implicit}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s135_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s135_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard135-emitter-" ++
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

        s135_assert_contains(Erl, <<"-export([read_pixels/7]).">>),
        s135_assert_contains(Erl, <<"-spec read_pixels(\n    X :: gl:int(),\n    Y :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>),
        s135_assert_contains(Erl, <<"?CALL_RAW_FUNC(glReadPixels_raw(X, Y, Width, Height, NewFormat, NewType, PixelsSize)).">>),
        s135_assert_not_contains(Erl, <<"-export([read_pixels/6]).">>),

        s135_assert_contains(C, <<"ErlNifUInt64 arg_6_size;">>),
        s135_assert_contains(C, <<"if (!enif_get_uint64(env, argv[6], &arg_6_size)) {">>),
        s135_assert_contains(C, <<"if (arg_6_size > (ErlNifUInt64)PTRDIFF_MAX) {">>),
        s135_assert_contains(C, <<"unsigned char* arg_6_data = enif_make_new_binary(env, arg_6_size, &arg_6);">>),
        s135_assert_contains(C, <<"if (arg_6_data == NULL && arg_6_size > 0) {">>),
        s135_assert_contains(C, <<"glReadPixels(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6_data);">>),
        s135_assert_contains(C, <<"{\"glReadPixels_raw\", 7, nif_glReadPixels, 0}">>)
    after
        ok = file:set_cwd(Cwd)
    end.

s135_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s135_assert_clause_param/1, lists:zip(Expected, Actual)).

s135_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s135_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s135_assert_enum_contains(BindingData, EnumName, Value) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumName, EnumTypes)),
    ?assert(lists:member(Value, maps:get(EnumName, EnumTypes))).

s135_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s135_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s135_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s135_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s135_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 144.
s144_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s144_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s144_emitter_get_tex_image_surface_test_() ->
    [
        {"gl 3.3", fun() ->
            s144_assert_emitted_surface({gl, {3, 3}}, s144_expected_erlang(), s144_expected_c(), s144_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s144_assert_emitted_surface({gl, {4, 1}}, s144_expected_erlang(), s144_expected_c(), s144_forbidden_surface())
        end},
        {"gl 4.6", fun() ->
            s144_assert_emitted_surface({gl, {4, 6}}, s144_expected_erlang(), s144_expected_c(), s144_forbidden_surface())
        end},
        {"gles 3.2", fun() ->
            s144_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s144_expected_erlang() ++ s144_expected_c()
            )
        end}
    ].

s144_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s144_present_functions(Target),

    s144_assert_presence(Present, Functions),
    s144_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s144_assert_enum_types(BindingData),
            s144_assert_get_tex_image(maps:get({"get_tex_image", 5}, Functions))
    end.

s144_present_functions({gl, _Version}) ->
    [{"get_tex_image", 5}];
s144_present_functions(_) ->
    [].

s144_assert_presence(Present, Functions) ->
    All = [{"get_tex_image", 5}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_tex_image", 4}, Functions)),
    ?assertNot(maps:is_key({"get_tex_image", 6}, Functions)).

s144_assert_enum_types(BindingData) ->
    s144_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s144_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s144_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s144_assert_get_tex_image(FunctionData) ->
    ?assertEqual("glGetTexImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {out, "Pixels", {gl_binary, {implicit, "PixelsSize"}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}},
            {"Format", {undefined, pixel_format, []}},
            {"Type", {undefined, pixel_type, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(5, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s144_assert_clause_params(
        [
            {"Target", {gl_enum_to_uint, ["texture_2d"]}},
            {"Level", do_nothing},
            {"Format", {gl_enum_to_uint, ["rgba"]}},
            {"Type", {gl_enum_to_uint, ["unsigned_byte"]}},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetTexImage", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetTexImage", maps:get(nif_functions, FunctionData)),
    ?assertEqual(5, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s144_enum_nif_data()},
            {"Level", s144_int_nif_data()},
            {"Format", s144_enum_nif_data()},
            {"Type", s144_enum_nif_data()},
            {"Pixels", out_binary_implicit}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s144_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s144_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard144-emitter-" ++
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

        [s144_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s144_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s144_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s144_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s144_expected_erlang() ->
    [
        <<"-export([get_tex_image/5]).">>,
        <<"-spec get_tex_image(\n    Target :: texture_target(),\n    Level :: gl:int(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetTexImage_raw(NewTarget, Level, NewFormat, NewType, PixelsSize)).">>
    ].

s144_expected_c() ->
    [
        <<"ErlNifUInt64 arg_4_size;">>,
        <<"if (!enif_get_uint64(env, argv[4], &arg_4_size)) {">>,
        <<"if (arg_4_size > (ErlNifUInt64)PTRDIFF_MAX) {">>,
        <<"unsigned char* arg_4_data = enif_make_new_binary(env, arg_4_size, &arg_4);">>,
        <<"if (arg_4_data == NULL && arg_4_size > 0) {">>,
        <<"glGetTexImage(arg_0, arg_1, arg_2, arg_3, arg_4_data);">>,
        <<"{\"glGetTexImage_raw\", 5, nif_glGetTexImage, 0}">>
    ].

s144_forbidden_surface() ->
    [
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnCompressedTexImage(">>
    ].

s144_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s144_assert_clause_param/1, lists:zip(Expected, Actual)).

s144_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s144_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s144_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s144_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s144_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s144_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s144_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 145.
s145_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s145_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s145_emitter_get_texture_image_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s145_assert_emitted_surface({gl, {4, 6}}, s145_expected_erlang(), s145_expected_c(), s145_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s145_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                s145_absent_surface()
            )
        end},
        {"gles 3.2", fun() ->
            s145_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s145_absent_surface()
            )
        end}
    ].

s145_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s145_present_functions(Target),

    s145_assert_presence(Present, Functions),
    s145_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s145_assert_enum_types(BindingData),
            s145_assert_get_texture_image(maps:get({"get_texture_image", 5}, Functions))
    end.

s145_present_functions({gl, {4, 6}}) ->
    [{"get_texture_image", 5}];
s145_present_functions(_) ->
    [].

s145_assert_presence(Present, Functions) ->
    All = [{"get_texture_image", 5}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_texture_image", 4}, Functions)),
    ?assertNot(maps:is_key({"get_texture_image", 6}, Functions)).

s145_assert_enum_types(BindingData) ->
    s145_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s145_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s145_assert_get_texture_image(FunctionData) ->
    ?assertEqual("glGetTextureImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {out, "Pixels", {gl_binary, {explicit, "PixelsSize", gl_sizei}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"Format", {undefined, pixel_format, []}},
            {"Type", {undefined, pixel_type, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(5, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s145_assert_clause_params(
        [
            {"Texture", do_nothing},
            {"Level", do_nothing},
            {"Format", {gl_enum_to_uint, ["rgba"]}},
            {"Type", {gl_enum_to_uint, ["unsigned_byte"]}},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetTextureImage", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetTextureImage", maps:get(nif_functions, FunctionData)),
    ?assertEqual(5, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s145_uint_nif_data()},
            {"Level", s145_int_nif_data()},
            {"Format", s145_enum_nif_data()},
            {"Type", s145_enum_nif_data()},
            {"Pixels", {out_binary_explicit, gl_sizei}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s145_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s145_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard145-emitter-" ++
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

        [s145_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s145_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s145_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s145_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s145_expected_erlang() ->
    [
        <<"-export([get_texture_image/5]).">>,
        <<"-spec get_texture_image(\n    Texture :: texture(),\n    Level :: gl:int(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetTextureImage_raw(Texture, Level, NewFormat, NewType, PixelsSize)).">>
    ].

s145_expected_c() ->
    [
        <<"#include <limits.h>">>,
        <<"ErlNifUInt64 arg_4_size;">>,
        <<"if (!enif_get_uint64(env, argv[4], &arg_4_size)) {">>,
        <<"if (arg_4_size > (ErlNifUInt64)INT_MAX) {">>,
        <<"unsigned char* arg_4_bin = enif_make_new_binary(env, arg_4_size, &arg_4_term);">>,
        <<"if (arg_4_bin == NULL && arg_4_size > 0) {">>,
        <<"glGetTextureImage(arg_0, arg_1, arg_2, arg_3, (GLsizei)arg_4_size, arg_4_bin);">>,
        <<"glGetBufferSubData(arg_0, arg_1, (GLsizeiptr)arg_2_size, arg_2_bin);">>,
        <<"{\"glGetTextureImage_raw\", 5, nif_glGetTextureImage, 0}">>
    ].

s145_forbidden_surface() ->
    [
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnCompressedTexImage(">>
    ].

s145_absent_surface() ->
    s145_expected_erlang() ++
        [
            <<"glGetTextureImage(">>,
            <<"{\"glGetTextureImage_raw\", 5, nif_glGetTextureImage, 0}">>
        ].

s145_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s145_assert_clause_param/1, lists:zip(Expected, Actual)).

s145_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s145_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s145_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s145_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s145_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s145_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s145_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s145_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 146.
s146_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s146_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s146_emitter_get_texture_sub_image_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s146_assert_emitted_surface({gl, {4, 6}}, s146_expected_erlang(), s146_expected_c(), s146_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s146_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                s146_absent_surface()
            )
        end},
        {"gles 3.2", fun() ->
            s146_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s146_absent_surface()
            )
        end}
    ].

s146_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s146_present_functions(Target),

    s146_assert_presence(Present, Functions),
    s146_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s146_assert_enum_types(BindingData),
            s146_assert_get_texture_sub_image(maps:get({"get_texture_sub_image", 11}, Functions))
    end.

s146_present_functions({gl, {4, 6}}) ->
    [{"get_texture_sub_image", 11}];
s146_present_functions(_) ->
    [].

s146_assert_presence(Present, Functions) ->
    All = [{"get_texture_sub_image", 11}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_texture_sub_image", 10}, Functions)),
    ?assertNot(maps:is_key({"get_texture_sub_image", 12}, Functions)).

s146_assert_enum_types(BindingData) ->
    s146_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s146_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s146_assert_get_texture_sub_image(FunctionData) ->
    ?assertEqual("glGetTextureSubImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "OffsetX", gl_int},
            {in, "OffsetY", gl_int},
            {in, "OffsetZ", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei},
            {in, "Depth", gl_sizei},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {out, "Pixels", {gl_binary, {explicit, "PixelsSize", gl_sizei}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"OffsetX", {gl, int, []}},
            {"OffsetY", {gl, int, []}},
            {"OffsetZ", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}},
            {"Depth", {gl, sizei, []}},
            {"Format", {undefined, pixel_format, []}},
            {"Type", {undefined, pixel_type, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(11, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s146_assert_clause_params(
        [
            {"Texture", do_nothing},
            {"Level", do_nothing},
            {"OffsetX", do_nothing},
            {"OffsetY", do_nothing},
            {"OffsetZ", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing},
            {"Depth", do_nothing},
            {"Format", {gl_enum_to_uint, ["rgba"]}},
            {"Type", {gl_enum_to_uint, ["unsigned_byte"]}},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetTextureSubImage", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetTextureSubImage", maps:get(nif_functions, FunctionData)),
    ?assertEqual(11, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s146_uint_nif_data()},
            {"Level", s146_int_nif_data()},
            {"OffsetX", s146_int_nif_data()},
            {"OffsetY", s146_int_nif_data()},
            {"OffsetZ", s146_int_nif_data()},
            {"Width", s146_sizei_nif_data()},
            {"Height", s146_sizei_nif_data()},
            {"Depth", s146_sizei_nif_data()},
            {"Format", s146_enum_nif_data()},
            {"Type", s146_enum_nif_data()},
            {"Pixels", {out_binary_explicit, gl_sizei}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s146_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s146_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard146-emitter-" ++
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

        [s146_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s146_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s146_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s146_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s146_expected_erlang() ->
    [
        <<"-export([get_texture_sub_image/11]).">>,
        <<"-spec get_texture_sub_image(\n    Texture :: texture(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    OffsetZ :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Depth :: gl:sizei(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetTextureSubImage_raw(Texture, Level, OffsetX, OffsetY, OffsetZ, Width, Height, Depth, NewFormat, NewType, PixelsSize)).">>
    ].

s146_expected_c() ->
    [
        <<"#include <limits.h>">>,
        <<"ErlNifUInt64 arg_10_size;">>,
        <<"if (!enif_get_uint64(env, argv[10], &arg_10_size)) {">>,
        <<"if (arg_10_size > (ErlNifUInt64)INT_MAX) {">>,
        <<"unsigned char* arg_10_bin = enif_make_new_binary(env, arg_10_size, &arg_10_term);">>,
        <<"if (arg_10_bin == NULL && arg_10_size > 0) {">>,
        <<"glGetTextureSubImage(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, (GLsizei)arg_10_size, arg_10_bin);">>,
        <<"glGetBufferSubData(arg_0, arg_1, (GLsizeiptr)arg_2_size, arg_2_bin);">>,
        <<"{\"glGetTextureSubImage_raw\", 11, nif_glGetTextureSubImage, 0}">>
    ].

s146_forbidden_surface() ->
    [
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnCompressedTexImage(">>
    ].

s146_absent_surface() ->
    s146_expected_erlang() ++
        [
            <<"glGetTextureSubImage(">>,
            <<"{\"glGetTextureSubImage_raw\", 11, nif_glGetTextureSubImage, 0}">>
        ].

s146_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s146_assert_clause_param/1, lists:zip(Expected, Actual)).

s146_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s146_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s146_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s146_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s146_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s146_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s146_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s146_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s146_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 147.
s147_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s147_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s147_emitter_get_compressed_tex_image_surface_test_() ->
    [
        {"gl 3.3", fun() ->
            s147_assert_emitted_surface({gl, {3, 3}}, s147_expected_erlang(), s147_expected_c(), s147_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s147_assert_emitted_surface({gl, {4, 1}}, s147_expected_erlang(), s147_expected_c(), s147_forbidden_surface())
        end},
        {"gl 4.6", fun() ->
            s147_assert_emitted_surface({gl, {4, 6}}, s147_expected_erlang(), s147_expected_c(), s147_forbidden_surface())
        end},
        {"gles 3.2", fun() ->
            s147_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s147_expected_erlang() ++ s147_expected_c()
            )
        end}
    ].

s147_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s147_present_functions(Target),

    s147_assert_presence(Present, Functions),
    s147_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s147_assert_enum_types(BindingData),
            s147_assert_get_compressed_tex_image(maps:get({"get_compressed_tex_image", 3}, Functions))
    end.

s147_present_functions({gl, _Version}) ->
    [{"get_compressed_tex_image", 3}];
s147_present_functions(_) ->
    [].

s147_assert_presence(Present, Functions) ->
    All = [{"get_compressed_tex_image", 3}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_compressed_tex_image", 2}, Functions)),
    ?assertNot(maps:is_key({"get_compressed_tex_image", 4}, Functions)).

s147_assert_enum_types(BindingData) ->
    s147_assert_enum_contains(BindingData, "texture_target", "texture_2d").

s147_assert_get_compressed_tex_image(FunctionData) ->
    ?assertEqual("glGetCompressedTexImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int},
            {out, "Pixels", {gl_binary, {implicit, "PixelsSize"}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s147_assert_clause_params(
        [
            {"Target", {gl_enum_to_uint, ["texture_2d"]}},
            {"Level", do_nothing},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetCompressedTexImage", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetCompressedTexImage", maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s147_enum_nif_data()},
            {"Level", s147_int_nif_data()},
            {"Pixels", out_binary_implicit}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s147_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s147_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard147-emitter-" ++
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

        [s147_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s147_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s147_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s147_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s147_expected_erlang() ->
    [
        <<"-export([get_compressed_tex_image/3]).">>,
        <<"-spec get_compressed_tex_image(\n    Target :: texture_target(),\n    Level :: gl:int(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetCompressedTexImage_raw(NewTarget, Level, PixelsSize)).">>
    ].

s147_expected_c() ->
    [
        <<"ErlNifUInt64 arg_2_size;">>,
        <<"if (!enif_get_uint64(env, argv[2], &arg_2_size)) {">>,
        <<"if (arg_2_size > (ErlNifUInt64)PTRDIFF_MAX) {">>,
        <<"unsigned char* arg_2_data = enif_make_new_binary(env, arg_2_size, &arg_2);">>,
        <<"if (arg_2_data == NULL && arg_2_size > 0) {">>,
        <<"glGetCompressedTexImage(arg_0, arg_1, arg_2_data);">>,
        <<"{\"glGetCompressedTexImage_raw\", 3, nif_glGetCompressedTexImage, 0}">>
    ].

s147_forbidden_surface() ->
    [
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnCompressedTexImage(">>
    ].

s147_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s147_assert_clause_param/1, lists:zip(Expected, Actual)).

s147_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s147_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s147_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s147_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s147_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s147_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s147_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 148.
s148_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s148_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s148_emitter_get_compressed_texture_image_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s148_assert_emitted_surface({gl, {4, 6}}, s148_expected_erlang(), s148_expected_c(), s148_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s148_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                s148_absent_surface()
            )
        end},
        {"gles 3.2", fun() ->
            s148_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s148_absent_surface()
            )
        end}
    ].

s148_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s148_present_functions(Target),

    s148_assert_presence(Present, Functions),
    s148_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s148_assert_get_compressed_texture_image(maps:get({"get_compressed_texture_image", 3}, Functions))
    end.

s148_present_functions({gl, {4, 6}}) ->
    [{"get_compressed_texture_image", 3}];
s148_present_functions(_) ->
    [].

s148_assert_presence(Present, Functions) ->
    All = [{"get_compressed_texture_image", 3}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_compressed_texture_image", 2}, Functions)),
    ?assertNot(maps:is_key({"get_compressed_texture_image", 4}, Functions)).

s148_assert_get_compressed_texture_image(FunctionData) ->
    ?assertEqual("glGetCompressedTextureImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {out, "Pixels", {gl_binary, {explicit, "PixelsSize", gl_sizei}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Texture", do_nothing},
            {"Level", do_nothing},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetCompressedTextureImage", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetCompressedTextureImage", maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s148_uint_nif_data()},
            {"Level", s148_int_nif_data()},
            {"Pixels", {out_binary_explicit, gl_sizei}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s148_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s148_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard148-emitter-" ++
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

        [s148_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s148_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s148_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s148_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s148_expected_erlang() ->
    [
        <<"-export([get_compressed_texture_image/3]).">>,
        <<"-spec get_compressed_texture_image(\n    Texture :: texture(),\n    Level :: gl:int(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetCompressedTextureImage_raw(Texture, Level, PixelsSize)).">>
    ].

s148_expected_c() ->
    [
        <<"#include <limits.h>">>,
        <<"ErlNifUInt64 arg_2_size;">>,
        <<"if (!enif_get_uint64(env, argv[2], &arg_2_size)) {">>,
        <<"if (arg_2_size > (ErlNifUInt64)INT_MAX) {">>,
        <<"unsigned char* arg_2_bin = enif_make_new_binary(env, arg_2_size, &arg_2_term);">>,
        <<"if (arg_2_bin == NULL && arg_2_size > 0) {">>,
        <<"glGetCompressedTextureImage(arg_0, arg_1, (GLsizei)arg_2_size, arg_2_bin);">>,
        <<"{\"glGetCompressedTextureImage_raw\", 3, nif_glGetCompressedTextureImage, 0}">>
    ].

s148_forbidden_surface() ->
    [
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnCompressedTexImage(">>
    ].

s148_absent_surface() ->
    s148_expected_erlang() ++
        [
            <<"glGetCompressedTextureImage(">>,
            <<"{\"glGetCompressedTextureImage_raw\", 3, nif_glGetCompressedTextureImage, 0}">>
        ].

s148_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s148_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s148_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s148_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 149.
s149_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s149_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s149_emitter_get_compressed_texture_sub_image_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s149_assert_emitted_surface({gl, {4, 6}}, s149_expected_erlang(), s149_expected_c(), s149_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s149_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                s149_absent_surface()
            )
        end},
        {"gles 3.2", fun() ->
            s149_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s149_absent_surface()
            )
        end}
    ].

s149_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s149_present_functions(Target),

    s149_assert_presence(Present, Functions),
    s149_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s149_assert_get_compressed_texture_sub_image(
                maps:get({"get_compressed_texture_sub_image", 9}, Functions)
            )
    end.

s149_present_functions({gl, {4, 6}}) ->
    [{"get_compressed_texture_sub_image", 9}];
s149_present_functions(_) ->
    [].

s149_assert_presence(Present, Functions) ->
    All = [{"get_compressed_texture_sub_image", 9}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"get_compressed_texture_sub_image", 8}, Functions)),
    ?assertNot(maps:is_key({"get_compressed_texture_sub_image", 10}, Functions)).

s149_assert_get_compressed_texture_sub_image(FunctionData) ->
    ?assertEqual("glGetCompressedTextureSubImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "OffsetX", gl_int},
            {in, "OffsetY", gl_int},
            {in, "OffsetZ", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei},
            {in, "Depth", gl_sizei},
            {out, "Pixels", {gl_binary, {explicit, "PixelsSize", gl_sizei}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"OffsetX", {gl, int, []}},
            {"OffsetY", {gl, int, []}},
            {"OffsetZ", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}},
            {"Depth", {gl, sizei, []}},
            {"PixelsSize", {undefined, non_neg_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Pixels", {undefined, binary, []}}], maps:get(specs_return, FunctionData)),
    ?assertEqual(9, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Texture", do_nothing},
            {"Level", do_nothing},
            {"OffsetX", do_nothing},
            {"OffsetY", do_nothing},
            {"OffsetZ", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing},
            {"Depth", do_nothing},
            {"PixelsSize", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glGetCompressedTextureSubImage", maps:get(raw_function, Clause)),

    NifData = maps:get("glGetCompressedTextureSubImage", maps:get(nif_functions, FunctionData)),
    ?assertEqual(9, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s149_uint_nif_data()},
            {"Level", s149_int_nif_data()},
            {"OffsetX", s149_int_nif_data()},
            {"OffsetY", s149_int_nif_data()},
            {"OffsetZ", s149_int_nif_data()},
            {"Width", s149_sizei_nif_data()},
            {"Height", s149_sizei_nif_data()},
            {"Depth", s149_sizei_nif_data()},
            {"Pixels", {out_binary_explicit, gl_sizei}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s149_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s149_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard149-emitter-" ++
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

        [s149_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s149_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s149_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s149_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s149_expected_erlang() ->
    [
        <<"-export([get_compressed_texture_sub_image/9]).">>,
        <<"-spec get_compressed_texture_sub_image(\n    Texture :: texture(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    OffsetZ :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Depth :: gl:sizei(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetCompressedTextureSubImage_raw(Texture, Level, OffsetX, OffsetY, OffsetZ, Width, Height, Depth, PixelsSize)).">>
    ].

s149_expected_c() ->
    [
        <<"#include <limits.h>">>,
        <<"ErlNifUInt64 arg_8_size;">>,
        <<"if (!enif_get_uint64(env, argv[8], &arg_8_size)) {">>,
        <<"if (arg_8_size > (ErlNifUInt64)INT_MAX) {">>,
        <<"unsigned char* arg_8_bin = enif_make_new_binary(env, arg_8_size, &arg_8_term);">>,
        <<"if (arg_8_bin == NULL && arg_8_size > 0) {">>,
        <<"glGetCompressedTextureSubImage(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, (GLsizei)arg_8_size, arg_8_bin);">>,
        <<"{\"glGetCompressedTextureSubImage_raw\", 9, nif_glGetCompressedTextureSubImage, 0}">>
    ].

s149_forbidden_surface() ->
    [
        <<"-export([getn_compressed_tex_image/">>,
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnCompressedTexImage(">>
    ].

s149_absent_surface() ->
    s149_expected_erlang() ++
        [
            <<"glGetCompressedTextureSubImage(">>,
            <<"{\"glGetCompressedTextureSubImage_raw\", 9, nif_glGetCompressedTextureSubImage, 0}">>
        ].

s149_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s149_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s149_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s149_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s149_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 156.
s156_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s156_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s156_emitter_uniform_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s156_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([get_uniform/4]).">>,
                    <<"-spec get_uniform(\n    Type :: d | f | i | ui,\n    Program :: program(),\n    Location :: gl:int(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_uniform_value()]} | {error, atom()}.">>,
                    <<"get_uniform(f, Program, Location, Count) ->">>,
                    <<"?CALL_RAW_FUNC(glGetUniformfv_raw(Program, Location, Count))">>,
                    <<"get_uniform(d, Program, Location, Count) ->">>,
                    <<"?CALL_RAW_FUNC(glGetUniformdv_raw(Program, Location, Count))">>
                ],
                [
                    <<"ErlNifUInt64 arg_2_count_tmp;">>,
                    <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
                    <<"GLfloat* arg_2_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_2_count);">>,
                    <<"glGetUniformfv(arg_0, arg_1, arg_2_values);">>,
                    <<"glGetUniformdv(arg_0, arg_1, arg_2_values);">>,
                    <<"{\"glGetUniformfv_raw\", 3, nif_glGetUniformfv, 0}">>,
                    <<"{\"glGetUniformdv_raw\", 3, nif_glGetUniformdv, 0}">>
                ],
                s156_forbidden_neighbors()
            )
        end},
        {"gles 3.2", fun() ->
            s156_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([get_uniform/4]).">>,
                    <<"get_uniform(ui, Program, Location, Count) ->">>,
                    <<"?CALL_RAW_FUNC(glGetUniformuiv_raw(Program, Location, Count))">>
                ],
                [
                    <<"glGetUniformfv(arg_0, arg_1, arg_2_values);">>,
                    <<"glGetUniformuiv(arg_0, arg_1, arg_2_values);">>
                ],
                [<<"glGetUniformdv_raw">> | s156_forbidden_neighbors()]
            )
        end},
        {"gles 2.0", fun() ->
            s156_assert_emitted_surface(
                {gles, {2, 0}},
                [
                    <<"-export([get_uniform/4]).">>,
                    <<"get_uniform(f, Program, Location, Count) ->">>,
                    <<"get_uniform(i, Program, Location, Count) ->">>
                ],
                [
                    <<"glGetUniformfv(arg_0, arg_1, arg_2_values);">>,
                    <<"glGetUniformiv(arg_0, arg_1, arg_2_values);">>
                ],
                [<<"glGetUniformuiv_raw">>, <<"glGetUniformdv_raw">> | s156_forbidden_neighbors()]
            )
        end},
        {"gl 3.3", fun() ->
            s156_assert_emitted_surface(
                {gl, {3, 3}},
                [
                    <<"-export([get_uniform/4]).">>,
                    <<"get_uniform(ui, Program, Location, Count) ->">>
                ],
                [
                    <<"glGetUniformuiv(arg_0, arg_1, arg_2_values);">>
                ],
                [<<"glGetUniformdv_raw">> | s156_forbidden_neighbors()]
            )
        end}
    ].

s156_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"get_uniform", 4}, Functions)),
    ?assertNot(maps:is_key({"get_uniform", 3}, Functions)),
    ?assertNot(maps:is_key({"get_uniform", 5}, Functions)),

    GetUniform = maps:get({"get_uniform", 4}, Functions),
    s156_assert_get_uniform_specs(Target, GetUniform),
    s156_assert_family(GetUniform, f, gl_float),
    s156_assert_family(GetUniform, i, gl_int),
    s156_assert_optional_family(Target, GetUniform, ui, gl_uint, s156_supports_unsigned_uniform(Target)),
    s156_assert_optional_family(Target, GetUniform, d, gl_double, s156_supports_double_uniform(Target)),
    s156_assert_deferred_neighbors_absent(Functions).

s156_supports_unsigned_uniform({gles, {2, 0}}) ->
    false;
s156_supports_unsigned_uniform(_) ->
    true.

s156_supports_double_uniform({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s156_supports_double_uniform({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s156_supports_double_uniform(_) ->
    false.

s156_assert_get_uniform_specs(Target, GetUniform) ->
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Location", gl_int},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetUniform)
    ),
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Count", {undefined, pos_integer, []}}
    ] = maps:get(specs_params, GetUniform),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    ?assertEqual(s156_supports_unsigned_uniform(Target), lists:member(ui, TypeAtoms)),
    ?assertEqual(s156_supports_double_uniform(Target), lists:member(d, TypeAtoms)),
    ?assertEqual(
        [{"Values", {list, {undefined, get_uniform_value, []}}}],
        maps:get(specs_return, GetUniform)
    ),
    ?assertEqual(4, maps:get(function_arity, GetUniform)),
    {get_uniform_value, {set, ValueTypes}} = maps:get(extra_type, GetUniform),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)),
    ?assertEqual(s156_supports_unsigned_uniform(Target), lists:member({gl, uint, []}, ValueTypes)),
    ?assertEqual(s156_supports_double_uniform(Target), lists:member({gl, double, []}, ValueTypes)).

s156_assert_family(GetUniform, TypeAtom, GlType) ->
    Command = s156_uniform_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetUniform))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetUniform))),
    s156_assert_clause(GetUniform, TypeAtom, Command),
    s156_assert_nif(GetUniform, Command, GlType).

s156_assert_optional_family(GetUniform, TypeAtom, GlType, true) ->
    s156_assert_family(GetUniform, TypeAtom, GlType);
s156_assert_optional_family(GetUniform, TypeAtom, _GlType, false) ->
    Command = s156_uniform_command(TypeAtom),
    ?assertNot(lists:keymember(Command, 1, maps:get(gl_commands, GetUniform))).

s156_assert_optional_family(_Target, GetUniform, TypeAtom, GlType, Supported) ->
    s156_assert_optional_family(GetUniform, TypeAtom, GlType, Supported).

s156_assert_clause(GetUniform, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s156_find_clause(Command, maps:get(function_clauses, GetUniform)),
    ?assertEqual(
        [
            {Suffix, ignore},
            {"Program", do_nothing},
            {"Location", do_nothing},
            {"Count", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s156_assert_nif(GetUniform, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetUniform)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s156_uint_nif_data()},
            {"Location", s156_int_nif_data()},
            {"Values", {out_typed_value_list, s156_gl_ctype(GlType), s156_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s156_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s156_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard156-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s156_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s156_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s156_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s156_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s156_uniform_command(f) -> "glGetUniformfv";
s156_uniform_command(i) -> "glGetUniformiv";
s156_uniform_command(ui) -> "glGetUniformuiv";
s156_uniform_command(d) -> "glGetUniformdv".

s156_gl_ctype(gl_float) -> "GLfloat";
s156_gl_ctype(gl_int) -> "GLint";
s156_gl_ctype(gl_uint) -> "GLuint";
s156_gl_ctype(gl_double) -> "GLdouble".

s156_term_function(gl_float) -> "enif_make_double";
s156_term_function(gl_int) -> "enif_make_int";
s156_term_function(gl_uint) -> "enif_make_uint";
s156_term_function(gl_double) -> "enif_make_double".

s156_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s156_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s156_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s156_forbidden_neighbors() ->
    [
    ].

s156_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s156_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 157.
s157_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s157_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s157_emitter_texture_parameter_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s157_assert_emitted_surface({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s157_assert_emitted_surface({gles, {3, 2}})
        end},
        {"gles 2.0", fun() ->
            s157_assert_emitted_surface({gles, {2, 0}})
        end}
    ].

s157_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"get_tex_parameter", 4}, Functions)),
    ?assertNot(maps:is_key({"get_tex_parameter", 3}, Functions)),

    GetTexParameter = maps:get({"get_tex_parameter", 4}, Functions),
    s157_assert_specs(GetTexParameter),
    s157_assert_family(GetTexParameter, f, gl_float),
    s157_assert_family(GetTexParameter, i, gl_int),
    s157_assert_deferred_neighbors_absent(Functions).

s157_assert_specs(GetTexParameter) ->
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetTexParameter)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Target", {undefined, texture_target, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetTexParameter)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_tex_parameter_value, []}}}],
        maps:get(specs_return, GetTexParameter)
    ),
    ?assertEqual(4, maps:get(function_arity, GetTexParameter)),
    {get_tex_parameter_value, {set, ValueTypes}} = maps:get(extra_type, GetTexParameter),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)).

s157_assert_family(GetTexParameter, TypeAtom, GlType) ->
    Command = s157_tex_parameter_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetTexParameter))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetTexParameter))),
    s157_assert_clause(GetTexParameter, TypeAtom, Command),
    s157_assert_nif(GetTexParameter, Command, GlType).

s157_assert_clause(GetTexParameter, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s157_find_clause(Command, maps:get(function_clauses, GetTexParameter)),
    [
        {Suffix, ignore},
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s157_assert_nif(GetTexParameter, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetTexParameter)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s157_enum_nif_data()},
            {"ParamName", s157_enum_nif_data()},
            {"Values", {out_typed_value_list, s157_gl_ctype(GlType), s157_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s157_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s157_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard157-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        ExpectedErl = [
            <<"-export([get_tex_parameter/4]).">>,
            <<"-spec get_tex_parameter(\n    Type :: f | i,\n    Target :: texture_target(),\n    ParamName :: texture_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_tex_parameter_value()]} | {error, atom()}.">>,
            <<"get_tex_parameter(f, Target, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTexParameterfv_raw(NewTarget, NewParamName, Count))">>,
            <<"get_tex_parameter(i, Target, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTexParameteriv_raw(NewTarget, NewParamName, Count))">>
        ],
        ExpectedC = [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"GLfloat* arg_2_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_2_count);">>,
            <<"glGetTexParameterfv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetTexParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetTexParameterfv_raw\", 3, nif_glGetTexParameterfv, 0}">>,
            <<"{\"glGetTexParameteriv_raw\", 3, nif_glGetTexParameteriv, 0}">>
        ],
        Forbidden = [],
        [s157_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s157_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s157_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s157_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s157_tex_parameter_command(f) -> "glGetTexParameterfv";
s157_tex_parameter_command(i) -> "glGetTexParameteriv".

s157_gl_ctype(gl_float) -> "GLfloat";
s157_gl_ctype(gl_int) -> "GLint".

s157_term_function(gl_float) -> "enif_make_double";
s157_term_function(gl_int) -> "enif_make_int".

s157_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s157_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s157_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s157_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 158.
s158_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s158_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s158_emitter_texture_parameter_i_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s158_assert_emitted_present({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s158_assert_emitted_present({gles, {3, 2}})
        end},
        {"gles 3.1", fun() ->
            s158_assert_emitted_absent({gles, {3, 1}})
        end}
    ].

s158_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s158_supports_get_tex_parameter_i(Target) of
        true ->
            ?assert(maps:is_key({"get_tex_parameter_i", 4}, Functions)),
            ?assertNot(maps:is_key({"get_tex_parameter_i", 3}, Functions)),
            GetTexParameterI = maps:get({"get_tex_parameter_i", 4}, Functions),
            s158_assert_specs(GetTexParameterI),
            s158_assert_family(GetTexParameterI, i, gl_int),
            s158_assert_family(GetTexParameterI, ui, gl_uint);
        false ->
            ?assertNot(maps:is_key({"get_tex_parameter_i", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTexParameterIiv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTexParameterIuiv", Functions))
    end,
    s158_assert_deferred_neighbors_absent(Functions).

s158_supports_get_tex_parameter_i({gl, _Version}) ->
    true;
s158_supports_get_tex_parameter_i({gles, {3, 2}}) ->
    true;
s158_supports_get_tex_parameter_i(_) ->
    false.

s158_assert_specs(GetTexParameterI) ->
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetTexParameterI)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, ui]}},
            {"Target", {undefined, texture_target, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetTexParameterI)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_tex_parameter_i_value, []}}}],
        maps:get(specs_return, GetTexParameterI)
    ),
    ?assertEqual(4, maps:get(function_arity, GetTexParameterI)),
    {get_tex_parameter_i_value, {set, ValueTypes}} = maps:get(extra_type, GetTexParameterI),
    ?assert(lists:member({gl, int, []}, ValueTypes)),
    ?assert(lists:member({gl, uint, []}, ValueTypes)).

s158_assert_family(GetTexParameterI, TypeAtom, GlType) ->
    Command = s158_tex_parameter_i_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetTexParameterI))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetTexParameterI))),
    s158_assert_clause(GetTexParameterI, TypeAtom, Command),
    s158_assert_nif(GetTexParameterI, Command, GlType).

s158_assert_clause(GetTexParameterI, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s158_find_clause(Command, maps:get(function_clauses, GetTexParameterI)),
    [
        {Suffix, ignore},
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("texture_border_color", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s158_assert_nif(GetTexParameterI, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetTexParameterI)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s158_enum_nif_data()},
            {"ParamName", s158_enum_nif_data()},
            {"Values", {out_typed_value_list, s158_gl_ctype(GlType), s158_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s158_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s158_assert_emitted_present(Target) ->
    s158_assert_emitted_surface(
        Target,
        [
            <<"-export([get_tex_parameter_i/4]).">>,
            <<"-spec get_tex_parameter_i(\n    Type :: i | ui,\n    Target :: texture_target(),\n    ParamName :: texture_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_tex_parameter_i_value()]} | {error, atom()}.">>,
            <<"get_tex_parameter_i(i, Target, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTexParameterIiv_raw(NewTarget, NewParamName, Count))">>,
            <<"get_tex_parameter_i(ui, Target, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTexParameterIuiv_raw(NewTarget, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"GLuint* arg_2_values = enif_alloc(sizeof(GLuint) * (size_t)arg_2_count);">>,
            <<"glGetTexParameterIiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetTexParameterIuiv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetTexParameterIiv_raw\", 3, nif_glGetTexParameterIiv, 0}">>,
            <<"{\"glGetTexParameterIuiv_raw\", 3, nif_glGetTexParameterIuiv, 0}">>
        ],
        s158_forbidden_neighbors()
    ).

s158_assert_emitted_absent(Target) ->
    s158_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_tex_parameter_i">>,
            <<"glGetTexParameterIiv">>,
            <<"glGetTexParameterIuiv">>
            | s158_forbidden_neighbors()
        ]
    ).

s158_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard158-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s158_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s158_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s158_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s158_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s158_tex_parameter_i_command(i) -> "glGetTexParameterIiv";
s158_tex_parameter_i_command(ui) -> "glGetTexParameterIuiv".

s158_gl_ctype(gl_int) -> "GLint";
s158_gl_ctype(gl_uint) -> "GLuint".

s158_term_function(gl_int) -> "enif_make_int";
s158_term_function(gl_uint) -> "enif_make_uint".

s158_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s158_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s158_forbidden_neighbors() ->
    [
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s158_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s158_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 159.
s159_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s159_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s159_emitter_texture_parameter_dsa_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s159_assert_emitted_present({gl, {4, 6}})
        end},
        {"gl 4.1", fun() ->
            s159_assert_emitted_absent({gl, {4, 1}})
        end},
        {"gles 3.2", fun() ->
            s159_assert_emitted_absent({gles, {3, 2}})
        end}
    ].

s159_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case Target of
        {gl, {4, 6}} ->
            ?assert(maps:is_key({"get_texture_parameter", 4}, Functions)),
            ?assertNot(maps:is_key({"get_texture_parameter", 3}, Functions)),
            GetTextureParameter = maps:get({"get_texture_parameter", 4}, Functions),
            s159_assert_specs(GetTextureParameter),
            s159_assert_family(GetTextureParameter, f, gl_float),
            s159_assert_family(GetTextureParameter, i, gl_int);
        _ ->
            ?assertNot(maps:is_key({"get_texture_parameter", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTextureParameterfv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTextureParameteriv", Functions))
    end,
    s159_assert_deferred_neighbors_absent(Functions).

s159_assert_specs(GetTextureParameter) ->
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetTextureParameter)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Texture", {undefined, texture, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetTextureParameter)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_texture_parameter_value, []}}}],
        maps:get(specs_return, GetTextureParameter)
    ),
    ?assertEqual(4, maps:get(function_arity, GetTextureParameter)),
    {get_texture_parameter_value, {set, ValueTypes}} = maps:get(extra_type, GetTextureParameter),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)).

s159_assert_family(GetTextureParameter, TypeAtom, GlType) ->
    Command = s159_texture_parameter_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetTextureParameter))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetTextureParameter))),
    s159_assert_clause(GetTextureParameter, TypeAtom, Command),
    s159_assert_nif(GetTextureParameter, Command, GlType).

s159_assert_clause(GetTextureParameter, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s159_find_clause(Command, maps:get(function_clauses, GetTextureParameter)),
    [
        {Suffix, ignore},
        {"Texture", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s159_assert_nif(GetTextureParameter, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetTextureParameter)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s159_texture_nif_data()},
            {"ParamName", s159_enum_nif_data()},
            {"Values", {out_typed_value_list, s159_gl_ctype(GlType), s159_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s159_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetTextureParameterfvEXT",
        "glGetTextureParameterivEXT",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s159_assert_emitted_present(Target) ->
    s159_assert_emitted_surface(
        Target,
        [
            <<"-export([get_texture_parameter/4]).">>,
            <<"-spec get_texture_parameter(\n    Type :: f | i,\n    Texture :: texture(),\n    ParamName :: texture_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_texture_parameter_value()]} | {error, atom()}.">>,
            <<"get_texture_parameter(f, Texture, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTextureParameterfv_raw(Texture, NewParamName, Count))">>,
            <<"get_texture_parameter(i, Texture, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTextureParameteriv_raw(Texture, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLfloat* arg_2_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_2_count);">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"glGetTextureParameterfv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetTextureParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetTextureParameterfv_raw\", 3, nif_glGetTextureParameterfv, 0}">>,
            <<"{\"glGetTextureParameteriv_raw\", 3, nif_glGetTextureParameteriv, 0}">>
        ],
        s159_forbidden_neighbors()
    ).

s159_assert_emitted_absent(Target) ->
    s159_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_texture_parameter">>,
            <<"glGetTextureParameterfv">>,
            <<"glGetTextureParameteriv">>
            | s159_forbidden_neighbors()
        ]
    ).

s159_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard159-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s159_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s159_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s159_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s159_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s159_texture_parameter_command(f) -> "glGetTextureParameterfv";
s159_texture_parameter_command(i) -> "glGetTextureParameteriv".

s159_gl_ctype(gl_float) -> "GLfloat";
s159_gl_ctype(gl_int) -> "GLint".

s159_term_function(gl_float) -> "enif_make_double";
s159_term_function(gl_int) -> "enif_make_int".

s159_texture_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s159_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s159_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s159_forbidden_neighbors() ->
    [
        <<"glGetTextureParameterfvEXT">>,
        <<"glGetTextureParameterivEXT">>,
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s159_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s159_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 160.
s160_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s160_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s160_emitter_texture_parameter_i_dsa_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s160_assert_emitted_present({gl, {4, 6}})
        end},
        {"gl 4.1", fun() ->
            s160_assert_emitted_absent({gl, {4, 1}})
        end},
        {"gles 3.2", fun() ->
            s160_assert_emitted_absent({gles, {3, 2}})
        end}
    ].

s160_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case Target of
        {gl, {4, 6}} ->
            ?assert(maps:is_key({"get_texture_parameter_i", 4}, Functions)),
            ?assertNot(maps:is_key({"get_texture_parameter_i", 3}, Functions)),
            GetTextureParameterI = maps:get({"get_texture_parameter_i", 4}, Functions),
            s160_assert_specs(GetTextureParameterI),
            s160_assert_family(GetTextureParameterI, i, gl_int),
            s160_assert_family(GetTextureParameterI, ui, gl_uint);
        _ ->
            ?assertNot(maps:is_key({"get_texture_parameter_i", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTextureParameterIiv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTextureParameterIuiv", Functions))
    end,
    s160_assert_deferred_neighbors_absent(Functions).

s160_assert_specs(GetTextureParameterI) ->
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetTextureParameterI)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, ui]}},
            {"Texture", {undefined, texture, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetTextureParameterI)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_texture_parameter_i_value, []}}}],
        maps:get(specs_return, GetTextureParameterI)
    ),
    ?assertEqual(4, maps:get(function_arity, GetTextureParameterI)),
    {get_texture_parameter_i_value, {set, ValueTypes}} = maps:get(extra_type, GetTextureParameterI),
    ?assert(lists:member({gl, int, []}, ValueTypes)),
    ?assert(lists:member({gl, uint, []}, ValueTypes)).

s160_assert_family(GetTextureParameterI, TypeAtom, GlType) ->
    Command = s160_texture_parameter_i_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetTextureParameterI))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetTextureParameterI))),
    s160_assert_clause(GetTextureParameterI, TypeAtom, Command),
    s160_assert_nif(GetTextureParameterI, Command, GlType).

s160_assert_clause(GetTextureParameterI, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s160_find_clause(Command, maps:get(function_clauses, GetTextureParameterI)),
    [
        {Suffix, ignore},
        {"Texture", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_border_color", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s160_assert_nif(GetTextureParameterI, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetTextureParameterI)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s160_texture_nif_data()},
            {"ParamName", s160_enum_nif_data()},
            {"Values", {out_typed_value_list, s160_gl_ctype(GlType), s160_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s160_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetTextureParameterfvEXT",
        "glGetTextureParameterivEXT",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s160_assert_emitted_present(Target) ->
    s160_assert_emitted_surface(
        Target,
        [
            <<"-export([get_texture_parameter_i/4]).">>,
            <<"-spec get_texture_parameter_i(\n    Type :: i | ui,\n    Texture :: texture(),\n    ParamName :: texture_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_texture_parameter_i_value()]} | {error, atom()}.">>,
            <<"get_texture_parameter_i(i, Texture, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTextureParameterIiv_raw(Texture, NewParamName, Count))">>,
            <<"get_texture_parameter_i(ui, Texture, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTextureParameterIuiv_raw(Texture, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"GLuint* arg_2_values = enif_alloc(sizeof(GLuint) * (size_t)arg_2_count);">>,
            <<"glGetTextureParameterIiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetTextureParameterIuiv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetTextureParameterIiv_raw\", 3, nif_glGetTextureParameterIiv, 0}">>,
            <<"{\"glGetTextureParameterIuiv_raw\", 3, nif_glGetTextureParameterIuiv, 0}">>
        ],
        s160_forbidden_neighbors()
    ).

s160_assert_emitted_absent(Target) ->
    s160_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_texture_parameter_i">>,
            <<"glGetTextureParameterIiv">>,
            <<"glGetTextureParameterIuiv">>
            | s160_forbidden_neighbors()
        ]
    ).

s160_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard160-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s160_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s160_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s160_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s160_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s160_texture_parameter_i_command(i) -> "glGetTextureParameterIiv";
s160_texture_parameter_i_command(ui) -> "glGetTextureParameterIuiv".

s160_gl_ctype(gl_int) -> "GLint";
s160_gl_ctype(gl_uint) -> "GLuint".

s160_term_function(gl_int) -> "enif_make_int";
s160_term_function(gl_uint) -> "enif_make_uint".

s160_texture_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s160_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s160_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s160_forbidden_neighbors() ->
    [
        <<"glGetTextureParameterfvEXT">>,
        <<"glGetTextureParameterivEXT">>,
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s160_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s160_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 161.
s161_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s161_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s161_emitter_sampler_parameter_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s161_assert_emitted_present({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s161_assert_emitted_present({gles, {3, 2}})
        end},
        {"gles 2.0", fun() ->
            s161_assert_emitted_absent({gles, {2, 0}})
        end}
    ].

s161_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s161_supports_sampler_parameter_readback(Target) of
        true ->
            ?assert(maps:is_key({"get_sampler_parameter", 4}, Functions)),
            ?assertNot(maps:is_key({"get_sampler_parameter", 3}, Functions)),
            GetSamplerParameter = maps:get({"get_sampler_parameter", 4}, Functions),
            s161_assert_specs(GetSamplerParameter),
            s161_assert_family(GetSamplerParameter, f, gl_float),
            s161_assert_family(GetSamplerParameter, i, gl_int);
        false ->
            ?assertNot(maps:is_key({"get_sampler_parameter", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetSamplerParameterfv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetSamplerParameteriv", Functions))
    end,
    s161_assert_deferred_neighbors_absent(Functions).

s161_supports_sampler_parameter_readback({gles, {2, 0}}) ->
    false;
s161_supports_sampler_parameter_readback(_) ->
    true.

s161_assert_specs(GetSamplerParameter) ->
    ?assertEqual(
        [
            {in, "Sampler", {gl_object, sampler}},
            {in, "ParamName", {gl_enum, ["SamplerParameterF", "SamplerParameterI"], sampler_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetSamplerParameter)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Sampler", {undefined, sampler, []}},
            {"ParamName", {undefined, sampler_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetSamplerParameter)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_sampler_parameter_value, []}}}],
        maps:get(specs_return, GetSamplerParameter)
    ),
    ?assertEqual(4, maps:get(function_arity, GetSamplerParameter)),
    {get_sampler_parameter_value, {set, ValueTypes}} = maps:get(extra_type, GetSamplerParameter),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)).

s161_assert_family(GetSamplerParameter, TypeAtom, GlType) ->
    Command = s161_sampler_parameter_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetSamplerParameter))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetSamplerParameter))),
    s161_assert_clause(GetSamplerParameter, TypeAtom, Command),
    s161_assert_nif(GetSamplerParameter, Command, GlType).

s161_assert_clause(GetSamplerParameter, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s161_find_clause(Command, maps:get(function_clauses, GetSamplerParameter)),
    [
        {Suffix, ignore},
        {"Sampler", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_min_lod", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s161_assert_nif(GetSamplerParameter, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetSamplerParameter)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Sampler", s161_sampler_nif_data()},
            {"ParamName", s161_enum_nif_data()},
            {"Values", {out_typed_value_list, s161_gl_ctype(GlType), s161_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s161_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s161_assert_emitted_present(Target) ->
    s161_assert_emitted_surface(
        Target,
        [
            <<"-export([get_sampler_parameter/4]).">>,
            <<"-spec get_sampler_parameter(\n    Type :: f | i,\n    Sampler :: sampler(),\n    ParamName :: sampler_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_sampler_parameter_value()]} | {error, atom()}.">>,
            <<"get_sampler_parameter(f, Sampler, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetSamplerParameterfv_raw(Sampler, NewParamName, Count))">>,
            <<"get_sampler_parameter(i, Sampler, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetSamplerParameteriv_raw(Sampler, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"GLfloat* arg_2_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_2_count);">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"glGetSamplerParameterfv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetSamplerParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetSamplerParameterfv_raw\", 3, nif_glGetSamplerParameterfv, 0}">>,
            <<"{\"glGetSamplerParameteriv_raw\", 3, nif_glGetSamplerParameteriv, 0}">>
        ],
        s161_forbidden_neighbors()
    ).

s161_assert_emitted_absent(Target) ->
    s161_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_sampler_parameter">>,
            <<"glGetSamplerParameterfv">>,
            <<"glGetSamplerParameteriv">>
            | s161_forbidden_neighbors()
        ]
    ).

s161_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard161-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s161_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s161_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s161_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s161_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s161_sampler_parameter_command(f) -> "glGetSamplerParameterfv";
s161_sampler_parameter_command(i) -> "glGetSamplerParameteriv".

s161_gl_ctype(gl_float) -> "GLfloat";
s161_gl_ctype(gl_int) -> "GLint".

s161_term_function(gl_float) -> "enif_make_double";
s161_term_function(gl_int) -> "enif_make_int".

s161_sampler_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s161_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s161_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s161_forbidden_neighbors() ->
    [
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s161_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s161_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 162.
s162_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s162_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s162_emitter_sampler_parameter_i_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s162_assert_emitted_present({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s162_assert_emitted_present({gles, {3, 2}})
        end},
        {"gles 3.1", fun() ->
            s162_assert_emitted_absent({gles, {3, 1}})
        end}
    ].

s162_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s162_supports_sampler_parameter_i_readback(Target) of
        true ->
            ?assert(maps:is_key({"get_sampler_parameter_i", 4}, Functions)),
            ?assertNot(maps:is_key({"get_sampler_parameter_i", 3}, Functions)),
            GetSamplerParameterI = maps:get({"get_sampler_parameter_i", 4}, Functions),
            s162_assert_specs(GetSamplerParameterI),
            s162_assert_family(GetSamplerParameterI, i, gl_int),
            s162_assert_family(GetSamplerParameterI, ui, gl_uint);
        false ->
            ?assertNot(maps:is_key({"get_sampler_parameter_i", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetSamplerParameterIiv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetSamplerParameterIuiv", Functions))
    end,
    s162_assert_deferred_neighbors_absent(Functions).

s162_supports_sampler_parameter_i_readback({gl, _Version}) ->
    true;
s162_supports_sampler_parameter_i_readback({gles, {3, 2}}) ->
    true;
s162_supports_sampler_parameter_i_readback(_) ->
    false.

s162_assert_specs(GetSamplerParameterI) ->
    ?assertEqual(
        [
            {in, "Sampler", {gl_object, sampler}},
            {in, "ParamName", {gl_enum, "SamplerParameterI", sampler_parameter_i_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetSamplerParameterI)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, ui]}},
            {"Sampler", {undefined, sampler, []}},
            {"ParamName", {undefined, sampler_parameter_i_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetSamplerParameterI)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_sampler_parameter_i_value, []}}}],
        maps:get(specs_return, GetSamplerParameterI)
    ),
    ?assertEqual(4, maps:get(function_arity, GetSamplerParameterI)),
    {get_sampler_parameter_i_value, {set, ValueTypes}} = maps:get(extra_type, GetSamplerParameterI),
    ?assert(lists:member({gl, int, []}, ValueTypes)),
    ?assert(lists:member({gl, uint, []}, ValueTypes)).

s162_assert_family(GetSamplerParameterI, TypeAtom, GlType) ->
    Command = s162_sampler_parameter_i_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetSamplerParameterI))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetSamplerParameterI))),
    s162_assert_clause(GetSamplerParameterI, TypeAtom, Command),
    s162_assert_nif(GetSamplerParameterI, Command, GlType).

s162_assert_clause(GetSamplerParameterI, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s162_find_clause(Command, maps:get(function_clauses, GetSamplerParameterI)),
    [
        {Suffix, ignore},
        {"Sampler", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_wrap_t", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s162_assert_nif(GetSamplerParameterI, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetSamplerParameterI)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Sampler", s162_sampler_nif_data()},
            {"ParamName", s162_enum_nif_data()},
            {"Values", {out_typed_value_list, s162_gl_ctype(GlType), s162_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s162_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s162_assert_emitted_present(Target) ->
    s162_assert_emitted_surface(
        Target,
        [
            <<"-export([get_sampler_parameter_i/4]).">>,
            <<"-spec get_sampler_parameter_i(\n    Type :: i | ui,\n    Sampler :: sampler(),\n    ParamName :: sampler_parameter_i_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_sampler_parameter_i_value()]} | {error, atom()}.">>,
            <<"get_sampler_parameter_i(i, Sampler, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetSamplerParameterIiv_raw(Sampler, NewParamName, Count))">>,
            <<"get_sampler_parameter_i(ui, Sampler, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetSamplerParameterIuiv_raw(Sampler, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"GLuint* arg_2_values = enif_alloc(sizeof(GLuint) * (size_t)arg_2_count);">>,
            <<"glGetSamplerParameterIiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetSamplerParameterIuiv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetSamplerParameterIiv_raw\", 3, nif_glGetSamplerParameterIiv, 0}">>,
            <<"{\"glGetSamplerParameterIuiv_raw\", 3, nif_glGetSamplerParameterIuiv, 0}">>
        ],
        s162_forbidden_neighbors()
    ).

s162_assert_emitted_absent(Target) ->
    s162_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_sampler_parameter_i">>,
            <<"glGetSamplerParameterIiv">>,
            <<"glGetSamplerParameterIuiv">>
            | s162_forbidden_neighbors()
        ]
    ).

s162_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard162-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s162_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s162_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s162_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s162_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s162_sampler_parameter_i_command(i) -> "glGetSamplerParameterIiv";
s162_sampler_parameter_i_command(ui) -> "glGetSamplerParameterIuiv".

s162_gl_ctype(gl_int) -> "GLint";
s162_gl_ctype(gl_uint) -> "GLuint".

s162_term_function(gl_int) -> "enif_make_int";
s162_term_function(gl_uint) -> "enif_make_uint".

s162_sampler_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s162_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s162_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s162_forbidden_neighbors() ->
    [
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>,
        <<"glGetSamplerParameterIivEXT">>,
        <<"glGetSamplerParameterIuivEXT">>,
        <<"glGetSamplerParameterIivOES">>,
        <<"glGetSamplerParameterIuivOES">>
    ].

s162_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s162_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 168.
s168_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s168_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s168_emitter_object_parameter_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s168_assert_emitted_gl46()
        end},
        {"gl 4.1", fun() ->
            s168_assert_emitted_gl41()
        end},
        {"gles 3.2", fun() ->
            s168_assert_emitted_es32()
        end},
        {"gles 2.0", fun() ->
            s168_assert_emitted_es20()
        end}
    ].

s168_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),

    s168_assert_buffer_parameter(Functions, s168_buffer_variants(Target)),
    s168_assert_named_buffer_parameter(Functions, s168_named_buffer_variants(Target)),
    s168_assert_renderbuffer_parameter(Functions, s168_supports_renderbuffer_parameter(Target)),
    s168_assert_named_renderbuffer_parameter(Functions, s168_supports_named_dsa(Target)),
    s168_assert_framebuffer_parameter(Functions, s168_supports_framebuffer_parameter(Target)),
    s168_assert_named_framebuffer_parameter(Functions, s168_supports_named_dsa(Target)),
    s168_assert_framebuffer_attachment_parameter(
        Functions,
        s168_supports_framebuffer_attachment_parameter(Target)
    ),
    s168_assert_named_framebuffer_attachment_parameter(Functions, s168_supports_named_dsa(Target)),
    s168_assert_deferred_neighbors_absent(Functions).

s168_buffer_variants({gles, {2, 0}}) ->
    [{"glGetBufferParameteriv", i, gl_int}];
s168_buffer_variants(_) ->
    [
        {"glGetBufferParameteriv", i, gl_int},
        {"glGetBufferParameteri64v", i64, gl_int64}
    ].

s168_named_buffer_variants({gl, {4, 6}}) ->
    [
        {"glGetNamedBufferParameteriv", i, gl_int},
        {"glGetNamedBufferParameteri64v", i64, gl_int64}
    ];
s168_named_buffer_variants(_) ->
    [].

s168_supports_renderbuffer_parameter(_Target) ->
    true.

s168_supports_framebuffer_attachment_parameter(_Target) ->
    true.

s168_supports_framebuffer_parameter({gl, {4, 6}}) ->
    true;
s168_supports_framebuffer_parameter({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s168_supports_framebuffer_parameter(_) ->
    false.

s168_supports_named_dsa({gl, {4, 6}}) ->
    true;
s168_supports_named_dsa(_) ->
    false.

s168_assert_buffer_parameter(Functions, Expected) ->
    ?assert(maps:is_key({"get_buffer_parameter", 4}, Functions)),
    ?assertNot(maps:is_key({"get_buffer_parameter", 3}, Functions)),
    Function = maps:get({"get_buffer_parameter", 4}, Functions),
    s168_assert_aggregate_specs(
        Function,
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "ParamName", {gl_enum, "BufferPNameARB", buffer_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"ParamName", {undefined, buffer_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        get_buffer_parameter_value,
        Expected
    ),
    [
        s168_assert_aggregate_family(Function, Variant, ["array_buffer", "buffer_size"])
     || Variant <- Expected
    ].

s168_assert_named_buffer_parameter(Functions, []) ->
    ?assertNot(maps:is_key({"get_named_buffer_parameter", 4}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetNamedBufferParameteriv", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetNamedBufferParameteri64v", Functions));
s168_assert_named_buffer_parameter(Functions, Expected) ->
    ?assert(maps:is_key({"get_named_buffer_parameter", 4}, Functions)),
    Function = maps:get({"get_named_buffer_parameter", 4}, Functions),
    s168_assert_aggregate_specs(
        Function,
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "ParamName", {gl_enum, "BufferPNameARB", buffer_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        [
            {"Buffer", {undefined, buffer, []}},
            {"ParamName", {undefined, buffer_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        get_named_buffer_parameter_value,
        Expected
    ),
    [
        s168_assert_aggregate_family(Function, Variant, ["buffer_size"])
     || Variant <- Expected
    ].

s168_assert_renderbuffer_parameter(Functions, true) ->
    Function = maps:get({"get_renderbuffer_parameter", 3}, Functions),
    s168_assert_direct_specs(
        Function,
        [
            {in, "Target", {gl_enum, "RenderbufferTarget"}},
            {in, "ParamName", {gl_enum, "RenderbufferParameterName", renderbuffer_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Target", {undefined, renderbuffer_target, []}},
            {"ParamName", {undefined, renderbuffer_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        "glGetRenderbufferParameteriv",
        3,
        ["renderbuffer", "renderbuffer_width"]
    ).

s168_assert_named_renderbuffer_parameter(Functions, false) ->
    ?assertNot(maps:is_key({"get_named_renderbuffer_parameter", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetNamedRenderbufferParameteriv", Functions));
s168_assert_named_renderbuffer_parameter(Functions, true) ->
    Function = maps:get({"get_named_renderbuffer_parameter", 3}, Functions),
    s168_assert_direct_specs(
        Function,
        [
            {in, "Renderbuffer", {gl_object, renderbuffer}},
            {in, "ParamName", {gl_enum, "RenderbufferParameterName", renderbuffer_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Renderbuffer", {undefined, renderbuffer, []}},
            {"ParamName", {undefined, renderbuffer_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        "glGetNamedRenderbufferParameteriv",
        3,
        ["renderbuffer_width"]
    ).

s168_assert_framebuffer_parameter(Functions, false) ->
    ?assertNot(maps:is_key({"get_framebuffer_parameter", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetFramebufferParameteriv", Functions));
s168_assert_framebuffer_parameter(Functions, true) ->
    Function = maps:get({"get_framebuffer_parameter", 3}, Functions),
    s168_assert_direct_specs(
        Function,
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "ParamName", {gl_enum, "GetFramebufferParameter", framebuffer_query_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"ParamName", {undefined, framebuffer_query_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        "glGetFramebufferParameteriv",
        3,
        ["framebuffer", "framebuffer_default_width"]
    ).

s168_assert_named_framebuffer_parameter(Functions, false) ->
    ?assertNot(maps:is_key({"get_named_framebuffer_parameter", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetNamedFramebufferParameteriv", Functions));
s168_assert_named_framebuffer_parameter(Functions, true) ->
    Function = maps:get({"get_named_framebuffer_parameter", 3}, Functions),
    s168_assert_direct_specs(
        Function,
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "ParamName", {gl_enum, "GetFramebufferParameter", framebuffer_query_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"ParamName", {undefined, framebuffer_query_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        "glGetNamedFramebufferParameteriv",
        3,
        ["framebuffer_default_width"]
    ).

s168_assert_framebuffer_attachment_parameter(Functions, true) ->
    Function = maps:get({"get_framebuffer_attachment_parameter", 4}, Functions),
    s168_assert_direct_specs(
        Function,
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
            {in, "ParamName",
                {gl_enum, "FramebufferAttachmentParameterName", framebuffer_attachment_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Attachment", {undefined, framebuffer_attachment, []}},
            {"ParamName", {undefined, framebuffer_attachment_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        "glGetFramebufferAttachmentParameteriv",
        4,
        ["framebuffer", "color_attachment0", "framebuffer_attachment_object_name"]
    ).

s168_assert_named_framebuffer_attachment_parameter(Functions, false) ->
    ?assertNot(maps:is_key({"get_named_framebuffer_attachment_parameter", 4}, Functions)),
    ?assertNot(
        generator_test_support:has_gl_command("glGetNamedFramebufferAttachmentParameteriv", Functions)
    );
s168_assert_named_framebuffer_attachment_parameter(Functions, true) ->
    Function = maps:get({"get_named_framebuffer_attachment_parameter", 4}, Functions),
    s168_assert_direct_specs(
        Function,
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
            {in, "ParamName",
                {gl_enum, "FramebufferAttachmentParameterName", framebuffer_attachment_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Attachment", {undefined, framebuffer_attachment, []}},
            {"ParamName", {undefined, framebuffer_attachment_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        "glGetNamedFramebufferAttachmentParameteriv",
        4,
        ["color_attachment0", "framebuffer_attachment_object_name"]
    ).

s168_assert_aggregate_specs(Function, ParamsSpecs, SpecsParamsTail, ExtraTypeName, Expected) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, Function)),
    ExpectedTypeAtoms = [TypeAtom || {_Command, TypeAtom, _GlType} <- Expected],
    ?assertEqual(
        [{"Type", {set, ExpectedTypeAtoms}} | SpecsParamsTail],
        maps:get(specs_params, Function)
    ),
    ExpectedValueTypes = [s168_type_spec(GlType) || {_Command, _TypeAtom, GlType} <- Expected],
    case ExpectedValueTypes of
        [_Single] ->
            ?assertEqual([{"Values", {list, hd(ExpectedValueTypes)}}], maps:get(specs_return, Function)),
            ?assertEqual(undefined, maps:get(extra_type, Function));
        _ ->
            ?assertEqual(
                [{"Values", {list, {undefined, ExtraTypeName, []}}}],
                maps:get(specs_return, Function)
            ),
            {ExtraTypeName, {set, ValueTypes}} = maps:get(extra_type, Function),
            [?assert(lists:member(ValueType, ValueTypes)) || ValueType <- ExpectedValueTypes]
    end,
    ?assertEqual(4, maps:get(function_arity, Function)).

s168_assert_aggregate_family(Function, {Command, TypeAtom, GlType}, ExpectedEnumAtoms) ->
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, Function))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, Function))),
    Suffix = atom_to_list(TypeAtom),
    Clause = s168_find_clause(Command, maps:get(function_clauses, Function)),
    Params = maps:get(params, Clause),
    ?assertMatch([{Suffix, ignore} | _], Params),
    s168_assert_transform_atoms(ExpectedEnumAtoms, Params),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s168_assert_nif(Function, Command, GlType).

s168_assert_direct_specs(Function, ParamsSpecs, SpecsParams, Command, Arity, ExpectedEnumAtoms) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, Function)),
    ?assertEqual(SpecsParams, maps:get(specs_params, Function)),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(undefined, maps:get(extra_type, Function)),
    ?assertEqual(Arity, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    s168_assert_transform_atoms(ExpectedEnumAtoms, maps:get(params, Clause)),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s168_assert_nif(Function, Command, gl_int).

s168_assert_transform_atoms(ExpectedEnumAtoms, Params) ->
    TransformAtoms = lists:append(
        [
            [Atom || {Atom, _Constant} <- TransformMap]
         || {_Name, {gl_enum_to_uint, TransformMap}} <- Params
        ]
    ),
    [?assert(lists:member(Atom, TransformAtoms)) || Atom <- ExpectedEnumAtoms].

s168_assert_nif(Function, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(void, maps:get(return, NifData)),
    Params = maps:get(params, NifData),
    ?assertEqual(
        {"Values", {out_typed_value_list, s168_gl_ctype(GlType), s168_term_function(GlType)}},
        lists:last(Params)
    ).

s168_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s168_assert_emitted_gl46() ->
    s168_assert_emitted_surface(
        {gl, {4, 6}},
        [
            <<"-export([get_buffer_parameter/4]).">>,
            <<"-export([get_named_buffer_parameter/4]).">>,
            <<"-export([get_renderbuffer_parameter/3]).">>,
            <<"-export([get_named_renderbuffer_parameter/3]).">>,
            <<"-export([get_framebuffer_parameter/3]).">>,
            <<"-export([get_named_framebuffer_parameter/3]).">>,
            <<"-export([get_framebuffer_attachment_parameter/4]).">>,
            <<"-export([get_named_framebuffer_attachment_parameter/4]).">>,
            <<"-export_type([buffer_parameter_name/0]).">>,
            <<"-export_type([framebuffer_query_parameter_name/0]).">>,
            <<"-export_type([framebuffer_attachment_parameter_name/0]).">>,
            <<"-export_type([renderbuffer_parameter_name/0]).">>,
            <<"-spec get_buffer_parameter(\n    Type :: i | i64,\n    Target :: buffer_target(),\n    ParamName :: buffer_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_buffer_parameter_value()]} | {error, atom()}.">>,
            <<"?CALL_RAW_FUNC(glGetBufferParameteriv_raw(NewTarget, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetBufferParameteri64v_raw(NewTarget, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetNamedBufferParameteriv_raw(Buffer, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetRenderbufferParameteriv_raw(NewTarget, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetFramebufferAttachmentParameteriv_raw(NewTarget, NewAttachment, NewParamName, Count))">>
        ],
        [
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"GLint64* arg_2_values = enif_alloc(sizeof(GLint64) * (size_t)arg_2_count);">>,
            <<"GLint* arg_3_values = enif_alloc(sizeof(GLint) * (size_t)arg_3_count);">>,
            <<"glGetBufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetBufferParameteri64v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetNamedBufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetNamedBufferParameteri64v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetRenderbufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetNamedRenderbufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFramebufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetNamedFramebufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFramebufferAttachmentParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"glGetNamedFramebufferAttachmentParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetNamedFramebufferAttachmentParameteriv_raw\", 4, nif_glGetNamedFramebufferAttachmentParameteriv, 0}">>
        ],
        s168_forbidden_neighbors()
    ).

s168_assert_emitted_gl41() ->
    s168_assert_emitted_surface(
        {gl, {4, 1}},
        [
            <<"-export([get_buffer_parameter/4]).">>,
            <<"-export([get_renderbuffer_parameter/3]).">>,
            <<"-export([get_framebuffer_attachment_parameter/4]).">>,
            <<"glGetBufferParameteri64v_raw">>
        ],
        [
            <<"glGetBufferParameteri64v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetRenderbufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFramebufferAttachmentParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>
        ],
        [
            <<"get_named_buffer_parameter">>,
            <<"get_named_renderbuffer_parameter">>,
            <<"get_framebuffer_parameter">>,
            <<"get_named_framebuffer_parameter">>,
            <<"get_named_framebuffer_attachment_parameter">>,
            <<"glGetNamedBufferParameteriv">>,
            <<"glGetNamedRenderbufferParameteriv">>,
            <<"glGetFramebufferParameteriv">>,
            <<"glGetNamedFramebufferParameteriv">>,
            <<"glGetNamedFramebufferAttachmentParameteriv">>
            | s168_forbidden_neighbors()
        ]
    ).

s168_assert_emitted_es32() ->
    s168_assert_emitted_surface(
        {gles, {3, 2}},
        [
            <<"-export([get_buffer_parameter/4]).">>,
            <<"-spec get_buffer_parameter(\n    Type :: i | i64,">>,
            <<"-export([get_renderbuffer_parameter/3]).">>,
            <<"-export([get_framebuffer_parameter/3]).">>,
            <<"-export([get_framebuffer_attachment_parameter/4]).">>,
            <<"glGetBufferParameteri64v_raw">>,
            <<"glGetFramebufferParameteriv_raw">>
        ],
        [
            <<"glGetBufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetBufferParameteri64v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFramebufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFramebufferAttachmentParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>
        ],
        [
            <<"get_named_buffer_parameter">>,
            <<"get_named_renderbuffer_parameter">>,
            <<"get_named_framebuffer_parameter">>,
            <<"get_named_framebuffer_attachment_parameter">>,
            <<"glGetNamedBufferParameteriv">>,
            <<"glGetNamedRenderbufferParameteriv">>,
            <<"glGetNamedFramebufferParameteriv">>,
            <<"glGetNamedFramebufferAttachmentParameteriv">>
            | s168_forbidden_neighbors()
        ]
    ).

s168_assert_emitted_es20() ->
    s168_assert_emitted_surface(
        {gles, {2, 0}},
        [
            <<"-export([get_buffer_parameter/4]).">>,
            <<"-spec get_buffer_parameter(\n    Type :: i,\n    Target :: buffer_target(),\n    ParamName :: buffer_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>,
            <<"-export([get_renderbuffer_parameter/3]).">>,
            <<"-export([get_framebuffer_attachment_parameter/4]).">>,
            <<"?CALL_RAW_FUNC(glGetBufferParameteriv_raw(NewTarget, NewParamName, Count))">>
        ],
        [
            <<"glGetBufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetRenderbufferParameteriv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFramebufferAttachmentParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>
        ],
        [
            <<"get_named_buffer_parameter">>,
            <<"get_framebuffer_parameter">>,
            <<"get_named_framebuffer_parameter">>,
            <<"get_named_renderbuffer_parameter">>,
            <<"get_named_framebuffer_attachment_parameter">>,
            <<"glGetBufferParameteri64v">>,
            <<"glGetNamedBufferParameteriv">>,
            <<"glGetFramebufferParameteriv">>,
            <<"glGetNamedFramebufferParameteriv">>,
            <<"glGetNamedRenderbufferParameteriv">>,
            <<"glGetNamedFramebufferAttachmentParameteriv">>
            | s168_forbidden_neighbors()
        ]
    ).

s168_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard168-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s168_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s168_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s168_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s168_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s168_type_spec(gl_int) -> {gl, int, []};
s168_type_spec(gl_int64) -> {gl, int64, []}.

s168_gl_ctype(gl_int) -> "GLint";
s168_gl_ctype(gl_int64) -> "GLint64".

s168_term_function(gl_int) -> "enif_make_int";
s168_term_function(gl_int64) -> "enif_make_int64".

s168_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s168_forbidden_neighbors() ->
    [
        <<"glGetBufferPointerv">>,
        <<"glGetNamedBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s168_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s168_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 169.
s169_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s169_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s169_emitter_indexed_query_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s169_assert_emitted_present({gl, {4, 6}})
        end},
        {"gl 4.1", fun() ->
            s169_assert_emitted_present({gl, {4, 1}})
        end},
        {"gl 3.3", fun() ->
            s169_assert_emitted_absent({gl, {3, 3}})
        end},
        {"gles 3.2", fun() ->
            s169_assert_emitted_absent({gles, {3, 2}})
        end}
    ].

s169_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s169_supports_get_query_indexed(Target) of
        true ->
            ?assert(maps:is_key({"get_query", 3}, Functions)),
            ?assert(maps:is_key({"get_query", 4}, Functions)),
            GetQuery = maps:get({"get_query", 4}, Functions),
            s169_assert_specs(GetQuery),
            s169_assert_clause(GetQuery),
            s169_assert_nif(GetQuery);
        false ->
            ?assertNot(maps:is_key({"get_query", 4}, Functions)),
            s169_assert_command_absent("glGetQueryIndexediv", Functions)
    end,
    s169_assert_deferred_neighbors_absent(Functions).

s169_supports_get_query_indexed({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s169_supports_get_query_indexed({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s169_supports_get_query_indexed(_) ->
    false.

s169_assert_command_absent(Command, Functions) ->
    ?assertNot(generator_test_support:has_gl_command(Command, Functions)),
    ok.

s169_assert_specs(GetQuery) ->
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "QueryTarget"}},
            {in, "Index", gl_uint},
            {in, "ParamName", {gl_enum, "QueryParameterName", query_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, GetQuery)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, query_target, []}},
            {"Index", {gl, uint, []}},
            {"ParamName", {undefined, query_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetQuery)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, GetQuery)),
    ?assertEqual(undefined, maps:get(extra_type, GetQuery)),
    ?assertEqual(4, maps:get(function_arity, GetQuery)).

s169_assert_clause(GetQuery) ->
    [Clause] = maps:get(function_clauses, GetQuery),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"Index", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("primitives_generated", 1, TargetTransformMap)),
    ?assert(lists:keymember("current_query", 1, ParamNameTransformMap)),
    ?assertEqual("glGetQueryIndexediv", maps:get(raw_function, Clause)).

s169_assert_nif(GetQuery) ->
    NifData = maps:get("glGetQueryIndexediv", maps:get(nif_functions, GetQuery)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s169_enum_nif_data()},
            {"Index", s169_uint_nif_data()},
            {"ParamName", s169_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s169_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetQueryivARB",
        "glGetQueryivEXT",
        "glGetQueryObjectivARB",
        "glGetQueryObjectuivARB",
        "glGetQueryObjectivEXT",
        "glGetQueryObjectuivEXT",
        "glGetQueryObjecti64vEXT",
        "glGetQueryObjectui64vEXT",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s169_assert_emitted_present(Target) ->
    s169_assert_emitted_surface(
        Target,
        [
            <<"-export([get_query/3]).">>,
            <<"-export([get_query/4]).">>,
            <<"-export_type([query_parameter_name/0]).">>,
            <<"-spec get_query(\n    Target :: query_target(),\n    Index :: gl:uint(),\n    ParamName :: query_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>,
            <<"get_query(Target, Index, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryIndexediv_raw(NewTarget, Index, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_3_count_tmp;">>,
            <<"if (arg_3_count_tmp == 0 || arg_3_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLint* arg_3_values = enif_alloc(sizeof(GLint) * (size_t)arg_3_count);">>,
            <<"glGetQueryIndexediv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetQueryIndexediv_raw\", 4, nif_glGetQueryIndexediv, 0}">>
        ],
        s169_forbidden_neighbors()
    ).

s169_assert_emitted_absent(Target) ->
    s169_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"glGetQueryIndexediv">>,
            <<"-export([get_query/4]).">>
            | s169_forbidden_neighbors()
        ]
    ).

s169_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard169-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s169_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s169_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s169_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s169_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s169_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s169_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s169_forbidden_neighbors() ->
    [
        <<"glGetQueryivARB">>,
        <<"glGetQueryivEXT">>,
        <<"glGetQueryObjectivARB">>,
        <<"glGetQueryObjectuivARB">>,
        <<"glGetQueryObjectivEXT">>,
        <<"glGetQueryObjectuivEXT">>,
        <<"glGetQueryObjecti64vEXT">>,
        <<"glGetQueryObjectui64vEXT">>,
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s169_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s169_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 170.
s170_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s170_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s170_emitter_vertex_transform_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s170_assert_emitted_gl46()
        end},
        {"gl 4.1", fun() ->
            s170_assert_emitted_gl41()
        end},
        {"gl 3.3", fun() ->
            s170_assert_emitted_gl33()
        end},
        {"gles 3.2", fun() ->
            s170_assert_emitted_es32()
        end},
        {"gles 2.0", fun() ->
            s170_assert_emitted_es20()
        end}
    ].

s170_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    s170_assert_vertex_attrib(Functions, s170_vertex_attrib_variants(Target)),
    s170_assert_vertex_attrib_i(Functions, s170_vertex_attrib_i_variants(Target)),
    s170_assert_vertex_attrib_l(Functions, s170_supports_vertex_attrib_l(Target)),
    s170_assert_transform_feedback(Functions, s170_supports_transform_feedback_readback(Target)),
    s170_assert_deferred_neighbors_absent(Functions).

s170_vertex_attrib_variants({gl, _Version}) ->
    [
        {"glGetVertexAttribdv", d, gl_double},
        {"glGetVertexAttribfv", f, gl_float},
        {"glGetVertexAttribiv", i, gl_int}
    ];
s170_vertex_attrib_variants({gles, _Version}) ->
    [
        {"glGetVertexAttribfv", f, gl_float},
        {"glGetVertexAttribiv", i, gl_int}
    ].

s170_vertex_attrib_i_variants({gles, {2, 0}}) ->
    [];
s170_vertex_attrib_i_variants(_) ->
    [
        {"glGetVertexAttribIiv", i, gl_int},
        {"glGetVertexAttribIuiv", ui, gl_uint}
    ].

s170_supports_vertex_attrib_l({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s170_supports_vertex_attrib_l({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s170_supports_vertex_attrib_l(_) ->
    false.

s170_supports_transform_feedback_readback({gl, {4, 6}}) ->
    true;
s170_supports_transform_feedback_readback(_) ->
    false.

s170_assert_vertex_attrib(Functions, Expected) ->
    Function = maps:get({"get_vertex_attrib", 4}, Functions),
    s170_assert_aggregate_specs(
        Function,
        [
            {in, "Index", gl_uint},
            {in, "ParamName", {gl_enum, "VertexAttribPropertyARB", vertex_attrib_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        [
            {"Index", {gl, uint, []}},
            {"ParamName", {undefined, vertex_attrib_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        get_vertex_attrib_value,
        Expected,
        4
    ),
    [
        s170_assert_aggregate_family(Function, Variant, ["current_vertex_attrib"], 3)
     || Variant <- Expected
    ].

s170_assert_vertex_attrib_i(Functions, []) ->
    ?assertNot(maps:is_key({"get_vertex_attrib_i", 4}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetVertexAttribIiv", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetVertexAttribIuiv", Functions));
s170_assert_vertex_attrib_i(Functions, Expected) ->
    Function = maps:get({"get_vertex_attrib_i", 4}, Functions),
    s170_assert_aggregate_specs(
        Function,
        [
            {in, "Index", gl_uint},
            {in, "ParamName", {gl_enum, "VertexAttribEnum", vertex_attrib_i_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        [
            {"Index", {gl, uint, []}},
            {"ParamName", {undefined, vertex_attrib_i_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        get_vertex_attrib_i_value,
        Expected,
        4
    ),
    [
        s170_assert_aggregate_family(Function, Variant, ["current_vertex_attrib"], 3)
     || Variant <- Expected
    ].

s170_assert_vertex_attrib_l(Functions, false) ->
    ?assertNot(maps:is_key({"get_vertex_attrib_l", 3}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetVertexAttribLdv", Functions));
s170_assert_vertex_attrib_l(Functions, true) ->
    Function = maps:get({"get_vertex_attrib_l", 3}, Functions),
    s170_assert_direct_specs(
        Function,
        [
            {in, "Index", gl_uint},
            {in, "ParamName", {gl_enum, "VertexAttribEnum", vertex_attrib_l_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_double}}
        ],
        [
            {"Index", {gl, uint, []}},
            {"ParamName", {undefined, vertex_attrib_l_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        [{"Values", {list, {gl, double, []}}}],
        "glGetVertexAttribLdv",
        gl_double,
        3,
        ["current_vertex_attrib"]
    ).

s170_assert_transform_feedback(Functions, false) ->
    ?assertNot(maps:is_key({"get_transform_feedback", 3}, Functions)),
    ?assertNot(maps:is_key({"get_transform_feedback", 5}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetTransformFeedbackiv", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetTransformFeedbacki_v", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetTransformFeedbacki64_v", Functions));
s170_assert_transform_feedback(Functions, true) ->
    Direct = maps:get({"get_transform_feedback", 3}, Functions),
    s170_assert_direct_specs(
        Direct,
        [
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "ParamName", {gl_enum, "TransformFeedbackPName", transform_feedback_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        [
            {"Feedback", {undefined, transform_feedback, []}},
            {"ParamName", {undefined, transform_feedback_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        [{"Values", {list, {gl, int, []}}}],
        "glGetTransformFeedbackiv",
        gl_int,
        3,
        ["transform_feedback_active", "transform_feedback_paused"]
    ),

    Indexed = maps:get({"get_transform_feedback", 5}, Functions),
    Expected = [
        {"glGetTransformFeedbacki_v", i, gl_int},
        {"glGetTransformFeedbacki64_v", i64, gl_int64}
    ],
    s170_assert_aggregate_specs(
        Indexed,
        [
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "ParamName", {gl_enum, "TransformFeedbackPName", transform_feedback_parameter_name}},
            {in, "Index", gl_uint},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        [
            {"Feedback", {undefined, transform_feedback, []}},
            {"ParamName", {undefined, transform_feedback_parameter_name, []}},
            {"Index", {gl, uint, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        get_transform_feedback_value,
        Expected,
        5
    ),
    [
        s170_assert_aggregate_family(
            Indexed,
            Variant,
            ["transform_feedback_buffer_binding", "transform_feedback_buffer_size"],
            4
        )
     || Variant <- Expected
    ].

s170_assert_aggregate_specs(Function, ParamsSpecs, SpecsParamsTail, ExtraTypeName, Expected, Arity) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, Function)),
    ExpectedTypeAtoms = [TypeAtom || {_Command, TypeAtom, _GlType} <- Expected],
    ?assertEqual(
        [{"Type", {set, ExpectedTypeAtoms}} | SpecsParamsTail],
        maps:get(specs_params, Function)
    ),
    ExpectedValueTypes = [s170_type_spec(GlType) || {_Command, _TypeAtom, GlType} <- Expected],
    case ExpectedValueTypes of
        [_Single] ->
            ?assertEqual([{"Values", {list, hd(ExpectedValueTypes)}}], maps:get(specs_return, Function)),
            ?assertEqual(undefined, maps:get(extra_type, Function));
        _ ->
            ?assertEqual(
                [{"Values", {list, {undefined, ExtraTypeName, []}}}],
                maps:get(specs_return, Function)
            ),
            {ExtraTypeName, {set, ValueTypes}} = maps:get(extra_type, Function),
            [?assert(lists:member(ValueType, ValueTypes)) || ValueType <- ExpectedValueTypes]
    end,
    ?assertEqual(Arity, maps:get(function_arity, Function)).

s170_assert_aggregate_family(Function, {Command, TypeAtom, GlType}, ExpectedEnumAtoms, RawArity) ->
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, Function))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, Function))),
    Suffix = atom_to_list(TypeAtom),
    Clause = s170_find_clause(Command, maps:get(function_clauses, Function)),
    Params = maps:get(params, Clause),
    ?assertMatch([{Suffix, ignore} | _], Params),
    s170_assert_transform_atoms(ExpectedEnumAtoms, Params),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s170_assert_nif(Function, Command, GlType, RawArity).

s170_assert_direct_specs(Function, ParamsSpecs, SpecsParams, SpecsReturn, Command, GlType, Arity, ExpectedEnumAtoms) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, Function)),
    ?assertEqual(SpecsParams, maps:get(specs_params, Function)),
    ?assertEqual(SpecsReturn, maps:get(specs_return, Function)),
    ?assertEqual(undefined, maps:get(extra_type, Function)),
    ?assertEqual(Arity, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    s170_assert_transform_atoms(ExpectedEnumAtoms, maps:get(params, Clause)),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s170_assert_nif(Function, Command, GlType, Arity).

s170_assert_transform_atoms(ExpectedEnumAtoms, Params) ->
    TransformAtoms = lists:append(
        [
            [Atom || {Atom, _Constant} <- TransformMap]
         || {_Name, {gl_enum_to_uint, TransformMap}} <- Params
        ]
    ),
    [?assert(lists:member(Atom, TransformAtoms)) || Atom <- ExpectedEnumAtoms].

s170_assert_nif(Function, Command, GlType, Arity) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    Params = maps:get(params, NifData),
    ?assertEqual(
        {"Values", {out_typed_value_list, s170_gl_ctype(GlType), s170_term_function(GlType)}},
        lists:last(Params)
    ).

s170_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s170_assert_emitted_gl46() ->
    s170_assert_emitted_surface(
        {gl, {4, 6}},
        [
            <<"-export([get_vertex_attrib/4]).">>,
            <<"-export([get_vertex_attrib_i/4]).">>,
            <<"-export([get_vertex_attrib_l/3]).">>,
            <<"-export([get_transform_feedback/3]).">>,
            <<"-export([get_transform_feedback/5]).">>,
            <<"-export_type([vertex_attrib_parameter_name/0]).">>,
            <<"-export_type([vertex_attrib_i_parameter_name/0]).">>,
            <<"-export_type([vertex_attrib_l_parameter_name/0]).">>,
            <<"-export_type([transform_feedback_parameter_name/0]).">>,
            <<"get_vertex_attrib(i, Index, ParamName, Count) ->">>,
            <<"get_vertex_attrib_i(ui, Index, ParamName, Count) ->">>,
            <<"get_vertex_attrib_l(Index, ParamName, Count) ->">>,
            <<"get_transform_feedback(Feedback, ParamName, Count) ->">>,
            <<"get_transform_feedback(i64, Feedback, ParamName, Index, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribdv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribIuiv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetTransformFeedbacki64_v_raw(Feedback, NewParamName, Index, Count))">>
        ],
        [
            <<"GLdouble* arg_2_values = enif_alloc(sizeof(GLdouble) * (size_t)arg_2_count);">>,
            <<"GLfloat* arg_2_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_2_count);">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"GLuint* arg_2_values = enif_alloc(sizeof(GLuint) * (size_t)arg_2_count);">>,
            <<"GLint64* arg_3_values = enif_alloc(sizeof(GLint64) * (size_t)arg_3_count);">>,
            <<"glGetVertexAttribdv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribfv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribIiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribIuiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribLdv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetTransformFeedbackiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetTransformFeedbacki_v(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"glGetTransformFeedbacki64_v(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetTransformFeedbacki64_v_raw\", 4, nif_glGetTransformFeedbacki64_v, 0}">>
        ],
        s170_forbidden_neighbors()
    ).

s170_assert_emitted_gl41() ->
    s170_assert_emitted_surface(
        {gl, {4, 1}},
        [
            <<"-export([get_vertex_attrib/4]).">>,
            <<"-export([get_vertex_attrib_i/4]).">>,
            <<"-export([get_vertex_attrib_l/3]).">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribdv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribIiv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribLdv_raw(Index, NewParamName, Count))">>
        ],
        [
            <<"glGetVertexAttribdv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribIiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribLdv(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"-export([get_transform_feedback/3]).">>,
            <<"-export([get_transform_feedback/5]).">>,
            <<"transform_feedback_parameter_name">>,
            <<"glGetTransformFeedbackiv">>,
            <<"glGetTransformFeedbacki_v">>,
            <<"glGetTransformFeedbacki64_v">>
            | s170_forbidden_neighbors()
        ]
    ).

s170_assert_emitted_gl33() ->
    s170_assert_emitted_surface(
        {gl, {3, 3}},
        [
            <<"-export([get_vertex_attrib/4]).">>,
            <<"-export([get_vertex_attrib_i/4]).">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribdv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribIuiv_raw(Index, NewParamName, Count))">>
        ],
        [
            <<"glGetVertexAttribdv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribIuiv(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"get_vertex_attrib_l">>,
            <<"vertex_attrib_l_parameter_name">>,
            <<"glGetVertexAttribLdv">>,
            <<"-export([get_transform_feedback/3]).">>,
            <<"-export([get_transform_feedback/5]).">>,
            <<"glGetTransformFeedbackiv">>,
            <<"glGetTransformFeedbacki_v">>,
            <<"glGetTransformFeedbacki64_v">>
            | s170_forbidden_neighbors()
        ]
    ).

s170_assert_emitted_es32() ->
    s170_assert_emitted_surface(
        {gles, {3, 2}},
        [
            <<"-export([get_vertex_attrib/4]).">>,
            <<"-export([get_vertex_attrib_i/4]).">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribfv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribIiv_raw(Index, NewParamName, Count))">>
        ],
        [
            <<"glGetVertexAttribfv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribIiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribIuiv(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"glGetVertexAttribdv">>,
            <<"get_vertex_attrib_l">>,
            <<"glGetVertexAttribLdv">>,
            <<"-export([get_transform_feedback/3]).">>,
            <<"-export([get_transform_feedback/5]).">>,
            <<"glGetTransformFeedbackiv">>,
            <<"glGetTransformFeedbacki_v">>,
            <<"glGetTransformFeedbacki64_v">>
            | s170_forbidden_neighbors()
        ]
    ).

s170_assert_emitted_es20() ->
    s170_assert_emitted_surface(
        {gles, {2, 0}},
        [
            <<"-export([get_vertex_attrib/4]).">>,
            <<"-spec get_vertex_attrib(\n    Type :: f | i,">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribfv_raw(Index, NewParamName, Count))">>,
            <<"?CALL_RAW_FUNC(glGetVertexAttribiv_raw(Index, NewParamName, Count))">>
        ],
        [
            <<"glGetVertexAttribfv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetVertexAttribiv(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"glGetVertexAttribdv">>,
            <<"get_vertex_attrib_i">>,
            <<"glGetVertexAttribIiv">>,
            <<"glGetVertexAttribIuiv">>,
            <<"get_vertex_attrib_l">>,
            <<"glGetVertexAttribLdv">>,
            <<"get_transform_feedback">>,
            <<"glGetTransformFeedbackiv">>,
            <<"glGetTransformFeedbacki_v">>,
            <<"glGetTransformFeedbacki64_v">>
            | s170_forbidden_neighbors()
        ]
    ).

s170_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard170-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s170_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s170_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s170_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s170_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s170_type_spec(gl_float) -> {gl, float, []};
s170_type_spec(gl_int) -> {gl, int, []};
s170_type_spec(gl_uint) -> {gl, uint, []};
s170_type_spec(gl_double) -> {gl, double, []};
s170_type_spec(gl_int64) -> {gl, int64, []}.

s170_gl_ctype(gl_float) -> "GLfloat";
s170_gl_ctype(gl_int) -> "GLint";
s170_gl_ctype(gl_uint) -> "GLuint";
s170_gl_ctype(gl_double) -> "GLdouble";
s170_gl_ctype(gl_int64) -> "GLint64".

s170_term_function(gl_float) -> "enif_make_double";
s170_term_function(gl_int) -> "enif_make_int";
s170_term_function(gl_uint) -> "enif_make_uint";
s170_term_function(gl_double) -> "enif_make_double";
s170_term_function(gl_int64) -> "enif_make_int64".

s170_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s170_forbidden_neighbors() ->
    [
        <<"glGetBufferPointerv">>,
        <<"glGetNamedBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>,
        <<"glMapBuffer">>,
        <<"glMapBufferRange">>,
        <<"glMapNamedBuffer">>,
        <<"glMapNamedBufferRange">>
    ].

s170_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s170_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 193.

s193_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s193_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s193_emitter_shader_binary_test_() ->
    [
        {"gl 4.6", fun() -> s193_assert_emitted_gl46() end},
        {"gl 4.1", fun() -> s193_assert_emitted_shader_binary_only({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s193_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s193_assert_emitted_shader_binary_only({gles, {3, 2}}) end},
        {"gles 2.0", fun() -> s193_assert_emitted_shader_binary_only({gles, {2, 0}}) end}
    ].

s193_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s193_supports_shader_binary(Target) of
        true ->
            s193_assert_shader_binary(maps:get({"shader_binary", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"shader_binary", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glShaderBinary", Functions))
    end,
    case s193_supports_specialize_shader(Target) of
        true ->
            s193_assert_specialize_shader(maps:get({"specialize_shader", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"specialize_shader", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glSpecializeShader", Functions))
    end,
    s193_assert_deferred_neighbors_absent(Functions).

s193_supports_shader_binary({gl, {4, 1}}) -> true;
s193_supports_shader_binary({gl, {4, 6}}) -> true;
s193_supports_shader_binary({gles, _Version}) -> true;
s193_supports_shader_binary(_) -> false.

s193_supports_specialize_shader({gl, {4, 6}}) -> true;
s193_supports_specialize_shader(_) -> false.

s193_assert_shader_binary(FunctionData) ->
    ?assertEqual("glShaderBinary", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Shaders", {gl_object_list_with_count, shader}},
            {in, "BinaryFormat", gl_enum_value},
            {in, "Binary", {byte_data_with_trailing_size, "Length", gl_sizei}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Shaders", {list, {undefined, shader, []}}},
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
            {"Shaders", do_nothing},
            {"BinaryFormat", do_nothing},
            {"Binary", {byte_data_with_trailing_size, "Length", gl_sizei}}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glShaderBinary", maps:get(raw_function, Clause)),

    NifData = maps:get("glShaderBinary", maps:get(nif_functions, FunctionData)),
    ?assertEqual("glShaderBinary", maps:get(gl_command, NifData)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Shaders", in_gl_object_list_with_count},
            {"BinaryFormat", s193_enum_value_nif_data()},
            {"Binary", binary_to_glbinary},
            {"Length", s193_sizei_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s193_assert_specialize_shader(FunctionData) ->
    ?assertEqual("glSpecializeShader", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Shader", {gl_object, shader}},
            {in, "EntryPoint", gl_string},
            {in, "SpecializationConstants", specialization_constant_list}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Shader", {undefined, shader, []}},
            {"EntryPoint", {undefined, iodata, []}},
            {"SpecializationConstants", {list, {tuple, [{gl, uint, []}, {gl, uint, []}]}}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Shader", do_nothing},
            {"EntryPoint", normalize_gl_string},
            {"SpecializationConstants", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glSpecializeShader", maps:get(raw_function, Clause)),

    NifData = maps:get("glSpecializeShader", maps:get(nif_functions, FunctionData)),
    ?assertEqual("glSpecializeShader", maps:get(gl_command, NifData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Shader", s193_uint_nif_data()},
            {"EntryPoint", in_gl_string},
            {"SpecializationConstants", in_specialization_constant_list}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s193_assert_emitted_gl46() ->
    s193_assert_emitted_surface(
        {gl, {4, 6}},
        s193_shader_binary_erlang_needles() ++ s193_specialize_shader_erlang_needles(),
        s193_shader_binary_c_needles() ++ s193_specialize_shader_c_needles(),
        s193_forbidden_needles()
    ).

s193_assert_emitted_shader_binary_only(Target) ->
    s193_assert_emitted_surface(
        Target,
        s193_shader_binary_erlang_needles(),
        s193_shader_binary_c_needles(),
        s193_specialize_shader_absent_needles() ++ s193_forbidden_needles()
    ).

s193_assert_emitted_absent(Target) ->
    s193_assert_emitted_surface(
        Target,
        [],
        [],
        s193_shader_binary_absent_needles() ++ s193_specialize_shader_absent_needles() ++ s193_forbidden_needles()
    ).

s193_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard193-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s193_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s193_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s193_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s193_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s193_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s193_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s193_shader_binary_erlang_needles() ->
    [
        <<"-export([shader_binary/3]).">>,
        <<"Shaders :: [shader()]">>,
        <<"BinaryFormat :: gl:uint()">>,
        <<"Binary :: iodata()">>,
        <<"Binary0 = iolist_to_binary(Binary),\n    Length = byte_size(Binary0),">>,
        <<"?CALL_RAW_FUNC(glShaderBinary_raw(Shaders, BinaryFormat, Binary0, Length)).">>
    ].

s193_specialize_shader_erlang_needles() ->
    [
        <<"-export([specialize_shader/3]).">>,
        <<"EntryPoint :: iodata()">>,
        <<"SpecializationConstants :: [{gl:uint(), gl:uint()}]">>,
        <<"EntryPoint0 = iolist_to_binary(EntryPoint),">>,
        <<"?CALL_RAW_FUNC(glSpecializeShader_raw(Shader, EntryPoint0, SpecializationConstants)).">>
    ].

s193_shader_binary_c_needles() ->
    [
        <<"GLuint* arg_0_objects = enif_alloc(sizeof(GLuint) * arg_0_count_tmp);">>,
        <<"GLsizei arg_0_count = (GLsizei)arg_0_count_tmp;">>,
        <<"if (!enif_get_uint(env, arg_0_head, &arg_0_object_tmp) || arg_0_object_tmp == 0) {">>,
        <<"glShaderBinary(arg_0_count, (const GLuint*)arg_0_objects, arg_1, (void*)arg_2.data, arg_3);">>,
        <<"enif_free(arg_0_objects);">>,
        <<"{\"glShaderBinary_raw\", 4, nif_glShaderBinary, 0}">>
    ].

s193_specialize_shader_c_needles() ->
    [
        <<"GLchar* arg_1_string = (GLchar*)enif_alloc(arg_1.size + 1);">>,
        <<"GLuint arg_2_count = (GLuint)arg_2_count_tmp;">>,
        <<"GLuint* arg_2_indices = NULL;">>,
        <<"GLuint* arg_2_values = NULL;">>,
        <<"glSpecializeShader(arg_0, (const GLchar*)arg_1_string, arg_2_count, (const GLuint*)arg_2_indices, (const GLuint*)arg_2_values);">>,
        <<"if (arg_2_indices) enif_free(arg_2_indices);">>,
        <<"if (arg_2_values) enif_free(arg_2_values);">>,
        <<"{\"glSpecializeShader_raw\", 3, nif_glSpecializeShader, 0}">>
    ].

s193_shader_binary_absent_needles() ->
    [
        <<"-export([shader_binary/3]).">>,
        <<"glShaderBinary(">>,
        <<"{\"glShaderBinary_raw\", 4, nif_glShaderBinary, 0}">>
    ].

s193_specialize_shader_absent_needles() ->
    [
        <<"-export([specialize_shader/3]).">>,
        <<"glSpecializeShader(">>,
        <<"{\"glSpecializeShader_raw\", 3, nif_glSpecializeShader, 0}">>
    ].

s193_forbidden_needles() ->
    [
        <<"glShaderBinaryOES(">>,
        <<"glSpecializeShaderARB(">>,
        <<"-export([debug_message_callback/">>,
        <<"glDebugMessageCallback(">>,
        <<"glObjectPtrLabel(">>,
        <<"glGetObjectPtrLabel(">>,
        <<"glGetPointerv(">>
    ].

s193_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glShaderBinaryOES",
            "glSpecializeShaderARB",
            "glDebugMessageCallback",
            "glObjectPtrLabel",
            "glGetObjectPtrLabel",
            "glGetPointerv"
        ]
    ].

s193_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s193_enum_value_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s193_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s193_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s193_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s193_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
