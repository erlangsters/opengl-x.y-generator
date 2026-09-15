-module(resolver_draw_tests).
-include_lib("eunit/include/eunit.hrl").

%% Draw, vertex, dispatch, and indirect command resolver contracts.

%% Historical shard 44.
s044_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s044_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s044_emitter_vertex_attrib_array_state_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(generator_test_support:tmp_root(), "opengl-shard44-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))),
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
            binary:match(GeneratedErl, <<"-spec enable_vertex_attrib_array(Index :: gl:uint()) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"-spec disable_vertex_attrib_array(Index :: gl:uint()) -> ok | {error, atom()}.">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glEnableVertexAttribArray_raw(Index)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedErl, <<"?CALL_RAW_FUNC(glDisableVertexAttribArray_raw(Index)).">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glEnableVertexAttribArray(arg_0);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"glDisableVertexAttribArray(arg_0);">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glEnableVertexAttribArray_raw\", 1, nif_glEnableVertexAttribArray, 0}">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(GeneratedC, <<"{\"glDisableVertexAttribArray_raw\", 1, nif_glDisableVertexAttribArray, 0}">>)
        )
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s044_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"enable_vertex_attrib_array", 1}, Functions)),
    ?assert(maps:is_key({"disable_vertex_attrib_array", 1}, Functions)),

    s044_assert_vertex_attrib_array_state(
        maps:get({"enable_vertex_attrib_array", 1}, Functions),
        "glEnableVertexAttribArray"
    ),
    s044_assert_vertex_attrib_array_state(
        maps:get({"disable_vertex_attrib_array", 1}, Functions),
        "glDisableVertexAttribArray"
    ).

s044_assert_vertex_attrib_array_state(FunctionData, RawCommand) ->
    ?assertEqual(RawCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Index", gl_uint}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Index", {gl, uint, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Index", do_nothing}], maps:get(params, Clause)),
    ?assertEqual(RawCommand, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(RawCommand, NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Index", s044_uint_nif_data()}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s044_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 57.
s057_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s057_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s057_emitter_indexed_binding_state_surface_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard57-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [?assertMatch({_, _}, binary:match(Erl, Export)) || Export <- s057_expected_exports()],
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s057_deferred_exports()],
        ?assertMatch({_, _}, binary:match(Erl, <<"Target :: buffer_target()">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"buffer_target_arb()">>)),

        [?assertMatch({_, _}, binary:match(C, Call)) || Call <- s057_expected_c_calls()],
        ?assertMatch({_, _}, binary:match(C, <<"{\"glBindBufferRange_raw\", 5, nif_glBindBufferRange, 0}">>)),
        ?assertMatch({_, _}, binary:match(C, <<"{\"glBindBufferBase_raw\", 3, nif_glBindBufferBase, 0}">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s057_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s057_assert_presence(Target, Functions),
    s057_assert_deferred_neighbors_absent(Functions),
    s057_assert_present_paths(Target, BindingData, Functions).

s057_assert_presence(Target, Functions) ->
    Present = s057_present_functions(Target),
    Absent = s057_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s057_present_functions({gl, {3, 3}}) ->
    s057_indexed_buffer_binding();
s057_present_functions({gl, {4, 1}}) ->
    s057_indexed_buffer_binding();
s057_present_functions({gl, {4, 6}}) ->
    s057_all_functions();
s057_present_functions({gles, {2, 0}}) ->
    [];
s057_present_functions({gles, {3, 0}}) ->
    s057_indexed_buffer_binding();
s057_present_functions({gles, {3, 1}}) ->
    s057_indexed_buffer_binding() ++ s057_vertex_binding();
s057_present_functions({gles, {3, 2}}) ->
    s057_indexed_buffer_binding() ++ s057_vertex_binding().

s057_all_functions() ->
    s057_indexed_buffer_binding() ++ s057_vertex_binding() ++ s057_vertex_array_dsa_binding().

s057_indexed_buffer_binding() ->
    [
        {"bind_buffer_range", 5},
        {"bind_buffer_base", 3}
    ].

s057_vertex_binding() ->
    [
        {"bind_vertex_buffer", 4},
        {"vertex_attrib_binding", 2},
        {"vertex_binding_divisor", 2}
    ].

s057_vertex_array_dsa_binding() ->
    [
        {"disable_vertex_array_attrib", 2},
        {"enable_vertex_array_attrib", 2},
        {"vertex_array_element_buffer", 2},
        {"vertex_array_vertex_buffer", 5},
        {"vertex_array_attrib_binding", 3},
        {"vertex_array_binding_divisor", 3}
    ].

s057_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s057_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s057_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s057_present_functions(Target)
    ).

s057_assert_path({"bind_buffer_range", 5}, _Target, BindingData, FunctionData) ->
    s057_assert_enum_contains(BindingData, "buffer_target", "transform_feedback_buffer"),
    s057_assert_direct(
        FunctionData,
        "glBindBufferRange",
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "Index", gl_uint},
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Size", gl_sizeiptr}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"Index", {gl, uint, []}},
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Size", {gl, sizeiptr, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["transform_feedback_buffer"]}},
            {"Index", do_nothing},
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"Target", s057_enum_nif_data()},
            {"Index", s057_uint_nif_data()},
            {"Buffer", s057_uint_nif_data()},
            {"Offset", s057_intptr_nif_data()},
            {"Size", s057_sizeiptr_nif_data()}
        ]
    );
s057_assert_path({"bind_buffer_base", 3}, _Target, BindingData, FunctionData) ->
    s057_assert_enum_contains(BindingData, "buffer_target", "transform_feedback_buffer"),
    s057_assert_direct(
        FunctionData,
        "glBindBufferBase",
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "Index", gl_uint},
            {in, "Buffer", {gl_object, buffer}}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"Index", {gl, uint, []}},
            {"Buffer", {undefined, buffer, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["transform_feedback_buffer"]}},
            {"Index", do_nothing},
            {"Buffer", do_nothing}
        ],
        [
            {"Target", s057_enum_nif_data()},
            {"Index", s057_uint_nif_data()},
            {"Buffer", s057_uint_nif_data()}
        ]
    );
s057_assert_path({"bind_vertex_buffer", 4}, _Target, _BindingData, FunctionData) ->
    s057_assert_direct(
        FunctionData,
        "glBindVertexBuffer",
        [
            {in, "BindingIndex", gl_uint},
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Stride", gl_sizei}
        ],
        [
            {"BindingIndex", {gl, uint, []}},
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Stride", {gl, sizei, []}}
        ],
        [
            {"BindingIndex", do_nothing},
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Stride", do_nothing}
        ],
        [
            {"BindingIndex", s057_uint_nif_data()},
            {"Buffer", s057_uint_nif_data()},
            {"Offset", s057_intptr_nif_data()},
            {"Stride", s057_sizei_nif_data()}
        ]
    );
s057_assert_path({"vertex_attrib_binding", 2}, _Target, _BindingData, FunctionData) ->
    s057_assert_uint_direct(
        FunctionData,
        "glVertexAttribBinding",
        [{"AttribIndex", gl_uint}, {"BindingIndex", gl_uint}]
    );
s057_assert_path({"vertex_binding_divisor", 2}, _Target, _BindingData, FunctionData) ->
    s057_assert_uint_direct(
        FunctionData,
        "glVertexBindingDivisor",
        [{"Index", gl_uint}, {"Divisor", gl_uint}]
    );
s057_assert_path({"disable_vertex_array_attrib", 2}, _Target, _BindingData, FunctionData) ->
    s057_assert_vertex_array_uint_direct(FunctionData, "glDisableVertexArrayAttrib", "Index");
s057_assert_path({"enable_vertex_array_attrib", 2}, _Target, _BindingData, FunctionData) ->
    s057_assert_vertex_array_uint_direct(FunctionData, "glEnableVertexArrayAttrib", "Index");
s057_assert_path({"vertex_array_element_buffer", 2}, _Target, _BindingData, FunctionData) ->
    s057_assert_direct(
        FunctionData,
        "glVertexArrayElementBuffer",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "Buffer", {gl_object, buffer}}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"Buffer", {undefined, buffer, []}}
        ],
        [
            {"Array", do_nothing},
            {"Buffer", do_nothing}
        ],
        [
            {"Array", s057_uint_nif_data()},
            {"Buffer", s057_uint_nif_data()}
        ]
    );
s057_assert_path({"vertex_array_vertex_buffer", 5}, _Target, _BindingData, FunctionData) ->
    s057_assert_direct(
        FunctionData,
        "glVertexArrayVertexBuffer",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "BindingIndex", gl_uint},
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Stride", gl_sizei}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"BindingIndex", {gl, uint, []}},
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Stride", {gl, sizei, []}}
        ],
        [
            {"Array", do_nothing},
            {"BindingIndex", do_nothing},
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Stride", do_nothing}
        ],
        [
            {"Array", s057_uint_nif_data()},
            {"BindingIndex", s057_uint_nif_data()},
            {"Buffer", s057_uint_nif_data()},
            {"Offset", s057_intptr_nif_data()},
            {"Stride", s057_sizei_nif_data()}
        ]
    );
s057_assert_path({"vertex_array_attrib_binding", 3}, _Target, _BindingData, FunctionData) ->
    s057_assert_direct(
        FunctionData,
        "glVertexArrayAttribBinding",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "AttribIndex", gl_uint},
            {in, "BindingIndex", gl_uint}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"AttribIndex", {gl, uint, []}},
            {"BindingIndex", {gl, uint, []}}
        ],
        [
            {"Array", do_nothing},
            {"AttribIndex", do_nothing},
            {"BindingIndex", do_nothing}
        ],
        [
            {"Array", s057_uint_nif_data()},
            {"AttribIndex", s057_uint_nif_data()},
            {"BindingIndex", s057_uint_nif_data()}
        ]
    );
s057_assert_path({"vertex_array_binding_divisor", 3}, _Target, _BindingData, FunctionData) ->
    s057_assert_direct(
        FunctionData,
        "glVertexArrayBindingDivisor",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "BindingIndex", gl_uint},
            {in, "Divisor", gl_uint}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"BindingIndex", {gl, uint, []}},
            {"Divisor", {gl, uint, []}}
        ],
        [
            {"Array", do_nothing},
            {"BindingIndex", do_nothing},
            {"Divisor", do_nothing}
        ],
        [
            {"Array", s057_uint_nif_data()},
            {"BindingIndex", s057_uint_nif_data()},
            {"Divisor", s057_uint_nif_data()}
        ]
    ).

s057_assert_uint_direct(FunctionData, GlCommand, Params) ->
    s057_assert_direct(
        FunctionData,
        GlCommand,
        [{in, Name, Type} || {Name, Type} <- Params],
        [{Name, {gl, uint, []}} || {Name, _Type} <- Params],
        [{Name, do_nothing} || {Name, _Type} <- Params],
        [{Name, s057_uint_nif_data()} || {Name, _Type} <- Params]
    ).

s057_assert_vertex_array_uint_direct(FunctionData, GlCommand, ParamName) ->
    s057_assert_direct(
        FunctionData,
        GlCommand,
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, ParamName, gl_uint}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {ParamName, {gl, uint, []}}
        ],
        [
            {"Array", do_nothing},
            {ParamName, do_nothing}
        ],
        [
            {"Array", s057_uint_nif_data()},
            {ParamName, s057_uint_nif_data()}
        ]
    ).

s057_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s057_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s057_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s057_assert_clause_param/1, lists:zip(Expected, Actual)).

s057_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s057_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s057_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s057_expected_exports() ->
    [
        <<"-export([bind_buffer_range/5]).">>,
        <<"-export([bind_buffer_base/3]).">>,
        <<"-export([bind_vertex_buffer/4]).">>,
        <<"-export([vertex_attrib_binding/2]).">>,
        <<"-export([vertex_binding_divisor/2]).">>,
        <<"-export([disable_vertex_array_attrib/2]).">>,
        <<"-export([enable_vertex_array_attrib/2]).">>,
        <<"-export([vertex_array_element_buffer/2]).">>,
        <<"-export([vertex_array_vertex_buffer/5]).">>,
        <<"-export([vertex_array_attrib_binding/3]).">>,
        <<"-export([vertex_array_binding_divisor/3]).">>
    ].

s057_deferred_exports() ->
    [
    ].

s057_expected_c_calls() ->
    [
        <<"glBindBufferRange(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glBindBufferBase(arg_0, arg_1, arg_2);">>,
        <<"glBindVertexBuffer(arg_0, arg_1, arg_2, arg_3);">>,
        <<"glVertexAttribBinding(arg_0, arg_1);">>,
        <<"glVertexBindingDivisor(arg_0, arg_1);">>,
        <<"glDisableVertexArrayAttrib(arg_0, arg_1);">>,
        <<"glEnableVertexArrayAttrib(arg_0, arg_1);">>,
        <<"glVertexArrayElementBuffer(arg_0, arg_1);">>,
        <<"glVertexArrayVertexBuffer(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glVertexArrayAttribBinding(arg_0, arg_1, arg_2);">>,
        <<"glVertexArrayBindingDivisor(arg_0, arg_1, arg_2);">>
    ].

s057_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s057_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s057_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s057_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

s057_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 114.
s114_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s114_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s114_emitter_vertex_attrib_pointer_layouts_test_() ->
    [
        {"gl 4.6", fun() -> s114_assert_emitted_surface({gl, {4, 6}}, true, true) end},
        {"gl 4.1", fun() -> s114_assert_emitted_surface({gl, {4, 1}}, true, true) end},
        {"gl 3.3", fun() -> s114_assert_emitted_surface({gl, {3, 3}}, true, false) end},
        {"gles 3.2", fun() -> s114_assert_emitted_surface({gles, {3, 2}}, true, false) end},
        {"gles 2.0", fun() -> s114_assert_emitted_surface({gles, {2, 0}}, false, false) end}
    ].

s114_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s114_assert_presence(
        s114_supports_i_pointer(Target),
        {"vertex_attrib_i_pointer", 5},
        "glVertexAttribIPointer",
        Functions
    ),
    s114_assert_presence(
        s114_supports_l_pointer(Target),
        {"vertex_attrib_l_pointer", 5},
        "glVertexAttribLPointer",
        Functions
    ),
    ?assertNot(maps:is_key({"vertex_attrib_i_pointer", 6}, Functions)),
    ?assertNot(maps:is_key({"vertex_attrib_l_pointer", 6}, Functions)),

    case s114_supports_i_pointer(Target) of
        true ->
            s114_assert_enum_contains(BindingData, "vertex_attrib_i_type", "int"),
            s114_assert_pointer(
                maps:get({"vertex_attrib_i_pointer", 5}, Functions),
                "glVertexAttribIPointer",
                "VertexAttribIType",
                vertex_attrib_i_type,
                "int"
            );
        false ->
            ok
    end,
    case s114_supports_l_pointer(Target) of
        true ->
            s114_assert_enum_contains(BindingData, "vertex_attrib_l_type", "double"),
            s114_assert_pointer(
                maps:get({"vertex_attrib_l_pointer", 5}, Functions),
                "glVertexAttribLPointer",
                "VertexAttribLType",
                vertex_attrib_l_type,
                "double"
            );
        false ->
            ok
    end.

s114_supports_i_pointer({gles, {2, 0}}) ->
    false;
s114_supports_i_pointer(_Target) ->
    true.

s114_supports_l_pointer({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s114_supports_l_pointer({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s114_supports_l_pointer(_Target) ->
    false.

s114_assert_presence(true, Function, _Command, Functions) ->
    ?assert(maps:is_key(Function, Functions));
s114_assert_presence(false, Function, Command, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)),
    ?assertNot(generator_test_support:has_gl_command(Command, Functions)).

s114_assert_pointer(FunctionData, GlCommand, EnumGroup, TypeName, RequiredAtom) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Index", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, EnumGroup}},
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
            {"Type", {undefined, TypeName, []}},
            {"Stride", {gl, sizei, []}},
            {"Offset", {gl, offset, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(5, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Index", do_nothing},
        {"Size", do_nothing},
        {"Type", {gl_enum_to_uint, TransformMap}},
        {"Stride", do_nothing},
        {"Offset", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember(RequiredAtom, 1, TransformMap)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(5, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Index", s114_uint_nif_data()},
            {"Size", s114_int_nif_data()},
            {"Type", s114_enum_nif_data()},
            {"Stride", s114_sizei_nif_data()},
            {"Offset", in_gl_offset}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s114_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s114_assert_emitted_surface(Target, ExpectIPointer, ExpectLPointer) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard114-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        s114_assert_emitted_pointer(
            ExpectIPointer,
            <<"vertex_attrib_i_pointer">>,
            <<"vertex_attrib_i_type()">>,
            <<"glVertexAttribIPointer">>,
            Erl,
            C
        ),
        s114_assert_emitted_pointer(
            ExpectLPointer,
            <<"vertex_attrib_l_pointer">>,
            <<"vertex_attrib_l_type()">>,
            <<"glVertexAttribLPointer">>,
            Erl,
            C
        ),
        s114_assert_not_contains(Erl, <<"-export([vertex_attrib_i_pointer/6]).">>),
        s114_assert_not_contains(Erl, <<"-export([vertex_attrib_l_pointer/6]).">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s114_assert_emitted_pointer(true, FunctionName, TypeName, GlCommand, Erl, C) ->
    s114_assert_contains(Erl, <<"-export([", FunctionName/binary, "/5]).">>),
    s114_assert_contains(Erl, <<"-spec ", FunctionName/binary, "(\n    Index :: gl:uint(),">>),
    s114_assert_contains(Erl, <<"    Type :: ", TypeName/binary, ",">>),
    s114_assert_contains(Erl, <<"?CALL_RAW_FUNC(", GlCommand/binary, "_raw(Index, Size, NewType, Stride, Offset)).">>),
    s114_assert_contains(C, <<"#include <stdint.h>">>),
    s114_assert_contains(C, <<GlCommand/binary, "(arg_0, arg_1, arg_2, arg_3, (GLvoid*)(uintptr_t)arg_4);">>),
    s114_assert_contains(C, <<"{\"", GlCommand/binary, "_raw\", 5, nif_", GlCommand/binary, ", 0}">>);
s114_assert_emitted_pointer(false, FunctionName, _TypeName, GlCommand, Erl, C) ->
    s114_assert_not_contains(Erl, <<"-export([", FunctionName/binary, "/5]).">>),
    s114_assert_not_contains(Erl, <<GlCommand/binary, "_raw">>),
    s114_assert_not_contains(C, <<GlCommand/binary, "(">>).

s114_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s114_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s114_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s114_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s114_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s114_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 127.
s127_duplicate_current_attribute_spellings_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s127_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s127_emitter_duplicate_spellings_absent_test_() ->
    [
        {"gl 4.6", fun() -> s127_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s127_assert_emitted_surface({gles, {3, 2}}) end}
    ].

s127_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    s127_assert_active_aggregate_wrappers(Target, Functions),
    s127_assert_commands_absent(Functions, s127_duplicate_current_attribute_vector_commands()),
    s127_assert_direct_wrappers_absent(Functions).

s127_assert_active_aggregate_wrappers(Target, Functions) ->
    ?assert(maps:is_key({"vertex_attrib", 3}, Functions)),
    case s127_supports_integer_vertex_attrib(Target) of
        true -> ?assert(maps:is_key({"vertex_attrib_i", 3}, Functions));
        false -> ?assertNot(maps:is_key({"vertex_attrib_i", 3}, Functions))
    end,
    case s127_supports_vertex_attrib_l(Target) of
        true -> ?assert(maps:is_key({"vertex_attrib_l", 3}, Functions));
        false -> ?assertNot(maps:is_key({"vertex_attrib_l", 3}, Functions))
    end,
    case s127_is_desktop(Target) of
        true -> ?assert(maps:is_key({"vertex_attrib_n", 3}, Functions));
        false -> ?assertNot(maps:is_key({"vertex_attrib_n", 3}, Functions))
    end.

s127_assert_direct_wrappers_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- s127_duplicate_public_wrappers()
    ].

s127_assert_commands_absent(Functions, Commands) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- Commands
    ].

s127_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard127-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        s127_assert_not_contains_any(Erl, s127_duplicate_raw_needles()),
        s127_assert_not_contains_any(C, s127_duplicate_call_needles())
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s127_supports_integer_vertex_attrib({gles, {2, 0}}) ->
    false;
s127_supports_integer_vertex_attrib(_) ->
    true.

s127_supports_vertex_attrib_l({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s127_supports_vertex_attrib_l({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s127_supports_vertex_attrib_l(_) ->
    false.

s127_is_desktop({gl, _Version}) ->
    true;
s127_is_desktop(_) ->
    false.

s127_duplicate_current_attribute_vector_commands() ->
    [
        "glVertexAttrib1dv",
        "glVertexAttrib1fv",
        "glVertexAttrib1sv",
        "glVertexAttrib2dv",
        "glVertexAttrib2fv",
        "glVertexAttrib2sv",
        "glVertexAttrib3dv",
        "glVertexAttrib3fv",
        "glVertexAttrib3sv",
        "glVertexAttrib4dv",
        "glVertexAttrib4fv",
        "glVertexAttrib4sv",
        "glVertexAttribI1iv",
        "glVertexAttribI2iv",
        "glVertexAttribI3iv",
        "glVertexAttribI4iv",
        "glVertexAttribI1uiv",
        "glVertexAttribI2uiv",
        "glVertexAttribI3uiv",
        "glVertexAttribI4uiv",
        "glVertexAttribL1dv",
        "glVertexAttribL2dv",
        "glVertexAttribL3dv",
        "glVertexAttribL4dv",
        "glVertexAttrib4Nubv"
    ].

s127_duplicate_public_wrappers() ->
    [
        {"vertex_attrib_1dv", 2},
        {"vertex_attrib_1fv", 2},
        {"vertex_attrib_1sv", 2},
        {"vertex_attrib_2dv", 2},
        {"vertex_attrib_2fv", 2},
        {"vertex_attrib_2sv", 2},
        {"vertex_attrib_3dv", 2},
        {"vertex_attrib_3fv", 2},
        {"vertex_attrib_3sv", 2},
        {"vertex_attrib_4dv", 2},
        {"vertex_attrib_4fv", 2},
        {"vertex_attrib_4sv", 2},
        {"vertex_attrib_i_1iv", 2},
        {"vertex_attrib_i_2iv", 2},
        {"vertex_attrib_i_3iv", 2},
        {"vertex_attrib_i_4iv", 2},
        {"vertex_attrib_i_1uiv", 2},
        {"vertex_attrib_i_2uiv", 2},
        {"vertex_attrib_i_3uiv", 2},
        {"vertex_attrib_i_4uiv", 2},
        {"vertex_attrib_l_1dv", 2},
        {"vertex_attrib_l_2dv", 2},
        {"vertex_attrib_l_3dv", 2},
        {"vertex_attrib_l_4dv", 2},
        {"vertex_attrib_4_nubv", 2}
    ].

s127_duplicate_raw_needles() ->
    [list_to_binary(Command ++ "_raw") || Command <- s127_duplicate_current_attribute_vector_commands()].

s127_duplicate_call_needles() ->
    [list_to_binary(Command ++ "(") || Command <- s127_duplicate_current_attribute_vector_commands()].

s127_assert_not_contains_any(Haystack, Needles) ->
    [
        ?assertEqual(nomatch, binary:match(Haystack, Needle))
     || Needle <- Needles
    ].

%% Historical shard 128.
s128_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s128_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s128_emitter_packed_attribute_test_() ->
    [
        {"gl 4.6", fun() -> s128_assert_emitted_surface({gl, {4, 6}}, true) end},
        {"gles 3.2", fun() -> s128_assert_emitted_surface({gles, {3, 2}}, false) end}
    ].

s128_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s128_is_desktop(Target) of
        true ->
            ?assert(maps:is_key({"vertex_attrib_p", 5}, Functions)),
            s128_assert_packed_enum_type(BindingData),
            s128_assert_vertex_attrib_p(maps:get({"vertex_attrib_p", 5}, Functions));
        false ->
            ?assertNot(maps:is_key({"vertex_attrib_p", 5}, Functions)),
            s128_assert_commands_absent(Functions, s128_scalar_commands())
    end,
    ?assertNot(maps:is_key({"vertex_attrib_p", 4}, Functions)),
    s128_assert_direct_wrappers_absent(Functions),
    s128_assert_commands_absent(Functions, s128_pointer_duplicate_commands()).

s128_assert_packed_enum_type(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("vertex_attrib_packed_type", EnumTypes)),
    PackedTypes = maps:get("vertex_attrib_packed_type", EnumTypes),
    ?assert(lists:member("int_2_10_10_10_rev", PackedTypes)),
    ?assert(lists:member("unsigned_int_2_10_10_10_rev", PackedTypes)),
    ?assertNot(lists:member("float", PackedTypes)),
    ?assertNot(lists:member("double", PackedTypes)).

s128_assert_vertex_attrib_p(FunctionData) ->
    ?assertEqual(
        [
            {"Components", {set, [1, 2, 3, 4]}},
            {"Index", {gl, uint, []}},
            {"Type", {undefined, vertex_attrib_packed_type, []}},
            {"Normalized", {gl, boolean, []}},
            {"Value", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(5, maps:get(function_arity, FunctionData)),
    [?assert(generator_test_support:has_gl_command(Command, #{dummy => FunctionData})) || Command <- s128_scalar_commands()],
    s128_assert_clauses(FunctionData),
    s128_assert_nifs(FunctionData).

s128_assert_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    [
        begin
            Clause = s128_find_clause(Command, Clauses),
            ?assertEqual("Components", maps:get(guard_var, Clause)),
            ?assertEqual([{equals, var, Components}], maps:get(guards, Clause)),
            s128_assert_clause_params(maps:get(params, Clause))
        end
     || {Components, Command} <- s128_component_commands()
    ].

s128_assert_clause_params(Params) ->
    ?assertMatch(
        [
            {"Components", ignore},
            {"Index", do_nothing},
            {"Type", {gl_enum_to_uint, _}},
            {"Normalized", do_nothing},
            {"Value", do_nothing}
        ],
        Params
    ),
    {"Type", {gl_enum_to_uint, TransformMap}} = lists:keyfind("Type", 1, Params),
    ?assert(lists:member({"int_2_10_10_10_rev", "GL_INT_2_10_10_10_REV"}, TransformMap)),
    ?assert(lists:member({"unsigned_int_2_10_10_10_rev", "GL_UNSIGNED_INT_2_10_10_10_REV"}, TransformMap)).

s128_assert_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    [
        begin
            NifData = maps:get(Command, NifFunctions),
            ?assertEqual(4, maps:get(arity, NifData)),
            ?assertEqual(
                [
                    {"Index", {gl_type, s128_gl_uint_spec()}},
                    {"Type", {gl_type, s128_gl_enum_spec()}},
                    {"Normalized", boolean_to_glbool},
                    {"Value", {gl_type, s128_gl_uint_spec()}}
                ],
                maps:get(params, NifData)
            ),
            ?assertEqual(void, maps:get(return, NifData))
        end
     || Command <- s128_scalar_commands()
    ].

s128_assert_emitted_surface(Target, ExpectedPresent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard128-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        case ExpectedPresent of
            true ->
                s128_assert_contains(Erl, <<"-export([vertex_attrib_p/5]).">>),
                s128_assert_contains(Erl, <<"-type vertex_attrib_packed_type() ::">>),
                s128_assert_contains(Erl, <<"int_2_10_10_10_rev">>),
                s128_assert_contains(Erl, <<"unsigned_int_2_10_10_10_rev">>),
                s128_assert_contains(Erl, <<"vertex_attrib_p(Components, Index, Type, Normalized, Value) when Components =:= 1 ->">>),
                s128_assert_contains(Erl, <<"glVertexAttribP1ui_raw(Index, NewType, Normalized, Value)">>),
                s128_assert_contains(Erl, <<"glVertexAttribP4ui_raw(Index, NewType, Normalized, Value)">>),
                s128_assert_contains(C, <<"glVertexAttribP1ui(arg_0, arg_1, arg_2, arg_3);">>),
                s128_assert_contains(C, <<"glVertexAttribP4ui(arg_0, arg_1, arg_2, arg_3);">>),
                s128_assert_contains(C, <<"{\"glVertexAttribP1ui_raw\", 4, nif_glVertexAttribP1ui, 0}">>);
            false ->
                s128_assert_not_contains(Erl, <<"vertex_attrib_p(">>),
                s128_assert_not_contains(C, <<"glVertexAttribP1ui(">>)
        end,
        s128_assert_not_contains(Erl, <<"glVertexAttribP1uiv_raw">>),
        s128_assert_not_contains(C, <<"glVertexAttribP1uiv(">>),
        s128_assert_not_contains(Erl, <<"-export([vertex_attrib_p1ui/4]).">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s128_assert_direct_wrappers_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- [
        {"vertex_attrib_p1ui", 4},
        {"vertex_attrib_p2ui", 4},
        {"vertex_attrib_p3ui", 4},
        {"vertex_attrib_p4ui", 4},
        {"vertex_attrib_p1uiv", 4},
        {"vertex_attrib_p2uiv", 4},
        {"vertex_attrib_p3uiv", 4},
        {"vertex_attrib_p4uiv", 4}
    ]].

s128_assert_commands_absent(Functions, Commands) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- Commands
    ].

s128_component_commands() ->
    [
        {1, "glVertexAttribP1ui"},
        {2, "glVertexAttribP2ui"},
        {3, "glVertexAttribP3ui"},
        {4, "glVertexAttribP4ui"}
    ].

s128_scalar_commands() ->
    [Command || {_Components, Command} <- s128_component_commands()].

s128_pointer_duplicate_commands() ->
    [
        "glVertexAttribP1uiv",
        "glVertexAttribP2uiv",
        "glVertexAttribP3uiv",
        "glVertexAttribP4uiv"
    ].

s128_is_desktop({gl, _Version}) ->
    true;
s128_is_desktop(_) ->
    false.

s128_gl_uint_spec() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s128_gl_enum_spec() ->
    {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s128_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s128_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s128_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 184.

s184_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s184_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s184_emitter_element_draws_test_() ->
    [
        {"gl 4.6", fun() -> s184_assert_emitted_present({gl, {4, 6}}, s184_gl46_functions()) end},
        {"gl 4.1", fun() -> s184_assert_emitted_present({gl, {4, 1}}, s184_gl41_functions()) end},
        {"gl 3.3", fun() -> s184_assert_emitted_present({gl, {3, 3}}, s184_gl41_functions()) end},
        {"gles 3.2", fun() -> s184_assert_emitted_present({gles, {3, 2}}, s184_gles32_functions()) end},
        {"gles 3.0", fun() -> s184_assert_emitted_present({gles, {3, 0}}, s184_gles30_functions()) end},
        {"gles 2.0", fun() -> s184_assert_emitted_present({gles, {2, 0}}, s184_es2_functions()) end}
    ].

s184_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s184_present_functions(Target),
    Absent = s184_all_functions() -- Present,

    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    s184_assert_enum_contains(BindingData, "primitive_type", "points"),
    s184_assert_enum_contains(BindingData, "draw_elements_type", "unsigned_byte"),
    [s184_assert_path(Function, maps:get(Function, Functions)) || Function <- Present],
    s184_assert_deferred_neighbors_absent(Functions).

s184_present_functions({gl, {4, 6}}) -> s184_gl46_functions();
s184_present_functions({gl, {_Major, _Minor}}) -> s184_gl41_functions();
s184_present_functions({gles, {2, 0}}) -> s184_es2_functions();
s184_present_functions({gles, {3, 2}}) -> s184_gles32_functions();
s184_present_functions({gles, {3, _Minor}}) -> s184_gles30_functions().

s184_all_functions() ->
    s184_gl46_functions().

s184_gl46_functions() ->
    s184_gl41_functions() ++ s184_base_instance_functions().

s184_gl41_functions() ->
    s184_gles32_functions().

s184_gles32_functions() ->
    s184_gles30_functions() ++ s184_base_vertex_functions().

s184_gles30_functions() ->
    s184_es2_functions() ++
        [
            {"draw_range_elements", 6},
            {"draw_elements_instanced", 5}
        ].

s184_es2_functions() ->
    [{"draw_elements", 4}].

s184_base_vertex_functions() ->
    [
        {"draw_elements_base_vertex", 5},
        {"draw_range_elements_base_vertex", 7},
        {"draw_elements_instanced_base_vertex", 6}
    ].

s184_base_instance_functions() ->
    [
        {"draw_elements_instanced_base_instance", 6},
        {"draw_elements_instanced_base_vertex_base_instance", 7}
    ].

s184_assert_path({"draw_elements", 4}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawElements",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Count", gl_sizei},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Offset", gl_offset}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Count", {gl, sizei, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Offset", {gl, offset, []}}
        ],
        [
            {"Mode", s184_enum_nif_data()},
            {"Count", s184_sizei_nif_data()},
            {"Type", s184_enum_nif_data()},
            {"Offset", in_gl_offset}
        ]
    );
s184_assert_path({"draw_range_elements", 6}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawRangeElements",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Start", gl_uint},
            {in, "End", gl_uint},
            {in, "Count", gl_sizei},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Offset", gl_offset}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Start", {gl, uint, []}},
            {"End", {gl, uint, []}},
            {"Count", {gl, sizei, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Offset", {gl, offset, []}}
        ],
        [
            {"Mode", s184_enum_nif_data()},
            {"Start", s184_uint_nif_data()},
            {"End", s184_uint_nif_data()},
            {"Count", s184_sizei_nif_data()},
            {"Type", s184_enum_nif_data()},
            {"Offset", in_gl_offset}
        ]
    );
s184_assert_path({"draw_elements_instanced", 5}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawElementsInstanced",
        s184_common_indexed_params() ++ [{in, "InstanceCount", gl_sizei}],
        s184_common_indexed_specs() ++ [{"InstanceCount", {gl, sizei, []}}],
        s184_common_indexed_nif_params() ++ [{"InstanceCount", s184_sizei_nif_data()}]
    );
s184_assert_path({"draw_elements_base_vertex", 5}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawElementsBaseVertex",
        s184_common_indexed_params() ++ [{in, "BaseVertex", gl_int}],
        s184_common_indexed_specs() ++ [{"BaseVertex", {gl, int, []}}],
        s184_common_indexed_nif_params() ++ [{"BaseVertex", s184_int_nif_data()}]
    );
s184_assert_path({"draw_range_elements_base_vertex", 7}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawRangeElementsBaseVertex",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Start", gl_uint},
            {in, "End", gl_uint},
            {in, "Count", gl_sizei},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Offset", gl_offset},
            {in, "BaseVertex", gl_int}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Start", {gl, uint, []}},
            {"End", {gl, uint, []}},
            {"Count", {gl, sizei, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Offset", {gl, offset, []}},
            {"BaseVertex", {gl, int, []}}
        ],
        [
            {"Mode", s184_enum_nif_data()},
            {"Start", s184_uint_nif_data()},
            {"End", s184_uint_nif_data()},
            {"Count", s184_sizei_nif_data()},
            {"Type", s184_enum_nif_data()},
            {"Offset", in_gl_offset},
            {"BaseVertex", s184_int_nif_data()}
        ]
    );
s184_assert_path({"draw_elements_instanced_base_vertex", 6}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawElementsInstancedBaseVertex",
        s184_common_indexed_params() ++ [{in, "InstanceCount", gl_sizei}, {in, "BaseVertex", gl_int}],
        s184_common_indexed_specs() ++ [{"InstanceCount", {gl, sizei, []}}, {"BaseVertex", {gl, int, []}}],
        s184_common_indexed_nif_params() ++ [{"InstanceCount", s184_sizei_nif_data()}, {"BaseVertex", s184_int_nif_data()}]
    );
s184_assert_path({"draw_elements_instanced_base_instance", 6}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawElementsInstancedBaseInstance",
        s184_common_indexed_params() ++ [{in, "InstanceCount", gl_sizei}, {in, "BaseInstance", gl_uint}],
        s184_common_indexed_specs() ++ [{"InstanceCount", {gl, sizei, []}}, {"BaseInstance", {gl, uint, []}}],
        s184_common_indexed_nif_params() ++ [{"InstanceCount", s184_sizei_nif_data()}, {"BaseInstance", s184_uint_nif_data()}]
    );
s184_assert_path({"draw_elements_instanced_base_vertex_base_instance", 7}, FunctionData) ->
    s184_assert_direct(
        FunctionData,
        "glDrawElementsInstancedBaseVertexBaseInstance",
        s184_common_indexed_params() ++ [
            {in, "InstanceCount", gl_sizei},
            {in, "BaseVertex", gl_int},
            {in, "BaseInstance", gl_uint}
        ],
        s184_common_indexed_specs() ++ [
            {"InstanceCount", {gl, sizei, []}},
            {"BaseVertex", {gl, int, []}},
            {"BaseInstance", {gl, uint, []}}
        ],
        s184_common_indexed_nif_params() ++ [
            {"InstanceCount", s184_sizei_nif_data()},
            {"BaseVertex", s184_int_nif_data()},
            {"BaseInstance", s184_uint_nif_data()}
        ]
    ).

s184_common_indexed_params() ->
    [
        {in, "Mode", {gl_enum, "PrimitiveType"}},
        {in, "Count", gl_sizei},
        {in, "Type", {gl_enum, "DrawElementsType"}},
        {in, "Offset", gl_offset}
    ].

s184_common_indexed_specs() ->
    [
        {"Mode", {undefined, primitive_type, []}},
        {"Count", {gl, sizei, []}},
        {"Type", {undefined, draw_elements_type, []}},
        {"Offset", {gl, offset, []}}
    ].

s184_common_indexed_nif_params() ->
    [
        {"Mode", s184_enum_nif_data()},
        {"Count", s184_sizei_nif_data()},
        {"Type", s184_enum_nif_data()},
        {"Offset", in_gl_offset}
    ].

s184_assert_direct(FunctionData, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(ParamsSpecs), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s184_assert_clause_params(ParamsSpecs, maps:get(params, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s184_assert_clause_params(ParamsSpecs, ClauseParams) ->
    Expected =
        lists:map(
            fun
                ({in, Name, {gl_enum, _Group}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, _Type}) ->
                    {Name, do_nothing}
            end,
            ParamsSpecs
        ),
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(lists:keymember("points", 1, TransformMap) orelse lists:keymember("unsigned_byte", 1, TransformMap)),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s184_assert_emitted_present(Target, ExpectedFunctions) ->
    {Erl, C} = s184_generate_surface(Target),
    [s184_assert_emitted_function(Function, Erl, C) || Function <- ExpectedFunctions],
    [s184_assert_emitted_absent_function(Function, Erl, C) || Function <- s184_all_functions() -- ExpectedFunctions],
    [s184_assert_not_contains(Erl, Needle) || Needle <- s184_forbidden_erl_needles()],
    [s184_assert_not_contains(C, Needle) || Needle <- s184_forbidden_c_needles()].

s184_assert_emitted_function({"draw_elements", 4}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_elements">>, 4, <<"glDrawElements">>),
    s184_assert_contains(Erl, <<"Offset :: gl:offset()">>),
    s184_assert_contains(C, <<"glDrawElements(arg_0, arg_1, arg_2, (GLvoid*)(uintptr_t)arg_3);">>);
s184_assert_emitted_function({"draw_range_elements", 6}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_range_elements">>, 6, <<"glDrawRangeElements">>),
    s184_assert_contains(C, <<"glDrawRangeElements(arg_0, arg_1, arg_2, arg_3, arg_4, (GLvoid*)(uintptr_t)arg_5);">>);
s184_assert_emitted_function({"draw_elements_instanced", 5}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_elements_instanced">>, 5, <<"glDrawElementsInstanced">>),
    s184_assert_contains(C, <<"glDrawElementsInstanced(arg_0, arg_1, arg_2, (GLvoid*)(uintptr_t)arg_3, arg_4);">>);
s184_assert_emitted_function({"draw_elements_base_vertex", 5}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_elements_base_vertex">>, 5, <<"glDrawElementsBaseVertex">>),
    s184_assert_contains(C, <<"glDrawElementsBaseVertex(arg_0, arg_1, arg_2, (GLvoid*)(uintptr_t)arg_3, arg_4);">>);
s184_assert_emitted_function({"draw_range_elements_base_vertex", 7}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_range_elements_base_vertex">>, 7, <<"glDrawRangeElementsBaseVertex">>),
    s184_assert_contains(C, <<"glDrawRangeElementsBaseVertex(arg_0, arg_1, arg_2, arg_3, arg_4, (GLvoid*)(uintptr_t)arg_5, arg_6);">>);
s184_assert_emitted_function({"draw_elements_instanced_base_vertex", 6}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_elements_instanced_base_vertex">>, 6, <<"glDrawElementsInstancedBaseVertex">>),
    s184_assert_contains(C, <<"glDrawElementsInstancedBaseVertex(arg_0, arg_1, arg_2, (GLvoid*)(uintptr_t)arg_3, arg_4, arg_5);">>);
s184_assert_emitted_function({"draw_elements_instanced_base_instance", 6}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_elements_instanced_base_instance">>, 6, <<"glDrawElementsInstancedBaseInstance">>),
    s184_assert_contains(C, <<"glDrawElementsInstancedBaseInstance(arg_0, arg_1, arg_2, (GLvoid*)(uintptr_t)arg_3, arg_4, arg_5);">>);
s184_assert_emitted_function({"draw_elements_instanced_base_vertex_base_instance", 7}, Erl, C) ->
    s184_assert_export_and_call(Erl, <<"draw_elements_instanced_base_vertex_base_instance">>, 7, <<"glDrawElementsInstancedBaseVertexBaseInstance">>),
    s184_assert_contains(C, <<"glDrawElementsInstancedBaseVertexBaseInstance(arg_0, arg_1, arg_2, (GLvoid*)(uintptr_t)arg_3, arg_4, arg_5, arg_6);">>).

s184_assert_export_and_call(Erl, FunctionName, Arity, Command) ->
    s184_assert_contains(Erl, <<"-export([", FunctionName/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s184_assert_contains(Erl, <<"?CALL_RAW_FUNC(", Command/binary, "_raw(">>).

s184_assert_emitted_absent_function({FunctionName, Arity}, Erl, C) ->
    FunctionBin = list_to_binary(FunctionName),
    s184_assert_not_contains(Erl, <<"-export([", FunctionBin/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s184_assert_not_contains(C, s184_raw_command_needle(FunctionName)).

s184_raw_command_needle("draw_elements") -> <<"glDrawElements(">>;
s184_raw_command_needle("draw_range_elements") -> <<"glDrawRangeElements(">>;
s184_raw_command_needle("draw_elements_instanced") -> <<"glDrawElementsInstanced(">>;
s184_raw_command_needle("draw_elements_base_vertex") -> <<"glDrawElementsBaseVertex(">>;
s184_raw_command_needle("draw_range_elements_base_vertex") -> <<"glDrawRangeElementsBaseVertex(">>;
s184_raw_command_needle("draw_elements_instanced_base_vertex") -> <<"glDrawElementsInstancedBaseVertex(">>;
s184_raw_command_needle("draw_elements_instanced_base_instance") -> <<"glDrawElementsInstancedBaseInstance(">>;
s184_raw_command_needle("draw_elements_instanced_base_vertex_base_instance") ->
    <<"glDrawElementsInstancedBaseVertexBaseInstance(">>.

s184_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard184-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s184_ensure_absent(Dir),
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

s184_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s184_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glCopyImageSubDataEXT",
            "glCopyImageSubDataNV",
            "glCopyImageSubDataOES"
        ]
    ].

s184_forbidden_erl_needles() ->
    [
        <<"-export([get_parameter/">>
    ].

s184_forbidden_c_needles() ->
    [
        <<"glCopyImageSubDataEXT(">>,
        <<"glCopyImageSubDataNV(">>,
        <<"glCopyImageSubDataOES(">>
    ].

s184_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s184_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s184_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s184_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s184_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s184_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s184_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s184_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 185.

s185_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s185_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s185_emitter_indirect_draws_test_() ->
    [
        {"gl 4.6", fun() -> s185_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s185_assert_emitted_present({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s185_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s185_assert_emitted_present({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s185_assert_emitted_present({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s185_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s185_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s185_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s185_supports_indirect_draws(Target) of
        true ->
            s185_assert_enum_contains(BindingData, "primitive_type", "points"),
            s185_assert_enum_contains(BindingData, "draw_elements_type", "unsigned_byte"),
            s185_assert_path({"draw_arrays_indirect", 2}, maps:get({"draw_arrays_indirect", 2}, Functions)),
            s185_assert_path({"draw_elements_indirect", 3}, maps:get({"draw_elements_indirect", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"draw_arrays_indirect", 2}, Functions)),
            ?assertNot(maps:is_key({"draw_elements_indirect", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glDrawArraysIndirect", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glDrawElementsIndirect", Functions))
    end,
    s185_assert_deferred_neighbors_absent(Functions).

s185_supports_indirect_draws({gl, {4, _Minor}}) ->
    true;
s185_supports_indirect_draws({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s185_supports_indirect_draws(_) ->
    false.

s185_assert_path({"draw_arrays_indirect", 2}, FunctionData) ->
    s185_assert_direct(
        FunctionData,
        "glDrawArraysIndirect",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Offset", gl_offset}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Offset", {gl, offset, []}}
        ],
        [
            {"Mode", s185_enum_nif_data()},
            {"Offset", in_gl_offset}
        ]
    );
s185_assert_path({"draw_elements_indirect", 3}, FunctionData) ->
    s185_assert_direct(
        FunctionData,
        "glDrawElementsIndirect",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Offset", gl_offset}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Offset", {gl, offset, []}}
        ],
        [
            {"Mode", s185_enum_nif_data()},
            {"Type", s185_enum_nif_data()},
            {"Offset", in_gl_offset}
        ]
    ).

s185_assert_direct(FunctionData, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(ParamsSpecs), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s185_assert_clause_params(ParamsSpecs, maps:get(params, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s185_assert_clause_params(ParamsSpecs, ClauseParams) ->
    Expected =
        lists:map(
            fun
                ({in, Name, {gl_enum, _Group}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, _Type}) ->
                    {Name, do_nothing}
            end,
            ParamsSpecs
        ),
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(
                        lists:keymember("points", 1, TransformMap) orelse
                            lists:keymember("unsigned_byte", 1, TransformMap)
                    ),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s185_assert_emitted_present(Target) ->
    {Erl, C} = s185_generate_surface(Target),
    s185_assert_export_and_call(Erl, <<"draw_arrays_indirect">>, 2, <<"glDrawArraysIndirect">>),
    s185_assert_export_and_call(Erl, <<"draw_elements_indirect">>, 3, <<"glDrawElementsIndirect">>),
    s185_assert_contains(Erl, <<"Offset :: gl:offset()">>),
    s185_assert_contains(C, <<"glDrawArraysIndirect(arg_0, (GLvoid*)(uintptr_t)arg_1);">>),
    s185_assert_contains(C, <<"glDrawElementsIndirect(arg_0, arg_1, (GLvoid*)(uintptr_t)arg_2);">>),
    s185_assert_deferred_emitted_absent(Erl, C).

s185_assert_emitted_absent(Target) ->
    {Erl, C} = s185_generate_surface(Target),
    s185_assert_not_contains(Erl, <<"-export([draw_arrays_indirect/2]).">>),
    s185_assert_not_contains(Erl, <<"-export([draw_elements_indirect/3]).">>),
    s185_assert_not_contains(C, <<"glDrawArraysIndirect(">>),
    s185_assert_not_contains(C, <<"glDrawElementsIndirect(">>),
    s185_assert_deferred_emitted_absent(Erl, C).

s185_assert_export_and_call(Erl, FunctionName, Arity, Command) ->
    s185_assert_contains(Erl, <<"-export([", FunctionName/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s185_assert_contains(Erl, <<"?CALL_RAW_FUNC(", Command/binary, "_raw(">>).

s185_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard185-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s185_ensure_absent(Dir),
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

s185_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s185_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glCopyImageSubDataEXT",
            "glCopyImageSubDataNV",
            "glCopyImageSubDataOES"
        ]
    ].

s185_assert_deferred_emitted_absent(Erl, C) ->
    [
        s185_assert_not_contains(Erl, Needle)
     || Needle <- [
        ]
    ],
    [
        s185_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glCopyImageSubDataEXT(">>,
            <<"glCopyImageSubDataNV(">>,
            <<"glCopyImageSubDataOES(">>
        ]
    ].

s185_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s185_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s185_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s185_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s185_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 186.

s186_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s186_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s186_emitter_multi_draws_test_() ->
    [
        {"gl 4.6", fun() -> s186_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s186_assert_emitted_present({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s186_assert_emitted_present({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s186_assert_emitted_absent({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s186_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s186_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s186_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s186_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s186_supports_multi_draw(Target) of
        true ->
            s186_assert_enum_contains(BindingData, "primitive_type", "points"),
            s186_assert_enum_contains(BindingData, "draw_elements_type", "unsigned_byte"),
            s186_assert_path({"multi_draw_arrays", 2}, maps:get({"multi_draw_arrays", 2}, Functions)),
            s186_assert_path({"multi_draw_elements", 3}, maps:get({"multi_draw_elements", 3}, Functions)),
            s186_assert_path(
                {"multi_draw_elements_base_vertex", 3},
                maps:get({"multi_draw_elements_base_vertex", 3}, Functions)
            );
        false ->
            ?assertNot(maps:is_key({"multi_draw_arrays", 2}, Functions)),
            ?assertNot(maps:is_key({"multi_draw_elements", 3}, Functions)),
            ?assertNot(maps:is_key({"multi_draw_elements_base_vertex", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawArrays", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawElements", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawElementsBaseVertex", Functions))
    end,
    s186_assert_deferred_neighbors_absent(Functions).

s186_supports_multi_draw({gl, _Version}) ->
    true;
s186_supports_multi_draw(_) ->
    false.

s186_assert_path({"multi_draw_arrays", 2}, FunctionData) ->
    s186_assert_direct(
        FunctionData,
        "glMultiDrawArrays",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Draws", multi_draw_arrays}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Draws", {list, {tuple, [{gl, int, []}, {gl, sizei, []}]}}}
        ],
        [
            {"Mode", s186_enum_nif_data()},
            {"Draws", in_multi_draw_arrays}
        ]
    );
s186_assert_path({"multi_draw_elements", 3}, FunctionData) ->
    s186_assert_direct(
        FunctionData,
        "glMultiDrawElements",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Draws", multi_draw_elements}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Draws", {list, {tuple, [{gl, sizei, []}, {gl, offset, []}]}}}
        ],
        [
            {"Mode", s186_enum_nif_data()},
            {"Type", s186_enum_nif_data()},
            {"Draws", in_multi_draw_elements}
        ]
    );
s186_assert_path({"multi_draw_elements_base_vertex", 3}, FunctionData) ->
    s186_assert_direct(
        FunctionData,
        "glMultiDrawElementsBaseVertex",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Draws", multi_draw_elements_base_vertex}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Draws", {list, {tuple, [{gl, sizei, []}, {gl, offset, []}, {gl, int, []}]}}}
        ],
        [
            {"Mode", s186_enum_nif_data()},
            {"Type", s186_enum_nif_data()},
            {"Draws", in_multi_draw_elements_base_vertex}
        ]
    ).

s186_assert_direct(FunctionData, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s186_assert_clause_params(ParamsSpecs, maps:get(params, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s186_assert_clause_params(ParamsSpecs, ClauseParams) ->
    Expected =
        lists:map(
            fun
                ({in, Name, {gl_enum, _Group}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, Type}) when
                    Type =:= multi_draw_arrays;
                    Type =:= multi_draw_elements;
                    Type =:= multi_draw_elements_base_vertex
                ->
                    {Name, Type}
            end,
            ParamsSpecs
        ),
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(
                        lists:keymember("points", 1, TransformMap) orelse
                            lists:keymember("unsigned_byte", 1, TransformMap)
                    ),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s186_assert_emitted_present(Target) ->
    {Erl, C} = s186_generate_surface(Target),
    s186_assert_export_and_call(Erl, <<"multi_draw_arrays">>, 2, <<"glMultiDrawArrays">>),
    s186_assert_export_and_call(Erl, <<"multi_draw_elements">>, 3, <<"glMultiDrawElements">>),
    s186_assert_export_and_call(
        Erl,
        <<"multi_draw_elements_base_vertex">>,
        3,
        <<"glMultiDrawElementsBaseVertex">>
    ),
    s186_assert_contains(Erl, <<"Draws :: [{gl:int(), gl:sizei()}]">>),
    s186_assert_contains(Erl, <<"Draws :: [{gl:sizei(), gl:offset()}]">>),
    s186_assert_contains(Erl, <<"Draws :: [{gl:sizei(), gl:offset(), gl:int()}]">>),
    s186_assert_contains(C, <<"glMultiDrawArrays(arg_0, arg_1_first, arg_1_count, arg_1_drawcount);">>),
    s186_assert_contains(
        C,
        <<"glMultiDrawElements(arg_0, arg_2_count, arg_1, (const GLvoid* const*)arg_2_indices, arg_2_drawcount);">>
    ),
    s186_assert_contains(
        C,
        <<"glMultiDrawElementsBaseVertex(arg_0, arg_2_count, arg_1, (const GLvoid* const*)arg_2_indices, arg_2_drawcount, arg_2_basevertex);">>
    ),
    s186_assert_contains(C, <<"enif_get_tuple(env, arg_2_head, &arg_2_arity, &arg_2_tuple)">>),
    s186_assert_contains(C, <<"arg_2_offset_tmp > UINTPTR_MAX">>),
    s186_assert_deferred_emitted_absent(Erl, C).

s186_assert_emitted_absent(Target) ->
    {Erl, C} = s186_generate_surface(Target),
    s186_assert_not_contains(Erl, <<"-export([multi_draw_arrays/2]).">>),
    s186_assert_not_contains(Erl, <<"-export([multi_draw_elements/3]).">>),
    s186_assert_not_contains(Erl, <<"-export([multi_draw_elements_base_vertex/3]).">>),
    s186_assert_not_contains(C, <<"glMultiDrawArrays(">>),
    s186_assert_not_contains(C, <<"glMultiDrawElements(">>),
    s186_assert_not_contains(C, <<"glMultiDrawElementsBaseVertex(">>),
    s186_assert_deferred_emitted_absent(Erl, C).

s186_assert_export_and_call(Erl, FunctionName, Arity, Command) ->
    s186_assert_contains(Erl, <<"-export([", FunctionName/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s186_assert_contains(Erl, <<"?CALL_RAW_FUNC(", Command/binary, "_raw(">>).

s186_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard186-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s186_ensure_absent(Dir),
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

s186_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s186_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s186_deferred_neighbor_commands()
    ].

s186_assert_deferred_emitted_absent(Erl, C) ->
    [
        s186_assert_not_contains(Erl, Needle)
     || Needle <- [
            <<"-export([draw_arrays_indirect_count/">>,
            <<"-export([draw_elements_indirect_count/">>
        ]
    ],
    [
        s186_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glCopyImageSubDataEXT(">>,
            <<"glCopyImageSubDataNV(">>,
            <<"glCopyImageSubDataOES(">>
        ]
    ].

s186_deferred_neighbor_commands() ->
    [
        "glCopyImageSubDataEXT",
        "glCopyImageSubDataNV",
        "glCopyImageSubDataOES"
    ].

s186_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s186_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s186_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s186_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s186_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 187.

s187_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s187_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s187_emitter_indirect_multi_draws_test_() ->
    [
        {"gl 4.6", fun() -> s187_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s187_assert_emitted_absent({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s187_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s187_assert_emitted_absent({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s187_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s187_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s187_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s187_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s187_supports_indirect_multi_draws(Target) of
        true ->
            s187_assert_enum_contains(BindingData, "primitive_type", "points"),
            s187_assert_enum_contains(BindingData, "draw_elements_type", "unsigned_byte"),
            s187_assert_path(
                {"multi_draw_arrays_indirect", 4},
                maps:get({"multi_draw_arrays_indirect", 4}, Functions)
            ),
            s187_assert_path(
                {"multi_draw_elements_indirect", 5},
                maps:get({"multi_draw_elements_indirect", 5}, Functions)
            );
        false ->
            ?assertNot(maps:is_key({"multi_draw_arrays_indirect", 4}, Functions)),
            ?assertNot(maps:is_key({"multi_draw_elements_indirect", 5}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawArraysIndirect", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawElementsIndirect", Functions))
    end,
    s187_assert_deferred_neighbors_absent(Functions).

s187_supports_indirect_multi_draws({gl, {4, 6}}) ->
    true;
s187_supports_indirect_multi_draws(_) ->
    false.

s187_assert_path({"multi_draw_arrays_indirect", 4}, FunctionData) ->
    s187_assert_direct(
        FunctionData,
        "glMultiDrawArraysIndirect",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Offset", gl_offset},
            {in, "DrawCount", gl_sizei},
            {in, "Stride", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Offset", {gl, offset, []}},
            {"DrawCount", {gl, sizei, []}},
            {"Stride", {gl, sizei, []}}
        ],
        [
            {"Mode", s187_enum_nif_data()},
            {"Offset", in_gl_offset},
            {"DrawCount", s187_sizei_nif_data()},
            {"Stride", s187_sizei_nif_data()}
        ]
    );
s187_assert_path({"multi_draw_elements_indirect", 5}, FunctionData) ->
    s187_assert_direct(
        FunctionData,
        "glMultiDrawElementsIndirect",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Offset", gl_offset},
            {in, "DrawCount", gl_sizei},
            {in, "Stride", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Offset", {gl, offset, []}},
            {"DrawCount", {gl, sizei, []}},
            {"Stride", {gl, sizei, []}}
        ],
        [
            {"Mode", s187_enum_nif_data()},
            {"Type", s187_enum_nif_data()},
            {"Offset", in_gl_offset},
            {"DrawCount", s187_sizei_nif_data()},
            {"Stride", s187_sizei_nif_data()}
        ]
    ).

s187_assert_direct(FunctionData, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s187_assert_clause_params(ParamsSpecs, maps:get(params, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s187_assert_clause_params(ParamsSpecs, ClauseParams) ->
    Expected =
        lists:map(
            fun
                ({in, Name, {gl_enum, _Group}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, _Type}) ->
                    {Name, do_nothing}
            end,
            ParamsSpecs
        ),
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(
                        lists:keymember("points", 1, TransformMap) orelse
                            lists:keymember("unsigned_byte", 1, TransformMap)
                    ),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s187_assert_emitted_present(Target) ->
    {Erl, C} = s187_generate_surface(Target),
    s187_assert_export_and_call(Erl, <<"multi_draw_arrays_indirect">>, 4, <<"glMultiDrawArraysIndirect">>),
    s187_assert_export_and_call(Erl, <<"multi_draw_elements_indirect">>, 5, <<"glMultiDrawElementsIndirect">>),
    s187_assert_contains(Erl, <<"Offset :: gl:offset()">>),
    s187_assert_contains(Erl, <<"DrawCount :: gl:sizei()">>),
    s187_assert_contains(Erl, <<"Stride :: gl:sizei()">>),
    s187_assert_contains(C, <<"glMultiDrawArraysIndirect(arg_0, (GLvoid*)(uintptr_t)arg_1, arg_2, arg_3);">>),
    s187_assert_contains(
        C,
        <<"glMultiDrawElementsIndirect(arg_0, arg_1, (GLvoid*)(uintptr_t)arg_2, arg_3, arg_4);">>
    ),
    s187_assert_deferred_emitted_absent(Erl, C).

s187_assert_emitted_absent(Target) ->
    {Erl, C} = s187_generate_surface(Target),
    s187_assert_not_contains(Erl, <<"-export([multi_draw_arrays_indirect/4]).">>),
    s187_assert_not_contains(Erl, <<"-export([multi_draw_elements_indirect/5]).">>),
    s187_assert_not_contains(C, <<"glMultiDrawArraysIndirect(">>),
    s187_assert_not_contains(C, <<"glMultiDrawElementsIndirect(">>),
    s187_assert_deferred_emitted_absent(Erl, C).

s187_assert_export_and_call(Erl, FunctionName, Arity, Command) ->
    s187_assert_contains(Erl, <<"-export([", FunctionName/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s187_assert_contains(Erl, <<"?CALL_RAW_FUNC(", Command/binary, "_raw(">>).

s187_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard187-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s187_ensure_absent(Dir),
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

s187_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s187_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s187_deferred_neighbor_commands()
    ].

s187_assert_deferred_emitted_absent(Erl, C) ->
    [
        s187_assert_not_contains(Erl, Needle)
     || Needle <- [
        ]
    ],
    [
        s187_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glCopyImageSubDataEXT(">>,
            <<"glCopyImageSubDataNV(">>,
            <<"glCopyImageSubDataOES(">>
        ]
    ].

s187_deferred_neighbor_commands() ->
    [
        "glCopyImageSubDataEXT",
        "glCopyImageSubDataNV",
        "glCopyImageSubDataOES"
    ].

s187_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s187_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s187_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s187_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s187_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s187_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 188.

s188_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s188_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s188_emitter_indirect_multi_draw_counts_test_() ->
    [
        {"gl 4.6", fun() -> s188_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s188_assert_emitted_absent({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s188_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s188_assert_emitted_absent({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s188_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s188_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s188_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s188_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s188_supports_indirect_count_multi_draws(Target) of
        true ->
            s188_assert_enum_contains(BindingData, "primitive_type", "points"),
            s188_assert_enum_contains(BindingData, "draw_elements_type", "unsigned_byte"),
            s188_assert_path(
                {"multi_draw_arrays_indirect_count", 5},
                maps:get({"multi_draw_arrays_indirect_count", 5}, Functions)
            ),
            s188_assert_path(
                {"multi_draw_elements_indirect_count", 6},
                maps:get({"multi_draw_elements_indirect_count", 6}, Functions)
            );
        false ->
            ?assertNot(maps:is_key({"multi_draw_arrays_indirect_count", 5}, Functions)),
            ?assertNot(maps:is_key({"multi_draw_elements_indirect_count", 6}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawArraysIndirectCount", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glMultiDrawElementsIndirectCount", Functions))
    end,
    s188_assert_deferred_neighbors_absent(Functions).

s188_supports_indirect_count_multi_draws({gl, {4, 6}}) ->
    true;
s188_supports_indirect_count_multi_draws(_) ->
    false.

s188_assert_path({"multi_draw_arrays_indirect_count", 5}, FunctionData) ->
    s188_assert_direct(
        FunctionData,
        "glMultiDrawArraysIndirectCount",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Offset", gl_offset},
            {in, "DrawCountOffset", gl_intptr},
            {in, "MaxDrawCount", gl_sizei},
            {in, "Stride", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Offset", {gl, offset, []}},
            {"DrawCountOffset", {gl, intptr, []}},
            {"MaxDrawCount", {gl, sizei, []}},
            {"Stride", {gl, sizei, []}}
        ],
        [
            {"Mode", s188_enum_nif_data()},
            {"Offset", in_gl_offset},
            {"DrawCountOffset", s188_intptr_nif_data()},
            {"MaxDrawCount", s188_sizei_nif_data()},
            {"Stride", s188_sizei_nif_data()}
        ]
    );
s188_assert_path({"multi_draw_elements_indirect_count", 6}, FunctionData) ->
    s188_assert_direct(
        FunctionData,
        "glMultiDrawElementsIndirectCount",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Type", {gl_enum, "DrawElementsType"}},
            {in, "Offset", gl_offset},
            {in, "DrawCountOffset", gl_intptr},
            {in, "MaxDrawCount", gl_sizei},
            {in, "Stride", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Type", {undefined, draw_elements_type, []}},
            {"Offset", {gl, offset, []}},
            {"DrawCountOffset", {gl, intptr, []}},
            {"MaxDrawCount", {gl, sizei, []}},
            {"Stride", {gl, sizei, []}}
        ],
        [
            {"Mode", s188_enum_nif_data()},
            {"Type", s188_enum_nif_data()},
            {"Offset", in_gl_offset},
            {"DrawCountOffset", s188_intptr_nif_data()},
            {"MaxDrawCount", s188_sizei_nif_data()},
            {"Stride", s188_sizei_nif_data()}
        ]
    ).

s188_assert_direct(FunctionData, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s188_assert_clause_params(ParamsSpecs, maps:get(params, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s188_assert_clause_params(ParamsSpecs, ClauseParams) ->
    Expected =
        lists:map(
            fun
                ({in, Name, {gl_enum, _Group}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, _Type}) ->
                    {Name, do_nothing}
            end,
            ParamsSpecs
        ),
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(
                        lists:keymember("points", 1, TransformMap) orelse
                            lists:keymember("unsigned_byte", 1, TransformMap)
                    ),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s188_assert_emitted_present(Target) ->
    {Erl, C} = s188_generate_surface(Target),
    s188_assert_export_and_call(Erl, <<"multi_draw_arrays_indirect_count">>, 5, <<"glMultiDrawArraysIndirectCount">>),
    s188_assert_export_and_call(Erl, <<"multi_draw_elements_indirect_count">>, 6, <<"glMultiDrawElementsIndirectCount">>),
    s188_assert_contains(Erl, <<"Offset :: gl:offset()">>),
    s188_assert_contains(Erl, <<"DrawCountOffset :: gl:intptr()">>),
    s188_assert_contains(Erl, <<"MaxDrawCount :: gl:sizei()">>),
    s188_assert_contains(Erl, <<"Stride :: gl:sizei()">>),
    s188_assert_contains(C, <<"glMultiDrawArraysIndirectCount(arg_0, (GLvoid*)(uintptr_t)arg_1, arg_2, arg_3, arg_4);">>),
    s188_assert_contains(
        C,
        <<"glMultiDrawElementsIndirectCount(arg_0, arg_1, (GLvoid*)(uintptr_t)arg_2, arg_3, arg_4, arg_5);">>
    ),
    s188_assert_deferred_emitted_absent(Erl, C).

s188_assert_emitted_absent(Target) ->
    {Erl, C} = s188_generate_surface(Target),
    s188_assert_not_contains(Erl, <<"-export([multi_draw_arrays_indirect_count/5]).">>),
    s188_assert_not_contains(Erl, <<"-export([multi_draw_elements_indirect_count/6]).">>),
    s188_assert_not_contains(C, <<"glMultiDrawArraysIndirectCount(">>),
    s188_assert_not_contains(C, <<"glMultiDrawElementsIndirectCount(">>),
    s188_assert_deferred_emitted_absent(Erl, C).

s188_assert_export_and_call(Erl, FunctionName, Arity, Command) ->
    s188_assert_contains(Erl, <<"-export([", FunctionName/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s188_assert_contains(Erl, <<"?CALL_RAW_FUNC(", Command/binary, "_raw(">>).

s188_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard188-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s188_ensure_absent(Dir),
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

s188_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s188_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s188_deferred_neighbor_commands()
    ].

s188_assert_deferred_emitted_absent(Erl, C) ->
    [
        s188_assert_not_contains(Erl, Needle)
     || Needle <- [
        ]
    ],
    [
        s188_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glCopyImageSubDataEXT(">>,
            <<"glCopyImageSubDataNV(">>,
            <<"glCopyImageSubDataOES(">>
        ]
    ].

s188_deferred_neighbor_commands() ->
    [
        "glCopyImageSubDataEXT",
        "glCopyImageSubDataNV",
        "glCopyImageSubDataOES"
    ].

s188_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s188_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s188_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s188_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s188_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s188_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s188_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
