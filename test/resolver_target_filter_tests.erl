-module(resolver_target_filter_tests).
-include_lib("eunit/include/eunit.hrl").

%% Target filtering and version/profile boundary contracts.

%% Historical shard 172.

s172_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s172_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s172_emitter_test_() ->
    [
        {"gl 4.6", fun() -> s172_assert_emitted_surface({gl, {4, 6}}, true) end},
        {"gl 4.1", fun() -> s172_assert_emitted_surface({gl, {4, 1}}, false) end},
        {"gles 3.2", fun() -> s172_assert_emitted_surface({gles, {3, 2}}, false) end}
    ].

s172_assert_target({gl, {4, 6}} = Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s172_assert_enum_types(BindingData),
    s172_assert_direct_vertex_array(maps:get({"get_vertex_array", 3}, Functions)),
    s172_assert_indexed_vertex_array(maps:get({"get_vertex_array", 5}, Functions)),
    s172_assert_deferred_neighbors_absent(Functions);
s172_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assertNot(maps:is_key({"get_vertex_array", 3}, Functions)),
    ?assertNot(maps:is_key({"get_vertex_array", 5}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetVertexArrayiv", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetVertexArrayIndexediv", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetVertexArrayIndexed64iv", Functions)),
    s172_assert_deferred_neighbors_absent(Functions).

s172_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    VertexArrayParameters = maps:get("vertex_array_parameter_name", EnumTypes),
    ?assertEqual(["element_array_buffer_binding"], VertexArrayParameters),

    IndexedParameters = maps:get("vertex_array_indexed_parameter_name", EnumTypes),
    [
        ?assert(lists:member(Atom, IndexedParameters))
     || Atom <- s172_indexed_parameter_atoms()
    ],
    ?assertNot(lists:member("current_vertex_attrib", IndexedParameters)).

s172_assert_direct_vertex_array(Function) ->
    ?assertEqual(
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "ParamName", s172_vertex_array_parameter_enum()},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Array", {undefined, vertex_array, []}},
            {"ParamName", {undefined, vertex_array_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(3, maps:get(function_arity, Function)),
    ?assertEqual(undefined, maps:get(extra_type, Function)),
    [Clause] = maps:get(function_clauses, Function),
    ?assertEqual("glGetVertexArrayiv", maps:get(raw_function, Clause)),
    s172_assert_transform_atoms(["element_array_buffer_binding"], maps:get(params, Clause)),
    s172_assert_nif(Function, "glGetVertexArrayiv", gl_int, 3).

s172_assert_indexed_vertex_array(Function) ->
    ?assertEqual(
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "Index", gl_uint},
            {in, "ParamName", s172_vertex_array_indexed_parameter_enum()},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, i64]}},
            {"Array", {undefined, vertex_array, []}},
            {"Index", {gl, uint, []}},
            {"ParamName", {undefined, vertex_array_indexed_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_vertex_array_value, []}}}],
        maps:get(specs_return, Function)
    ),
    ?assertEqual(
        {get_vertex_array_value, {set, [{gl, int, []}, {gl, int64, []}]}},
        maps:get(extra_type, Function)
    ),
    ?assertEqual(5, maps:get(function_arity, Function)),

    Expected = [
        {"glGetVertexArrayIndexediv", i, gl_int},
        {"glGetVertexArrayIndexed64iv", i64, gl_int64}
    ],
    [
        begin
            ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, Function))),
            ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, Function))),
            Clause = s172_find_clause(Command, maps:get(function_clauses, Function)),
            ?assertMatch([{Suffix, ignore} | _], maps:get(params, Clause)),
            s172_assert_transform_atoms(s172_indexed_parameter_atoms(), maps:get(params, Clause)),
            s172_assert_nif(Function, Command, GlType, 4)
        end
     || {Command, TypeAtom, GlType} <- Expected,
        Suffix <- [atom_to_list(TypeAtom)]
    ].

s172_assert_nif(Function, Command, GlType, Arity) ->
    NifData = maps:get(Command, maps:get(nif_functions, Function)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    Params = maps:get(params, NifData),
    ?assertEqual(
        {"Values", {out_typed_value_list, s172_gl_ctype(GlType), s172_term_function(GlType)}},
        lists:last(Params)
    ).

s172_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s172_deferred_neighbor_commands()
    ].

s172_assert_emitted_surface(Target, ExpectedPresent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard172-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, Hrl} = file:read_file("gl.hrl"),
        {ok, C} = file:read_file("gl.c"),
        case ExpectedPresent of
            true -> s172_assert_emitted_present(Erl, Hrl, C);
            false -> s172_assert_emitted_absent(Erl, Hrl, C)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s172_assert_emitted_present(Erl, Hrl, C) ->
    s172_assert_contains(Erl, <<"-export([get_vertex_array/3]).">>),
    s172_assert_contains(Erl, <<"-export([get_vertex_array/5]).">>),
    s172_assert_contains(Erl, <<"-type vertex_array_parameter_name() ::">>),
    s172_assert_contains(Erl, <<"-type vertex_array_indexed_parameter_name() ::">>),
    s172_assert_contains(Erl, <<"get_vertex_array(Array, ParamName, Count) ->">>),
    s172_assert_contains(Erl, <<"get_vertex_array(i64, Array, Index, ParamName, Count) ->">>),
    s172_assert_contains(Erl, <<"glGetVertexArrayiv_raw(Array, NewParamName, Count)">>),
    s172_assert_contains(Erl, <<"glGetVertexArrayIndexediv_raw(Array, Index, NewParamName, Count)">>),
    s172_assert_contains(Erl, <<"glGetVertexArrayIndexed64iv_raw(Array, Index, NewParamName, Count)">>),
    s172_assert_contains(Hrl, <<"element_array_buffer_binding => 16#8895">>),
    s172_assert_contains(Hrl, <<"vertex_binding_buffer => 16#8F4F">>),
    s172_assert_contains(C, <<"glGetVertexArrayiv(arg_0, arg_1, arg_2_values);">>),
    s172_assert_contains(C, <<"glGetVertexArrayIndexediv(arg_0, arg_1, arg_2, arg_3_values);">>),
    s172_assert_contains(C, <<"glGetVertexArrayIndexed64iv(arg_0, arg_1, arg_2, arg_3_values);">>),
    s172_assert_deferred_needles_absent(Erl, C).

s172_assert_emitted_absent(Erl, Hrl, C) ->
    s172_assert_not_contains(Erl, <<"-export([get_vertex_array/3]).">>),
    s172_assert_not_contains(Erl, <<"-export([get_vertex_array/5]).">>),
    s172_assert_not_contains(Erl, <<"glGetVertexArrayiv_raw">>),
    s172_assert_not_contains(Hrl, <<"vertex_array_parameter_name">>),
    s172_assert_not_contains(C, <<"glGetVertexArrayiv(">>),
    s172_assert_not_contains(C, <<"glGetVertexArrayIndexediv(">>),
    s172_assert_not_contains(C, <<"glGetVertexArrayIndexed64iv(">>),
    s172_assert_deferred_needles_absent(Erl, C).

s172_assert_transform_atoms(ExpectedEnumAtoms, Params) ->
    TransformAtoms = lists:append(
        [
            [Atom || {Atom, _Constant} <- TransformMap]
         || {_Name, {gl_enum_to_uint, TransformMap}} <- Params
        ]
    ),
    [?assert(lists:member(Atom, TransformAtoms)) || Atom <- ExpectedEnumAtoms].

s172_assert_deferred_needles_absent(Erl, C) ->
    [
        begin
            s172_assert_not_contains(Erl, <<Needle/binary, "_raw">>),
            s172_assert_not_contains(C, <<Needle/binary, "(">>)
        end
     || Needle <- s172_deferred_neighbor_needles()
    ].

s172_vertex_array_parameter_enum() ->
    {gl_enum, {values, ["GL_ELEMENT_ARRAY_BUFFER_BINDING"]}, vertex_array_parameter_name}.

s172_vertex_array_indexed_parameter_enum() ->
    {gl_enum, {values, [
        "GL_VERTEX_ATTRIB_ARRAY_ENABLED",
        "GL_VERTEX_ATTRIB_ARRAY_SIZE",
        "GL_VERTEX_ATTRIB_ARRAY_STRIDE",
        "GL_VERTEX_ATTRIB_ARRAY_TYPE",
        "GL_VERTEX_ATTRIB_ARRAY_NORMALIZED",
        "GL_VERTEX_ATTRIB_ARRAY_INTEGER",
        "GL_VERTEX_ATTRIB_ARRAY_LONG",
        "GL_VERTEX_ATTRIB_ARRAY_DIVISOR",
        "GL_VERTEX_ATTRIB_RELATIVE_OFFSET",
        "GL_VERTEX_ATTRIB_BINDING",
        "GL_VERTEX_BINDING_BUFFER",
        "GL_VERTEX_BINDING_OFFSET",
        "GL_VERTEX_BINDING_STRIDE",
        "GL_VERTEX_BINDING_DIVISOR"
    ]}, vertex_array_indexed_parameter_name}.

s172_indexed_parameter_atoms() ->
    [
        "vertex_attrib_array_enabled",
        "vertex_attrib_array_size",
        "vertex_attrib_array_stride",
        "vertex_attrib_array_type",
        "vertex_attrib_array_normalized",
        "vertex_attrib_array_integer",
        "vertex_attrib_array_long",
        "vertex_attrib_array_divisor",
        "vertex_attrib_relative_offset",
        "vertex_attrib_binding",
        "vertex_binding_buffer",
        "vertex_binding_offset",
        "vertex_binding_stride",
        "vertex_binding_divisor"
    ].

s172_deferred_neighbor_commands() ->
    [
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange"
    ].

s172_deferred_neighbor_needles() ->
    [list_to_binary(Command) || Command <- s172_deferred_neighbor_commands()].

s172_gl_ctype(gl_int) -> "GLint";
s172_gl_ctype(gl_int64) -> "GLint64".

s172_term_function(gl_int) -> "enif_make_int";
s172_term_function(gl_int64) -> "enif_make_int64".

s172_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s172_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    case string:find(Haystack, Needle) of
        nomatch -> erlang:error({missing_needle, Needle});
        _ -> ok
    end;
s172_assert_contains(Haystack, Needle) ->
    case binary:match(Haystack, Needle) of
        nomatch -> erlang:error({missing_needle, Needle});
        _ -> ok
    end.

s172_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 173.

s173_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s173_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s173_emitter_query_buffer_object_test_() ->
    [
        {"gl 4.6", fun() -> s173_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s173_assert_emitted_absent({gl, {4, 1}}) end},
        {"gles 3.2", fun() -> s173_assert_emitted_absent({gles, {3, 2}}) end}
    ].

s173_assert_target({gl, {4, 6}} = Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"get_query_buffer_object", 5}, Functions)),
    GetQueryBufferObject = maps:get({"get_query_buffer_object", 5}, Functions),
    s173_assert_specs(GetQueryBufferObject),
    [s173_assert_variant(GetQueryBufferObject, Variant) || Variant <- s173_all_variants()],
    s173_assert_deferred_neighbors_absent(Functions);
s173_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assertNot(maps:is_key({"get_query_buffer_object", 5}, Functions)),
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || {Command, _TypeAtom, _GlType} <- s173_all_variants()
    ],
    s173_assert_deferred_neighbors_absent(Functions).

s173_assert_specs(GetQueryBufferObject) ->
    ?assertEqual(
        [
            {in, "Query", {gl_object, query}},
            {in, "Buffer", {gl_object, buffer}},
            {in, "ParamName", {gl_enum, "QueryObjectParameterName", query_object_parameter_name}},
            {in, "Offset", gl_intptr}
        ],
        maps:get(params_specs, GetQueryBufferObject)
    ),
    [
        {"Type", {set, TypeAtoms}},
        {"Query", {undefined, query, []}},
        {"Buffer", {undefined, buffer, []}},
        {"ParamName", {undefined, query_object_parameter_name, []}},
        {"Offset", {gl, intptr, []}}
    ] = maps:get(specs_params, GetQueryBufferObject),
    ?assertEqual(lists:sort([i, i64, ui, ui64]), lists:sort(TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, GetQueryBufferObject)),
    {get_query_buffer_object_value, {set, ValueTypes}} =
        maps:get(extra_type, GetQueryBufferObject),
    ?assertEqual(
        lists:sort([{gl, int, []}, {gl, int64, []}, {gl, uint, []}, {gl, uint64, []}]),
        lists:sort(ValueTypes)
    ),
    ?assertEqual(5, maps:get(function_arity, GetQueryBufferObject)).

s173_assert_variant(GetQueryBufferObject, {Command, TypeAtom, GlType}) ->
    ?assert(lists:member({Command, GlType, element}, maps:get(gl_commands, GetQueryBufferObject))),
    ?assert(lists:member({GlType, element}, maps:get(variants, GetQueryBufferObject))),
    s173_assert_clause(GetQueryBufferObject, Command, TypeAtom),
    s173_assert_nif(GetQueryBufferObject, Command).

s173_assert_clause(GetQueryBufferObject, Command, TypeAtom) ->
    Clause = s173_find_clause(Command, maps:get(function_clauses, GetQueryBufferObject)),
    [
        {Suffix, ignore},
        {"Query", do_nothing},
        {"Buffer", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Offset", do_nothing}
    ] = maps:get(params, Clause),
    ?assertEqual(atom_to_list(TypeAtom), Suffix),
    ?assert(lists:keymember("query_result", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("query_result_available", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s173_assert_nif(GetQueryBufferObject, Command) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetQueryBufferObject)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Query", s173_query_nif_data()},
            {"Buffer", s173_buffer_nif_data()},
            {"ParamName", s173_enum_nif_data()},
            {"Offset", s173_intptr_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s173_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s173_deferred_neighbor_commands()
    ].

s173_assert_emitted_present(Target) ->
    s173_assert_emitted_surface(
        Target,
        [
            <<"-export([get_query_buffer_object/5]).">>,
            <<"-export_type([query_object_parameter_name/0]).">>,
            <<"-spec get_query_buffer_object(">>,
            <<"Type :: i | i64 | ui | ui64">>,
            <<"Query :: query()">>,
            <<"Buffer :: buffer()">>,
            <<"Offset :: gl:intptr()">>,
            <<"get_query_buffer_object(i, Query, Buffer, ParamName, Offset) ->">>,
            <<"glGetQueryBufferObjectiv_raw(Query, Buffer, NewParamName, Offset)">>,
            <<"get_query_buffer_object(ui, Query, Buffer, ParamName, Offset) ->">>,
            <<"glGetQueryBufferObjectuiv_raw(Query, Buffer, NewParamName, Offset)">>,
            <<"get_query_buffer_object(i64, Query, Buffer, ParamName, Offset) ->">>,
            <<"glGetQueryBufferObjecti64v_raw(Query, Buffer, NewParamName, Offset)">>,
            <<"get_query_buffer_object(ui64, Query, Buffer, ParamName, Offset) ->">>,
            <<"glGetQueryBufferObjectui64v_raw(Query, Buffer, NewParamName, Offset)">>
        ],
        [
            <<"glGetQueryBufferObjectiv(arg_0, arg_1, arg_2, arg_3);">>,
            <<"glGetQueryBufferObjectuiv(arg_0, arg_1, arg_2, arg_3);">>,
            <<"glGetQueryBufferObjecti64v(arg_0, arg_1, arg_2, arg_3);">>,
            <<"glGetQueryBufferObjectui64v(arg_0, arg_1, arg_2, arg_3);">>,
            <<"{\"glGetQueryBufferObjectiv_raw\", 4, nif_glGetQueryBufferObjectiv, 0}">>,
            <<"{\"glGetQueryBufferObjectuiv_raw\", 4, nif_glGetQueryBufferObjectuiv, 0}">>,
            <<"{\"glGetQueryBufferObjecti64v_raw\", 4, nif_glGetQueryBufferObjecti64v, 0}">>,
            <<"{\"glGetQueryBufferObjectui64v_raw\", 4, nif_glGetQueryBufferObjectui64v, 0}">>
        ],
        s173_forbidden_needles()
    ).

s173_assert_emitted_absent(Target) ->
    s173_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_query_buffer_object">>,
            <<"glGetQueryBufferObjectiv">>,
            <<"glGetQueryBufferObjectuiv">>,
            <<"glGetQueryBufferObjecti64v">>,
            <<"glGetQueryBufferObjectui64v">>
            | s173_forbidden_needles()
        ]
    ).

s173_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard173-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s173_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s173_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s173_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s173_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s173_all_variants() ->
    [
        {"glGetQueryBufferObjectiv", i, gl_int},
        {"glGetQueryBufferObjecti64v", i64, gl_int64},
        {"glGetQueryBufferObjectuiv", ui, gl_uint},
        {"glGetQueryBufferObjectui64v", ui64, gl_uint64}
    ].

s173_deferred_neighbor_commands() ->
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

s173_forbidden_needles() ->
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

s173_query_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s173_buffer_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s173_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s173_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s173_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s173_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    case string:find(Haystack, Needle) of
        nomatch -> erlang:error({missing_needle, Needle});
        _ -> ok
    end;
s173_assert_contains(Haystack, Needle) ->
    case binary:match(Haystack, Needle) of
        nomatch -> erlang:error({missing_needle, Needle});
        _ -> ok
    end.

s173_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

