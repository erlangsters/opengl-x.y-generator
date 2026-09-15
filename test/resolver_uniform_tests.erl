-module(resolver_uniform_tests).
-include_lib("eunit/include/eunit.hrl").

%% Uniform and program-uniform resolver contracts.

%% Historical shard 67.
s067_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s067_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s067_emitter_program_uniform_element_setters_test_() ->
    [
        {"gl 4.6", fun() ->
            s067_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [<<"d | f | i | ui">>, <<"glProgramUniform1d_raw/3">>],
                [<<"glProgramUniform1d(arg_0, arg_1, arg_2);">>]
            )
        end},
        {"gles 3.2", fun() ->
            s067_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [<<"f | i | ui">>],
                []
            )
        end}
    ].

s067_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s067_supports_program_uniform(Target) of
        true ->
            ?assert(maps:is_key({"program_uniform", 4}, Functions)),
            s067_assert_direct_wrappers_absent(Functions),
            s067_assert_deferred_neighbors_absent(Functions),
            ProgramUniform = maps:get({"program_uniform", 4}, Functions),
            s067_assert_specs(Target, ProgramUniform),
            s067_assert_commands(Target, ProgramUniform),
            s067_assert_clauses(Target, ProgramUniform),
            s067_assert_nifs(Target, ProgramUniform);
        false ->
            ?assertNot(maps:is_key({"program_uniform", 4}, Functions)),
            s067_assert_no_program_uniform_commands(Functions)
    end.

s067_supports_program_uniform({gl, {4, 1}}) -> true;
s067_supports_program_uniform({gl, {4, 6}}) -> true;
s067_supports_program_uniform({gles, {3, 1}}) -> true;
s067_supports_program_uniform({gles, {3, 2}}) -> true;
s067_supports_program_uniform(_) -> false.

s067_supports_double_program_uniform({gl, _}) -> true;
s067_supports_double_program_uniform({gles, _}) -> false.

s067_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"program_uniform_1d", 3},
        {"program_uniform_1f", 3},
        {"program_uniform_1i", 3},
        {"program_uniform_1ui", 3},
        {"program_uniform_2d", 4},
        {"program_uniform_2f", 4},
        {"program_uniform_2i", 4},
        {"program_uniform_2ui", 4},
        {"program_uniform_3d", 5},
        {"program_uniform_3f", 5},
        {"program_uniform_3i", 5},
        {"program_uniform_3ui", 5},
        {"program_uniform_4d", 6},
        {"program_uniform_4f", 6},
        {"program_uniform_4i", 6},
        {"program_uniform_4ui", 6}
    ]).

s067_assert_deferred_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, s067_deferred_commands()).

s067_assert_no_program_uniform_commands(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, s067_element_commands() ++ s067_deferred_commands()).

s067_element_commands() ->
    [
        "glProgramUniform1d",
        "glProgramUniform1f",
        "glProgramUniform1i",
        "glProgramUniform1ui",
        "glProgramUniform2d",
        "glProgramUniform2f",
        "glProgramUniform2i",
        "glProgramUniform2ui",
        "glProgramUniform3d",
        "glProgramUniform3f",
        "glProgramUniform3i",
        "glProgramUniform3ui",
        "glProgramUniform4d",
        "glProgramUniform4f",
        "glProgramUniform4i",
        "glProgramUniform4ui"
    ].

s067_deferred_commands() ->
    [].

s067_assert_specs(Target, ProgramUniform) ->
    {program_uniform_value, {set, ExtraTypeVariants}} = maps:get(extra_type, ProgramUniform),
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, program_uniform_value, []}}
    ] = maps:get(specs_params, ProgramUniform),
    ?assertEqual([], maps:get(specs_return, ProgramUniform)),
    ?assertEqual(4, maps:get(function_arity, ProgramUniform)),

    s067_assert_type_family(f, {gl, float, []}, TypeAtoms, ExtraTypeVariants),
    s067_assert_type_family(i, {gl, int, []}, TypeAtoms, ExtraTypeVariants),
    s067_assert_type_family(ui, {gl, uint, []}, TypeAtoms, ExtraTypeVariants),
    case s067_supports_double_program_uniform(Target) of
        true ->
            s067_assert_type_family(d, {gl, double, []}, TypeAtoms, ExtraTypeVariants);
        false ->
            ?assertNot(lists:member(d, TypeAtoms))
    end.

s067_assert_type_family(TypeAtom, ScalarSpec, TypeAtoms, ExtraTypeVariants) ->
    ?assert(lists:member(TypeAtom, TypeAtoms)),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, ExtraTypeVariants))
    end, [
        ScalarSpec,
        {undefined, vector2, [ScalarSpec]},
        {undefined, vector3, [ScalarSpec]},
        {undefined, vector4, [ScalarSpec]}
    ]).

s067_assert_commands(Target, ProgramUniform) ->
    s067_assert_command_family(f, gl_float, ProgramUniform),
    s067_assert_command_family(i, gl_int, ProgramUniform),
    s067_assert_command_family(ui, gl_uint, ProgramUniform),
    case s067_supports_double_program_uniform(Target) of
        true ->
            s067_assert_command_family(d, gl_double, ProgramUniform);
        false ->
            s067_assert_command_absent("glProgramUniform1d", ProgramUniform)
    end.

s067_assert_command_family(TypeAtom, GlType, ProgramUniform) ->
    Suffix = atom_to_list(TypeAtom),
    Expected = [
        {"glProgramUniform2" ++ Suffix, {gl_vector, 2, GlType}, element},
        {"glProgramUniform3" ++ Suffix, {gl_vector, 3, GlType}, element},
        {"glProgramUniform4" ++ Suffix, {gl_vector, 4, GlType}, element},
        {"glProgramUniform1" ++ Suffix, GlType, element}
    ],
    GlCommands = maps:get(gl_commands, ProgramUniform),
    Variants = maps:get(variants, ProgramUniform),
    lists:foreach(fun(Command) ->
        ?assert(lists:member(Command, GlCommands))
    end, Expected),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {{gl_vector, 2, GlType}, element},
        {{gl_vector, 3, GlType}, element},
        {{gl_vector, 4, GlType}, element},
        {GlType, element}
    ]).

s067_assert_command_absent(Command, ProgramUniform) ->
    ?assertNot(lists:any(fun({GlCommand, _Type, _Form}) ->
        GlCommand =:= Command
    end, maps:get(gl_commands, ProgramUniform))).

s067_assert_clauses(Target, ProgramUniform) ->
    Clauses = maps:get(function_clauses, ProgramUniform),
    s067_assert_clause_family(f, Clauses),
    s067_assert_clause_family(i, Clauses),
    s067_assert_clause_family(ui, Clauses),
    case s067_supports_double_program_uniform(Target) of
        true -> s067_assert_clause_family(d, Clauses);
        false -> ok
    end.

s067_assert_clause_family(TypeAtom, Clauses) ->
    Suffix = atom_to_list(TypeAtom),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s067_assert_before("glProgramUniform2" ++ Suffix, "glProgramUniform1" ++ Suffix, RawOrder),
    s067_assert_before("glProgramUniform3" ++ Suffix, "glProgramUniform1" ++ Suffix, RawOrder),
    s067_assert_before("glProgramUniform4" ++ Suffix, "glProgramUniform1" ++ Suffix, RawOrder),
    s067_assert_vector_clause(Suffix, "glProgramUniform2" ++ Suffix, 2, Clauses),
    s067_assert_vector_clause(Suffix, "glProgramUniform3" ++ Suffix, 3, Clauses),
    s067_assert_vector_clause(Suffix, "glProgramUniform4" ++ Suffix, 4, Clauses),
    s067_assert_scalar_clause(Suffix, "glProgramUniform1" ++ Suffix, Clauses).

s067_assert_vector_clause(TypeAtom, RawFunction, VectorSize, Clauses) ->
    Clause = s067_find_clause(RawFunction, Clauses),
    ?assertEqual("Value", maps:get(guard_var, Clause)),
    ?assertEqual(
        [{is_tuple, var}, {tuple_size, var, VectorSize}],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Program", do_nothing},
            {"Location", do_nothing},
            {"Value", {gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s067_assert_scalar_clause(TypeAtom, RawFunction, Clauses) ->
    Clause = s067_find_clause(RawFunction, Clauses),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Program", do_nothing},
            {"Location", do_nothing},
            {"Value", do_nothing}
        ],
        maps:get(params, Clause)
    ).

s067_assert_nifs(Target, ProgramUniform) ->
    s067_assert_nif_family(f, s067_gl_float_spec(), ProgramUniform),
    s067_assert_nif_family(i, s067_gl_int_spec(), ProgramUniform),
    s067_assert_nif_family(ui, s067_gl_uint_spec(), ProgramUniform),
    case s067_supports_double_program_uniform(Target) of
        true -> s067_assert_nif_family(d, s067_gl_double_spec(), ProgramUniform);
        false -> ok
    end.

s067_assert_nif_family(TypeAtom, ValueSpec, ProgramUniform) ->
    Suffix = atom_to_list(TypeAtom),
    NifFunctions = maps:get(nif_functions, ProgramUniform),
    s067_assert_scalar_nif(maps:get("glProgramUniform1" ++ Suffix, NifFunctions), ValueSpec),
    s067_assert_vector_nif(maps:get("glProgramUniform2" ++ Suffix, NifFunctions), 2, ValueSpec),
    s067_assert_vector_nif(maps:get("glProgramUniform3" ++ Suffix, NifFunctions), 3, ValueSpec),
    s067_assert_vector_nif(maps:get("glProgramUniform4" ++ Suffix, NifFunctions), 4, ValueSpec).

s067_assert_scalar_nif(NifData, ValueSpec) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, s067_gl_uint_spec()}},
            {"Location", {gl_type, s067_gl_int_spec()}},
            {"Value", {gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s067_assert_vector_nif(NifData, VectorSize, ValueSpec) ->
    ?assertEqual(VectorSize + 2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", {gl_type, s067_gl_uint_spec()}},
            {"Location", {gl_type, s067_gl_int_spec()}}
        ] ++ [{"V" ++ integer_to_list(N), {gl_type, ValueSpec}} || N <- lists:seq(1, VectorSize)],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s067_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard67-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),

        s067_assert_contains(GeneratedErl, <<"-export([program_uniform/4]).">>),
        s067_assert_contains(GeneratedErl, <<"-type program_uniform_value() ::">>),
        s067_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glProgramUniform1f_raw(Program, Location, Value))">>),
        s067_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glProgramUniform4f_raw(Program, Location, V1, V2, V3, V4))">>),
        s067_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glProgramUniform1i_raw(Program, Location, Value))">>),
        s067_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glProgramUniform1ui_raw(Program, Location, Value))">>),
        [s067_assert_contains(GeneratedErl, Required) || Required <- RequiredErl],
        s067_assert_not_contains(GeneratedErl, <<"-export([program_uniform_1f/3]).">>),
        s067_assert_not_contains(GeneratedErl, <<"-export([program_uniform_matrix_4fv/5]).">>),

        s067_assert_contains(GeneratedC, <<"glProgramUniform1f(arg_0, arg_1, arg_2);">>),
        s067_assert_contains(GeneratedC, <<"glProgramUniform4f(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>),
        s067_assert_contains(GeneratedC, <<"{\"glProgramUniform1f_raw\", 3, nif_glProgramUniform1f, 0}">>),
        s067_assert_contains(GeneratedC, <<"{\"glProgramUniform4f_raw\", 6, nif_glProgramUniform4f, 0}">>),
        [s067_assert_contains(GeneratedC, Required) || Required <- RequiredC],
        case Target of
            {gles, _} ->
                s067_assert_not_contains(GeneratedErl, <<"glProgramUniform1d_raw">>),
                s067_assert_not_contains(GeneratedC, <<"glProgramUniform1d(">>);
            {gl, _} ->
                ok
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s067_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s067_assert_before(First, Second, Values) ->
    ?assert(s067_index_of(First, Values) < s067_index_of(Second, Values)).

s067_index_of(Value, Values) ->
    s067_index_of(Value, Values, 1).

s067_index_of(Value, [Value | _], Index) ->
    Index;
s067_index_of(Value, [_ | Rest], Index) ->
    s067_index_of(Value, Rest, Index + 1).

s067_gl_uint_spec() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s067_gl_int_spec() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s067_gl_float_spec() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s067_gl_double_spec() ->
    {"GLdouble", "double", "enif_get_double", "enif_make_double"}.

s067_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s067_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 96.
s096_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s096_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s096_emitter_scalar_reflection_lookup_test_() ->
    [
        {"gl 4.6", fun() ->
            s096_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([get_program_resource_index/3]).">>,
                    <<"-export([get_program_resource_location/3]).">>,
                    <<"-export([get_program_resource_location_index/3]).">>,
                    <<"-export([get_subroutine_uniform_location/3]).">>,
                    <<"-export([get_subroutine_index/3]).">>,
                    <<"ProgramInterface :: program_interface()">>,
                    <<"ShaderType :: shader_type()">>,
                    <<"Name0 = iolist_to_binary(Name)">>,
                    <<"glGetProgramResourceIndex_raw(Program, NewProgramInterface, Name0)">>,
                    <<"glGetSubroutineIndex_raw(Program, NewShaderType, Name0)">>
                ],
                [
                    <<"GLuint ret = glGetProgramResourceIndex(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"GLint ret = glGetProgramResourceLocation(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"GLint ret = glGetProgramResourceLocationIndex(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"GLint ret = glGetSubroutineUniformLocation(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"GLuint ret = glGetSubroutineIndex(arg_0, arg_1, (const GLchar*)arg_2_string);">>
                ],
                s096_deferred_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s096_assert_emitted_surface(
                {gl, {4, 1}},
                [
                    <<"-export([get_subroutine_uniform_location/3]).">>,
                    <<"-export([get_subroutine_index/3]).">>,
                    <<"glGetSubroutineUniformLocation_raw(Program, NewShaderType, Name0)">>,
                    <<"glGetSubroutineIndex_raw(Program, NewShaderType, Name0)">>
                ],
                [
                    <<"GLint ret = glGetSubroutineUniformLocation(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"GLuint ret = glGetSubroutineIndex(arg_0, arg_1, (const GLchar*)arg_2_string);">>
                ],
                [
                    <<"-export([get_program_resource_index/3]).">>,
                    <<"-export([get_program_resource_location/3]).">>,
                    <<"-export([get_program_resource_location_index/3]).">>,
                    <<"glGetProgramResourceIndex">>,
                    <<"glGetProgramResourceLocation">>
                ] ++ s096_deferred_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s096_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([get_program_resource_index/3]).">>,
                    <<"-export([get_program_resource_location/3]).">>,
                    <<"glGetProgramResourceIndex_raw(Program, NewProgramInterface, Name0)">>,
                    <<"glGetProgramResourceLocation_raw(Program, NewProgramInterface, Name0)">>
                ],
                [
                    <<"GLuint ret = glGetProgramResourceIndex(arg_0, arg_1, (const GLchar*)arg_2_string);">>,
                    <<"GLint ret = glGetProgramResourceLocation(arg_0, arg_1, (const GLchar*)arg_2_string);">>
                ],
                [
                    <<"-export([get_program_resource_location_index/3]).">>,
                    <<"-export([get_subroutine_uniform_location/3]).">>,
                    <<"-export([get_subroutine_index/3]).">>,
                    <<"glGetProgramResourceLocationIndex">>,
                    <<"glGetSubroutineUniformLocation">>,
                    <<"glGetSubroutineIndex">>
                ] ++ s096_deferred_needles()
            )
        end},
        {"gles 2.0", fun() ->
            s096_assert_emitted_surface(
                {gles, {2, 0}},
                [],
                [],
                [
                    <<"-export([get_program_resource_index/3]).">>,
                    <<"-export([get_program_resource_location/3]).">>,
                    <<"-export([get_program_resource_location_index/3]).">>,
                    <<"-export([get_subroutine_uniform_location/3]).">>,
                    <<"-export([get_subroutine_index/3]).">>,
                    <<"glGetProgramResourceIndex">>,
                    <<"glGetSubroutineIndex">>
                ]
            )
        end}
    ].

s096_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s096_assert_presence(Target, Functions),
    s096_assert_deferred_neighbors_absent(Functions),
    [s096_assert_path(Function, BindingData, maps:get(Function, Functions))
     || Function <- s096_present_functions(Target)].

s096_assert_presence(Target, Functions) ->
    Present = s096_present_functions(Target),
    Absent = s096_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s096_present_functions({gl, {4, 1}}) ->
    s096_subroutine_functions();
s096_present_functions({gl, {4, 6}}) ->
    s096_all_functions();
s096_present_functions({gles, {3, 1}}) ->
    s096_program_resource_functions();
s096_present_functions({gles, {3, 2}}) ->
    s096_program_resource_functions();
s096_present_functions(_) ->
    [].

s096_all_functions() ->
    s096_program_resource_functions() ++
        [{"get_program_resource_location_index", 3}] ++
        s096_subroutine_functions().

s096_program_resource_functions() ->
    [
        {"get_program_resource_index", 3},
        {"get_program_resource_location", 3}
    ].

s096_subroutine_functions() ->
    [
        {"get_subroutine_uniform_location", 3},
        {"get_subroutine_index", 3}
    ].

s096_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s096_assert_path({"get_program_resource_index", 3}, BindingData, FunctionData) ->
    s096_assert_enum_contains(BindingData, "program_interface", "uniform"),
    s096_assert_string_scalar_lookup(
        FunctionData,
        "glGetProgramResourceIndex",
        [{in, "ProgramInterface", {gl_enum, "ProgramInterface"}}],
        [{"ProgramInterface", {undefined, program_interface, []}}],
        [{"ProgramInterface", {gl_enum_to_uint, ["uniform"]}}],
        [{"ProgramInterface", s096_enum_nif_data()}],
        {"Index", gl_uint},
        [{"Index", {gl, uint, []}}],
        gluint_to_uint
    );
s096_assert_path({"get_program_resource_location", 3}, BindingData, FunctionData) ->
    s096_assert_enum_contains(BindingData, "program_interface", "uniform"),
    s096_assert_string_scalar_lookup(
        FunctionData,
        "glGetProgramResourceLocation",
        [{in, "ProgramInterface", {gl_enum, "ProgramInterface"}}],
        [{"ProgramInterface", {undefined, program_interface, []}}],
        [{"ProgramInterface", {gl_enum_to_uint, ["uniform"]}}],
        [{"ProgramInterface", s096_enum_nif_data()}],
        {"Location", gl_int},
        [{"Location", {gl, int, []}}],
        glint_to_integer
    );
s096_assert_path({"get_program_resource_location_index", 3}, BindingData, FunctionData) ->
    s096_assert_enum_contains(BindingData, "program_interface", "program_output"),
    s096_assert_string_scalar_lookup(
        FunctionData,
        "glGetProgramResourceLocationIndex",
        [{in, "ProgramInterface", {gl_enum, "ProgramInterface"}}],
        [{"ProgramInterface", {undefined, program_interface, []}}],
        [{"ProgramInterface", {gl_enum_to_uint, ["program_output"]}}],
        [{"ProgramInterface", s096_enum_nif_data()}],
        {"Index", gl_int},
        [{"Index", {gl, int, []}}],
        glint_to_integer
    );
s096_assert_path({"get_subroutine_uniform_location", 3}, BindingData, FunctionData) ->
    s096_assert_enum_contains(BindingData, "shader_type", "vertex_shader"),
    s096_assert_string_scalar_lookup(
        FunctionData,
        "glGetSubroutineUniformLocation",
        [{in, "ShaderType", {gl_enum, "ShaderType"}}],
        [{"ShaderType", {undefined, shader_type, []}}],
        [{"ShaderType", {gl_enum_to_uint, ["vertex_shader"]}}],
        [{"ShaderType", s096_enum_nif_data()}],
        {"Location", gl_int},
        [{"Location", {gl, int, []}}],
        glint_to_integer
    );
s096_assert_path({"get_subroutine_index", 3}, BindingData, FunctionData) ->
    s096_assert_enum_contains(BindingData, "shader_type", "vertex_shader"),
    s096_assert_string_scalar_lookup(
        FunctionData,
        "glGetSubroutineIndex",
        [{in, "ShaderType", {gl_enum, "ShaderType"}}],
        [{"ShaderType", {undefined, shader_type, []}}],
        [{"ShaderType", {gl_enum_to_uint, ["vertex_shader"]}}],
        [{"ShaderType", s096_enum_nif_data()}],
        {"Index", gl_uint},
        [{"Index", {gl, uint, []}}],
        gluint_to_uint
    ).

s096_assert_string_scalar_lookup(
    FunctionData,
    GlCommand,
    MiddleParamsSpecs,
    MiddleSpecsParams,
    MiddleClauseParams,
    MiddleNifParams,
    ReturnSpecs,
    SpecsReturn,
    NifReturn
) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Program", {gl_object, program}}] ++ MiddleParamsSpecs ++ [{in, "Name", gl_string}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(ReturnSpecs, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Program", {undefined, program, []}}] ++ MiddleSpecsParams ++ [{"Name", {undefined, iodata, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(SpecsReturn, maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    s096_assert_clause_params(
        [{"Program", do_nothing}] ++ MiddleClauseParams ++ [{"Name", normalize_gl_string}],
        maps:get(params, Clause)
    ),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Program", s096_uint_nif_data()}] ++ MiddleNifParams ++ [{"Name", in_gl_string}],
        maps:get(params, NifData)
    ),
    ?assertEqual(NifReturn, maps:get(return, NifData)).

s096_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s096_assert_clause_param/1, lists:zip(Expected, Actual)).

s096_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s096_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s096_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s096_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard96-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s096_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s096_assert_contains(C, Needle) || Needle <- RequiredC],
        [s096_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s096_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s096_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s096_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s096_deferred_needles() ->
    [].

s096_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s096_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 98.
s098_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s098_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s098_emitter_object_parameter_queries_test_() ->
    [
        {"gl 4.6", fun() -> s098_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s098_assert_emitted_surface({gles, {3, 2}}) end}
    ].

s098_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_shader", 3}, Functions)),
    ?assert(maps:is_key({"get_program", 3}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 2}, Functions)),
    ?assertNot(maps:is_key({"get_shader", 4}, Functions)),
    ?assertNot(maps:is_key({"get_program", 2}, Functions)),
    ?assertNot(maps:is_key({"get_program", 4}, Functions)),
    s098_assert_removed_exports_absent(Functions),
    s098_assert_shader_query(maps:get({"get_shader", 3}, Functions)),
    s098_assert_program_query(maps:get({"get_program", 3}, Functions)),
    s098_assert_enum_contains(BindingData, shader_parameter_name, [
        "compile_status",
        "delete_status",
        "info_log_length",
        "shader_source_length"
    ]),
    s098_assert_enum_contains(BindingData, program_parameter_name, [
        "delete_status",
        "link_status",
        "validate_status",
        "info_log_length",
        "attached_shaders",
        "active_uniforms",
        "active_uniform_max_length",
        "active_attributes",
        "active_attribute_max_length"
    ]).

s098_assert_removed_exports_absent(Functions) ->
    [?assertNot(maps:is_key(Key, Functions)) || Key <- s098_removed_exports()].

s098_removed_exports() ->
    [
        {"get_shader_compile_status", 1},
        {"get_shader_delete_status", 1},
        {"get_shader_info_log_length", 1},
        {"get_shader_source_length", 1},
        {"get_program_link_status", 1},
        {"get_program_delete_status", 1},
        {"get_program_validation_status", 1},
        {"get_program_info_log_length", 1},
        {"get_program_attached_shaders_count", 1},
        {"get_program_active_attributes_count", 1},
        {"get_program_active_attribute_max_length", 1},
        {"get_program_active_uniforms_count", 1},
        {"get_program_active_uniform_max_length", 1},
        {"get_program_active_uniform_blocks_count", 1},
        {"get_program_active_uniform_block_max_name_length", 1},
        {"get_program_transform_feedback_varyings_count", 1},
        {"get_program_transform_feedback_varying_max_length", 1},
        {"get_program_binary_length", 1},
        {"get_program_active_atomic_counter_buffers_count", 1},
        {"get_program_geometry_vertices_out", 1},
        {"get_program_separable", 1},
        {"get_program_pipeline_validation_status", 1},
        {"get_program_pipeline_info_log_length", 1}
    ].

s098_assert_shader_query(FunctionData) ->
    s098_assert_object_query(
        FunctionData,
        "glGetShaderiv",
        "glGetShaderivValues",
        "Shader",
        {gl_object, shader},
        shader_parameter_name,
        ["compile_status", "delete_status", "info_log_length", "shader_source_length"]
    ).

s098_assert_program_query(FunctionData) ->
    s098_assert_object_query(
        FunctionData,
        "glGetProgramiv",
        "glGetProgramivValues",
        "Program",
        {gl_object, program},
        program_parameter_name,
        ["link_status", "validate_status", "info_log_length", "active_uniforms"]
    ).

s098_assert_object_query(FunctionData, Command, RawName, ObjectParamName, ObjectType, PublicEnumName, ExpectedAtoms) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, ObjectParamName, ObjectType},
            {in, "ParamName", s098_expected_enum_spec(PublicEnumName)},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {ObjectParamName, s098_object_type_spec(ObjectType)},
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
    [?assert(lists:keymember(Atom, 1, ParamNameMap)) || Atom <- ExpectedAtoms],
    ?assertEqual(RawName, maps:get(raw_function, Clause)),

    NifData = maps:get(RawName, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {ObjectParamName, s098_object_nif_data()},
            {"ParamName", s098_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s098_expected_enum_spec(shader_parameter_name) ->
    {gl_enum, "ShaderParameterName", shader_parameter_name};
s098_expected_enum_spec(program_parameter_name) ->
    {gl_enum, ["ProgramPropertyARB", "ProgramParameterPName"], program_parameter_name}.

s098_assert_enum_contains(BindingData, TypeName, ExpectedAtoms) ->
    EnumTypes = maps:get(enum_types, BindingData),
    Values = maps:get(atom_to_list(TypeName), EnumTypes),
    [?assert(lists:member(Atom, Values)) || Atom <- ExpectedAtoms].

s098_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard98-generic-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        s098_assert_contains(Erl, <<"-export([get_shader/3]).">>),
        s098_assert_contains(Erl, <<"-export([get_program/3]).">>),
        s098_assert_contains(Erl, <<"-export_type([shader_parameter_name/0]).">>),
        s098_assert_contains(Erl, <<"-export_type([program_parameter_name/0]).">>),
        s098_assert_contains(Erl, <<"-spec get_shader(\n    Shader :: shader(),\n    ParamName :: shader_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>),
        s098_assert_contains(Erl, <<"-spec get_program(\n    Program :: program(),\n    ParamName :: program_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>),
        s098_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetShaderivValues_raw(Shader, NewParamName, Count)).">>),
        s098_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetProgramivValues_raw(Program, NewParamName, Count)).">>),
        s098_assert_contains(C, <<"glGetShaderiv(arg_0, arg_1, arg_2_values);">>),
        s098_assert_contains(C, <<"glGetProgramiv(arg_0, arg_1, arg_2_values);">>),
        s098_assert_contains(C, <<"{\"glGetShaderivValues_raw\", 3, nif_glGetShaderivValues, 0}">>),
        s098_assert_contains(C, <<"{\"glGetProgramivValues_raw\", 3, nif_glGetProgramivValues, 0}">>),
        s098_assert_contains(C, <<"enif_make_int(env, arg_2_values[i])">>),
        s098_assert_removed_needles_absent(Erl),
        s098_assert_removed_raw_needles_absent(Erl),
        s098_assert_removed_raw_needles_absent(C)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s098_assert_removed_needles_absent(Erl) ->
    [s098_assert_not_contains(Erl, iolist_to_binary(Name)) || {Name, _Arity} <- s098_removed_exports()].

s098_assert_removed_raw_needles_absent(Haystack) ->
    [
        s098_assert_not_contains(Haystack, Needle)
     || Needle <- [
            <<"glGetShaderivInteger_raw">>,
            <<"glGetProgramivInteger_raw">>,
            <<"glGetProgramPipelineivInteger_raw">>
        ]
    ].

s098_object_type_spec({gl_object, Name}) ->
    {undefined, Name, []}.

s098_object_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s098_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s098_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s098_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 99.
s099_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s099_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s099_emitter_target_filtered_metadata_queries_test_() ->
    [
        {"gl 4.6", fun() -> s099_assert_emitted_surface({gl, {4, 6}}) end},
        {"gles 3.2", fun() -> s099_assert_emitted_surface({gles, {3, 2}}) end},
        {"gles 2.0 absence", fun() -> s099_assert_emitted_absence({gles, {2, 0}}) end}
    ].

s099_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"get_program", 3}, Functions)),
    s098_assert_removed_exports_absent(Functions),
    s098_assert_enum_contains(BindingData, program_parameter_name, s099_expected_program_atoms(Target)),
    case lists:member(Target, s099_pipeline_targets()) of
        true ->
            ?assert(maps:is_key({"get_program_pipeline", 3}, Functions)),
            s098_assert_enum_contains(BindingData, program_pipeline_parameter_name, [
                "info_log_length",
                "validate_status"
            ]);
        false ->
            ?assertNot(maps:is_key({"get_program_pipeline", 3}, Functions))
    end.

s099_expected_program_atoms(Target) ->
    [
        "delete_status",
        "link_status",
        "validate_status",
        "info_log_length",
        "attached_shaders",
        "active_uniforms",
        "active_uniform_max_length",
        "active_attributes",
        "active_attribute_max_length"
    ] ++
        s099_atoms_when(lists:member(Target, s099_gl_3_3_plus_and_gles_3_0_plus_targets()), [
            "active_uniform_blocks",
            "active_uniform_block_max_name_length",
            "transform_feedback_varyings",
            "transform_feedback_varying_max_length"
        ]) ++
        s099_atoms_when(lists:member(Target, [{gl, {4, 1}}, {gl, {4, 6}}, {gles, {3, 0}}, {gles, {3, 1}}, {gles, {3, 2}}]), [
            "program_binary_length"
        ]) ++
        s099_atoms_when(lists:member(Target, [{gl, {4, 6}}, {gles, {3, 1}}, {gles, {3, 2}}]), [
            "active_atomic_counter_buffers"
        ]) ++
        s099_atoms_when(lists:member(Target, [{gl, {3, 3}}, {gl, {4, 1}}, {gl, {4, 6}}, {gles, {3, 2}}]), [
            "geometry_vertices_out"
        ]) ++
        s099_atoms_when(lists:member(Target, s099_pipeline_targets()), [
            "program_binary_retrievable_hint",
            "program_separable"
        ]).

s099_atoms_when(true, Atoms) ->
    Atoms;
s099_atoms_when(false, _Atoms) ->
    [].

s099_gl_3_3_plus_and_gles_3_0_plus_targets() ->
    [{gl, {3, 3}}, {gl, {4, 1}}, {gl, {4, 6}}, {gles, {3, 0}}, {gles, {3, 1}}, {gles, {3, 2}}].

s099_pipeline_targets() ->
    [{gl, {4, 1}}, {gl, {4, 6}}, {gles, {3, 1}}, {gles, {3, 2}}].

s099_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard99-generic-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        s098_assert_contains(Erl, <<"-export([get_program/3]).">>),
        s098_assert_contains(Erl, <<"-export([get_program_pipeline/3]).">>),
        s098_assert_contains(Erl, <<"program_binary_length -> ?GL_PROGRAM_BINARY_LENGTH">>),
        s098_assert_contains(Erl, <<"program_separable -> ?GL_PROGRAM_SEPARABLE">>),
        s098_assert_contains(Erl, <<"validate_status -> ?GL_VALIDATE_STATUS">>),
        s098_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetProgramivValues_raw(Program, NewParamName, Count)).">>),
        s098_assert_contains(Erl, <<"?CALL_RAW_FUNC(glGetProgramPipelineivValues_raw(Pipeline, NewParamName, Count)).">>),
        s098_assert_contains(C, <<"glGetProgramiv(arg_0, arg_1, arg_2_values);">>),
        s098_assert_contains(C, <<"glGetProgramPipelineiv(arg_0, arg_1, arg_2_values);">>),
        s098_assert_contains(C, <<"{\"glGetProgramivValues_raw\", 3, nif_glGetProgramivValues, 0}">>),
        s098_assert_contains(C, <<"{\"glGetProgramPipelineivValues_raw\", 3, nif_glGetProgramPipelineivValues, 0}">>),
        s098_assert_removed_needles_absent(Erl),
        s098_assert_removed_raw_needles_absent(Erl),
        s098_assert_removed_raw_needles_absent(C)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s099_assert_emitted_absence(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard99-absence-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        s098_assert_contains(Erl, <<"-export([get_program/3]).">>),
        s098_assert_not_contains(Erl, <<"-export([get_program_pipeline/3]).">>),
        s098_assert_removed_needles_absent(Erl),
        s098_assert_removed_raw_needles_absent(Erl)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.


%% Historical shard 129.
s129_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s129_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s129_emitter_matrix_array_test_() ->
    [
        {"gl 4.6", fun() -> s129_assert_emitted_surface({gl, {4, 6}}, full) end},
        {"gles 3.2", fun() -> s129_assert_emitted_surface({gles, {3, 2}}, es_program) end},
        {"gles 2.0", fun() -> s129_assert_emitted_surface({gles, {2, 0}}, es2) end}
    ].

s129_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform_matrix", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_matrix_2fv", 2}, Functions)),
    UniformMatrix = maps:get({"uniform_matrix", 3}, Functions),
    s129_assert_uniform_matrix_specs(Target, UniformMatrix),
    s129_assert_uniform_matrix_family(Target, UniformMatrix, f, gl_float),
    case s129_supports_double_uniform_matrix(Target) of
        true -> s129_assert_uniform_matrix_family(Target, UniformMatrix, d, gl_double);
        false -> s129_assert_uniform_matrix_family_absent(Functions, UniformMatrix, d)
    end,

    case s129_supports_program_uniform_matrix(Target) of
        true ->
            ?assert(maps:is_key({"program_uniform_matrix", 4}, Functions)),
            ?assertNot(maps:is_key({"program_uniform_matrix_2fv", 5}, Functions)),
            ProgramUniformMatrix = maps:get({"program_uniform_matrix", 4}, Functions),
            s129_assert_program_uniform_matrix_specs(Target, ProgramUniformMatrix),
            s129_assert_program_uniform_matrix_family(Target, ProgramUniformMatrix, f, gl_float),
            case s129_supports_double_program_uniform_matrix(Target) of
                true ->
                    s129_assert_program_uniform_matrix_family(Target, ProgramUniformMatrix, d, gl_double);
                false ->
                    s129_assert_program_uniform_matrix_family_absent(Functions, ProgramUniformMatrix, d)
            end;
        false ->
            ?assertNot(maps:is_key({"program_uniform_matrix", 4}, Functions))
    end,

    s129_assert_deferred_neighbors_absent(Functions).

s129_supports_non_square_uniform_matrix({gles, {2, 0}}) -> false;
s129_supports_non_square_uniform_matrix(_) -> true.

s129_supports_double_uniform_matrix({gl, {4, _}}) -> true;
s129_supports_double_uniform_matrix(_) -> false.

s129_supports_program_uniform_matrix({gl, {4, 1}}) -> true;
s129_supports_program_uniform_matrix({gl, {4, 6}}) -> true;
s129_supports_program_uniform_matrix({gles, {3, 1}}) -> true;
s129_supports_program_uniform_matrix({gles, {3, 2}}) -> true;
s129_supports_program_uniform_matrix(_) -> false.

s129_supports_double_program_uniform_matrix({gl, {4, _}}) -> true;
s129_supports_double_program_uniform_matrix(_) -> false.

s129_assert_uniform_matrix_specs(Target, UniformMatrix) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, uniform_matrix_value, []}}
    ] = maps:get(specs_params, UniformMatrix),
    ?assert(lists:member(f, TypeAtoms)),
    ?assertEqual(s129_supports_double_uniform_matrix(Target), lists:member(d, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, UniformMatrix)),
    ?assertEqual(3, maps:get(function_arity, UniformMatrix)).

s129_assert_program_uniform_matrix_specs(Target, ProgramUniformMatrix) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, program_uniform_matrix_value, []}}
    ] = maps:get(specs_params, ProgramUniformMatrix),
    ?assert(lists:member(f, TypeAtoms)),
    ?assertEqual(s129_supports_double_program_uniform_matrix(Target), lists:member(d, TypeAtoms)),
    ?assertEqual([], maps:get(specs_return, ProgramUniformMatrix)),
    ?assertEqual(4, maps:get(function_arity, ProgramUniformMatrix)).

s129_assert_uniform_matrix_family(Target, UniformMatrix, TypeAtom, GlType) ->
    Shapes = s129_supported_matrix_shapes(Target),
    s129_assert_matrix_array_specs(UniformMatrix, uniform_matrix_value, Shapes, GlType),
    s129_assert_matrix_array_commands(UniformMatrix, fun s129_uniform_matrix_command/3, Shapes, TypeAtom, GlType),
    s129_assert_uniform_matrix_array_clauses(UniformMatrix, Shapes, TypeAtom),
    s129_assert_uniform_matrix_array_nifs(UniformMatrix, Shapes, TypeAtom, GlType).

s129_assert_program_uniform_matrix_family(Target, ProgramUniformMatrix, TypeAtom, GlType) ->
    Shapes = s129_supported_matrix_shapes(Target),
    s129_assert_matrix_array_specs(ProgramUniformMatrix, program_uniform_matrix_value, Shapes, GlType),
    s129_assert_matrix_array_commands(ProgramUniformMatrix, fun s129_program_uniform_matrix_command/3, Shapes, TypeAtom, GlType),
    s129_assert_program_uniform_matrix_array_clauses(ProgramUniformMatrix, Shapes, TypeAtom),
    s129_assert_program_uniform_matrix_array_nifs(ProgramUniformMatrix, Shapes, TypeAtom, GlType).

s129_assert_uniform_matrix_family_absent(Functions, UniformMatrix, TypeAtom) ->
    [{"Type", {set, TypeAtoms}} | _] = maps:get(specs_params, UniformMatrix),
    ?assertNot(lists:member(TypeAtom, TypeAtoms)),
    s129_assert_no_commands(Functions, fun s129_uniform_matrix_command/3, s129_matrix_shapes(), TypeAtom).

s129_assert_program_uniform_matrix_family_absent(Functions, ProgramUniformMatrix, TypeAtom) ->
    [{"Type", {set, TypeAtoms}} | _] = maps:get(specs_params, ProgramUniformMatrix),
    ?assertNot(lists:member(TypeAtom, TypeAtoms)),
    s129_assert_no_commands(Functions, fun s129_program_uniform_matrix_command/3, s129_matrix_shapes(), TypeAtom).

s129_assert_no_commands(Functions, CommandFun, Shapes, TypeAtom) ->
    lists:foreach(fun({M, N}) ->
        ?assertNot(generator_test_support:has_gl_command(CommandFun(TypeAtom, M, N), Functions))
    end, Shapes).

s129_assert_matrix_array_specs(FunctionData, ExtraTypeName, Shapes, GlType) ->
    {ExtraTypeName, {set, TypeVariants}} = maps:get(extra_type, FunctionData),
    lists:foreach(fun({M, N}) ->
        MatrixSpec = {undefined, s129_matrix_type(M, N), [s129_scalar_spec(GlType)]},
        ?assert(lists:member(MatrixSpec, TypeVariants)),
        ?assert(lists:member({list, MatrixSpec}, TypeVariants))
    end, Shapes).

s129_assert_matrix_array_commands(FunctionData, CommandFun, Shapes, TypeAtom, GlType) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    Variants = maps:get(variants, FunctionData),
    lists:foreach(fun({M, N}) ->
        Command = CommandFun(TypeAtom, M, N),
        MatrixType = {gl_matrix, M, N, GlType},
        ?assert(lists:member({Command, MatrixType, element}, GlCommands)),
        ?assert(lists:member({Command, MatrixType, counted_array}, GlCommands)),
        ?assert(lists:member({MatrixType, element}, Variants)),
        ?assert(lists:member({MatrixType, counted_array}, Variants))
    end, Shapes).

s129_assert_uniform_matrix_array_clauses(UniformMatrix, Shapes, TypeAtom) ->
    Clauses = maps:get(function_clauses, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s129_uniform_matrix_command(TypeAtom, M, N),
        ArrayClause = s129_find_clause(RawFunction, Clauses, {list_gl_matrix_to_list, M, N}),
        ElementClause = s129_find_clause(RawFunction, Clauses, {gl_matrix_to_list, M, N}),
        ?assert(s129_index_of(ArrayClause, Clauses) < s129_index_of(ElementClause, Clauses)),
        ?assertEqual("Value", maps:get(guard_var, ArrayClause)),
        ?assertEqual(s129_matrix_array_guards(M, N), maps:get(guards, ArrayClause)),
        ?assertEqual(
            [
                {atom_to_list(TypeAtom), ignore},
                {"Location", do_nothing},
                {"Count", {derived_count, "Value"}},
                {"Transpose", {gl_bool_constant, false}},
                {"Value", {list_gl_matrix_to_list, M, N}}
            ],
            maps:get(params, ArrayClause)
        )
    end, Shapes).

s129_assert_program_uniform_matrix_array_clauses(ProgramUniformMatrix, Shapes, TypeAtom) ->
    Clauses = maps:get(function_clauses, ProgramUniformMatrix),
    lists:foreach(fun({M, N}) ->
        RawFunction = s129_program_uniform_matrix_command(TypeAtom, M, N),
        ArrayClause = s129_find_clause(RawFunction, Clauses, {list_gl_matrix_to_list, M, N}),
        ElementClause = s129_find_clause(RawFunction, Clauses, {gl_matrix_to_list, M, N}),
        ?assert(s129_index_of(ArrayClause, Clauses) < s129_index_of(ElementClause, Clauses)),
        ?assertEqual("Value", maps:get(guard_var, ArrayClause)),
        ?assertEqual(s129_matrix_array_guards(M, N), maps:get(guards, ArrayClause)),
        ?assertEqual(
            [
                {atom_to_list(TypeAtom), ignore},
                {"Program", do_nothing},
                {"Location", do_nothing},
                {"Count", {derived_count, "Value"}},
                {"Transpose", {gl_bool_constant, false}},
                {"Value", {list_gl_matrix_to_list, M, N}}
            ],
            maps:get(params, ArrayClause)
        )
    end, Shapes).

s129_assert_uniform_matrix_array_nifs(UniformMatrix, Shapes, TypeAtom, GlType) ->
    NifFunctions = maps:get(nif_functions, UniformMatrix),
    lists:foreach(fun({M, N}) ->
        NifData = maps:get(s129_uniform_matrix_command(TypeAtom, M, N), NifFunctions),
        ?assertEqual(4, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Location", {gl_type, s129_convert_spec(gl_int)}},
                {"Count", {gl_type, s129_convert_spec(gl_sizei)}},
                {"Transpose", boolean_to_glbool},
                {"Value", {list_gl_type, s129_convert_spec(GlType)}}
            ],
            maps:get(params, NifData)
        )
    end, Shapes).

s129_assert_program_uniform_matrix_array_nifs(ProgramUniformMatrix, Shapes, TypeAtom, GlType) ->
    NifFunctions = maps:get(nif_functions, ProgramUniformMatrix),
    lists:foreach(fun({M, N}) ->
        NifData = maps:get(s129_program_uniform_matrix_command(TypeAtom, M, N), NifFunctions),
        ?assertEqual(5, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Program", {gl_type, s129_convert_spec(gl_uint)}},
                {"Location", {gl_type, s129_convert_spec(gl_int)}},
                {"Count", {gl_type, s129_convert_spec(gl_sizei)}},
                {"Transpose", boolean_to_glbool},
                {"Value", {list_gl_type, s129_convert_spec(GlType)}}
            ],
            maps:get(params, NifData)
        )
    end, Shapes).

s129_matrix_array_guards(M, N) ->
    [
        {is_list, var},
        {is_tuple, head_var},
        {tuple_size, head_var, M},
        {is_tuple, {element, 1, head_var}},
        {tuple_size, {element, 1, head_var}, N}
    ].

s129_find_clause(RawFunction, Clauses, Transform) ->
    [Clause] = [
        Clause0
     || Clause0 <- Clauses,
        maps:get(raw_function, Clause0) =:= RawFunction,
        lists:member({"Value", Transform}, maps:get(params, Clause0))
    ],
    Clause.

s129_index_of(Value, Values) ->
    s129_index_of(Value, Values, 1).

s129_index_of(Value, [Value | _], Index) ->
    Index;
s129_index_of(Value, [_ | Rest], Index) ->
    s129_index_of(Value, Rest, Index + 1).

s129_assert_deferred_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, []).

s129_supported_matrix_shapes(Target) ->
    case s129_supports_non_square_uniform_matrix(Target) of
        true -> s129_matrix_shapes();
        false -> s129_square_shapes()
    end.

s129_square_shapes() ->
    [{2, 2}, {3, 3}, {4, 4}].

s129_matrix_shapes() ->
    [{2, 2}, {3, 3}, {4, 4}, {2, 3}, {3, 2}, {2, 4}, {4, 2}, {3, 4}, {4, 3}].

s129_uniform_matrix_command(TypeAtom, N, N) ->
    "glUniformMatrix" ++ integer_to_list(N) ++ atom_to_list(TypeAtom) ++ "v";
s129_uniform_matrix_command(TypeAtom, M, N) ->
    "glUniformMatrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N) ++ atom_to_list(TypeAtom) ++ "v".

s129_program_uniform_matrix_command(TypeAtom, N, N) ->
    "glProgramUniformMatrix" ++ integer_to_list(N) ++ atom_to_list(TypeAtom) ++ "v";
s129_program_uniform_matrix_command(TypeAtom, M, N) ->
    "glProgramUniformMatrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N) ++ atom_to_list(TypeAtom) ++ "v".

s129_matrix_type(N, N) ->
    list_to_atom("matrix" ++ integer_to_list(N));
s129_matrix_type(M, N) ->
    list_to_atom("matrix" ++ integer_to_list(M) ++ "x" ++ integer_to_list(N)).

s129_scalar_spec(gl_float) -> {gl, float, []};
s129_scalar_spec(gl_double) -> {gl, double, []}.

s129_convert_spec(gl_int) -> {"GLint", "int", "enif_get_int", "enif_make_int"};
s129_convert_spec(gl_uint) -> {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"};
s129_convert_spec(gl_sizei) -> {"GLsizei", "int", "enif_get_int", "enif_make_int"};
s129_convert_spec(gl_float) -> {"GLfloat", "double", "enif_get_double", "enif_make_double"};
s129_convert_spec(gl_double) -> {"GLdouble", "double", "enif_get_double", "enif_make_double"}.

s129_assert_emitted_surface(Target, Support) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard129-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
            full -> s129_assert_full_emission(Erl, C);
            es_program -> s129_assert_es_program_emission(Erl, C);
            es2 -> s129_assert_es2_emission(Erl, C)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s129_assert_full_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s129_assert_contains(Erl, Needle) end, [
        <<"-export([uniform_matrix/3]).">>,
        <<"-export([program_uniform_matrix/4]).">>,
        <<"[matrix2(gl:float())]">>,
        <<"[matrix4x3(gl:double())]">>,
        <<"uniform_matrix(f, Location, Value) when">>,
        <<"program_uniform_matrix(d, Program, Location, Value) when">>,
        <<"    is_list(Value) andalso">>,
        <<"Count = length(Value),">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix2fv_raw(Location, Count, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix4x3dv_raw(Location, Count, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glProgramUniformMatrix2fv_raw(Program, Location, Count, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glProgramUniformMatrix4x3dv_raw(Program, Location, Count, false, NewValue))">>
    ]),
    lists:foreach(fun(Needle) -> s129_assert_contains(C, Needle) end, [
        <<"glUniformMatrix2fv(arg_0, arg_1, arg_2, arg_3_array);">>,
        <<"glProgramUniformMatrix4x3dv(arg_0, arg_1, arg_2, arg_3, arg_4_array);">>
    ]),
    s129_assert_not_contains(Erl, <<"-export([uniform_matrix_2fv/2]).">>),
    s129_assert_not_contains(Erl, <<"-export([program_uniform_matrix_2fv/5]).">>).

s129_assert_es_program_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s129_assert_contains(Erl, Needle) end, [
        <<"-export([uniform_matrix/3]).">>,
        <<"-export([program_uniform_matrix/4]).">>,
        <<"[matrix4x3(gl:float())]">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix4x3fv_raw(Location, Count, false, NewValue))">>,
        <<"?CALL_RAW_FUNC(glProgramUniformMatrix4x3fv_raw(Program, Location, Count, false, NewValue))">>
    ]),
    s129_assert_contains(C, <<"glProgramUniformMatrix4x3fv(arg_0, arg_1, arg_2, arg_3, arg_4_array);">>),
    s129_assert_not_contains(Erl, <<"program_uniform_matrix(d, Program, Location, Value)">>),
    s129_assert_not_contains(C, <<"glProgramUniformMatrix2dv(">>).

s129_assert_es2_emission(Erl, C) ->
    lists:foreach(fun(Needle) -> s129_assert_contains(Erl, Needle) end, [
        <<"-export([uniform_matrix/3]).">>,
        <<"[matrix2(gl:float())]">>,
        <<"?CALL_RAW_FUNC(glUniformMatrix2fv_raw(Location, Count, false, NewValue))">>
    ]),
    s129_assert_contains(C, <<"glUniformMatrix2fv(arg_0, arg_1, arg_2, arg_3_array);">>),
    s129_assert_not_contains(Erl, <<"glUniformMatrix2x3fv_raw">>),
    s129_assert_not_contains(C, <<"glUniformMatrix2x3fv(">>),
    s129_assert_not_contains(Erl, <<"-export([program_uniform_matrix/4]).">>).

s129_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s129_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 189.

s189_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s189_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s189_emitter_robust_uniform_readback_test_() ->
    [
        {"gl 4.6", fun() -> s189_assert_emitted_gl46() end},
        {"gles 3.2", fun() -> s189_assert_emitted_es32() end},
        {"gl 4.1", fun() -> s189_assert_emitted_absent({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s189_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.1", fun() -> s189_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s189_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s189_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s189_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s189_supports_robust_uniform(Target) of
        true ->
            GetUniform = maps:get({"get_n_uniform", 4}, Functions),
            s189_assert_get_n_uniform(Target, GetUniform);
        false ->
            ?assertNot(maps:is_key({"get_n_uniform", 4}, Functions)),
            [
                ?assertNot(generator_test_support:has_gl_command(Command, Functions))
             || Command <- s189_uniform_commands()
            ]
    end,
    s189_assert_deferred_neighbors_absent(Functions).

s189_supports_robust_uniform({gl, {4, 6}}) ->
    true;
s189_supports_robust_uniform({gles, {3, 2}}) ->
    true;
s189_supports_robust_uniform(_) ->
    false.

s189_supports_double({gl, {4, 6}}) ->
    true;
s189_supports_double(_) ->
    false.

s189_assert_get_n_uniform(Target, GetUniform) ->
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "Location", gl_int},
            {out, "Values", {typed_value_list_with_byte_size, "Count", gl_x}}
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
    ?assert(lists:member(ui, TypeAtoms)),
    ?assertEqual(s189_supports_double(Target), lists:member(d, TypeAtoms)),
    ?assertEqual(
        [{"Values", {list, {undefined, get_n_uniform_value, []}}}],
        maps:get(specs_return, GetUniform)
    ),
    ?assertEqual(4, maps:get(function_arity, GetUniform)),
    {get_n_uniform_value, {set, ValueTypes}} = maps:get(extra_type, GetUniform),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)),
    ?assert(lists:member({gl, uint, []}, ValueTypes)),
    ?assertEqual(s189_supports_double(Target), lists:member({gl, double, []}, ValueTypes)),
    s189_assert_family(GetUniform, f, gl_float),
    s189_assert_family(GetUniform, i, gl_int),
    s189_assert_family(GetUniform, ui, gl_uint),
    case s189_supports_double(Target) of
        true -> s189_assert_family(GetUniform, d, gl_double);
        false -> s189_assert_family_absent(GetUniform, d)
    end.

s189_assert_family(GetUniform, TypeAtom, GlType) ->
    Command = s189_uniform_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list_with_byte_size}, maps:get(gl_commands, GetUniform))),
    ?assert(lists:member({GlType, typed_value_list_with_byte_size}, maps:get(variants, GetUniform))),
    s189_assert_clause(GetUniform, TypeAtom, Command),
    s189_assert_nif(GetUniform, Command, GlType).

s189_assert_family_absent(GetUniform, TypeAtom) ->
    Command = s189_uniform_command(TypeAtom),
    ?assertNot(lists:keymember(Command, 1, maps:get(gl_commands, GetUniform))),
    ?assertNot(maps:is_key(Command, maps:get(nif_functions, GetUniform))).

s189_assert_clause(GetUniform, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s189_find_clause(Command, maps:get(function_clauses, GetUniform)),
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

s189_assert_nif(GetUniform, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetUniform)),
    ?assertEqual(Command, maps:get(gl_command, NifData, Command)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Program", s189_uint_nif_data()},
            {"Location", s189_int_nif_data()},
            {"Values", {out_typed_value_list_with_byte_size, s189_gl_ctype(GlType), s189_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s189_assert_emitted_gl46() ->
    s189_assert_emitted_surface(
        {gl, {4, 6}},
        [
            <<"-export([get_n_uniform/4]).">>,
            <<"-spec get_n_uniform(\n    Type :: d | f | i | ui,\n    Program :: program(),\n    Location :: gl:int(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_n_uniform_value()]} | {error, atom()}.">>,
            <<"get_n_uniform(f, Program, Location, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetnUniformfv_raw(Program, Location, Count))">>,
            <<"get_n_uniform(d, Program, Location, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetnUniformdv_raw(Program, Location, Count))">>
        ],
        [
            <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)(INT_MAX / sizeof(GLfloat))) {">>,
            <<"GLsizei arg_2_size = (GLsizei)(arg_2_count_tmp * (ErlNifUInt64)sizeof(GLfloat));">>,
            <<"GLfloat* arg_2_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_2_count);">>,
            <<"glGetnUniformfv(arg_0, arg_1, arg_2_size, arg_2_values);">>,
            <<"glGetnUniformdv(arg_0, arg_1, arg_2_size, arg_2_values);">>,
            <<"{\"glGetnUniformfv_raw\", 3, nif_glGetnUniformfv, 0}">>,
            <<"{\"glGetnUniformdv_raw\", 3, nif_glGetnUniformdv, 0}">>
        ],
        s189_forbidden_needles()
    ).

s189_assert_emitted_es32() ->
    s189_assert_emitted_surface(
        {gles, {3, 2}},
        [
            <<"-export([get_n_uniform/4]).">>,
            <<"get_n_uniform(f, Program, Location, Count) ->">>,
            <<"get_n_uniform(i, Program, Location, Count) ->">>,
            <<"get_n_uniform(ui, Program, Location, Count) ->">>
        ],
        [
            <<"glGetnUniformfv(arg_0, arg_1, arg_2_size, arg_2_values);">>,
            <<"glGetnUniformiv(arg_0, arg_1, arg_2_size, arg_2_values);">>,
            <<"glGetnUniformuiv(arg_0, arg_1, arg_2_size, arg_2_values);">>
        ],
        [<<"glGetnUniformdv">> | s189_forbidden_needles()]
    ).

s189_assert_emitted_absent(Target) ->
    s189_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_n_uniform">>,
            <<"glGetnUniform">>
            | s189_forbidden_needles()
        ]
    ).

s189_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard189-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s189_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s189_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s189_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s189_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s189_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s189_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s189_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glGetnTexImage",
            "glGetnCompressedTexImage",
            "glCopyImageSubDataEXT",
            "glCopyImageSubDataNV",
            "glCopyImageSubDataOES"
        ]
    ].

s189_forbidden_needles() ->
    [
        <<"glGetnTexImage">>,
        <<"glGetnCompressedTexImage">>,
        <<"glCopyImageSubDataEXT(">>,
        <<"glCopyImageSubDataNV(">>,
        <<"glCopyImageSubDataOES(">>
    ].

s189_uniform_commands() ->
    [
        "glGetnUniformfv",
        "glGetnUniformiv",
        "glGetnUniformuiv",
        "glGetnUniformdv"
    ].

s189_uniform_command(f) -> "glGetnUniformfv";
s189_uniform_command(i) -> "glGetnUniformiv";
s189_uniform_command(ui) -> "glGetnUniformuiv";
s189_uniform_command(d) -> "glGetnUniformdv".

s189_gl_ctype(gl_float) -> "GLfloat";
s189_gl_ctype(gl_int) -> "GLint";
s189_gl_ctype(gl_uint) -> "GLuint";
s189_gl_ctype(gl_double) -> "GLdouble".

s189_term_function(gl_float) -> "enif_make_double";
s189_term_function(gl_int) -> "enif_make_int";
s189_term_function(gl_uint) -> "enif_make_uint";
s189_term_function(gl_double) -> "enif_make_double".

s189_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s189_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s189_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s189_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s189_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s189_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
