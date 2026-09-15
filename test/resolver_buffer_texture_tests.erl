-module(resolver_buffer_texture_tests).
-include_lib("eunit/include/eunit.hrl").

%% Buffer, texture, sampler, image, and copy resolver contracts.

%% Historical shard 10.
s010_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s010_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s010_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"buffer_data", 3}, Functions)),
    ?assertNot(maps:is_key({"buffer_data", 4}, Functions)),
    ?assertNot(maps:is_key({"buffer_sub_data", 4}, Functions)),

    s010_assert_enum_types(BindingData),
    s010_assert_buffer_data(maps:get({"buffer_data", 3}, Functions)).

s010_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("buffer_target", EnumTypes)),
    ?assert(maps:is_key("buffer_usage", EnumTypes)),
    ?assertNot(maps:is_key("buffer_target_arb", EnumTypes)),
    ?assertNot(maps:is_key("buffer_usage_arb", EnumTypes)),
    ?assert(lists:member("array_buffer", maps:get("buffer_target", EnumTypes))),
    ?assert(lists:member("static_draw", maps:get("buffer_usage", EnumTypes))).

s010_assert_buffer_data(FunctionData) ->
    ?assertEqual("glBufferData", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {in, "Usage", {gl_enum, "BufferUsageARB", buffer_usage}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, buffer_target, []}},
            {"DataOrSize", {set, [{undefined, iodata, []}, {undefined, non_neg_integer, []}]}},
            {"Usage", {undefined, buffer_usage, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"DataOrSize", {byte_data_or_size, "Size", "Data"}},
        {"Usage", {gl_enum_to_uint, UsageTransformMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("array_buffer", 1, TargetTransformMap)),
    ?assert(lists:keymember("static_draw", 1, UsageTransformMap)),
    ?assertEqual("glBufferData", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBufferData", NifFunctions)),
    NifData = maps:get("glBufferData", NifFunctions),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Size", {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}},
            {"Data", in_gl_binary_or_null},
            {"Usage", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 11.
s011_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s011_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s011_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"buffer_sub_data", 3}, Functions)),
    ?assertNot(maps:is_key({"buffer_sub_data", 4}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glMapBuffer", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glMapBufferRange", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glFlushMappedBufferRange", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glUnmapBuffer", Functions)),

    s011_assert_enum_types(BindingData),
    s011_assert_buffer_sub_data(maps:get({"buffer_sub_data", 3}, Functions)).

s011_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("buffer_target", EnumTypes)),
    ?assertNot(maps:is_key("buffer_target_arb", EnumTypes)),
    ?assert(lists:member("array_buffer", maps:get("buffer_target", EnumTypes))).

s011_assert_buffer_sub_data(FunctionData) ->
    ?assertEqual("glBufferSubData", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "Offset", gl_intptr},
            {in, "Data", {byte_data, "Size"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, buffer_target, []}},
            {"Offset", {gl, intptr, []}},
            {"Data", {undefined, iodata, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"Offset", do_nothing},
        {"Data", {byte_data, "Size"}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("array_buffer", 1, TargetTransformMap)),
    ?assertEqual("glBufferSubData", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBufferSubData", NifFunctions)),
    NifData = maps:get("glBufferSubData", NifFunctions),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Offset", {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}},
            {"Size", {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}},
            {"Data", binary_to_glbinary}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 29.
s029_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s029_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s029_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"active_texture", 1}, Functions)),
    ?assertNot(maps:is_key({"active_texture", 2}, Functions)),

    s029_assert_texture_unit_enum(BindingData),
    s029_assert_active_texture_path(maps:get({"active_texture", 1}, Functions)).

s029_assert_texture_unit_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("texture_unit", EnumTypes)),
    TextureUnits = maps:get("texture_unit", EnumTypes),
    ?assert(lists:member("texture0", TextureUnits)),
    ?assert(lists:member("texture1", TextureUnits)).

s029_assert_active_texture_path(FunctionData) ->
    ?assertEqual("glActiveTexture", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Texture", {gl_enum, "TextureUnit"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Texture", {undefined, texture_unit, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Texture", {gl_enum_to_uint, TextureTransformMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture0", 1, TextureTransformMap)),
    ?assert(lists:keymember("texture1", 1, TextureTransformMap)),
    ?assertEqual("glActiveTexture", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glActiveTexture", NifFunctions)),
    NifData = maps:get("glActiveTexture", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s029_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s029_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 30.
s030_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s030_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s030_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"tex_min_filter", 2}, Functions)),
    ?assertNot(maps:is_key({"tex_min_filter", 3}, Functions)),

    s030_assert_enum_types(BindingData),
    s030_assert_tex_min_filter_path(maps:get({"tex_min_filter", 2}, Functions)).

s030_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("texture_target", EnumTypes)),
    ?assert(maps:is_key("texture_min_filter", EnumTypes)),
    TextureTargets = maps:get("texture_target", EnumTypes),
    TextureMinFilters = maps:get("texture_min_filter", EnumTypes),
    ?assert(lists:member("texture_2d", TextureTargets)),
    ?assert(lists:member("nearest", TextureMinFilters)).

s030_assert_tex_min_filter_path(FunctionData) ->
    ?assertEqual("glTexParameteri", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {const, "ParameterName", {gl_enum_constant, "GL_TEXTURE_MIN_FILTER"}},
            {in, "Filter", {gl_enum, "TextureMinFilter"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Filter", {undefined, texture_min_filter, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParameterName", {gl_enum_constant, "GL_TEXTURE_MIN_FILTER"}},
        {"Filter", {gl_enum_to_uint, FilterTransformMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("nearest", 1, FilterTransformMap)),
    ?assertEqual("glTexParameteri", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glTexParameteri", NifFunctions)),
    NifData = maps:get("glTexParameteri", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s030_enum_nif_data()},
            {"ParameterName", s030_enum_nif_data()},
            {"Filter", s030_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s030_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 31.
s031_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s031_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s031_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"tex_min_filter", 2}, Functions)),
    ?assert(maps:is_key({"tex_mag_filter", 2}, Functions)),
    ?assertNot(maps:is_key({"tex_min_filter", 3}, Functions)),
    ?assertNot(maps:is_key({"tex_mag_filter", 3}, Functions)),
    ?assert(s031_gl_command_count("glTexParameteri", Functions) >= 2),

    s031_assert_enum_types(BindingData),
    TexMinFilter = maps:get({"tex_min_filter", 2}, Functions),
    TexMagFilter = maps:get({"tex_mag_filter", 2}, Functions),
    s031_assert_tex_mag_filter_path(TexMagFilter),
    s031_assert_shared_raw_nif(TexMinFilter, TexMagFilter).

s031_gl_command_count(Command, Functions) ->
    length(lists:filter(fun({_Function, FunctionData}) ->
        maps:get(gl_command, FunctionData, undefined) =:= Command
    end, maps:to_list(Functions))).

s031_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("texture_target", EnumTypes)),
    ?assert(maps:is_key("texture_min_filter", EnumTypes)),
    ?assert(maps:is_key("texture_mag_filter", EnumTypes)),
    TextureTargets = maps:get("texture_target", EnumTypes),
    TextureMinFilters = maps:get("texture_min_filter", EnumTypes),
    TextureMagFilters = maps:get("texture_mag_filter", EnumTypes),
    ?assert(lists:member("texture_2d", TextureTargets)),
    ?assert(lists:member("nearest", TextureMinFilters)),
    ?assert(lists:member("nearest", TextureMagFilters)),
    ?assert(lists:member("linear", TextureMagFilters)).

s031_assert_tex_mag_filter_path(FunctionData) ->
    ?assertEqual("glTexParameteri", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {const, "ParameterName", {gl_enum_constant, "GL_TEXTURE_MAG_FILTER"}},
            {in, "Filter", {gl_enum, "TextureMagFilter"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Filter", {undefined, texture_mag_filter, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParameterName", {gl_enum_constant, "GL_TEXTURE_MAG_FILTER"}},
        {"Filter", {gl_enum_to_uint, FilterTransformMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("nearest", 1, FilterTransformMap)),
    ?assert(lists:keymember("linear", 1, FilterTransformMap)),
    ?assertEqual("glTexParameteri", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glTexParameteri", NifFunctions)),
    NifData = maps:get("glTexParameteri", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s031_enum_nif_data()},
            {"ParameterName", s031_enum_nif_data()},
            {"Filter", s031_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s031_assert_shared_raw_nif(TexMinFilter, TexMagFilter) ->
    TexMinNifData = maps:get("glTexParameteri", maps:get(nif_functions, TexMinFilter)),
    TexMagNifData = maps:get("glTexParameteri", maps:get(nif_functions, TexMagFilter)),
    ?assertEqual(TexMinNifData, TexMagNifData).

s031_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 32.
s032_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s032_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s032_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"tex_min_filter", 2}, Functions)),
    ?assert(maps:is_key({"tex_mag_filter", 2}, Functions)),
    ?assert(maps:is_key({"tex_wrap_s", 2}, Functions)),
    ?assert(maps:is_key({"tex_wrap_t", 2}, Functions)),
    ?assertNot(maps:is_key({"tex_wrap_s", 3}, Functions)),
    ?assertNot(maps:is_key({"tex_wrap_t", 3}, Functions)),
    ?assertNot(maps:is_key({"tex_wrap_r", 2}, Functions)),
    ?assert(s032_gl_command_count("glTexParameteri", Functions) >= 4),

    s032_assert_enum_types(BindingData),
    TexMinFilter = maps:get({"tex_min_filter", 2}, Functions),
    TexMagFilter = maps:get({"tex_mag_filter", 2}, Functions),
    TexWrapS = maps:get({"tex_wrap_s", 2}, Functions),
    TexWrapT = maps:get({"tex_wrap_t", 2}, Functions),
    s032_assert_tex_wrap_path(
        TexWrapS,
        "tex_wrap_s",
        "GL_TEXTURE_WRAP_S",
        "clamp_to_edge"
    ),
    s032_assert_tex_wrap_path(
        TexWrapT,
        "tex_wrap_t",
        "GL_TEXTURE_WRAP_T",
        "clamp_to_edge"
    ),
    s032_assert_shared_raw_nif([TexMinFilter, TexMagFilter, TexWrapS, TexWrapT]).

s032_gl_command_count(Command, Functions) ->
    length(lists:filter(fun({_Function, FunctionData}) ->
        maps:get(gl_command, FunctionData, undefined) =:= Command
    end, maps:to_list(Functions))).

s032_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("texture_target", EnumTypes)),
    ?assert(maps:is_key("texture_min_filter", EnumTypes)),
    ?assert(maps:is_key("texture_mag_filter", EnumTypes)),
    ?assert(maps:is_key("texture_wrap_mode", EnumTypes)),
    ?assertNot(maps:is_key("texture_wrap_s", EnumTypes)),
    ?assertNot(maps:is_key("texture_wrap_t", EnumTypes)),
    TextureTargets = maps:get("texture_target", EnumTypes),
    TextureWrapModes = maps:get("texture_wrap_mode", EnumTypes),
    ?assert(lists:member("texture_2d", TextureTargets)),
    ?assert(lists:member("repeat", TextureWrapModes)),
    ?assert(lists:member("clamp_to_edge", TextureWrapModes)).

s032_assert_tex_wrap_path(FunctionData, _FunctionName, ConstantName, CanaryMode) ->
    ?assertEqual("glTexParameteri", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {const, "ParameterName", {gl_enum_constant, ConstantName}},
            {in, "Mode", {gl_enum, "TextureWrapMode"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Mode", {undefined, texture_wrap_mode, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParameterName", {gl_enum_constant, ConstantName}},
        {"Mode", {gl_enum_to_uint, ModeTransformMap}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember(CanaryMode, 1, ModeTransformMap)),
    ?assertEqual("glTexParameteri", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glTexParameteri", NifFunctions)),
    NifData = maps:get("glTexParameteri", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s032_enum_nif_data()},
            {"ParameterName", s032_enum_nif_data()},
            {"Mode", s032_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s032_assert_shared_raw_nif([FirstFunctionData|OtherFunctionData]) ->
    FirstNifData = s032_gl_tex_parameteri_nif_signature(FirstFunctionData),
    lists:foreach(fun(FunctionData) ->
        ?assertEqual(FirstNifData, s032_gl_tex_parameteri_nif_signature(FunctionData))
    end, OtherFunctionData).

s032_gl_tex_parameteri_nif_signature(FunctionData) ->
    NifData = maps:get("glTexParameteri", maps:get(nif_functions, FunctionData)),
    maps:put(
        params,
        [ParamData || {_ParamName, ParamData} <- maps:get(params, NifData)],
        NifData
    ).

s032_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 58.
s058_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s058_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s058_emitter_texture_allocation_surface_test_() ->
    [
        {"gl 4.6", fun() -> s058_assert_emitted_surface({gl, {4, 6}}, "OpenGL 4.6", s058_gl_46_exports(), s058_gl_46_c_calls()) end},
        {"gles 3.2", fun() ->
            s058_assert_emitted_surface({gles, {3, 2}}, "OpenGL ES 3.2", s058_gles_32_exports(), s058_gles_32_c_calls())
        end}
    ].

s058_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s058_assert_presence(Target, Functions),
    s058_assert_deferred_neighbors_absent(Functions),
    s058_assert_present_paths(Target, BindingData, Functions).

s058_assert_presence(Target, Functions) ->
    Present = s058_present_functions(Target),
    Absent = s058_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s058_present_functions({gl, {3, 3}}) ->
    s058_bind_to_buffer_texture();
s058_present_functions({gl, {4, 1}}) ->
    s058_bind_to_buffer_texture();
s058_present_functions({gl, {4, 6}}) ->
    s058_all_functions();
s058_present_functions({gles, {2, 0}}) ->
    [{"generate_mipmap", 1}];
s058_present_functions({gles, {3, 0}}) ->
    s058_mipmap_and_2d_3d_storage();
s058_present_functions({gles, {3, 1}}) ->
    s058_mipmap_and_2d_3d_storage();
s058_present_functions({gles, {3, 2}}) ->
    s058_mipmap_and_2d_3d_storage() ++ [{"tex_buffer", 3}, {"tex_buffer_range", 5}].

s058_all_functions() ->
    [
        {"generate_mipmap", 1},
        {"tex_buffer", 3},
        {"tex_storage_1d", 4},
        {"tex_storage_2d", 5},
        {"tex_storage_3d", 6},
        {"tex_buffer_range", 5},
        {"texture_buffer", 3},
        {"texture_buffer_range", 5},
        {"texture_storage_1d", 4},
        {"texture_storage_2d", 5},
        {"texture_storage_3d", 6},
        {"generate_texture_mipmap", 1},
        {"bind_texture_unit", 2}
    ].

s058_bind_to_buffer_texture() ->
    [{"generate_mipmap", 1}, {"tex_buffer", 3}].

s058_mipmap_and_2d_3d_storage() ->
    [{"generate_mipmap", 1}, {"tex_storage_2d", 5}, {"tex_storage_3d", 6}].

s058_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s058_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s058_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s058_present_functions(Target)
    ).

s058_assert_path({"generate_mipmap", 1}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s058_assert_direct(
        FunctionData,
        "glGenerateMipmap",
        [{in, "Target", {gl_enum, "TextureTarget"}}],
        [{"Target", {undefined, texture_target, []}}],
        [{"Target", {gl_enum_to_uint, ["texture_2d"]}}],
        [{"Target", s058_enum_nif_data()}]
    );
s058_assert_path({"tex_buffer", 3}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "texture_target", "texture_buffer"),
    s058_assert_enum_contains(BindingData, "sized_internal_format", "r32f"),
    s058_assert_direct(
        FunctionData,
        "glTexBuffer",
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Buffer", {gl_object, buffer}}
        ],
        [
            {"Target", {undefined, texture_target, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Buffer", {undefined, buffer, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["texture_buffer"]}},
            {"InternalFormat", {gl_enum_to_uint, ["r32f"]}},
            {"Buffer", do_nothing}
        ],
        [
            {"Target", s058_enum_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Buffer", s058_uint_nif_data()}
        ]
    );
s058_assert_path({"tex_storage_1d", 4}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "texture_target", "texture_1d"),
    s058_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s058_assert_direct(
        FunctionData,
        "glTexStorage1D",
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Levels", gl_sizei},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Width", gl_sizei}
        ],
        [
            {"Target", {undefined, texture_target, []}},
            {"Levels", {gl, sizei, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Width", {gl, sizei, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["texture_1d"]}},
            {"Levels", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}},
            {"Width", do_nothing}
        ],
        [
            {"Target", s058_enum_nif_data()},
            {"Levels", s058_sizei_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Width", s058_sizei_nif_data()}
        ]
    );
s058_assert_path({"tex_storage_2d", 5}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s058_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s058_assert_direct(
        FunctionData,
        "glTexStorage2D",
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Levels", gl_sizei},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Target", {undefined, texture_target, []}},
            {"Levels", {gl, sizei, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["texture_2d"]}},
            {"Levels", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        [
            {"Target", s058_enum_nif_data()},
            {"Levels", s058_sizei_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Width", s058_sizei_nif_data()},
            {"Height", s058_sizei_nif_data()}
        ]
    );
s058_assert_path({"tex_storage_3d", 6}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "texture_target", "texture_3d"),
    s058_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s058_assert_direct(
        FunctionData,
        "glTexStorage3D",
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Levels", gl_sizei},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei},
            {in, "Depth", gl_sizei}
        ],
        [
            {"Target", {undefined, texture_target, []}},
            {"Levels", {gl, sizei, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}},
            {"Depth", {gl, sizei, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["texture_3d"]}},
            {"Levels", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}},
            {"Width", do_nothing},
            {"Height", do_nothing},
            {"Depth", do_nothing}
        ],
        [
            {"Target", s058_enum_nif_data()},
            {"Levels", s058_sizei_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Width", s058_sizei_nif_data()},
            {"Height", s058_sizei_nif_data()},
            {"Depth", s058_sizei_nif_data()}
        ]
    );
s058_assert_path({"tex_buffer_range", 5}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "texture_target", "texture_buffer"),
    s058_assert_enum_contains(BindingData, "sized_internal_format", "r32f"),
    s058_assert_direct(
        FunctionData,
        "glTexBufferRange",
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Size", gl_sizeiptr}
        ],
        [
            {"Target", {undefined, texture_target, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Size", {gl, sizeiptr, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["texture_buffer"]}},
            {"InternalFormat", {gl_enum_to_uint, ["r32f"]}},
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"Target", s058_enum_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Buffer", s058_uint_nif_data()},
            {"Offset", s058_intptr_nif_data()},
            {"Size", s058_sizeiptr_nif_data()}
        ]
    );
s058_assert_path({"texture_buffer", 3}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "sized_internal_format", "r32f"),
    s058_assert_texture_buffer_direct(FunctionData, "glTextureBuffer", "r32f");
s058_assert_path({"texture_buffer_range", 5}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "sized_internal_format", "r32f"),
    s058_assert_direct(
        FunctionData,
        "glTextureBufferRange",
        [
            {in, "Texture", {gl_object, texture}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Size", gl_sizeiptr}
        ],
        [
            {"Texture", {undefined, texture, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Size", {gl, sizeiptr, []}}
        ],
        [
            {"Texture", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["r32f"]}},
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"Texture", s058_uint_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Buffer", s058_uint_nif_data()},
            {"Offset", s058_intptr_nif_data()},
            {"Size", s058_sizeiptr_nif_data()}
        ]
    );
s058_assert_path({"texture_storage_1d", 4}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s058_assert_texture_storage_direct(FunctionData, "glTextureStorage1D", [{"Width", gl_sizei}]);
s058_assert_path({"texture_storage_2d", 5}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s058_assert_texture_storage_direct(FunctionData, "glTextureStorage2D", [{"Width", gl_sizei}, {"Height", gl_sizei}]);
s058_assert_path({"texture_storage_3d", 6}, _Target, BindingData, FunctionData) ->
    s058_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s058_assert_texture_storage_direct(
        FunctionData,
        "glTextureStorage3D",
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    );
s058_assert_path({"generate_texture_mipmap", 1}, _Target, _BindingData, FunctionData) ->
    s058_assert_direct(
        FunctionData,
        "glGenerateTextureMipmap",
        [{in, "Texture", {gl_object, texture}}],
        [{"Texture", {undefined, texture, []}}],
        [{"Texture", do_nothing}],
        [{"Texture", s058_uint_nif_data()}]
    );
s058_assert_path({"bind_texture_unit", 2}, _Target, _BindingData, FunctionData) ->
    s058_assert_direct(
        FunctionData,
        "glBindTextureUnit",
        [
            {in, "Unit", gl_uint},
            {in, "Texture", {gl_object, texture}}
        ],
        [
            {"Unit", {gl, uint, []}},
            {"Texture", {undefined, texture, []}}
        ],
        [
            {"Unit", do_nothing},
            {"Texture", do_nothing}
        ],
        [
            {"Unit", s058_uint_nif_data()},
            {"Texture", s058_uint_nif_data()}
        ]
    ).

s058_assert_texture_buffer_direct(FunctionData, GlCommand, RequiredFormat) ->
    s058_assert_direct(
        FunctionData,
        GlCommand,
        [
            {in, "Texture", {gl_object, texture}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Buffer", {gl_object, buffer}}
        ],
        [
            {"Texture", {undefined, texture, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"Buffer", {undefined, buffer, []}}
        ],
        [
            {"Texture", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, [RequiredFormat]}},
            {"Buffer", do_nothing}
        ],
        [
            {"Texture", s058_uint_nif_data()},
            {"InternalFormat", s058_enum_nif_data()},
            {"Buffer", s058_uint_nif_data()}
        ]
    ).

s058_assert_texture_storage_direct(FunctionData, GlCommand, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Levels", gl_sizei},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}}
        ] ++ [{in, Name, Type} || {Name, Type} <- DimensionParams],
    SpecsParams =
        [
            {"Texture", {undefined, texture, []}},
            {"Levels", {gl, sizei, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}}
        ] ++ [{Name, {gl, sizei, []}} || {Name, _Type} <- DimensionParams],
    ClauseParams =
        [
            {"Texture", do_nothing},
            {"Levels", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}}
        ] ++ [{Name, do_nothing} || {Name, _Type} <- DimensionParams],
    NifParams =
        [
            {"Texture", s058_uint_nif_data()},
            {"Levels", s058_sizei_nif_data()},
            {"InternalFormat", s058_enum_nif_data()}
        ] ++ [{Name, s058_sizei_nif_data()} || {Name, _Type} <- DimensionParams],
    s058_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s058_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s058_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s058_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s058_assert_clause_param/1, lists:zip(Expected, Actual)).

s058_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s058_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s058_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s058_assert_emitted_surface(Target, ApiName, ExpectedExports, ExpectedCalls) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard58-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s058_deferred_exports()],
        [?assertEqual(nomatch, binary:match(C, Call)) || Call <- s058_deferred_c_calls()],
        ?assertMatch({_, _}, binary:match(Erl, <<"sized_internal_format()">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s058_gl_46_exports() ->
    [
        <<"-export([generate_mipmap/1]).">>,
        <<"-export([tex_buffer/3]).">>,
        <<"-export([tex_storage_1d/4]).">>,
        <<"-export([tex_storage_2d/5]).">>,
        <<"-export([tex_storage_3d/6]).">>,
        <<"-export([tex_buffer_range/5]).">>,
        <<"-export([texture_buffer/3]).">>,
        <<"-export([texture_buffer_range/5]).">>,
        <<"-export([texture_storage_1d/4]).">>,
        <<"-export([texture_storage_2d/5]).">>,
        <<"-export([texture_storage_3d/6]).">>,
        <<"-export([generate_texture_mipmap/1]).">>,
        <<"-export([bind_texture_unit/2]).">>
    ].

s058_gles_32_exports() ->
    [
        <<"-export([generate_mipmap/1]).">>,
        <<"-export([tex_buffer/3]).">>,
        <<"-export([tex_storage_2d/5]).">>,
        <<"-export([tex_storage_3d/6]).">>,
        <<"-export([tex_buffer_range/5]).">>
    ].

s058_gl_46_c_calls() ->
    [
        <<"glGenerateMipmap(arg_0);">>,
        <<"glTexBuffer(arg_0, arg_1, arg_2);">>,
        <<"glTexStorage1D(arg_0, arg_1, arg_2, arg_3);">>,
        <<"glTexStorage2D(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glTexStorage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
        <<"glTexBufferRange(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glTextureBuffer(arg_0, arg_1, arg_2);">>,
        <<"glTextureBufferRange(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glTextureStorage1D(arg_0, arg_1, arg_2, arg_3);">>,
        <<"glTextureStorage2D(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glTextureStorage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
        <<"glGenerateTextureMipmap(arg_0);">>,
        <<"glBindTextureUnit(arg_0, arg_1);">>
    ].

s058_gles_32_c_calls() ->
    [
        <<"glGenerateMipmap(arg_0);">>,
        <<"glTexBuffer(arg_0, arg_1, arg_2);">>,
        <<"glTexStorage2D(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glTexStorage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
        <<"glTexBufferRange(arg_0, arg_1, arg_2, arg_3, arg_4);">>
    ].

s058_deferred_exports() ->
    [
    ].

s058_deferred_c_calls() ->
    [
    ].

s058_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s058_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s058_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s058_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

s058_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 60.
s060_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s060_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s060_emitter_texture_copy_surface_test_() ->
    [
        {"gl 4.6", fun() -> s060_assert_emitted_surface({gl, {4, 6}}, "OpenGL 4.6", s060_gl_46_exports(), s060_gl_46_c_calls()) end},
        {"gles 3.2", fun() ->
            s060_assert_emitted_surface({gles, {3, 2}}, "OpenGL ES 3.2", s060_gles_32_exports(), s060_gles_32_c_calls())
        end}
    ].

s060_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s060_assert_presence(Target, Functions),
    s060_assert_deferred_neighbors_absent(Functions),
    s060_assert_present_paths(Target, BindingData, Functions).

s060_assert_presence(Target, Functions) ->
    Present = s060_present_functions(Target),
    Absent = s060_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s060_present_functions({gl, {3, 3}}) ->
    s060_target_bound_copy();
s060_present_functions({gl, {4, 1}}) ->
    s060_target_bound_copy();
s060_present_functions({gl, {4, 6}}) ->
    s060_all_functions();
s060_present_functions({gles, {2, 0}}) ->
    s060_es2_copy();
s060_present_functions({gles, {3, 0}}) ->
    s060_es3_copy();
s060_present_functions({gles, {3, 1}}) ->
    s060_es3_copy();
s060_present_functions({gles, {3, 2}}) ->
    s060_es3_copy().

s060_all_functions() ->
    s060_target_bound_copy() ++ s060_dsa_copy().

s060_target_bound_copy() ->
    [
        {"copy_tex_image_1d", 7},
        {"copy_tex_image_2d", 8},
        {"copy_tex_sub_image_1d", 6},
        {"copy_tex_sub_image_2d", 8},
        {"copy_tex_sub_image_3d", 9}
    ].

s060_es2_copy() ->
    [
        {"copy_tex_image_2d", 8},
        {"copy_tex_sub_image_2d", 8}
    ].

s060_es3_copy() ->
    s060_es2_copy() ++ [{"copy_tex_sub_image_3d", 9}].

s060_dsa_copy() ->
    [
        {"copy_texture_sub_image_1d", 6},
        {"copy_texture_sub_image_2d", 8},
        {"copy_texture_sub_image_3d", 9}
    ].

s060_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glMapBuffer",
        "glMapBufferRange",
        "glGetBufferPointerv"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s060_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s060_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s060_present_functions(Target)
    ).

s060_assert_path({"copy_tex_image_1d", 7}, _Target, BindingData, FunctionData) ->
    s060_assert_enum_contains(BindingData, "texture_target", "texture_1d"),
    s060_assert_enum_contains(BindingData, "internal_format", "rgba"),
    s060_assert_copy_tex_image_direct(FunctionData, "glCopyTexImage1D", "texture_1d", [{"Width", gl_sizei}]);
s060_assert_path({"copy_tex_image_2d", 8}, _Target, BindingData, FunctionData) ->
    s060_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s060_assert_enum_contains(BindingData, "internal_format", "rgba"),
    s060_assert_copy_tex_image_direct(
        FunctionData,
        "glCopyTexImage2D",
        "texture_2d",
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s060_assert_path({"copy_tex_sub_image_1d", 6}, _Target, BindingData, FunctionData) ->
    s060_assert_enum_contains(BindingData, "texture_target", "texture_1d"),
    s060_assert_copy_tex_sub_image_direct(
        FunctionData,
        "glCopyTexSubImage1D",
        "texture_1d",
        [{"Offset", gl_int}],
        [{"Width", gl_sizei}]
    );
s060_assert_path({"copy_tex_sub_image_2d", 8}, _Target, BindingData, FunctionData) ->
    s060_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s060_assert_copy_tex_sub_image_direct(
        FunctionData,
        "glCopyTexSubImage2D",
        "texture_2d",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s060_assert_path({"copy_tex_sub_image_3d", 9}, Target, BindingData, FunctionData) ->
    TextureTarget =
        case Target of
            {gl, _} -> "texture_3d";
            {gles, _} -> "texture_3d"
        end,
    s060_assert_enum_contains(BindingData, "texture_target", TextureTarget),
    s060_assert_copy_tex_sub_image_direct(
        FunctionData,
        "glCopyTexSubImage3D",
        TextureTarget,
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}, {"OffsetZ", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s060_assert_path({"copy_texture_sub_image_1d", 6}, _Target, _BindingData, FunctionData) ->
    s060_assert_copy_texture_sub_image_direct(
        FunctionData,
        "glCopyTextureSubImage1D",
        [{"Offset", gl_int}],
        [{"Width", gl_sizei}]
    );
s060_assert_path({"copy_texture_sub_image_2d", 8}, _Target, _BindingData, FunctionData) ->
    s060_assert_copy_texture_sub_image_direct(
        FunctionData,
        "glCopyTextureSubImage2D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s060_assert_path({"copy_texture_sub_image_3d", 9}, _Target, _BindingData, FunctionData) ->
    s060_assert_copy_texture_sub_image_direct(
        FunctionData,
        "glCopyTextureSubImage3D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}, {"OffsetZ", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    ).

s060_assert_copy_tex_image_direct(FunctionData, GlCommand, TargetAtom, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}},
            {in, "X", gl_int},
            {in, "Y", gl_int}
        ] ++ [{in, Name, Type} || {Name, Type} <- DimensionParams] ++ [{in, "Border", gl_int}],
    SpecsParams =
        [
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}},
            {"InternalFormat", {undefined, internal_format, []}},
            {"X", {gl, int, []}},
            {"Y", {gl, int, []}}
        ] ++ s060_spec_params(DimensionParams) ++ [{"Border", {gl, int, []}}],
    ClauseParams =
        [
            {"Target", {gl_enum_to_uint, [TargetAtom]}},
            {"Level", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba"]}},
            {"X", do_nothing},
            {"Y", do_nothing}
        ] ++ s060_plain_clause_params(DimensionParams) ++ [{"Border", do_nothing}],
    NifParams =
        [
            {"Target", s060_enum_nif_data()},
            {"Level", s060_int_nif_data()},
            {"InternalFormat", s060_enum_nif_data()},
            {"X", s060_int_nif_data()},
            {"Y", s060_int_nif_data()}
        ] ++ s060_nif_params(DimensionParams) ++ [{"Border", s060_int_nif_data()}],
    s060_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s060_assert_copy_tex_sub_image_direct(FunctionData, GlCommand, TargetAtom, OffsetParams, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int}
        ] ++ [{in, Name, Type} || {Name, Type} <- OffsetParams] ++
            [
                {in, "X", gl_int},
                {in, "Y", gl_int}
            ] ++ [{in, Name, Type} || {Name, Type} <- DimensionParams],
    SpecsParams =
        [
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}}
        ] ++ s060_spec_params(OffsetParams) ++
            [
                {"X", {gl, int, []}},
                {"Y", {gl, int, []}}
            ] ++ s060_spec_params(DimensionParams),
    ClauseParams =
        [
            {"Target", {gl_enum_to_uint, [TargetAtom]}},
            {"Level", do_nothing}
        ] ++ s060_plain_clause_params(OffsetParams) ++
            [
                {"X", do_nothing},
                {"Y", do_nothing}
            ] ++ s060_plain_clause_params(DimensionParams),
    NifParams =
        [
            {"Target", s060_enum_nif_data()},
            {"Level", s060_int_nif_data()}
        ] ++ s060_nif_params(OffsetParams) ++
            [
                {"X", s060_int_nif_data()},
                {"Y", s060_int_nif_data()}
            ] ++ s060_nif_params(DimensionParams),
    s060_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s060_assert_copy_texture_sub_image_direct(FunctionData, GlCommand, OffsetParams, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int}
        ] ++ [{in, Name, Type} || {Name, Type} <- OffsetParams] ++
            [
                {in, "X", gl_int},
                {in, "Y", gl_int}
            ] ++ [{in, Name, Type} || {Name, Type} <- DimensionParams],
    SpecsParams =
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}}
        ] ++ s060_spec_params(OffsetParams) ++
            [
                {"X", {gl, int, []}},
                {"Y", {gl, int, []}}
            ] ++ s060_spec_params(DimensionParams),
    ClauseParams =
        [
            {"Texture", do_nothing},
            {"Level", do_nothing}
        ] ++ s060_plain_clause_params(OffsetParams) ++
            [
                {"X", do_nothing},
                {"Y", do_nothing}
            ] ++ s060_plain_clause_params(DimensionParams),
    NifParams =
        [
            {"Texture", s060_uint_nif_data()},
            {"Level", s060_int_nif_data()}
        ] ++ s060_nif_params(OffsetParams) ++
            [
                {"X", s060_int_nif_data()},
                {"Y", s060_int_nif_data()}
            ] ++ s060_nif_params(DimensionParams),
    s060_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s060_spec_params(Params) ->
    [{Name, s060_spec_type(Type)} || {Name, Type} <- Params].

s060_spec_type(gl_int) ->
    {gl, int, []};
s060_spec_type(gl_sizei) ->
    {gl, sizei, []}.

s060_plain_clause_params(Params) ->
    [{Name, do_nothing} || {Name, _Type} <- Params].

s060_nif_params(Params) ->
    [{Name, s060_nif_type(Type)} || {Name, Type} <- Params].

s060_nif_type(gl_int) ->
    s060_int_nif_data();
s060_nif_type(gl_sizei) ->
    s060_sizei_nif_data().

s060_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s060_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s060_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s060_assert_clause_param/1, lists:zip(Expected, Actual)).

s060_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s060_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s060_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s060_assert_emitted_surface(Target, ApiName, ExpectedExports, ExpectedCalls) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard60-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s060_deferred_exports()],
        [?assertEqual(nomatch, binary:match(C, Call)) || Call <- s060_deferred_c_calls()],
        ?assertMatch({_, _}, binary:match(Erl, <<"internal_format()">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s060_gl_46_exports() ->
    [
        <<"-export([copy_tex_image_1d/7]).">>,
        <<"-export([copy_tex_image_2d/8]).">>,
        <<"-export([copy_tex_sub_image_1d/6]).">>,
        <<"-export([copy_tex_sub_image_2d/8]).">>,
        <<"-export([copy_tex_sub_image_3d/9]).">>,
        <<"-export([copy_texture_sub_image_1d/6]).">>,
        <<"-export([copy_texture_sub_image_2d/8]).">>,
        <<"-export([copy_texture_sub_image_3d/9]).">>
    ].

s060_gles_32_exports() ->
    [
        <<"-export([copy_tex_image_2d/8]).">>,
        <<"-export([copy_tex_sub_image_2d/8]).">>,
        <<"-export([copy_tex_sub_image_3d/9]).">>
    ].

s060_gl_46_c_calls() ->
    [
        <<"glCopyTexImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>,
        <<"glCopyTexImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
        <<"glCopyTexSubImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
        <<"glCopyTexSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
        <<"glCopyTexSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8);">>,
        <<"glCopyTextureSubImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
        <<"glCopyTextureSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
        <<"glCopyTextureSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8);">>
    ].

s060_gles_32_c_calls() ->
    [
        <<"glCopyTexImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
        <<"glCopyTexSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
        <<"glCopyTexSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8);">>
    ].

s060_deferred_exports() ->
    [
        <<"-export([map_buffer/2]).">>
    ].

s060_deferred_c_calls() ->
    [
        <<"glMapBuffer(">>
    ].

s060_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s060_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s060_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s060_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 63.
s063_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s063_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s063_emitter_vertex_format_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s063_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                s063_gl_46_exports(),
                s063_gl_46_c_calls(),
                []
            )
        end},
        {"gles 3.2", fun() ->
            s063_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                s063_gles_32_exports(),
                s063_gles_32_c_calls(),
                s063_dsa_and_double_exports()
            )
        end},
        {"gl 4.1", fun() ->
            s063_assert_emitted_surface(
                {gl, {4, 1}},
                "OpenGL 4.1",
                [],
                [],
                s063_all_functions()
            )
        end}
    ].

s063_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s063_assert_presence(Target, Functions),
    s063_assert_deferred_neighbors_absent(Functions),
    s063_assert_present_paths(Target, BindingData, Functions).

s063_assert_presence(Target, Functions) ->
    Present = s063_present_functions(Target),
    Absent = s063_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s063_present_functions({gl, {3, 3}}) ->
    [];
s063_present_functions({gl, {4, 1}}) ->
    [];
s063_present_functions({gl, {4, 6}}) ->
    s063_all_functions();
s063_present_functions({gles, {2, 0}}) ->
    [];
s063_present_functions({gles, {3, 0}}) ->
    [];
s063_present_functions({gles, {3, 1}}) ->
    s063_es_vertex_formats();
s063_present_functions({gles, {3, 2}}) ->
    s063_es_vertex_formats().

s063_all_functions() ->
    s063_es_vertex_formats() ++ s063_desktop_double_formats() ++ s063_dsa_formats().

s063_es_vertex_formats() ->
    [
        {"vertex_attrib_format", 5},
        {"vertex_attrib_i_format", 4}
    ].

s063_desktop_double_formats() ->
    [
        {"vertex_attrib_l_format", 4}
    ].

s063_dsa_formats() ->
    [
        {"vertex_array_attrib_format", 6},
        {"vertex_array_attrib_i_format", 5},
        {"vertex_array_attrib_l_format", 5}
    ].

s063_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s063_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s063_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s063_present_functions(Target)
    ).

s063_assert_path({"vertex_attrib_format", 5}, _Target, BindingData, FunctionData) ->
    s063_assert_enum_contains(BindingData, "vertex_attrib_type", "float"),
    s063_assert_vertex_attrib_format(
        FunctionData,
        "glVertexAttribFormat",
        [
            {in, "AttribIndex", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribType"}},
            {in, "Normalized", gl_bool},
            {in, "RelativeOffset", gl_uint}
        ],
        [
            {"AttribIndex", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_type, []}},
            {"Normalized", {gl, boolean, []}},
            {"RelativeOffset", {gl, uint, []}}
        ],
        [
            {"AttribIndex", do_nothing},
            {"Size", do_nothing},
            {"Type", {gl_enum_to_uint, ["float"]}},
            {"Normalized", do_nothing},
            {"RelativeOffset", do_nothing}
        ],
        [
            {"AttribIndex", s063_uint_nif_data()},
            {"Size", s063_int_nif_data()},
            {"Type", s063_enum_nif_data()},
            {"Normalized", boolean_to_glbool},
            {"RelativeOffset", s063_uint_nif_data()}
        ]
    );
s063_assert_path({"vertex_attrib_i_format", 4}, _Target, BindingData, FunctionData) ->
    s063_assert_enum_contains(BindingData, "vertex_attrib_i_type", "int"),
    s063_assert_vertex_attrib_format(
        FunctionData,
        "glVertexAttribIFormat",
        [
            {in, "AttribIndex", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribIType"}},
            {in, "RelativeOffset", gl_uint}
        ],
        [
            {"AttribIndex", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_i_type, []}},
            {"RelativeOffset", {gl, uint, []}}
        ],
        [
            {"AttribIndex", do_nothing},
            {"Size", do_nothing},
            {"Type", {gl_enum_to_uint, ["int"]}},
            {"RelativeOffset", do_nothing}
        ],
        [
            {"AttribIndex", s063_uint_nif_data()},
            {"Size", s063_int_nif_data()},
            {"Type", s063_enum_nif_data()},
            {"RelativeOffset", s063_uint_nif_data()}
        ]
    );
s063_assert_path({"vertex_attrib_l_format", 4}, _Target, BindingData, FunctionData) ->
    s063_assert_enum_contains(BindingData, "vertex_attrib_l_type", "double"),
    s063_assert_vertex_attrib_format(
        FunctionData,
        "glVertexAttribLFormat",
        [
            {in, "AttribIndex", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribLType"}},
            {in, "RelativeOffset", gl_uint}
        ],
        [
            {"AttribIndex", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_l_type, []}},
            {"RelativeOffset", {gl, uint, []}}
        ],
        [
            {"AttribIndex", do_nothing},
            {"Size", do_nothing},
            {"Type", {gl_enum_to_uint, ["double"]}},
            {"RelativeOffset", do_nothing}
        ],
        [
            {"AttribIndex", s063_uint_nif_data()},
            {"Size", s063_int_nif_data()},
            {"Type", s063_enum_nif_data()},
            {"RelativeOffset", s063_uint_nif_data()}
        ]
    );
s063_assert_path({"vertex_array_attrib_format", 6}, _Target, BindingData, FunctionData) ->
    s063_assert_enum_contains(BindingData, "vertex_attrib_type", "float"),
    s063_assert_vertex_attrib_format(
        FunctionData,
        "glVertexArrayAttribFormat",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "AttribIndex", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribType"}},
            {in, "Normalized", gl_bool},
            {in, "RelativeOffset", gl_uint}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"AttribIndex", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_type, []}},
            {"Normalized", {gl, boolean, []}},
            {"RelativeOffset", {gl, uint, []}}
        ],
        [
            {"Array", do_nothing},
            {"AttribIndex", do_nothing},
            {"Size", do_nothing},
            {"Type", {gl_enum_to_uint, ["float"]}},
            {"Normalized", do_nothing},
            {"RelativeOffset", do_nothing}
        ],
        [
            {"Array", s063_uint_nif_data()},
            {"AttribIndex", s063_uint_nif_data()},
            {"Size", s063_int_nif_data()},
            {"Type", s063_enum_nif_data()},
            {"Normalized", boolean_to_glbool},
            {"RelativeOffset", s063_uint_nif_data()}
        ]
    );
s063_assert_path({"vertex_array_attrib_i_format", 5}, _Target, BindingData, FunctionData) ->
    s063_assert_enum_contains(BindingData, "vertex_attrib_i_type", "int"),
    s063_assert_vertex_attrib_format(
        FunctionData,
        "glVertexArrayAttribIFormat",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "AttribIndex", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribIType"}},
            {in, "RelativeOffset", gl_uint}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"AttribIndex", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_i_type, []}},
            {"RelativeOffset", {gl, uint, []}}
        ],
        [
            {"Array", do_nothing},
            {"AttribIndex", do_nothing},
            {"Size", do_nothing},
            {"Type", {gl_enum_to_uint, ["int"]}},
            {"RelativeOffset", do_nothing}
        ],
        [
            {"Array", s063_uint_nif_data()},
            {"AttribIndex", s063_uint_nif_data()},
            {"Size", s063_int_nif_data()},
            {"Type", s063_enum_nif_data()},
            {"RelativeOffset", s063_uint_nif_data()}
        ]
    );
s063_assert_path({"vertex_array_attrib_l_format", 5}, _Target, BindingData, FunctionData) ->
    s063_assert_enum_contains(BindingData, "vertex_attrib_l_type", "double"),
    s063_assert_vertex_attrib_format(
        FunctionData,
        "glVertexArrayAttribLFormat",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "AttribIndex", gl_uint},
            {in, "Size", gl_int},
            {in, "Type", {gl_enum, "VertexAttribLType"}},
            {in, "RelativeOffset", gl_uint}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"AttribIndex", {gl, uint, []}},
            {"Size", {gl, int, []}},
            {"Type", {undefined, vertex_attrib_l_type, []}},
            {"RelativeOffset", {gl, uint, []}}
        ],
        [
            {"Array", do_nothing},
            {"AttribIndex", do_nothing},
            {"Size", do_nothing},
            {"Type", {gl_enum_to_uint, ["double"]}},
            {"RelativeOffset", do_nothing}
        ],
        [
            {"Array", s063_uint_nif_data()},
            {"AttribIndex", s063_uint_nif_data()},
            {"Size", s063_int_nif_data()},
            {"Type", s063_enum_nif_data()},
            {"RelativeOffset", s063_uint_nif_data()}
        ]
    ).

s063_assert_vertex_attrib_format(
    FunctionData,
    GlCommand,
    ParamsSpecs,
    SpecsParams,
    ClauseParams,
    NifParams
) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s063_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s063_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s063_assert_clause_param/1, lists:zip(Expected, Actual)).

s063_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s063_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s063_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s063_assert_emitted_surface(Target, ApiName, ExpectedExports, ExpectedCalls, ExtraAbsentFunctions) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard63-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
            ?assertEqual(nomatch, binary:match(Erl, s063_export_binary(Function, Arity)))
         || {Function, Arity} <- ExtraAbsentFunctions
        ],
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s063_deferred_exports()],
        [?assertEqual(nomatch, binary:match(C, Call)) || Call <- s063_deferred_c_calls()]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s063_gl_46_exports() ->
    [
        <<"-export([vertex_attrib_format/5]).">>,
        <<"-export([vertex_attrib_i_format/4]).">>,
        <<"-export([vertex_attrib_l_format/4]).">>,
        <<"-export([vertex_array_attrib_format/6]).">>,
        <<"-export([vertex_array_attrib_i_format/5]).">>,
        <<"-export([vertex_array_attrib_l_format/5]).">>
    ].

s063_gles_32_exports() ->
    [
        <<"-export([vertex_attrib_format/5]).">>,
        <<"-export([vertex_attrib_i_format/4]).">>
    ].

s063_gl_46_c_calls() ->
    [
        <<"glVertexAttribFormat(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glVertexAttribIFormat(arg_0, arg_1, arg_2, arg_3);">>,
        <<"glVertexAttribLFormat(arg_0, arg_1, arg_2, arg_3);">>,
        <<"glVertexArrayAttribFormat(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
        <<"glVertexArrayAttribIFormat(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glVertexArrayAttribLFormat(arg_0, arg_1, arg_2, arg_3, arg_4);">>
    ].

s063_gles_32_c_calls() ->
    [
        <<"glVertexAttribFormat(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glVertexAttribIFormat(arg_0, arg_1, arg_2, arg_3);">>
    ].

s063_dsa_and_double_exports() ->
    [
        {"vertex_attrib_l_format", 4},
        {"vertex_array_attrib_format", 6},
        {"vertex_array_attrib_i_format", 5},
        {"vertex_array_attrib_l_format", 5}
    ].

s063_deferred_exports() ->
    [
    ].

s063_deferred_c_calls() ->
    [
    ].

s063_export_binary(Function, Arity) ->
    unicode:characters_to_binary(
        lists:flatten(io_lib:format("-export([~s/~p]).", [Function, Arity]))
    ).

s063_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s063_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s063_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 65.
s065_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s065_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s065_emitter_generic_texture_parameter_setters_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard65-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),

        s065_assert_contains(GeneratedErl, <<"-export([tex_parameter/4]).">>),
        s065_assert_not_contains(GeneratedErl, <<"-export([tex_parameter/3]).">>),
        s065_assert_contains(GeneratedErl, <<"-type tex_parameter_value() ::">>),
        s065_assert_contains(GeneratedErl, <<"gl:float()">>),
        s065_assert_contains(GeneratedErl, <<"[gl:float()]">>),
        s065_assert_contains(GeneratedErl, <<"[gl:int()]">>),
        s065_assert_contains(GeneratedErl, <<"-spec tex_parameter(\n    Type :: f | i,">>),
        s065_assert_contains(GeneratedErl, <<"tex_parameter(i, Target, ParamName, Param) when is_list(Param) ->">>),
        s065_assert_contains(GeneratedErl, <<"tex_parameter(f, Target, ParamName, Param) when is_list(Param) ->">>),
        s065_assert_contains(GeneratedErl, <<"tex_parameter(f, Target, ParamName, Param) ->">>),
        s065_assert_before(
            <<"tex_parameter(f, Target, ParamName, Param) when is_list(Param) ->">>,
            <<"tex_parameter(f, Target, ParamName, Param) ->">>,
            GeneratedErl
        ),
        s065_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glTexParameteriv_raw(NewTarget, NewParamName, Param))">>),
        s065_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glTexParameterfv_raw(NewTarget, NewParamName, Param))">>),
        s065_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glTexParameterf_raw(NewTarget, NewParamName, Param))">>),

        s065_assert_contains(GeneratedC, <<"glTexParameteriv(arg_0, arg_1, arg_2_array);">>),
        s065_assert_contains(GeneratedC, <<"glTexParameterfv(arg_0, arg_1, arg_2_array);">>),
        s065_assert_contains(GeneratedC, <<"glTexParameterf(arg_0, arg_1, arg_2);">>),
        s065_assert_contains(GeneratedC, <<"{\"glTexParameteriv_raw\", 3, nif_glTexParameteriv, 0}">>),
        s065_assert_contains(GeneratedC, <<"{\"glTexParameterfv_raw\", 3, nif_glTexParameterfv, 0}">>),
        s065_assert_contains(GeneratedC, <<"{\"glTexParameterf_raw\", 3, nif_glTexParameterf, 0}">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s065_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"tex_parameter", 4}, Functions)),
    ?assertNot(maps:is_key({"tex_parameter", 3}, Functions)),
    s065_assert_tex_parameter(maps:get({"tex_parameter", 4}, Functions)).

s065_assert_tex_parameter(FunctionData) ->
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Target", {undefined, texture_target, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Param", {undefined, tex_parameter_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    s065_assert_extra_type(FunctionData),
    s065_assert_commands(FunctionData),
    s065_assert_clauses(FunctionData),
    s065_assert_nifs(FunctionData).

s065_assert_extra_type(FunctionData) ->
    {tex_parameter_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {gl, float, []},
        {list, {gl, float, []}},
        {list, {gl, int, []}}
    ]).

s065_assert_commands(FunctionData) ->
    ExpectedCommands = [
        {"glTexParameteriv", gl_int, array},
        {"glTexParameterfv", gl_float, array},
        {"glTexParameterf", gl_float, element}
    ],
    GlCommands = maps:get(gl_commands, FunctionData),
    Variants = maps:get(variants, FunctionData),
    lists:foreach(fun(Command) ->
        ?assert(lists:member(Command, GlCommands))
    end, ExpectedCommands),
    lists:foreach(fun(Variant) ->
        ?assert(lists:member(Variant, Variants))
    end, [
        {gl_int, array},
        {gl_float, array},
        {gl_float, element}
    ]).

s065_assert_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s065_assert_before("glTexParameterfv", "glTexParameterf", RawOrder),
    s065_assert_array_clause("glTexParameteriv", "i", Clauses),
    s065_assert_array_clause("glTexParameterfv", "f", Clauses),
    s065_assert_scalar_clause("glTexParameterf", "f", Clauses).

s065_assert_array_clause(RawFunction, Suffix, Clauses) ->
    Clause = s065_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    s065_assert_clause_params(Suffix, Clause).

s065_assert_scalar_clause(RawFunction, Suffix, Clauses) ->
    Clause = s065_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([], maps:get(guards, Clause)),
    s065_assert_clause_params(Suffix, Clause).

s065_assert_clause_params(Suffix, Clause) ->
    [
        {Suffix, ignore},
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)).

s065_assert_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    Enum = {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}},
    Float = {"GLfloat", "double", "enif_get_double", "enif_make_double"},
    Int = {"GLint", "int", "enif_get_int", "enif_make_int"},
    s065_assert_nif(
        maps:get("glTexParameterf", NifFunctions),
        [{"Target", Enum}, {"ParamName", Enum}, {"Param", {gl_type, Float}}]
    ),
    s065_assert_nif(
        maps:get("glTexParameterfv", NifFunctions),
        [{"Target", Enum}, {"ParamName", Enum}, {"Param", {list_gl_type, Float}}]
    ),
    s065_assert_nif(
        maps:get("glTexParameteriv", NifFunctions),
        [{"Target", Enum}, {"ParamName", Enum}, {"Param", {list_gl_type, Int}}]
    ).

s065_assert_nif(NifData, Params) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s065_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s065_assert_before(First, Second, Values) when is_list(Values) ->
    ?assert(s065_index_of(First, Values) < s065_index_of(Second, Values));
s065_assert_before(First, Second, Binary) when is_binary(Binary) ->
    {FirstStart, _} = binary:match(Binary, First),
    {SecondStart, _} = binary:match(Binary, Second),
    ?assert(FirstStart < SecondStart).

s065_index_of(Value, Values) ->
    s065_index_of(Value, Values, 1).

s065_index_of(Value, [Value | _], Index) ->
    Index;
s065_index_of(Value, [_ | Rest], Index) ->
    s065_index_of(Value, Rest, Index + 1).

s065_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s065_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 66.
s066_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s066_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s066_emitter_vertex_attrib_element_setters_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard66-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, GeneratedErl} = file:read_file("gl.erl"),
        {ok, GeneratedC} = file:read_file("gl.c"),

        s066_assert_contains(GeneratedErl, <<"-export([vertex_attrib/3]).">>),
        s066_assert_not_contains(GeneratedErl, <<"-export([vertex_attrib_1f/2]).">>),
        s066_assert_not_contains(GeneratedErl, <<"-export([vertex_attrib_4f/5]).">>),
        s066_assert_contains(GeneratedErl, <<"-type vertex_attrib_value() ::">>),
        s066_assert_contains(GeneratedErl, <<"gl:double()">>),
        s066_assert_contains(GeneratedErl, <<"vector4(gl:double())">>),
        s066_assert_contains(GeneratedErl, <<"gl:float()">>),
        s066_assert_contains(GeneratedErl, <<"vector4(gl:float())">>),
        s066_assert_contains(GeneratedErl, <<"gl:short()">>),
        s066_assert_contains(GeneratedErl, <<"vector4(gl:short())">>),
        s066_assert_contains(GeneratedErl, <<"-spec vertex_attrib(\n    Type :: ">>),
        s066_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttrib1f_raw(Index, Values))">>),
        s066_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttrib4f_raw(Index, V1, V2, V3, V4))">>),
        s066_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttrib1d_raw(Index, Values))">>),
        s066_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttrib4d_raw(Index, V1, V2, V3, V4))">>),
        s066_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttrib1s_raw(Index, Values))">>),
        s066_assert_contains(GeneratedErl, <<"?CALL_RAW_FUNC(glVertexAttrib4s_raw(Index, V1, V2, V3, V4))">>),

        s066_assert_contains(GeneratedC, <<"glVertexAttrib1f(arg_0, arg_1);">>),
        s066_assert_contains(GeneratedC, <<"glVertexAttrib4f(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
        s066_assert_contains(GeneratedC, <<"glVertexAttrib1d(arg_0, arg_1);">>),
        s066_assert_contains(GeneratedC, <<"glVertexAttrib4d(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
        s066_assert_contains(GeneratedC, <<"glVertexAttrib1s(arg_0, arg_1);">>),
        s066_assert_contains(GeneratedC, <<"glVertexAttrib4s(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
        s066_assert_contains(GeneratedC, <<"{\"glVertexAttrib1f_raw\", 2, nif_glVertexAttrib1f, 0}">>),
        s066_assert_contains(GeneratedC, <<"{\"glVertexAttrib4f_raw\", 5, nif_glVertexAttrib4f, 0}">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s066_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"vertex_attrib", 3}, Functions)),
    s066_assert_direct_wrappers_absent(Functions),
    s066_assert_deferred_neighbors_absent(Functions),

    VertexAttrib = maps:get({"vertex_attrib", 3}, Functions),
    s066_assert_specs(Target, VertexAttrib),
    s066_assert_commands(Target, VertexAttrib),
    s066_assert_clauses(Target, VertexAttrib),
    s066_assert_nifs(Target, VertexAttrib).

s066_assert_direct_wrappers_absent(Functions) ->
    lists:foreach(fun({Name, Arity}) ->
        ?assertNot(maps:is_key({Name, Arity}, Functions))
    end, [
        {"vertex_attrib_1d", 2},
        {"vertex_attrib_1f", 2},
        {"vertex_attrib_1s", 2},
        {"vertex_attrib_2d", 3},
        {"vertex_attrib_2f", 3},
        {"vertex_attrib_2s", 3},
        {"vertex_attrib_3d", 4},
        {"vertex_attrib_3f", 4},
        {"vertex_attrib_3s", 4},
        {"vertex_attrib_4d", 5},
        {"vertex_attrib_4f", 5},
        {"vertex_attrib_4s", 5}
    ]).

s066_assert_deferred_neighbors_absent(Functions) ->
    lists:foreach(fun(Command) ->
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
    end, [
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
        "glVertexAttrib4sv"
    ]).

s066_assert_specs(Target, VertexAttrib) ->
    {vertex_attrib_value, {set, ExtraTypeVariants}} = maps:get(extra_type, VertexAttrib),
    [{"Type", {set, TypeAtoms}}, {"Index", {gl, uint, []}}, {"Values", {undefined, vertex_attrib_value, []}}] =
        maps:get(specs_params, VertexAttrib),
    ?assertEqual([], maps:get(specs_return, VertexAttrib)),
    ?assertEqual(3, maps:get(function_arity, VertexAttrib)),

    s066_assert_type_family(f, {gl, float, []}, TypeAtoms, ExtraTypeVariants),
    case s066_is_desktop(Target) of
        true ->
            s066_assert_type_family(d, {gl, double, []}, TypeAtoms, ExtraTypeVariants),
            s066_assert_type_family(s, {gl, short, []}, TypeAtoms, ExtraTypeVariants);
        false ->
            ?assertNot(lists:member(d, TypeAtoms)),
            ?assertNot(lists:member(s, TypeAtoms))
    end.

s066_assert_type_family(TypeAtom, ScalarSpec, TypeAtoms, ExtraTypeVariants) ->
    ?assert(lists:member(TypeAtom, TypeAtoms)),
    lists:foreach(fun(TypeSpec) ->
        ?assert(lists:member(TypeSpec, ExtraTypeVariants))
    end, [
        ScalarSpec,
        {undefined, vector2, [ScalarSpec]},
        {undefined, vector3, [ScalarSpec]},
        {undefined, vector4, [ScalarSpec]}
    ]).

s066_assert_commands(Target, VertexAttrib) ->
    s066_assert_command_family(f, gl_float, VertexAttrib),
    case s066_is_desktop(Target) of
        true ->
            s066_assert_command_family(d, gl_double, VertexAttrib),
            s066_assert_command_family(s, gl_short, VertexAttrib);
        false ->
            s066_assert_command_absent("glVertexAttrib1d", VertexAttrib),
            s066_assert_command_absent("glVertexAttrib1s", VertexAttrib)
    end.

s066_assert_command_family(TypeAtom, GlType, VertexAttrib) ->
    Suffix = atom_to_list(TypeAtom),
    Expected = [
        {"glVertexAttrib2" ++ Suffix, {gl_vector, 2, GlType}, element},
        {"glVertexAttrib3" ++ Suffix, {gl_vector, 3, GlType}, element},
        {"glVertexAttrib4" ++ Suffix, {gl_vector, 4, GlType}, element},
        {"glVertexAttrib1" ++ Suffix, GlType, element}
    ],
    GlCommands = maps:get(gl_commands, VertexAttrib),
    Variants = maps:get(variants, VertexAttrib),
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

s066_assert_command_absent(Command, VertexAttrib) ->
    ?assertNot(lists:any(fun({GlCommand, _Type, _Form}) ->
        GlCommand =:= Command
    end, maps:get(gl_commands, VertexAttrib))).

s066_assert_clauses(Target, VertexAttrib) ->
    Clauses = maps:get(function_clauses, VertexAttrib),
    s066_assert_clause_family(f, Clauses),
    case s066_is_desktop(Target) of
        true ->
            s066_assert_clause_family(d, Clauses),
            s066_assert_clause_family(s, Clauses);
        false ->
            ok
    end.

s066_assert_clause_family(TypeAtom, Clauses) ->
    Suffix = atom_to_list(TypeAtom),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s066_assert_before("glVertexAttrib2" ++ Suffix, "glVertexAttrib1" ++ Suffix, RawOrder),
    s066_assert_before("glVertexAttrib3" ++ Suffix, "glVertexAttrib1" ++ Suffix, RawOrder),
    s066_assert_before("glVertexAttrib4" ++ Suffix, "glVertexAttrib1" ++ Suffix, RawOrder),
    s066_assert_vector_clause(Suffix, "glVertexAttrib2" ++ Suffix, 2, Clauses),
    s066_assert_vector_clause(Suffix, "glVertexAttrib3" ++ Suffix, 3, Clauses),
    s066_assert_vector_clause(Suffix, "glVertexAttrib4" ++ Suffix, 4, Clauses),
    s066_assert_scalar_clause(Suffix, "glVertexAttrib1" ++ Suffix, Clauses).

s066_assert_vector_clause(TypeAtom, RawFunction, VectorSize, Clauses) ->
    Clause = s066_find_clause(RawFunction, Clauses),
    ?assertEqual("Values", maps:get(guard_var, Clause)),
    ?assertEqual(
        [{is_tuple, var}, {tuple_size, var, VectorSize}],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Index", do_nothing},
            {"Values", {gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s066_assert_scalar_clause(TypeAtom, RawFunction, Clauses) ->
    Clause = s066_find_clause(RawFunction, Clauses),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {TypeAtom, ignore},
            {"Index", do_nothing},
            {"Values", do_nothing}
        ],
        maps:get(params, Clause)
    ).

s066_assert_nifs(Target, VertexAttrib) ->
    s066_assert_nif_family(f, s066_gl_float_spec(), VertexAttrib),
    case s066_is_desktop(Target) of
        true ->
            s066_assert_nif_family(d, s066_gl_double_spec(), VertexAttrib),
            s066_assert_nif_family(s, s066_gl_short_spec(), VertexAttrib);
        false ->
            ok
    end.

s066_assert_nif_family(TypeAtom, ValueSpec, VertexAttrib) ->
    Suffix = atom_to_list(TypeAtom),
    NifFunctions = maps:get(nif_functions, VertexAttrib),
    s066_assert_scalar_nif(maps:get("glVertexAttrib1" ++ Suffix, NifFunctions), ValueSpec),
    s066_assert_vector_nif(maps:get("glVertexAttrib2" ++ Suffix, NifFunctions), 2, ValueSpec),
    s066_assert_vector_nif(maps:get("glVertexAttrib3" ++ Suffix, NifFunctions), 3, ValueSpec),
    s066_assert_vector_nif(maps:get("glVertexAttrib4" ++ Suffix, NifFunctions), 4, ValueSpec).

s066_assert_scalar_nif(NifData, ValueSpec) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Index", {gl_type, s066_gl_uint_spec()}},
            {"Values", {gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s066_assert_vector_nif(NifData, VectorSize, ValueSpec) ->
    ?assertEqual(VectorSize + 1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Index", {gl_type, s066_gl_uint_spec()}}]
            ++ [{"V" ++ integer_to_list(N), {gl_type, ValueSpec}} || N <- lists:seq(1, VectorSize)],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s066_gl_uint_spec() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s066_gl_float_spec() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s066_gl_double_spec() ->
    {"GLdouble", "double", "enif_get_double", "enif_make_double"}.

s066_gl_short_spec() ->
    {"GLshort", "int", "enif_get_int", "enif_make_int"}.

s066_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s066_assert_before(First, Second, Values) ->
    ?assert(s066_index_of(First, Values) < s066_index_of(Second, Values)).

s066_index_of(Value, Values) ->
    s066_index_of(Value, Values, 1).

s066_index_of(Value, [Value | _], Index) ->
    Index;
s066_index_of(Value, [_ | Rest], Index) ->
    s066_index_of(Value, Rest, Index + 1).

s066_is_desktop({gl, _}) ->
    true;
s066_is_desktop({gles, _}) ->
    false.

s066_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s066_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 68.
s068_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s068_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s068_emitter_texture_object_maintenance_test_() ->
    [
        {"gl 4.6", fun() ->
            s068_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([texture_parameter/4]).">>,
                    <<"-export([texture_parameter_i/4]).">>,
                    <<"-export([invalidate_tex_sub_image/8]).">>,
                    <<"-export([invalidate_tex_image/2]).">>,
                    <<"-export([invalidate_buffer_sub_data/3]).">>,
                    <<"-export([invalidate_buffer_data/1]).">>,
                    <<"texture_parameter(f, Texture, ParamName, Param) when is_list(Param) ->">>,
                    <<"texture_parameter(i, Texture, ParamName, Param) ->">>,
                    <<"texture_parameter_i(ui, Texture, ParamName, Param) when is_list(Param) ->">>
                ],
                [
                    <<"glTextureParameterf(arg_0, arg_1, arg_2);">>,
                    <<"glTextureParameterfv(arg_0, arg_1, arg_2_array);">>,
                    <<"glTextureParameteriv(arg_0, arg_1, arg_2_array);">>,
                    <<"glTextureParameteri(arg_0, arg_1, arg_2);">>,
                    <<"glTextureParameterIiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glTextureParameterIuiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glInvalidateTexSubImage(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
                    <<"glInvalidateBufferSubData(arg_0, arg_1, arg_2);">>
                ],
                [
                    <<"-export([texture_parameter/3]).">>,
                    <<"glMapBufferRange">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s068_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [],
                [],
                [
                    <<"-export([texture_parameter/4]).">>,
                    <<"-export([texture_parameter_i/4]).">>,
                    <<"-export([invalidate_tex_image/2]).">>,
                    <<"glTextureParameterf(">>,
                    <<"glInvalidateTexImage(">>
                ]
            )
        end}
    ].

s068_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case Target of
        {gl, {4, 6}} ->
            s068_assert_gl46_surface(BindingData, Functions);
        _ ->
            s068_assert_absent(Functions)
    end.

s068_assert_gl46_surface(BindingData, Functions) ->
    [?assert(maps:is_key(Function, Functions)) || Function <- s068_functions()],
    s068_assert_deferred_neighbors_absent(Functions),
    s068_assert_texture_parameter(BindingData, maps:get({"texture_parameter", 4}, Functions)),
    s068_assert_texture_parameter_i(BindingData, maps:get({"texture_parameter_i", 4}, Functions)),
    s068_assert_invalidate_tex_sub_image(maps:get({"invalidate_tex_sub_image", 8}, Functions)),
    s068_assert_invalidate_tex_image(maps:get({"invalidate_tex_image", 2}, Functions)),
    s068_assert_invalidate_buffer_sub_data(maps:get({"invalidate_buffer_sub_data", 3}, Functions)),
    s068_assert_invalidate_buffer_data(maps:get({"invalidate_buffer_data", 1}, Functions)).

s068_assert_absent(Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- s068_functions()],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- s068_commands()].

s068_functions() ->
    [
        {"texture_parameter", 4},
        {"texture_parameter_i", 4},
        {"invalidate_tex_sub_image", 8},
        {"invalidate_tex_image", 2},
        {"invalidate_buffer_sub_data", 3},
        {"invalidate_buffer_data", 1}
    ].

s068_commands() ->
    [
        "glTextureParameterf",
        "glTextureParameterfv",
        "glTextureParameteriv",
        "glTextureParameteri",
        "glTextureParameterIiv",
        "glTextureParameterIuiv",
        "glInvalidateTexSubImage",
        "glInvalidateTexImage",
        "glInvalidateBufferSubData",
        "glInvalidateBufferData"
    ].

s068_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glMapBufferRange"
        ]
    ].

s068_assert_texture_parameter(BindingData, FunctionData) ->
    s068_assert_enum_contains(BindingData, "texture_parameter_name", "texture_border_color"),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Texture", {undefined, texture, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Param", {undefined, texture_parameter_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s068_assert_texture_parameter_extra_type(FunctionData),
    s068_assert_texture_parameter_commands(FunctionData),
    s068_assert_texture_parameter_clauses(FunctionData),
    s068_assert_texture_parameter_nifs(FunctionData).

s068_assert_texture_parameter_extra_type(FunctionData) ->
    {texture_parameter_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    [?assert(lists:member(Variant, Variants)) || Variant <- [
        {gl, float, []},
        {gl, int, []},
        {list, {gl, float, []}},
        {list, {gl, int, []}}
    ]].

s068_assert_texture_parameter_commands(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    [?assert(lists:member(Command, GlCommands)) || Command <- [
        {"glTextureParameteriv", gl_int, array},
        {"glTextureParameteri", gl_int, element},
        {"glTextureParameterfv", gl_float, array},
        {"glTextureParameterf", gl_float, element}
    ]].

s068_assert_texture_parameter_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s068_assert_before("glTextureParameteriv", "glTextureParameteri", RawOrder),
    s068_assert_before("glTextureParameterfv", "glTextureParameterf", RawOrder),
    s068_assert_parameter_array_clause("glTextureParameteriv", "i", Clauses),
    s068_assert_parameter_scalar_clause("glTextureParameteri", "i", Clauses),
    s068_assert_parameter_array_clause("glTextureParameterfv", "f", Clauses),
    s068_assert_parameter_scalar_clause("glTextureParameterf", "f", Clauses).

s068_assert_texture_parameter_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s068_assert_nif(
        maps:get("glTextureParameterf", NifFunctions),
        [{"Texture", s068_uint_nif_data()}, {"ParamName", s068_enum_nif_data()}, {"Param", {gl_type, s068_float_nif_data()}}]
    ),
    s068_assert_nif(
        maps:get("glTextureParameterfv", NifFunctions),
        [{"Texture", s068_uint_nif_data()}, {"ParamName", s068_enum_nif_data()}, {"Param", {list_gl_type, s068_float_nif_data()}}]
    ),
    s068_assert_nif(
        maps:get("glTextureParameteriv", NifFunctions),
        [{"Texture", s068_uint_nif_data()}, {"ParamName", s068_enum_nif_data()}, {"Param", {list_gl_type, s068_int_raw_nif_data()}}]
    ),
    s068_assert_nif(
        maps:get("glTextureParameteri", NifFunctions),
        [{"Texture", s068_uint_nif_data()}, {"ParamName", s068_enum_nif_data()}, {"Param", {gl_type, s068_int_raw_nif_data()}}]
    ).

s068_assert_texture_parameter_i(BindingData, FunctionData) ->
    s068_assert_enum_contains(BindingData, "texture_parameter_name", "texture_border_color"),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, ui]}},
            {"Texture", {undefined, texture, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Param", {undefined, texture_parameter_i_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s068_assert_texture_parameter_i_extra_type(FunctionData),
    s068_assert_texture_parameter_i_commands(FunctionData),
    s068_assert_texture_parameter_i_clauses(FunctionData),
    s068_assert_texture_parameter_i_nifs(FunctionData).

s068_assert_texture_parameter_i_extra_type(FunctionData) ->
    {texture_parameter_i_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assertEqual([{list, {gl, int, []}}, {list, {gl, uint, []}}], Variants).

s068_assert_texture_parameter_i_commands(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    [?assert(lists:member(Command, GlCommands)) || Command <- [
        {"glTextureParameterIiv", gl_int, array},
        {"glTextureParameterIuiv", gl_uint, array}
    ]].

s068_assert_texture_parameter_i_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    s068_assert_parameter_array_clause("glTextureParameterIiv", "i", Clauses),
    s068_assert_parameter_array_clause("glTextureParameterIuiv", "ui", Clauses).

s068_assert_texture_parameter_i_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s068_assert_nif(
        maps:get("glTextureParameterIiv", NifFunctions),
        [{"Texture", s068_uint_nif_data()}, {"ParamName", s068_enum_nif_data()}, {"Param", {list_gl_type, s068_int_raw_nif_data()}}]
    ),
    s068_assert_nif(
        maps:get("glTextureParameterIuiv", NifFunctions),
        [{"Texture", s068_uint_nif_data()}, {"ParamName", s068_enum_nif_data()}, {"Param", {list_gl_type, s068_uint_raw_nif_data()}}]
    ).

s068_assert_parameter_array_clause(RawFunction, Suffix, Clauses) ->
    Clause = s068_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    s068_assert_parameter_clause_params(Suffix, Clause).

s068_assert_parameter_scalar_clause(RawFunction, Suffix, Clauses) ->
    Clause = s068_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([], maps:get(guards, Clause)),
    s068_assert_parameter_clause_params(Suffix, Clause).

s068_assert_parameter_clause_params(Suffix, Clause) ->
    [
        {Suffix, ignore},
        {"Texture", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_border_color", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_wrap_s", 1, ParamNameTransformMap)).

s068_assert_invalidate_tex_sub_image(FunctionData) ->
    Params = [
        {"Texture", {undefined, texture, []}},
        {"Level", {gl, int, []}},
        {"OffsetX", {gl, int, []}},
        {"OffsetY", {gl, int, []}},
        {"OffsetZ", {gl, int, []}},
        {"Width", {gl, sizei, []}},
        {"Height", {gl, sizei, []}},
        {"Depth", {gl, sizei, []}}
    ],
    s068_assert_direct(
        FunctionData,
        "glInvalidateTexSubImage",
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "OffsetX", gl_int},
            {in, "OffsetY", gl_int},
            {in, "OffsetZ", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei},
            {in, "Depth", gl_sizei}
        ],
        Params,
        [{"Texture", do_nothing}, {"Level", do_nothing}, {"OffsetX", do_nothing}, {"OffsetY", do_nothing},
            {"OffsetZ", do_nothing}, {"Width", do_nothing}, {"Height", do_nothing}, {"Depth", do_nothing}],
        [{"Texture", s068_uint_nif_data()}, {"Level", s068_int_nif_data()}, {"OffsetX", s068_int_nif_data()},
            {"OffsetY", s068_int_nif_data()}, {"OffsetZ", s068_int_nif_data()}, {"Width", s068_sizei_nif_data()},
            {"Height", s068_sizei_nif_data()}, {"Depth", s068_sizei_nif_data()}]
    ).

s068_assert_invalidate_tex_image(FunctionData) ->
    s068_assert_direct(
        FunctionData,
        "glInvalidateTexImage",
        [{in, "Texture", {gl_object, texture}}, {in, "Level", gl_int}],
        [{"Texture", {undefined, texture, []}}, {"Level", {gl, int, []}}],
        [{"Texture", do_nothing}, {"Level", do_nothing}],
        [{"Texture", s068_uint_nif_data()}, {"Level", s068_int_nif_data()}]
    ).

s068_assert_invalidate_buffer_sub_data(FunctionData) ->
    s068_assert_direct(
        FunctionData,
        "glInvalidateBufferSubData",
        [{in, "Buffer", {gl_object, buffer}}, {in, "Offset", gl_intptr}, {in, "Length", gl_sizeiptr}],
        [{"Buffer", {undefined, buffer, []}}, {"Offset", {gl, intptr, []}}, {"Length", {gl, sizeiptr, []}}],
        [{"Buffer", do_nothing}, {"Offset", do_nothing}, {"Length", do_nothing}],
        [{"Buffer", s068_uint_nif_data()}, {"Offset", s068_intptr_nif_data()}, {"Length", s068_sizeiptr_nif_data()}]
    ).

s068_assert_invalidate_buffer_data(FunctionData) ->
    s068_assert_direct(
        FunctionData,
        "glInvalidateBufferData",
        [{in, "Buffer", {gl_object, buffer}}],
        [{"Buffer", {undefined, buffer, []}}],
        [{"Buffer", do_nothing}],
        [{"Buffer", s068_uint_nif_data()}]
    ).

s068_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(ClauseParams, maps:get(params, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s068_assert_nif(NifData, Params) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s068_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s068_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s068_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard68-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s068_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s068_assert_contains(C, Needle) || Needle <- RequiredC],
        [s068_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s068_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s068_assert_before(First, Second, Values) ->
    ?assert(s068_index_of(First, Values) < s068_index_of(Second, Values)).

s068_index_of(Value, Values) ->
    s068_index_of(Value, Values, 1).

s068_index_of(Value, [Value | _], Index) ->
    Index;
s068_index_of(Value, [_ | Rest], Index) ->
    s068_index_of(Value, Rest, Index + 1).

s068_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s068_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s068_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s068_uint_nif_data() ->
    {gl_type, s068_uint_raw_nif_data()}.

s068_int_nif_data() ->
    {gl_type, s068_int_raw_nif_data()}.

s068_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s068_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

s068_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s068_float_nif_data() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s068_int_raw_nif_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s068_uint_raw_nif_data() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

%% Historical shard 70.
s070_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s070_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s070_emitter_target_bound_texture_parameter_i_test_() ->
    [
        {"gl 4.6", fun() ->
            s070_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([tex_parameter_i/4]).">>,
                    <<"-type tex_parameter_i_value() ::">>,
                    <<"[gl:int()]">>,
                    <<"[gl:uint()]">>,
                    <<"-spec tex_parameter_i(\n    Type :: i | ui,">>,
                    <<"tex_parameter_i(i, Target, ParamName, Param) when is_list(Param) ->">>,
                    <<"tex_parameter_i(ui, Target, ParamName, Param) when is_list(Param) ->">>,
                    <<"?CALL_RAW_FUNC(glTexParameterIiv_raw(NewTarget, NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glTexParameterIuiv_raw(NewTarget, NewParamName, Param))">>
                ],
                [
                    <<"glTexParameterIiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glTexParameterIuiv(arg_0, arg_1, arg_2_array);">>,
                    <<"{\"glTexParameterIiv_raw\", 3, nif_glTexParameterIiv, 0}">>,
                    <<"{\"glTexParameterIuiv_raw\", 3, nif_glTexParameterIuiv, 0}">>
                ],
                [
                    <<"-export([tex_parameter_i/3]).">>,
                    <<"-export([get_tex_parameter_i/3]).">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s070_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([tex_parameter_i/4]).">>,
                    <<"texture_border_color">>,
                    <<"glTexParameterIiv_raw">>,
                    <<"glTexParameterIuiv_raw">>
                ],
                [
                    <<"glTexParameterIiv(arg_0, arg_1, arg_2_array);">>,
                    <<"glTexParameterIuiv(arg_0, arg_1, arg_2_array);">>
                ],
                [
                    <<"-export([get_tex_parameter_i/3]).">>
                ]
            )
        end},
        {"gles 3.1", fun() ->
            s070_assert_emitted_surface(
                {gles, {3, 1}},
                "OpenGL ES 3.1",
                [],
                [],
                [
                    <<"-export([tex_parameter_i/4]).">>,
                    <<"glTexParameterIiv">>,
                    <<"glTexParameterIuiv">>
                ]
            )
        end}
    ].

s070_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s070_supports_tex_parameter_i(Target) of
        true ->
            ?assert(maps:is_key({"tex_parameter_i", 4}, Functions)),
            ?assertNot(maps:is_key({"tex_parameter_i", 3}, Functions)),
            s070_assert_tex_parameter_i(BindingData, maps:get({"tex_parameter_i", 4}, Functions));
        false ->
            ?assertNot(maps:is_key({"tex_parameter_i", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glTexParameterIiv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glTexParameterIuiv", Functions))
    end.

s070_supports_tex_parameter_i({gl, _Version}) ->
    true;
s070_supports_tex_parameter_i({gles, {3, 2}}) ->
    true;
s070_supports_tex_parameter_i(_) ->
    false.

s070_assert_tex_parameter_i(BindingData, FunctionData) ->
    s070_assert_enum_contains(BindingData, "texture_parameter_name", "texture_border_color"),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "ParamName", {gl_enum, "TextureParameterName"}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i, ui]}},
            {"Target", {undefined, texture_target, []}},
            {"ParamName", {undefined, texture_parameter_name, []}},
            {"Param", {undefined, tex_parameter_i_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s070_assert_extra_type(FunctionData),
    s070_assert_commands(FunctionData),
    s070_assert_clauses(FunctionData),
    s070_assert_nifs(FunctionData).

s070_assert_extra_type(FunctionData) ->
    {tex_parameter_i_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assertEqual([{list, {gl, int, []}}, {list, {gl, uint, []}}], Variants).

s070_assert_commands(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    ?assert(lists:member({"glTexParameterIiv", gl_int, array}, GlCommands)),
    ?assert(lists:member({"glTexParameterIuiv", gl_uint, array}, GlCommands)),
    ?assertEqual(lists:sort([{gl_int, array}, {gl_uint, array}]), lists:sort(maps:get(variants, FunctionData))).

s070_assert_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    s070_assert_array_clause("glTexParameterIiv", "i", Clauses),
    s070_assert_array_clause("glTexParameterIuiv", "ui", Clauses).

s070_assert_array_clause(RawFunction, Suffix, Clauses) ->
    Clause = s070_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    [
        {Suffix, ignore},
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("texture_border_color", 1, ParamNameTransformMap)).

s070_assert_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s070_assert_nif(
        maps:get("glTexParameterIiv", NifFunctions),
        [{"Target", s070_enum_nif_data()}, {"ParamName", s070_enum_nif_data()}, {"Param", {list_gl_type, s070_int_raw_nif_data()}}]
    ),
    s070_assert_nif(
        maps:get("glTexParameterIuiv", NifFunctions),
        [{"Target", s070_enum_nif_data()}, {"ParamName", s070_enum_nif_data()}, {"Param", {list_gl_type, s070_uint_raw_nif_data()}}]
    ).

s070_assert_nif(NifData, Params) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s070_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s070_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s070_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard70-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s070_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s070_assert_contains(C, Needle) || Needle <- RequiredC],
        [s070_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s070_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s070_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s070_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s070_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s070_int_raw_nif_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s070_uint_raw_nif_data() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

%% Historical shard 73.
s073_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s073_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s073_emitter_multisample_storage_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s073_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([renderbuffer_storage_multisample/5]).">>,
                    <<"-export([tex_image_2d_multisample/6]).">>,
                    <<"-export([tex_image_3d_multisample/7]).">>,
                    <<"-export([named_renderbuffer_storage_multisample/5]).">>,
                    <<"-export([texture_storage_2d_multisample/6]).">>,
                    <<"-export([texture_storage_3d_multisample/7]).">>,
                    <<"-spec renderbuffer_storage_multisample(">>,
                    <<"-spec texture_storage_2d_multisample(">>
                ],
                [
                    <<"glRenderbufferStorageMultisample(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
                    <<"glTexImage2DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
                    <<"glTexImage3DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>,
                    <<"glNamedRenderbufferStorageMultisample(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
                    <<"glTextureStorage2DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
                    <<"glTextureStorage3DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>
                ],
                []
            )
        end},
        {"gles 3.2", fun() ->
            s073_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([renderbuffer_storage_multisample/5]).">>
                ],
                [
                    <<"glRenderbufferStorageMultisample(arg_0, arg_1, arg_2, arg_3, arg_4);">>
                ],
                [
                    <<"-export([tex_image_2d_multisample/6]).">>,
                    <<"-export([tex_image_3d_multisample/7]).">>,
                    <<"-export([named_renderbuffer_storage_multisample/5]).">>,
                    <<"-export([texture_storage_2d_multisample/6]).">>,
                    <<"-export([texture_storage_3d_multisample/7]).">>,
                    <<"glTexImage2DMultisample(">>,
                    <<"glTextureStorage2DMultisample(">>
                ]
            )
        end},
        {"gles 2.0", fun() ->
            s073_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [],
                [],
                [
                    <<"-export([renderbuffer_storage_multisample/5]).">>,
                    <<"-export([tex_image_2d_multisample/6]).">>,
                    <<"glRenderbufferStorageMultisample(">>,
                    <<"glTexImage2DMultisample(">>
                ]
            )
        end}
    ].

s073_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s073_assert_presence(Target, Functions),
    s073_assert_deferred_neighbors_absent(Functions),
    s073_assert_present_paths(Target, BindingData, Functions).

s073_assert_presence(Target, Functions) ->
    Present = s073_present_functions(Target),
    Absent = s073_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s073_present_functions({gl, {4, 6}}) ->
    s073_all_functions();
s073_present_functions({gl, _}) ->
    s073_target_bound_multisample_storage();
s073_present_functions({gles, {2, 0}}) ->
    [];
s073_present_functions({gles, _}) ->
    [{"renderbuffer_storage_multisample", 5}].

s073_all_functions() ->
    s073_target_bound_multisample_storage() ++ s073_dsa_multisample_storage().

s073_target_bound_multisample_storage() ->
    [
        {"renderbuffer_storage_multisample", 5},
        {"tex_image_2d_multisample", 6},
        {"tex_image_3d_multisample", 7}
    ].

s073_dsa_multisample_storage() ->
    [
        {"named_renderbuffer_storage_multisample", 5},
        {"texture_storage_2d_multisample", 6},
        {"texture_storage_3d_multisample", 7}
    ].

s073_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glMapBuffer",
        "glMapNamedBuffer"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s073_assert_present_paths(Target, BindingData, Functions) ->
    [
        s073_assert_path(Function, Target, BindingData, maps:get(Function, Functions))
     || Function <- s073_present_functions(Target)
    ].

s073_assert_path({"renderbuffer_storage_multisample", 5}, _Target, BindingData, FunctionData) ->
    s073_assert_enum_contains(BindingData, "renderbuffer_target", "renderbuffer"),
    s073_assert_enum_contains(BindingData, "internal_format", "rgba4"),
    s073_assert_renderbuffer_storage_multisample(FunctionData, "glRenderbufferStorageMultisample");
s073_assert_path({"tex_image_2d_multisample", 6}, _Target, BindingData, FunctionData) ->
    s073_assert_enum_contains(BindingData, "texture_target", "texture_2d_multisample"),
    s073_assert_enum_contains(BindingData, "internal_format", "rgba8"),
    s073_assert_bound_texture_multisample(FunctionData, "glTexImage2DMultisample", [{"Width", gl_sizei}, {"Height", gl_sizei}]);
s073_assert_path({"tex_image_3d_multisample", 7}, _Target, BindingData, FunctionData) ->
    s073_assert_enum_contains(BindingData, "texture_target", "texture_2d_multisample_array"),
    s073_assert_enum_contains(BindingData, "internal_format", "rgba8"),
    s073_assert_bound_texture_multisample(
        FunctionData,
        "glTexImage3DMultisample",
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    );
s073_assert_path({"named_renderbuffer_storage_multisample", 5}, _Target, BindingData, FunctionData) ->
    s073_assert_enum_contains(BindingData, "internal_format", "rgba4"),
    s073_assert_named_renderbuffer_storage_multisample(FunctionData);
s073_assert_path({"texture_storage_2d_multisample", 6}, _Target, BindingData, FunctionData) ->
    s073_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s073_assert_dsa_texture_multisample(FunctionData, "glTextureStorage2DMultisample", [{"Width", gl_sizei}, {"Height", gl_sizei}]);
s073_assert_path({"texture_storage_3d_multisample", 7}, _Target, BindingData, FunctionData) ->
    s073_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s073_assert_dsa_texture_multisample(
        FunctionData,
        "glTextureStorage3DMultisample",
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    ).

s073_assert_renderbuffer_storage_multisample(FunctionData, GlCommand) ->
    s073_assert_direct(
        FunctionData,
        GlCommand,
        [
            {in, "Target", {gl_enum, "RenderbufferTarget"}},
            {in, "Samples", gl_sizei},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Target", {undefined, renderbuffer_target, []}},
            {"Samples", {gl, sizei, []}},
            {"InternalFormat", {undefined, internal_format, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["renderbuffer"]}},
            {"Samples", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba4"]}},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        [
            {"Target", s073_enum_nif_data()},
            {"Samples", s073_sizei_nif_data()},
            {"InternalFormat", s073_enum_nif_data()},
            {"Width", s073_sizei_nif_data()},
            {"Height", s073_sizei_nif_data()}
        ]
    ).

s073_assert_bound_texture_multisample(FunctionData, GlCommand, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Samples", gl_sizei},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}}
        ] ++
        [{in, Name, Type} || {Name, Type} <- DimensionParams] ++
        [{in, "FixedSampleLocations", gl_bool}],
    SpecsParams =
        [
            {"Target", {undefined, texture_target, []}},
            {"Samples", {gl, sizei, []}},
            {"InternalFormat", {undefined, internal_format, []}}
        ] ++
        [{Name, {gl, sizei, []}} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", {gl, boolean, []}}],
    ClauseParams =
        [
            {"Target", {gl_enum_to_uint, s073_texture_target_atoms(GlCommand)}},
            {"Samples", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}}
        ] ++
        [{Name, do_nothing} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", do_nothing}],
    NifParams =
        [
            {"Target", s073_enum_nif_data()},
            {"Samples", s073_sizei_nif_data()},
            {"InternalFormat", s073_enum_nif_data()}
        ] ++
        [{Name, s073_sizei_nif_data()} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", boolean_to_glbool}],
    s073_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s073_assert_named_renderbuffer_storage_multisample(FunctionData) ->
    s073_assert_direct(
        FunctionData,
        "glNamedRenderbufferStorageMultisample",
        [
            {in, "Renderbuffer", {gl_object, renderbuffer}},
            {in, "Samples", gl_sizei},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Renderbuffer", {undefined, renderbuffer, []}},
            {"Samples", {gl, sizei, []}},
            {"InternalFormat", {undefined, internal_format, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        [
            {"Renderbuffer", do_nothing},
            {"Samples", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba4"]}},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        [
            {"Renderbuffer", s073_uint_nif_data()},
            {"Samples", s073_sizei_nif_data()},
            {"InternalFormat", s073_enum_nif_data()},
            {"Width", s073_sizei_nif_data()},
            {"Height", s073_sizei_nif_data()}
        ]
    ).

s073_assert_dsa_texture_multisample(FunctionData, GlCommand, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Samples", gl_sizei},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}}
        ] ++
        [{in, Name, Type} || {Name, Type} <- DimensionParams] ++
        [{in, "FixedSampleLocations", gl_bool}],
    SpecsParams =
        [
            {"Texture", {undefined, texture, []}},
            {"Samples", {gl, sizei, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}}
        ] ++
        [{Name, {gl, sizei, []}} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", {gl, boolean, []}}],
    ClauseParams =
        [
            {"Texture", do_nothing},
            {"Samples", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}}
        ] ++
        [{Name, do_nothing} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", do_nothing}],
    NifParams =
        [
            {"Texture", s073_uint_nif_data()},
            {"Samples", s073_sizei_nif_data()},
            {"InternalFormat", s073_enum_nif_data()}
        ] ++
        [{Name, s073_sizei_nif_data()} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", boolean_to_glbool}],
    s073_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s073_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    s073_assert_clause_params(ClauseParams, maps:get(params, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(GlCommand, NifFunctions),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)).

s073_assert_clause_params(ExpectedParams, ActualParams) ->
    ?assertEqual(length(ExpectedParams), length(ActualParams)),
    lists:foreach(fun s073_assert_clause_param/1, lists:zip(ExpectedParams, ActualParams)).

s073_assert_clause_param({{Name, {gl_enum_to_uint, ExpectedAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- ExpectedAtoms];
s073_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s073_texture_target_atoms("glTexImage2DMultisample") ->
    ["texture_2d_multisample"];
s073_texture_target_atoms("glTexImage3DMultisample") ->
    ["texture_2d_multisample_array"].

s073_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s073_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard73-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s073_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s073_assert_contains(C, Needle) || Needle <- RequiredC],
        [s073_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s073_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s073_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s073_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s073_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s073_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s073_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 74.
s074_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s074_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s074_emitter_target_bound_multisample_texture_storage_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s074_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([tex_storage_2d_multisample/6]).">>,
                    <<"-export([tex_storage_3d_multisample/7]).">>
                ],
                [
                    <<"glTexStorage2DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
                    <<"glTexStorage3DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>
                ],
                []
            )
        end},
        {"gles 3.2", fun() ->
            s074_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([tex_storage_2d_multisample/6]).">>,
                    <<"-export([tex_storage_3d_multisample/7]).">>
                ],
                [
                    <<"glTexStorage2DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>,
                    <<"glTexStorage3DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>
                ],
                []
            )
        end},
        {"gles 3.1", fun() ->
            s074_assert_emitted_surface(
                {gles, {3, 1}},
                "OpenGL ES 3.1",
                [
                    <<"-export([tex_storage_2d_multisample/6]).">>
                ],
                [
                    <<"glTexStorage2DMultisample(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5);">>
                ],
                [
                    <<"-export([tex_storage_3d_multisample/7]).">>,
                    <<"glTexStorage3DMultisample(">>
                ]
            )
        end},
        {"gles 3.0", fun() ->
            s074_assert_emitted_surface(
                {gles, {3, 0}},
                "OpenGL ES 3.0",
                [],
                [],
                [
                    <<"-export([tex_storage_2d_multisample/6]).">>,
                    <<"-export([tex_storage_3d_multisample/7]).">>,
                    <<"glTexStorage2DMultisample(">>,
                    <<"glTexStorage3DMultisample(">>
                ]
            )
        end}
    ].

s074_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s074_assert_presence(Target, Functions),
    s074_assert_deferred_neighbors_absent(Functions),
    s074_assert_present_paths(Target, BindingData, Functions).

s074_assert_presence(Target, Functions) ->
    Present = s074_present_functions(Target),
    Absent = s074_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s074_present_functions({gl, {4, 6}}) ->
    s074_all_functions();
s074_present_functions({gles, {3, 1}}) ->
    [{"tex_storage_2d_multisample", 6}];
s074_present_functions({gles, {3, 2}}) ->
    s074_all_functions();
s074_present_functions(_) ->
    [].

s074_all_functions() ->
    [
        {"tex_storage_2d_multisample", 6},
        {"tex_storage_3d_multisample", 7}
    ].

s074_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glMapBuffer",
        "glMapNamedBuffer"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s074_assert_present_paths(Target, BindingData, Functions) ->
    [
        s074_assert_path(Function, Target, BindingData, maps:get(Function, Functions))
     || Function <- s074_present_functions(Target)
    ].

s074_assert_path({"tex_storage_2d_multisample", 6}, _Target, BindingData, FunctionData) ->
    s074_assert_enum_contains(BindingData, "texture_target", "texture_2d_multisample"),
    s074_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s074_assert_texture_storage_multisample(
        FunctionData,
        "glTexStorage2DMultisample",
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s074_assert_path({"tex_storage_3d_multisample", 7}, _Target, BindingData, FunctionData) ->
    s074_assert_enum_contains(BindingData, "texture_target", "texture_2d_multisample_array"),
    s074_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s074_assert_texture_storage_multisample(
        FunctionData,
        "glTexStorage3DMultisample",
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    ).

s074_assert_texture_storage_multisample(FunctionData, GlCommand, DimensionParams) ->
    ParamsSpecs =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Samples", gl_sizei},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}}
        ] ++
        [{in, Name, Type} || {Name, Type} <- DimensionParams] ++
        [{in, "FixedSampleLocations", gl_bool}],
    SpecsParams =
        [
            {"Target", {undefined, texture_target, []}},
            {"Samples", {gl, sizei, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}}
        ] ++
        [{Name, {gl, sizei, []}} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", {gl, boolean, []}}],
    ClauseParams =
        [
            {"Target", {gl_enum_to_uint, s074_texture_target_atoms(GlCommand)}},
            {"Samples", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}}
        ] ++
        [{Name, do_nothing} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", do_nothing}],
    NifParams =
        [
            {"Target", s074_enum_nif_data()},
            {"Samples", s074_sizei_nif_data()},
            {"InternalFormat", s074_enum_nif_data()}
        ] ++
        [{Name, s074_sizei_nif_data()} || {Name, _Type} <- DimensionParams] ++
        [{"FixedSampleLocations", boolean_to_glbool}],
    s074_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s074_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    s074_assert_clause_params(ClauseParams, maps:get(params, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(GlCommand, NifFunctions),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)).

s074_assert_clause_params(ExpectedParams, ActualParams) ->
    ?assertEqual(length(ExpectedParams), length(ActualParams)),
    lists:foreach(fun s074_assert_clause_param/1, lists:zip(ExpectedParams, ActualParams)).

s074_assert_clause_param({{Name, {gl_enum_to_uint, ExpectedAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- ExpectedAtoms];
s074_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s074_texture_target_atoms("glTexStorage2DMultisample") ->
    ["texture_2d_multisample"];
s074_texture_target_atoms("glTexStorage3DMultisample") ->
    ["texture_2d_multisample_array"].

s074_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s074_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard74-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s074_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s074_assert_contains(C, Needle) || Needle <- RequiredC],
        [s074_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s074_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s074_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s074_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s074_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s074_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 79.
s079_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s079_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s079_emitter_clamp_and_conditional_render_test_() ->
    [
        {"gl 4.6", fun() ->
            s079_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([clamp_color/2]).">>,
                    <<"-export([begin_conditional_render/2]).">>,
                    <<"-export([end_conditional_render/0]).">>,
                    <<"-export_type([clamp_color_target/0]).">>,
                    <<"-export_type([clamp_color_mode/0]).">>,
                    <<"-export_type([conditional_render_mode/0]).">>,
                    <<"-type clamp_color_target() ::">>,
                    <<"clamp_read_color">>,
                    <<"-type clamp_color_mode() ::">>,
                    <<"fixed_only">>,
                    <<"-type conditional_render_mode() ::">>,
                    <<"query_wait">>,
                    <<"-spec clamp_color(\n    Target :: clamp_color_target(),\n    Clamp :: clamp_color_mode()">>,
                    <<"-spec begin_conditional_render(\n    Query :: query(),\n    Mode :: conditional_render_mode()">>,
                    <<"-spec end_conditional_render() -> ok | {error, atom()}.">>,
                    <<"ok = gl:clamp_color(clamp_read_color, fixed_only).">>,
                    <<"ok = gl:begin_conditional_render(Query, query_wait).">>,
                    <<"ok = gl:end_conditional_render().">>,
                    <<"?CALL_RAW_FUNC(glClampColor_raw(NewTarget, NewClamp))">>,
                    <<"?CALL_RAW_FUNC(glBeginConditionalRender_raw(Query, NewMode))">>,
                    <<"?CALL_RAW_FUNC(glEndConditionalRender_raw())">>
                ],
                [
                    <<"glClampColor(arg_0, arg_1);">>,
                    <<"glBeginConditionalRender(arg_0, arg_1);">>,
                    <<"glEndConditionalRender();">>,
                    <<"{\"glClampColor_raw\", 2, nif_glClampColor, 0}">>,
                    <<"{\"glBeginConditionalRender_raw\", 2, nif_glBeginConditionalRender, 0}">>,
                    <<"{\"glEndConditionalRender_raw\", 0, nif_glEndConditionalRender, 0}">>
                ],
                [
                    <<"clamp_color_target_arb">>,
                    <<"clamp_color_mode_arb">>,
                    <<"begin_conditional_render_nv">>,
                    <<"end_conditional_render_nv">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s079_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                [
                    <<"-export([clamp_color/2]).">>,
                    <<"-export([begin_conditional_render/2]).">>,
                    <<"-export([end_conditional_render/0]).">>,
                    <<"glClampColor_raw">>,
                    <<"glBeginConditionalRender_raw">>,
                    <<"glEndConditionalRender_raw">>
                ]
            )
        end}
    ].

s079_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s079_supports(Target) of
        true ->
            ?assert(maps:is_key({"clamp_color", 2}, Functions)),
            ?assert(maps:is_key({"begin_conditional_render", 2}, Functions)),
            ?assert(maps:is_key({"end_conditional_render", 0}, Functions)),
            s079_assert_enums(BindingData),
            s079_assert_clamp_color(maps:get({"clamp_color", 2}, Functions)),
            s079_assert_begin_conditional_render(maps:get({"begin_conditional_render", 2}, Functions)),
            s079_assert_end_conditional_render(maps:get({"end_conditional_render", 0}, Functions));
        false ->
            ?assertNot(maps:is_key({"clamp_color", 2}, Functions)),
            ?assertNot(maps:is_key({"begin_conditional_render", 2}, Functions)),
            ?assertNot(maps:is_key({"end_conditional_render", 0}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glClampColor", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glBeginConditionalRender", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glEndConditionalRender", Functions))
    end,
    s079_assert_still_deferred(Functions).

s079_supports({gl, _Version}) ->
    true;
s079_supports(_) ->
    false.

s079_assert_enums(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("clamp_color_target", EnumTypes)),
    ?assert(maps:is_key("clamp_color_mode", EnumTypes)),
    ?assert(maps:is_key("conditional_render_mode", EnumTypes)),
    ?assertNot(maps:is_key("clamp_color_target_arb", EnumTypes)),
    ?assertNot(maps:is_key("clamp_color_mode_arb", EnumTypes)),
    ?assert(lists:member("clamp_read_color", maps:get("clamp_color_target", EnumTypes))),
    ?assert(lists:member("fixed_only", maps:get("clamp_color_mode", EnumTypes))),
    ?assert(lists:member("query_wait", maps:get("conditional_render_mode", EnumTypes))).

s079_assert_clamp_color(FunctionData) ->
    ?assertEqual("glClampColor", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "ClampColorTargetARB", clamp_color_target}},
            {in, "Clamp", {gl_enum, "ClampColorModeARB", clamp_color_mode}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, clamp_color_target, []}},
            {"Clamp", {undefined, clamp_color_mode, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glClampColor", maps:get(raw_function, Clause)),
    Params = maps:get(params, Clause),
    s079_assert_enum_param("Target", "clamp_read_color", "GL_CLAMP_READ_COLOR", Params),
    s079_assert_enum_param("Clamp", "fixed_only", "GL_FIXED_ONLY", Params),
    s079_assert_nif_params(FunctionData, "glClampColor", [s079_enum_nif_data(), s079_enum_nif_data()]).

s079_assert_begin_conditional_render(FunctionData) ->
    ?assertEqual("glBeginConditionalRender", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Query", {gl_object, query}},
            {in, "Mode", {gl_enum, "ConditionalRenderMode"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Query", {undefined, query, []}},
            {"Mode", {undefined, conditional_render_mode, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glBeginConditionalRender", maps:get(raw_function, Clause)),
    Params = maps:get(params, Clause),
    ?assert(lists:member({"Query", do_nothing}, Params)),
    s079_assert_enum_param("Mode", "query_wait", "GL_QUERY_WAIT", Params),
    s079_assert_nif_params(FunctionData, "glBeginConditionalRender", [s079_uint_nif_data(), s079_enum_nif_data()]).

s079_assert_end_conditional_render(FunctionData) ->
    ?assertEqual("glEndConditionalRender", maps:get(gl_command, FunctionData)),
    ?assertEqual([], maps:get(params_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_params, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glEndConditionalRender", maps:get(raw_function, Clause)),
    ?assertEqual([], maps:get(params, Clause)),
    s079_assert_nif_params(FunctionData, "glEndConditionalRender", []).

s079_assert_still_deferred(Functions) ->
    DeferredCommands = [
        "glFenceSync",
        "glMapBuffer",
        "glMapBufferRange"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s079_assert_nif_params(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(ExpectedParams), maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s079_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).

s079_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard79-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s079_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s079_assert_contains(C, Needle) || Needle <- RequiredC],
        [s079_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s079_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s079_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s079_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s079_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s079_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 80.
s080_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s080_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s080_emitter_framebuffer_parameter_test_() ->
    [
        {"gl 4.6", fun() ->
            s080_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([framebuffer_parameter/3]).">>,
                    <<"-export([named_framebuffer_parameter/3]).">>,
                    <<"-export_type([framebuffer_parameter_name/0]).">>,
                    <<"-type framebuffer_parameter_name() ::">>,
                    <<"framebuffer_default_width">>,
                    <<"-spec framebuffer_parameter(\n    Target :: framebuffer_target(),\n    ParamName :: framebuffer_parameter_name(),\n    Param :: gl:int()">>,
                    <<"-spec named_framebuffer_parameter(\n    Framebuffer :: framebuffer(),\n    ParamName :: framebuffer_parameter_name(),\n    Param :: gl:int()">>,
                    <<"gl:framebuffer_parameter(framebuffer, framebuffer_default_width, 1).">>,
                    <<"gl:named_framebuffer_parameter(Framebuffer, framebuffer_default_width, 1).">>,
                    <<"?CALL_RAW_FUNC(glFramebufferParameteri_raw(NewTarget, NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glNamedFramebufferParameteri_raw(Framebuffer, NewParamName, Param))">>
                ],
                [
                    <<"glFramebufferParameteri(arg_0, arg_1, arg_2);">>,
                    <<"glNamedFramebufferParameteri(arg_0, arg_1, arg_2);">>,
                    <<"{\"glFramebufferParameteri_raw\", 3, nif_glFramebufferParameteri, 0}">>,
                    <<"{\"glNamedFramebufferParameteri_raw\", 3, nif_glNamedFramebufferParameteri, 0}">>
                ],
                [
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s080_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([framebuffer_parameter/3]).">>,
                    <<"-export_type([framebuffer_parameter_name/0]).">>,
                    <<"?CALL_RAW_FUNC(glFramebufferParameteri_raw(NewTarget, NewParamName, Param))">>
                ],
                [
                    <<"glFramebufferParameteri(arg_0, arg_1, arg_2);">>,
                    <<"{\"glFramebufferParameteri_raw\", 3, nif_glFramebufferParameteri, 0}">>
                ],
                [
                    <<"-export([named_framebuffer_parameter/3]).">>,
                    <<"glNamedFramebufferParameteri_raw">>
                ]
            )
        end},
        {"gl 4.1", fun() ->
            s080_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                [
                    <<"-export([framebuffer_parameter/3]).">>,
                    <<"-export([named_framebuffer_parameter/3]).">>,
                    <<"glFramebufferParameteri_raw">>,
                    <<"glNamedFramebufferParameteri_raw">>
                ]
            )
        end}
    ].

s080_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s080_supports_framebuffer_parameter(Target) of
        true ->
            ?assert(maps:is_key({"framebuffer_parameter", 3}, Functions)),
            s080_assert_framebuffer_parameter_enum(BindingData),
            s080_assert_framebuffer_parameter(maps:get({"framebuffer_parameter", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"framebuffer_parameter", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glFramebufferParameteri", Functions))
    end,
    case s080_supports_named_framebuffer_parameter(Target) of
        true ->
            ?assert(maps:is_key({"named_framebuffer_parameter", 3}, Functions)),
            s080_assert_framebuffer_parameter_enum(BindingData),
            s080_assert_named_framebuffer_parameter(maps:get({"named_framebuffer_parameter", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"named_framebuffer_parameter", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glNamedFramebufferParameteri", Functions))
    end,
    s080_assert_queries_absent(Functions).

s080_supports_framebuffer_parameter({gl, {4, 6}}) ->
    true;
s080_supports_framebuffer_parameter({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s080_supports_framebuffer_parameter(_) ->
    false.

s080_supports_named_framebuffer_parameter({gl, {4, 6}}) ->
    true;
s080_supports_named_framebuffer_parameter(_) ->
    false.

s080_assert_framebuffer_parameter_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("framebuffer_parameter_name", EnumTypes)),
    ?assertNot(maps:is_key("get_framebuffer_parameter", EnumTypes)),
    ?assert(lists:member("framebuffer_default_width", maps:get("framebuffer_parameter_name", EnumTypes))),
    ?assert(lists:member("framebuffer_default_height", maps:get("framebuffer_parameter_name", EnumTypes))).

s080_assert_framebuffer_parameter(FunctionData) ->
    ?assertEqual("glFramebufferParameteri", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "ParamName", {gl_enum, "FramebufferParameterName"}},
            {in, "Param", gl_int}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"ParamName", {undefined, framebuffer_parameter_name, []}},
            {"Param", {gl, int, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glFramebufferParameteri", maps:get(raw_function, Clause)),
    Params = maps:get(params, Clause),
    s080_assert_enum_param("Target", "framebuffer", "GL_FRAMEBUFFER", Params),
    s080_assert_enum_param("ParamName", "framebuffer_default_width", "GL_FRAMEBUFFER_DEFAULT_WIDTH", Params),
    ?assert(lists:member({"Param", do_nothing}, Params)),
    s080_assert_nif_params(FunctionData, "glFramebufferParameteri", [s080_enum_nif_data(), s080_enum_nif_data(), s080_int_nif_data()]).

s080_assert_named_framebuffer_parameter(FunctionData) ->
    ?assertEqual("glNamedFramebufferParameteri", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "ParamName", {gl_enum, "FramebufferParameterName"}},
            {in, "Param", gl_int}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"ParamName", {undefined, framebuffer_parameter_name, []}},
            {"Param", {gl, int, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glNamedFramebufferParameteri", maps:get(raw_function, Clause)),
    Params = maps:get(params, Clause),
    ?assert(lists:member({"Framebuffer", do_nothing}, Params)),
    s080_assert_enum_param("ParamName", "framebuffer_default_width", "GL_FRAMEBUFFER_DEFAULT_WIDTH", Params),
    ?assert(lists:member({"Param", do_nothing}, Params)),
    s080_assert_nif_params(FunctionData, "glNamedFramebufferParameteri", [s080_uint_nif_data(), s080_enum_nif_data(), s080_int_nif_data()]).

s080_assert_queries_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s080_assert_nif_params(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s080_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).

s080_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard80-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s080_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s080_assert_contains(C, Needle) || Needle <- RequiredC],
        [s080_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s080_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s080_assert_contains(Haystack, Needle) ->
    ?assertNotEqual({Needle, nomatch}, {Needle, binary:match(Haystack, Needle)}).

s080_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s080_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s080_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s080_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 82.
s082_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s082_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s082_emitter_scalar_integer_texture_parameter_test_() ->
    [
        {"gl 4.6", fun() ->
            s082_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"texture_parameter(i, Texture, ParamName, Param) ->">>,
                    <<"glTextureParameteri_raw(Texture, NewParamName, Param)">>
                ],
                [
                    <<"glTextureParameteri(arg_0, arg_1, arg_2);">>
                ],
                [
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s082_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [],
                [],
                [
                    <<"texture_parameter(i, Texture, ParamName, Param) ->">>,
                    <<"glTextureParameteri_raw">>
                ]
            )
        end}
    ].

s082_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"tex_parameter", 4}, Functions)),
    s082_assert_target_bound_scalar_integer_still_deferred(maps:get({"tex_parameter", 4}, Functions)),

    case Target of
        {gl, {4, 6}} ->
            ?assert(maps:is_key({"texture_parameter", 4}, Functions)),
            s082_assert_scalar_integer_texture_parameter(
                maps:get({"texture_parameter", 4}, Functions),
                "glTextureParameteriv",
                "glTextureParameteri",
                "Texture"
            );
        _ ->
            ?assertNot(maps:is_key({"texture_parameter", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glTextureParameteri", Functions))
    end,

    s082_assert_sampler_parameter_queries_still_deferred(Functions),
    s082_assert_texture_parameter_name(BindingData).

s082_assert_target_bound_scalar_integer_still_deferred(FunctionData) ->
    ?assertNot(lists:member({gl_int, element}, maps:get(variants, FunctionData))),
    ?assertNot(lists:member({"glTexParameteri", gl_int, element}, maps:get(gl_commands, FunctionData))),
    {tex_parameter_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assertNot(lists:member({gl, int, []}, Variants)),
    Clauses = maps:get(function_clauses, FunctionData),
    ?assertEqual([], [Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= "glTexParameteri"]).

s082_assert_scalar_integer_texture_parameter(FunctionData, ArrayCommand, ScalarCommand, ObjectParamName) ->
    ?assert(lists:member({gl_int, element}, maps:get(variants, FunctionData))),
    ?assert(lists:member({ScalarCommand, gl_int, element}, maps:get(gl_commands, FunctionData))),

    {ExtraTypeName, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assert(lists:member(ExtraTypeName, [tex_parameter_value, texture_parameter_value])),
    ?assert(lists:member({gl, int, []}, Variants)),
    ?assert(lists:member({list, {gl, int, []}}, Variants)),

    Clauses = maps:get(function_clauses, FunctionData),
    ArrayClause = s082_find_clause(ArrayCommand, Clauses),
    ScalarClause = s082_find_clause(ScalarCommand, Clauses),
    ?assert(s082_index_of(ArrayCommand, [maps:get(raw_function, Clause) || Clause <- Clauses]) <
        s082_index_of(ScalarCommand, [maps:get(raw_function, Clause) || Clause <- Clauses])),
    ?assertEqual("Param", maps:get(guard_var, ArrayClause)),
    ?assertEqual([{is_list, var}], maps:get(guards, ArrayClause)),
    ?assertEqual("Param", maps:get(guard_var, ScalarClause)),
    ?assertEqual([], maps:get(guards, ScalarClause)),

    [
        {"i", ignore},
        {ObjectParamName, _ObjectTransform},
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Param", do_nothing}
    ] = maps:get(params, ScalarClause),
    ?assert(lists:keymember("texture_wrap_t", 1, ParamNameMap)),

    NifData = maps:get(ScalarCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    ?assert(lists:member({"Param", {gl_type, s082_int_nif_raw()}}, maps:get(params, NifData))).

s082_assert_sampler_parameter_queries_still_deferred(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
        ]
    ].

s082_assert_texture_parameter_name(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    TextureParameterNames = maps:get("texture_parameter_name", EnumTypes),
    ?assert(lists:member("texture_wrap_s", TextureParameterNames)),
    ?assert(lists:member("texture_wrap_t", TextureParameterNames)).

s082_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard82-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s082_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s082_assert_contains(C, Needle) || Needle <- RequiredC],
        [s082_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s082_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s082_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s082_index_of(Value, Values) ->
    s082_index_of(Value, Values, 1).

s082_index_of(Value, [Value | _Rest], Index) ->
    Index;
s082_index_of(Value, [_Other | Rest], Index) ->
    s082_index_of(Value, Rest, Index + 1).

s082_int_nif_raw() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s082_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s082_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 85.
s085_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s085_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s085_emitter_framebuffer_attachment_list_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s085_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([draw_buffers/1]).">>,
                    <<"-export([named_framebuffer_draw_buffer/2]).">>,
                    <<"-export([named_framebuffer_draw_buffers/2]).">>,
                    <<"-export([named_framebuffer_read_buffer/2]).">>,
                    <<"-export([invalidate_framebuffer/2]).">>,
                    <<"-export([invalidate_sub_framebuffer/6]).">>,
                    <<"-export([invalidate_named_framebuffer_data/2]).">>,
                    <<"-export([invalidate_named_framebuffer_sub_data/6]).">>,
                    <<"-spec draw_buffers(Buffers :: [draw_buffer_mode()]) -> ok | {error, atom()}.">>,
                    <<"-spec invalidate_framebuffer(\n    Target :: framebuffer_target(),\n    Attachments :: [framebuffer_attachment()]">>,
                    <<"glDrawBuffers_raw(N, NewBuffers)">>,
                    <<"glInvalidateNamedFramebufferSubData_raw(Framebuffer, NumAttachments, NewAttachments, X, Y, Width, Height)">>
                ],
                [
                    <<"glDrawBuffers(arg_0, (void*)arg_1.data);">>,
                    <<"glNamedFramebufferDrawBuffer(arg_0, arg_1);">>,
                    <<"glNamedFramebufferDrawBuffers(arg_0, arg_1, (void*)arg_2.data);">>,
                    <<"glNamedFramebufferReadBuffer(arg_0, arg_1);">>,
                    <<"glInvalidateFramebuffer(arg_0, arg_1, (void*)arg_2.data);">>,
                    <<"glInvalidateSubFramebuffer(arg_0, arg_1, (void*)arg_2.data, arg_3, arg_4, arg_5, arg_6);">>,
                    <<"glInvalidateNamedFramebufferData(arg_0, arg_1, (void*)arg_2.data);">>,
                    <<"glInvalidateNamedFramebufferSubData(arg_0, arg_1, (void*)arg_2.data, arg_3, arg_4, arg_5, arg_6);">>
                ],
                []
            )
        end},
        {"gles 3.2", fun() ->
            s085_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([draw_buffers/1]).">>,
                    <<"-export([invalidate_framebuffer/2]).">>,
                    <<"-export([invalidate_sub_framebuffer/6]).">>,
                    <<"glDrawBuffers_raw(N, NewBuffers)">>,
                    <<"glInvalidateSubFramebuffer_raw(NewTarget, NumAttachments, NewAttachments, X, Y, Width, Height)">>
                ],
                [
                    <<"glDrawBuffers(arg_0, (void*)arg_1.data);">>,
                    <<"glInvalidateFramebuffer(arg_0, arg_1, (void*)arg_2.data);">>,
                    <<"glInvalidateSubFramebuffer(arg_0, arg_1, (void*)arg_2.data, arg_3, arg_4, arg_5, arg_6);">>
                ],
                [
                    <<"-export([named_framebuffer_draw_buffer/2]).">>,
                    <<"glNamedFramebufferDrawBuffer_raw">>,
                    <<"glInvalidateNamedFramebufferData_raw">>
                ]
            )
        end},
        {"gl 4.1", fun() ->
            s085_assert_emitted_surface(
                {gl, {4, 1}},
                [
                    <<"-export([draw_buffers/1]).">>,
                    <<"glDrawBuffers_raw(N, NewBuffers)">>
                ],
                [
                    <<"glDrawBuffers(arg_0, (void*)arg_1.data);">>
                ],
                [
                    <<"glInvalidateFramebuffer_raw">>,
                    <<"glNamedFramebufferDrawBuffer_raw">>
                ]
            )
        end}
    ].

s085_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s085_assert_presence(Target, Functions),
    s085_assert_deferred_neighbors_absent(Functions),
    [s085_assert_path(Function, Target, BindingData, maps:get(Function, Functions))
     || Function <- s085_present_functions(Target)].

s085_assert_presence(Target, Functions) ->
    Present = s085_present_functions(Target),
    Absent = s085_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s085_present_functions({gl, {4, 6}}) ->
    s085_all_functions();
s085_present_functions({gl, _}) ->
    [{"draw_buffers", 1}];
s085_present_functions({gles, {2, 0}}) ->
    [];
s085_present_functions({gles, _}) ->
    [
        {"draw_buffers", 1},
        {"invalidate_framebuffer", 2},
        {"invalidate_sub_framebuffer", 6}
    ].

s085_all_functions() ->
    [
        {"draw_buffers", 1},
        {"named_framebuffer_draw_buffer", 2},
        {"named_framebuffer_draw_buffers", 2},
        {"named_framebuffer_read_buffer", 2},
        {"invalidate_framebuffer", 2},
        {"invalidate_sub_framebuffer", 6},
        {"invalidate_named_framebuffer_data", 2},
        {"invalidate_named_framebuffer_sub_data", 6}
    ].

s085_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s085_assert_path({"draw_buffers", 1}, _Target, BindingData, FunctionData) ->
    s085_assert_enum_contains(BindingData, "draw_buffer_mode", "color_attachment0"),
    s085_assert_counted_enum_direct(
        FunctionData,
        "glDrawBuffers",
        [{in, "Buffers", {counted_list, "N", {gl_enum, "DrawBufferMode"}}}],
        [{"Buffers", {list, {undefined, draw_buffer_mode, []}}}],
        "Buffers",
        "N",
        ["color_attachment0"]
    );
s085_assert_path({"named_framebuffer_draw_buffer", 2}, _Target, BindingData, FunctionData) ->
    s085_assert_enum_contains(BindingData, "draw_buffer_mode", "color_attachment0"),
    s085_assert_direct(
        FunctionData,
        "glNamedFramebufferDrawBuffer",
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Buffer", {gl_enum, "DrawBufferMode"}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Buffer", {undefined, draw_buffer_mode, []}}
        ],
        [
            {"Framebuffer", do_nothing},
            {"Buffer", {gl_enum_to_uint, ["color_attachment0"]}}
        ],
        [
            {"Framebuffer", s085_uint_nif_data()},
            {"Buffer", s085_enum_nif_data()}
        ]
    );
s085_assert_path({"named_framebuffer_draw_buffers", 2}, _Target, BindingData, FunctionData) ->
    s085_assert_enum_contains(BindingData, "draw_buffer_mode", "color_attachment0"),
    s085_assert_counted_enum_direct(
        FunctionData,
        "glNamedFramebufferDrawBuffers",
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Buffers", {counted_list, "N", {gl_enum, "DrawBufferMode"}}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Buffers", {list, {undefined, draw_buffer_mode, []}}}
        ],
        "Buffers",
        "N",
        ["color_attachment0"]
    );
s085_assert_path({"named_framebuffer_read_buffer", 2}, _Target, BindingData, FunctionData) ->
    s085_assert_enum_contains(BindingData, "read_buffer_mode", "color_attachment0"),
    s085_assert_direct(
        FunctionData,
        "glNamedFramebufferReadBuffer",
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Source", {gl_enum, "ReadBufferMode"}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Source", {undefined, read_buffer_mode, []}}
        ],
        [
            {"Framebuffer", do_nothing},
            {"Source", {gl_enum_to_uint, ["color_attachment0"]}}
        ],
        [
            {"Framebuffer", s085_uint_nif_data()},
            {"Source", s085_enum_nif_data()}
        ]
    );
s085_assert_path({"invalidate_framebuffer", 2}, _Target, BindingData, FunctionData) ->
    s085_assert_enum_contains(BindingData, "framebuffer_attachment", "color_attachment0"),
    s085_assert_counted_enum_direct(
        FunctionData,
        "glInvalidateFramebuffer",
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Attachments", {counted_list, "NumAttachments", {gl_enum, "FramebufferAttachment"}}}
        ],
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Attachments", {list, {undefined, framebuffer_attachment, []}}}
        ],
        "Attachments",
        "NumAttachments",
        ["color_attachment0"]
    );
s085_assert_path({"invalidate_sub_framebuffer", 6}, _Target, BindingData, FunctionData) ->
    s085_assert_enum_contains(BindingData, "framebuffer_attachment", "color_attachment0"),
    s085_assert_counted_enum_direct(
        FunctionData,
        "glInvalidateSubFramebuffer",
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Attachments", {counted_list, "NumAttachments", {gl_enum, "FramebufferAttachment"}}},
            {in, "X", gl_int},
            {in, "Y", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Attachments", {list, {undefined, framebuffer_attachment, []}}},
            {"X", {gl, int, []}},
            {"Y", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        "Attachments",
        "NumAttachments",
        ["color_attachment0"]
    );
s085_assert_path({"invalidate_named_framebuffer_data", 2}, _Target, _BindingData, FunctionData) ->
    s085_assert_counted_enum_direct(
        FunctionData,
        "glInvalidateNamedFramebufferData",
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Attachments", {counted_list, "NumAttachments", {gl_enum, "FramebufferAttachment"}}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Attachments", {list, {undefined, framebuffer_attachment, []}}}
        ],
        "Attachments",
        "NumAttachments",
        ["color_attachment0"]
    );
s085_assert_path({"invalidate_named_framebuffer_sub_data", 6}, _Target, _BindingData, FunctionData) ->
    s085_assert_counted_enum_direct(
        FunctionData,
        "glInvalidateNamedFramebufferSubData",
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Attachments", {counted_list, "NumAttachments", {gl_enum, "FramebufferAttachment"}}},
            {in, "X", gl_int},
            {in, "Y", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Attachments", {list, {undefined, framebuffer_attachment, []}}},
            {"X", {gl, int, []}},
            {"Y", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        "Attachments",
        "NumAttachments",
        ["color_attachment0"]
    ).

s085_assert_counted_enum_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ParamName, CountName, ExpectedAtoms) ->
    s085_assert_counted_enum_direct(
        FunctionData,
        GlCommand,
        ParamsSpecs,
        SpecsParams,
        ParamName,
        CountName,
        ExpectedAtoms,
        []
    ).

s085_assert_counted_enum_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ParamName, CountName, ExpectedAtoms, PrefixNifs) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    ClauseParams = maps:get(params, Clause),
    {ParamName, {counted_list_gl_enums_to_binary, CountName, TransformMap}} = lists:keyfind(ParamName, 1, ClauseParams),
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- ExpectedAtoms],

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ExpectedNifs = PrefixNifs ++ [
        {CountName, s085_sizei_nif_data()},
        {ParamName, binary_to_glbinary}
    ],
    lists:foreach(fun(ExpectedParam) ->
        ?assert(lists:member(ExpectedParam, maps:get(params, NifData)))
    end, ExpectedNifs),
    ?assertEqual(void, maps:get(return, NifData)).

s085_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    s085_assert_clause_params(ClauseParams, maps:get(params, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s085_assert_clause_params(Expected, Actual) ->
    lists:foreach(fun
        ({Name, {gl_enum_to_uint, RequiredAtoms}}) ->
            {Name, {gl_enum_to_uint, TransformMap}} = lists:keyfind(Name, 1, Actual),
            [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
        (Param) ->
            ?assert(lists:member(Param, Actual))
    end, Expected).

s085_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s085_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-gen-shard85-" ++ atom_to_list(element(1, Target)) ++ "-" ++
            string:replace(generator_test_support:target_name(Target), " ", "-", all)
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file(filename:join(Dir, "gl.erl")),
        {ok, C} = file:read_file(filename:join(Dir, "gl.c")),
        [?assertMatch({_, _}, binary:match(Erl, Needle)) || Needle <- RequiredErl],
        [?assertMatch({_, _}, binary:match(C, Needle)) || Needle <- RequiredC],
        [?assertEqual(nomatch, binary:match(Erl, Needle)) || Needle <- Forbidden],
        [?assertEqual(nomatch, binary:match(C, Needle)) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s085_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s085_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s085_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 87.
s087_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s087_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s087_emitter_ordinary_sampler_parameter_test_() ->
    [
        {"gl 4.6", fun() ->
            s087_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([sampler_parameter/4]).">>,
                    <<"-type sampler_parameter_name() ::">>,
                    <<"-type sampler_parameter_value() ::">>,
                    <<"-spec sampler_parameter(\n    Type :: f | i,">>,
                    <<"sampler_parameter(f, Sampler, ParamName, Param) when is_list(Param) ->">>,
                    <<"sampler_parameter(f, Sampler, ParamName, Param) ->">>,
                    <<"sampler_parameter(i, Sampler, ParamName, Param) when is_list(Param) ->">>,
                    <<"sampler_parameter(i, Sampler, ParamName, Param) ->">>,
                    <<"?CALL_RAW_FUNC(glSamplerParameterfv_raw(Sampler, NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glSamplerParameterf_raw(Sampler, NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glSamplerParameteriv_raw(Sampler, NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glSamplerParameteri_raw(Sampler, NewParamName, Param))">>
                ],
                [
                    <<"glSamplerParameterf(arg_0, arg_1, arg_2);">>,
                    <<"glSamplerParameterfv(arg_0, arg_1, arg_2_array);">>,
                    <<"glSamplerParameteri(arg_0, arg_1, arg_2);">>,
                    <<"glSamplerParameteriv(arg_0, arg_1, arg_2_array);">>,
                    <<"{\"glSamplerParameterf_raw\", 3, nif_glSamplerParameterf, 0}">>,
                    <<"{\"glSamplerParameterfv_raw\", 3, nif_glSamplerParameterfv, 0}">>,
                    <<"{\"glSamplerParameteri_raw\", 3, nif_glSamplerParameteri, 0}">>,
                    <<"{\"glSamplerParameteriv_raw\", 3, nif_glSamplerParameteriv, 0}">>
                ],
                [
                    <<"-export([sampler_parameter/3]).">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s087_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [
                    <<"-export([sampler_parameter/4]).">>,
                    <<"texture_min_lod">>,
                    <<"texture_min_filter">>,
                    <<"glSamplerParameterfv_raw">>,
                    <<"glSamplerParameteriv_raw">>
                ],
                [
                    <<"glSamplerParameterf(arg_0, arg_1, arg_2);">>,
                    <<"glSamplerParameteriv(arg_0, arg_1, arg_2_array);">>
                ],
                [
                ]
            )
        end},
        {"gles 2.0", fun() ->
            s087_assert_emitted_surface(
                {gles, {2, 0}},
                "OpenGL ES 2.0",
                [],
                [],
                [
                    <<"-export([sampler_parameter/4]).">>,
                    <<"glSamplerParameterf">>,
                    <<"glSamplerParameteri">>
                ]
            )
        end}
    ].

s087_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s087_supports_ordinary_sampler_parameter(Target) of
        true ->
            ?assert(maps:is_key({"sampler_parameter", 4}, Functions)),
            ?assertNot(maps:is_key({"sampler_parameter", 3}, Functions)),
            s087_assert_ordinary_sampler_parameter(
                BindingData,
                maps:get({"sampler_parameter", 4}, Functions)
            );
        false ->
            ?assertNot(maps:is_key({"sampler_parameter", 4}, Functions)),
            s087_assert_commands_absent(s087_ordinary_sampler_parameter_commands(), Functions)
    end,
    s087_assert_commands_absent(s087_sampler_parameter_query_commands(), Functions).

s087_supports_ordinary_sampler_parameter({gles, {2, 0}}) ->
    false;
s087_supports_ordinary_sampler_parameter(_) ->
    true.

s087_assert_ordinary_sampler_parameter(BindingData, FunctionData) ->
    s087_assert_enum_contains(BindingData, "sampler_parameter_name", "texture_min_lod"),
    s087_assert_enum_contains(BindingData, "sampler_parameter_name", "texture_max_lod"),
    s087_assert_enum_contains(BindingData, "sampler_parameter_name", "texture_min_filter"),
    s087_assert_enum_contains(BindingData, "sampler_parameter_name", "texture_wrap_s"),
    ?assertEqual(
        [
            {in, "Sampler", {gl_object, sampler}},
            {in, "ParamName", {gl_enum, ["SamplerParameterF", "SamplerParameterI"], sampler_parameter_name}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Sampler", {undefined, sampler, []}},
            {"ParamName", {undefined, sampler_parameter_name, []}},
            {"Param", {undefined, sampler_parameter_value, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    s087_assert_extra_type(FunctionData),
    s087_assert_commands(FunctionData),
    s087_assert_clauses(FunctionData),
    s087_assert_nifs(FunctionData).

s087_assert_extra_type(FunctionData) ->
    {sampler_parameter_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    [?assert(lists:member(Variant, Variants)) || Variant <- [
        {gl, float, []},
        {list, {gl, float, []}},
        {gl, int, []},
        {list, {gl, int, []}}
    ]].

s087_assert_commands(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    [?assert(lists:member(Command, GlCommands)) || Command <- [
        {"glSamplerParameterf", gl_float, element},
        {"glSamplerParameterfv", gl_float, array},
        {"glSamplerParameteri", gl_int, element},
        {"glSamplerParameteriv", gl_int, array}
    ]],
    ?assertEqual(
        lists:sort([{gl_float, element}, {gl_float, array}, {gl_int, element}, {gl_int, array}]),
        lists:sort(maps:get(variants, FunctionData))
    ).

s087_assert_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s087_assert_before("glSamplerParameterfv", "glSamplerParameterf", RawOrder),
    s087_assert_before("glSamplerParameteriv", "glSamplerParameteri", RawOrder),
    s087_assert_parameter_array_clause("glSamplerParameterfv", "f", Clauses),
    s087_assert_parameter_scalar_clause("glSamplerParameterf", "f", Clauses),
    s087_assert_parameter_array_clause("glSamplerParameteriv", "i", Clauses),
    s087_assert_parameter_scalar_clause("glSamplerParameteri", "i", Clauses).

s087_assert_parameter_array_clause(RawFunction, Suffix, Clauses) ->
    Clause = s087_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    s087_assert_parameter_clause_params(Suffix, Clause).

s087_assert_parameter_scalar_clause(RawFunction, Suffix, Clauses) ->
    Clause = s087_find_clause(RawFunction, Clauses),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([], maps:get(guards, Clause)),
    s087_assert_parameter_clause_params(Suffix, Clause).

s087_assert_parameter_clause_params(Suffix, Clause) ->
    [
        {Suffix, ignore},
        {"Sampler", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_min_lod", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_min_filter", 1, ParamNameTransformMap)).

s087_assert_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    s087_assert_nif(
        maps:get("glSamplerParameterf", NifFunctions),
        [{"Sampler", s087_uint_nif_data()}, {"ParamName", s087_enum_nif_data()}, {"Param", {gl_type, s087_float_raw_nif_data()}}]
    ),
    s087_assert_nif(
        maps:get("glSamplerParameterfv", NifFunctions),
        [{"Sampler", s087_uint_nif_data()}, {"ParamName", s087_enum_nif_data()}, {"Param", {list_gl_type, s087_float_raw_nif_data()}}]
    ),
    s087_assert_nif(
        maps:get("glSamplerParameteri", NifFunctions),
        [{"Sampler", s087_uint_nif_data()}, {"ParamName", s087_enum_nif_data()}, {"Param", {gl_type, s087_int_raw_nif_data()}}]
    ),
    s087_assert_nif(
        maps:get("glSamplerParameteriv", NifFunctions),
        [{"Sampler", s087_uint_nif_data()}, {"ParamName", s087_enum_nif_data()}, {"Param", {list_gl_type, s087_int_raw_nif_data()}}]
    ).

s087_assert_nif(NifData, Params) ->
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s087_assert_commands_absent(Commands, Functions) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- Commands].

s087_ordinary_sampler_parameter_commands() ->
    [
        "glSamplerParameterf",
        "glSamplerParameterfv",
        "glSamplerParameteri",
        "glSamplerParameteriv"
    ].

s087_sampler_parameter_query_commands() ->
    [
    ].

s087_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s087_assert_before(First, Second, Values) ->
    ?assert(s087_index_of(First, Values) < s087_index_of(Second, Values)).

s087_index_of(Value, Values) ->
    s087_index_of(Value, Values, 1).

s087_index_of(Value, [Value | _Rest], Index) ->
    Index;
s087_index_of(Value, [_Other | Rest], Index) ->
    s087_index_of(Value, Rest, Index + 1).

s087_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s087_assert_emitted_surface(Target, ApiName, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard87-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, ApiName, generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s087_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s087_assert_contains(C, Needle) || Needle <- RequiredC],
        [s087_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s087_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s087_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s087_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s087_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s087_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s087_float_raw_nif_data() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s087_int_raw_nif_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

%% Historical shard 88.
s088_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s088_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s088_emitter_named_buffer_store_test_() ->
    [
        {"gl 4.6", fun() ->
            s088_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([buffer_storage/3]).">>,
                    <<"-export([named_buffer_storage/3]).">>,
                    <<"-export([named_buffer_data/3]).">>,
                    <<"-export([named_buffer_sub_data/3]).">>,
                    <<"-type buffer_storage_mask() ::">>,
                    <<"buffer_storage(Target, DataOrSize, Flags) ->">>,
                    <<"named_buffer_storage(Buffer, DataOrSize, Flags) ->">>,
                    <<"named_buffer_data(Buffer, DataOrSize, Usage) ->">>,
                    <<"named_buffer_sub_data(Buffer, Offset, Data) ->">>
                ],
                [
                    <<"glBufferStorage(arg_0, arg_1, arg_2, arg_3);">>,
                    <<"glNamedBufferStorage(arg_0, arg_1, arg_2, arg_3);">>,
                    <<"glNamedBufferData(arg_0, arg_1, arg_2, arg_3);">>,
                    <<"glNamedBufferSubData(arg_0, arg_1, arg_2, (void*)arg_3.data);">>
                ],
                [
                    <<"-export([map_buffer/2]).">>,
                    <<"glMapNamedBuffer">>,
                    <<"glGetNamedBufferPointerv">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s088_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [],
                [],
                [
                    <<"-export([buffer_storage/3]).">>,
                    <<"-export([named_buffer_storage/3]).">>,
                    <<"glBufferStorage">>,
                    <<"glNamedBufferData">>
                ]
            )
        end}
    ].

s088_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s088_supports(Target) of
        true ->
            [?assert(maps:is_key(Function, Functions)) || Function <- s088_functions()],
            s088_assert_buffer_storage_mask(BindingData),
            s088_assert_buffer_storage(BindingData, maps:get({"buffer_storage", 3}, Functions)),
            s088_assert_named_buffer_storage(BindingData, maps:get({"named_buffer_storage", 3}, Functions)),
            s088_assert_named_buffer_data(BindingData, maps:get({"named_buffer_data", 3}, Functions)),
            s088_assert_named_buffer_sub_data(maps:get({"named_buffer_sub_data", 3}, Functions));
        false ->
            [?assertNot(maps:is_key(Function, Functions)) || Function <- s088_functions()],
            [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- s088_commands()]
    end,
    s088_assert_deferred_neighbors_absent(Functions).

s088_supports({gl, {4, 6}}) ->
    true;
s088_supports(_) ->
    false.

s088_functions() ->
    [
        {"buffer_storage", 3},
        {"named_buffer_storage", 3},
        {"named_buffer_data", 3},
        {"named_buffer_sub_data", 3}
    ].

s088_commands() ->
    [
        "glBufferStorage",
        "glNamedBufferStorage",
        "glNamedBufferData",
        "glNamedBufferSubData"
    ].

s088_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glMapBuffer",
        "glMapBufferRange",
        "glUnmapBuffer",
        "glFlushMappedBufferRange",
        "glGetBufferPointerv",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glUnmapNamedBuffer",
        "glFlushMappedNamedBufferRange",
        "glGetNamedBufferPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s088_assert_buffer_storage_mask(BindingData) ->
    BitfieldTypes = maps:get(bitfield_types, BindingData),
    ?assert(maps:is_key("buffer_storage_mask", BitfieldTypes)),
    Values = maps:get("buffer_storage_mask", BitfieldTypes),
    [?assert(lists:member(Atom, Values)) || Atom <- [
        "dynamic_storage_bit",
        "map_read_bit",
        "map_write_bit"
    ]].

s088_assert_buffer_storage(BindingData, FunctionData) ->
    s088_assert_enum_contains(BindingData, "buffer_target", "array_buffer"),
    s088_assert_direct(
        FunctionData,
        "glBufferStorage",
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {in, "Flags", {gl_bitfield, "BufferStorageMask"}}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"DataOrSize", {set, [{undefined, iodata, []}, {undefined, non_neg_integer, []}]}},
            {"Flags", {undefined, buffer_storage_mask, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["array_buffer"]}},
            {"DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {"Flags", {gl_bitfield_to_uint, ["dynamic_storage_bit", "map_write_bit"]}}
        ],
        [
            {"Target", s088_enum_nif_data()},
            {"Size", s088_sizeiptr_nif_data()},
            {"Data", in_gl_binary_or_null},
            {"Flags", s088_bitfield_nif_data()}
        ]
    ).

s088_assert_named_buffer_storage(BindingData, FunctionData) ->
    s088_assert_bitfield_contains(BindingData, "buffer_storage_mask", "dynamic_storage_bit"),
    s088_assert_direct(
        FunctionData,
        "glNamedBufferStorage",
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {in, "Flags", {gl_bitfield, "BufferStorageMask"}}
        ],
        [
            {"Buffer", {undefined, buffer, []}},
            {"DataOrSize", {set, [{undefined, iodata, []}, {undefined, non_neg_integer, []}]}},
            {"Flags", {undefined, buffer_storage_mask, []}}
        ],
        [
            {"Buffer", do_nothing},
            {"DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {"Flags", {gl_bitfield_to_uint, ["dynamic_storage_bit"]}}
        ],
        [
            {"Buffer", s088_uint_nif_data()},
            {"Size", s088_sizeiptr_nif_data()},
            {"Data", in_gl_binary_or_null},
            {"Flags", s088_bitfield_nif_data()}
        ]
    ).

s088_assert_named_buffer_data(BindingData, FunctionData) ->
    s088_assert_enum_contains(BindingData, "buffer_usage", "static_draw"),
    s088_assert_direct(
        FunctionData,
        "glNamedBufferData",
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {in, "Usage", {gl_enum, "BufferUsageARB", buffer_usage}}
        ],
        [
            {"Buffer", {undefined, buffer, []}},
            {"DataOrSize", {set, [{undefined, iodata, []}, {undefined, non_neg_integer, []}]}},
            {"Usage", {undefined, buffer_usage, []}}
        ],
        [
            {"Buffer", do_nothing},
            {"DataOrSize", {byte_data_or_size, "Size", "Data"}},
            {"Usage", {gl_enum_to_uint, ["static_draw"]}}
        ],
        [
            {"Buffer", s088_uint_nif_data()},
            {"Size", s088_sizeiptr_nif_data()},
            {"Data", in_gl_binary_or_null},
            {"Usage", s088_enum_nif_data()}
        ]
    ).

s088_assert_named_buffer_sub_data(FunctionData) ->
    s088_assert_direct(
        FunctionData,
        "glNamedBufferSubData",
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "Offset", gl_intptr},
            {in, "Data", {byte_data, "Size"}}
        ],
        [
            {"Buffer", {undefined, buffer, []}},
            {"Offset", {gl, intptr, []}},
            {"Data", {undefined, iodata, []}}
        ],
        [
            {"Buffer", do_nothing},
            {"Offset", do_nothing},
            {"Data", {byte_data, "Size"}}
        ],
        [
            {"Buffer", s088_uint_nif_data()},
            {"Offset", s088_intptr_nif_data()},
            {"Size", s088_sizeiptr_nif_data()},
            {"Data", binary_to_glbinary}
        ]
    ).

s088_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s088_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s088_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s088_assert_clause_param/1, lists:zip(Expected, Actual)).

s088_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s088_assert_clause_param({{Name, {gl_bitfield_to_uint, RequiredAtoms}}, {Name, {gl_bitfield_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s088_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s088_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s088_assert_bitfield_contains(BindingData, BitfieldType, Atom) ->
    BitfieldTypes = maps:get(bitfield_types, BindingData),
    ?assert(lists:member(Atom, maps:get(BitfieldType, BitfieldTypes))).

s088_assert_emitted_surface(Target, ApiName, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard88-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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

s088_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s088_bitfield_nif_data() ->
    {gl_type, {"GLbitfield", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s088_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s088_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s088_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 89.
s089_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s089_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s089_emitter_texture_view_test_() ->
    [
        {"gl 4.6", fun() ->
            s089_assert_emitted_surface(
                {gl, {4, 6}},
                "OpenGL 4.6",
                [
                    <<"-export([texture_view/8]).">>,
                    <<"texture_view(Texture, Target, OrigTexture, InternalFormat, MinLevel, NumLevels, MinLayer, NumLayers) ->">>,
                    <<"ok = gl:texture_view(ViewTexture, texture_2d, OriginalTexture, rgba8, 0, 1, 0, 1).">>
                ],
                [
                    <<"glTextureView(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>
                ],
                [
                    <<"glCopyImageSubDataEXT(">>,
                    <<"glCopyImageSubDataNV(">>,
                    <<"glCopyImageSubDataOES(">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s089_assert_emitted_surface(
                {gles, {3, 2}},
                "OpenGL ES 3.2",
                [],
                [],
                [
                    <<"-export([texture_view/8]).">>,
                    <<"glTextureView(">>,
                    <<"glCopyImageSubDataEXT(">>,
                    <<"glCopyImageSubDataNV(">>,
                    <<"glCopyImageSubDataOES(">>
                ]
            )
        end}
    ].

s089_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s089_supports(Target) of
        true ->
            ?assert(maps:is_key({"texture_view", 8}, Functions)),
            s089_assert_texture_view(BindingData, maps:get({"texture_view", 8}, Functions));
        false ->
            ?assertNot(maps:is_key({"texture_view", 8}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glTextureView", Functions))
    end,
    s089_assert_deferred_neighbors_absent(Functions).

s089_supports({gl, {4, 6}}) ->
    true;
s089_supports(_) ->
    false.

s089_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glCopyImageSubDataEXT",
        "glCopyImageSubDataNV",
        "glCopyImageSubDataOES"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s089_assert_texture_view(BindingData, FunctionData) ->
    s089_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s089_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s089_assert_direct(
        FunctionData,
        "glTextureView",
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "OrigTexture", {gl_object, texture}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "MinLevel", gl_uint},
            {in, "NumLevels", gl_uint},
            {in, "MinLayer", gl_uint},
            {in, "NumLayers", gl_uint}
        ],
        [
            {"Texture", {undefined, texture, []}},
            {"Target", {undefined, texture_target, []}},
            {"OrigTexture", {undefined, texture, []}},
            {"InternalFormat", {undefined, sized_internal_format, []}},
            {"MinLevel", {gl, uint, []}},
            {"NumLevels", {gl, uint, []}},
            {"MinLayer", {gl, uint, []}},
            {"NumLayers", {gl, uint, []}}
        ],
        [
            {"Texture", do_nothing},
            {"Target", {gl_enum_to_uint, ["texture_2d"]}},
            {"OrigTexture", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba8"]}},
            {"MinLevel", do_nothing},
            {"NumLevels", do_nothing},
            {"MinLayer", do_nothing},
            {"NumLayers", do_nothing}
        ],
        [
            {"Texture", s089_uint_nif_data()},
            {"Target", s089_enum_nif_data()},
            {"OrigTexture", s089_uint_nif_data()},
            {"InternalFormat", s089_enum_nif_data()},
            {"MinLevel", s089_uint_nif_data()},
            {"NumLevels", s089_uint_nif_data()},
            {"MinLayer", s089_uint_nif_data()},
            {"NumLayers", s089_uint_nif_data()}
        ]
    ).

s089_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s089_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s089_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s089_assert_clause_param/1, lists:zip(Expected, Actual)).

s089_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s089_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s089_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s089_assert_emitted_surface(Target, ApiName, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard89-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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

s089_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s089_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 97.
s097_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s097_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s097_emitter_late_state_status_controls_test_() ->
    [
        {"gl 4.6", fun() ->
            s097_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([sample_mask/2]).">>,
                    <<"-export([viewport/5]).">>,
                    <<"-export([polygon_offset_clamp/3]).">>,
                    <<"-export([get_graphics_reset_status/0]).">>
                ],
                [
                    <<"glSampleMaski(arg_0, arg_1);">>,
                    <<"glViewportIndexedf(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
                    <<"glPolygonOffsetClamp(arg_0, arg_1, arg_2);">>,
                    <<"GLenum ret = glGetGraphicsResetStatus();">>
                ],
                s097_forbidden_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s097_assert_emitted_surface(
                {gl, {4, 1}},
                [
                    <<"-export([sample_mask/2]).">>,
                    <<"-export([viewport/5]).">>
                ],
                [
                    <<"glSampleMaski(arg_0, arg_1);">>,
                    <<"glViewportIndexedf(arg_0, arg_1, arg_2, arg_3, arg_4);">>
                ],
                [
                    <<"-export([polygon_offset_clamp/3]).">>,
                    <<"-export([get_graphics_reset_status/0]).">>,
                    <<"glPolygonOffsetClamp(">>,
                    <<"glGetGraphicsResetStatus(">>
                ] ++ s097_forbidden_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s097_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([sample_mask/2]).">>,
                    <<"-export([get_graphics_reset_status/0]).">>
                ],
                [
                    <<"glSampleMaski(arg_0, arg_1);">>,
                    <<"GLenum ret = glGetGraphicsResetStatus();">>
                ],
                [
                    <<"-export([viewport/5]).">>,
                    <<"-export([polygon_offset_clamp/3]).">>,
                    <<"glViewportIndexedf(">>,
                    <<"glPolygonOffsetClamp(">>
                ] ++ s097_forbidden_needles()
            )
        end},
        {"gles 3.1", fun() ->
            s097_assert_emitted_surface(
                {gles, {3, 1}},
                [<<"-export([sample_mask/2]).">>],
                [<<"glSampleMaski(arg_0, arg_1);">>],
                [
                    <<"-export([viewport/5]).">>,
                    <<"-export([polygon_offset_clamp/3]).">>,
                    <<"-export([get_graphics_reset_status/0]).">>,
                    <<"glViewportIndexedf(">>,
                    <<"glPolygonOffsetClamp(">>,
                    <<"glGetGraphicsResetStatus(">>
                ] ++ s097_forbidden_needles()
            )
        end},
        {"gles 3.0", fun() ->
            s097_assert_emitted_surface(
                {gles, {3, 0}},
                [],
                [],
                [
                    <<"-export([sample_mask/2]).">>,
                    <<"-export([viewport/5]).">>,
                    <<"-export([polygon_offset_clamp/3]).">>,
                    <<"-export([get_graphics_reset_status/0]).">>,
                    <<"glSampleMaski(">>,
                    <<"glViewportIndexedf(">>,
                    <<"glPolygonOffsetClamp(">>,
                    <<"glGetGraphicsResetStatus(">>
                ] ++ s097_forbidden_needles()
            )
        end}
    ].

s097_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s097_assert_presence(Target, Functions),
    s097_assert_deferred_neighbors_absent(Functions),
    [
        s097_assert_path(Function, BindingData, maps:get(Function, Functions))
     || Function <- s097_present_functions(Target)
    ].

s097_assert_presence(Target, Functions) ->
    Present = s097_present_functions(Target),
    Absent = s097_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s097_present_functions({gl, {3, 3}}) ->
    [{"sample_mask", 2}];
s097_present_functions({gl, {4, 1}}) ->
    [{"sample_mask", 2}, {"viewport", 5}];
s097_present_functions({gl, {4, 6}}) ->
    s097_all_functions();
s097_present_functions({gles, {3, 1}}) ->
    [{"sample_mask", 2}];
s097_present_functions({gles, {3, 2}}) ->
    [{"sample_mask", 2}, {"get_graphics_reset_status", 0}];
s097_present_functions(_) ->
    [].

s097_all_functions() ->
    [
        {"sample_mask", 2},
        {"viewport", 5},
        {"polygon_offset_clamp", 3},
        {"get_graphics_reset_status", 0}
    ].

s097_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s097_assert_path({"sample_mask", 2}, _BindingData, FunctionData) ->
    s097_assert_direct(
        FunctionData,
        "glSampleMaski",
        [{in, "MaskNumber", gl_uint}, {in, "Mask", gl_uint}],
        [{"MaskNumber", {gl, uint, []}}, {"Mask", {gl, uint, []}}],
        [{"MaskNumber", do_nothing}, {"Mask", do_nothing}],
        [{"MaskNumber", s097_uint_nif_data()}, {"Mask", s097_uint_nif_data()}],
        gl_void,
        []
    );
s097_assert_path({"viewport", 5}, _BindingData, FunctionData) ->
    s097_assert_direct(
        FunctionData,
        "glViewportIndexedf",
        [
            {in, "Index", gl_uint},
            {in, "X", gl_float},
            {in, "Y", gl_float},
            {in, "Width", gl_float},
            {in, "Height", gl_float}
        ],
        [
            {"Index", {gl, uint, []}},
            {"X", {gl, float, []}},
            {"Y", {gl, float, []}},
            {"Width", {gl, float, []}},
            {"Height", {gl, float, []}}
        ],
        [
            {"Index", do_nothing},
            {"X", do_nothing},
            {"Y", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        [
            {"Index", s097_uint_nif_data()},
            {"X", s097_float_nif_data()},
            {"Y", s097_float_nif_data()},
            {"Width", s097_float_nif_data()},
            {"Height", s097_float_nif_data()}
        ],
        gl_void,
        []
    );
s097_assert_path({"polygon_offset_clamp", 3}, _BindingData, FunctionData) ->
    s097_assert_direct(
        FunctionData,
        "glPolygonOffsetClamp",
        [{in, "Factor", gl_float}, {in, "Units", gl_float}, {in, "Clamp", gl_float}],
        [{"Factor", {gl, float, []}}, {"Units", {gl, float, []}}, {"Clamp", {gl, float, []}}],
        [{"Factor", do_nothing}, {"Units", do_nothing}, {"Clamp", do_nothing}],
        [{"Factor", s097_float_nif_data()}, {"Units", s097_float_nif_data()}, {"Clamp", s097_float_nif_data()}],
        gl_void,
        []
    );
s097_assert_path({"get_graphics_reset_status", 0}, BindingData, FunctionData) ->
    s097_assert_enum_contains(BindingData, "graphics_reset_status", "no_error"),
    s097_assert_direct(
        FunctionData,
        "glGetGraphicsResetStatus",
        [],
        [],
        [],
        [],
        {"Status", {gl_enum, "GraphicsResetStatus"}},
        [{"Status", {undefined, graphics_reset_status, []}}]
    ),
    NifData = maps:get("glGetGraphicsResetStatus", maps:get(nif_functions, FunctionData)),
    {glenum_to_atom, TransformMap} = maps:get(return, NifData),
    ?assert(lists:keymember("GL_NO_ERROR", 1, TransformMap)).

s097_assert_direct(
    FunctionData,
    GlCommand,
    ParamsSpecs,
    SpecsParams,
    ClauseParams,
    NifParams,
    ReturnSpecs,
    SpecsReturn
) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(ReturnSpecs, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual(SpecsReturn, maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    ?assertEqual(ClauseParams, maps:get(params, Clause)),

    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    case ReturnSpecs of
        gl_void -> ?assertEqual(void, maps:get(return, NifData));
        _ -> ok
    end.

s097_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s097_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard97-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s097_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s097_assert_contains(C, Needle) || Needle <- RequiredC],
        [s097_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s097_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s097_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s097_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s097_forbidden_needles() ->
    [
        <<"glGetnTexImage">>
    ].

s097_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s097_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 105.
s105_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s105_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s105_emitter_vertex_attrib_l_test_() ->
    [
        {"gl 4.6", fun() -> s105_assert_emitted_surface({gl, {4, 6}}, true) end},
        {"gl 4.1", fun() -> s105_assert_emitted_surface({gl, {4, 1}}, true) end},
        {"gl 3.3", fun() -> s105_assert_emitted_surface({gl, {3, 3}}, false) end},
        {"gles 3.2", fun() -> s105_assert_emitted_surface({gles, {3, 2}}, false) end}
    ].

s105_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s105_supports_vertex_attrib_l(Target) of
        true ->
            ?assert(maps:is_key({"vertex_attrib_l", 3}, Functions)),
            s105_assert_vertex_attrib_l(maps:get({"vertex_attrib_l", 3}, Functions));
        false ->
            ?assertNot(maps:is_key({"vertex_attrib_l", 3}, Functions)),
            s105_assert_commands_absent(Functions, ["glVertexAttribL1d"])
    end,
    s105_assert_direct_wrappers_absent(Functions),
    s105_assert_commands_absent(Functions, [
        "glVertexAttribL1dv",
        "glVertexAttribL2dv",
        "glVertexAttribL3dv",
        "glVertexAttribL4dv"
    ]).

s105_supports_vertex_attrib_l({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s105_supports_vertex_attrib_l({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s105_supports_vertex_attrib_l(_) ->
    false.

s105_assert_vertex_attrib_l(VertexAttribL) ->
    ?assertEqual(
        [
            {in, "Index", gl_uint},
            {in, "Values", gl_x}
        ],
        maps:get(params_specs, VertexAttribL)
    ),
    ?assertEqual(
        [
            {"Type", {set, [d]}},
            {"Index", {gl, uint, []}},
            {"Values", {undefined, vertex_attrib_l_value, []}}
        ],
        maps:get(specs_params, VertexAttribL)
    ),
    ?assertEqual([], maps:get(specs_return, VertexAttribL)),
    ?assertEqual(3, maps:get(function_arity, VertexAttribL)),
    s105_assert_extra_type_family(VertexAttribL),
    s105_assert_command_family(VertexAttribL),
    s105_assert_clause_family(VertexAttribL),
    s105_assert_nif_family(VertexAttribL).

s105_assert_extra_type_family(VertexAttribL) ->
    {vertex_attrib_l_value, {set, ExtraTypeVariants}} = maps:get(extra_type, VertexAttribL),
    ScalarSpec = {gl, double, []},
    [?assert(lists:member(TypeSpec, ExtraTypeVariants)) || TypeSpec <- [
        ScalarSpec,
        {undefined, vector2, [ScalarSpec]},
        {undefined, vector3, [ScalarSpec]},
        {undefined, vector4, [ScalarSpec]}
    ]].

s105_assert_command_family(VertexAttribL) ->
    GlCommands = maps:get(gl_commands, VertexAttribL),
    [?assert(lists:member(Command, GlCommands)) || Command <- [
        {"glVertexAttribL1d", gl_double, element},
        {"glVertexAttribL2d", {gl_vector, 2, gl_double}, element},
        {"glVertexAttribL3d", {gl_vector, 3, gl_double}, element},
        {"glVertexAttribL4d", {gl_vector, 4, gl_double}, element}
    ]],
    Variants = maps:get(variants, VertexAttribL),
    [?assert(lists:member(Variant, Variants)) || Variant <- [
        {gl_double, element},
        {{gl_vector, 2, gl_double}, element},
        {{gl_vector, 3, gl_double}, element},
        {{gl_vector, 4, gl_double}, element}
    ]].

s105_assert_clause_family(VertexAttribL) ->
    Clauses = maps:get(function_clauses, VertexAttribL),
    RawOrder = [maps:get(raw_function, Clause) || Clause <- Clauses],
    s105_assert_before("glVertexAttribL2d", "glVertexAttribL1d", RawOrder),
    s105_assert_before("glVertexAttribL3d", "glVertexAttribL1d", RawOrder),
    s105_assert_before("glVertexAttribL4d", "glVertexAttribL1d", RawOrder),
    s105_assert_vector_clause("glVertexAttribL2d", 2, Clauses),
    s105_assert_vector_clause("glVertexAttribL3d", 3, Clauses),
    s105_assert_vector_clause("glVertexAttribL4d", 4, Clauses),
    s105_assert_scalar_clause("glVertexAttribL1d", Clauses).

s105_assert_vector_clause(RawFunction, VectorSize, Clauses) ->
    Clause = s105_find_clause(RawFunction, Clauses),
    ?assertEqual("Values", maps:get(guard_var, Clause)),
    ?assertEqual(
        [{is_tuple, var}, {tuple_size, var, VectorSize}],
        maps:get(guards, Clause)
    ),
    ?assertEqual(
        [
            {"d", ignore},
            {"Index", do_nothing},
            {"Values", {gl_vector_to_list, VectorSize}}
        ],
        maps:get(params, Clause)
    ).

s105_assert_scalar_clause(RawFunction, Clauses) ->
    Clause = s105_find_clause(RawFunction, Clauses),
    ?assertEqual([], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {"d", ignore},
            {"Index", do_nothing},
            {"Values", do_nothing}
        ],
        maps:get(params, Clause)
    ).

s105_assert_nif_family(VertexAttribL) ->
    NifFunctions = maps:get(nif_functions, VertexAttribL),
    ValueSpec = {"GLdouble", "double", "enif_get_double", "enif_make_double"},
    s105_assert_scalar_nif(maps:get("glVertexAttribL1d", NifFunctions), ValueSpec),
    s105_assert_vector_nif(maps:get("glVertexAttribL2d", NifFunctions), 2, ValueSpec),
    s105_assert_vector_nif(maps:get("glVertexAttribL3d", NifFunctions), 3, ValueSpec),
    s105_assert_vector_nif(maps:get("glVertexAttribL4d", NifFunctions), 4, ValueSpec).

s105_assert_scalar_nif(NifData, ValueSpec) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Index", {gl_type, s105_gl_uint_spec()}},
            {"Values", {gl_type, ValueSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s105_assert_vector_nif(NifData, VectorSize, ValueSpec) ->
    ?assertEqual(VectorSize + 1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Index", {gl_type, s105_gl_uint_spec()}}]
            ++ [{"V" ++ integer_to_list(N), {gl_type, ValueSpec}} || N <- lists:seq(1, VectorSize)],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s105_assert_direct_wrappers_absent(Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- [
        {"vertex_attrib_l_1d", 2},
        {"vertex_attrib_l_2d", 3},
        {"vertex_attrib_l_3d", 4},
        {"vertex_attrib_l_4d", 5}
    ]].

s105_assert_commands_absent(Functions, Commands) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- Commands].

s105_assert_emitted_surface(Target, ExpectedPresent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard105-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case ExpectedPresent of
            true ->
                s105_assert_contains(Erl, <<"-export([vertex_attrib_l/3]).">>),
                s105_assert_contains(Erl, <<"-spec vertex_attrib_l(\n    Type :: d,">>),
                s105_assert_contains(Erl, <<"vertex_attrib_l_value() ::">>),
                s105_assert_contains(Erl, <<"vector4(gl:double())">>),
                s105_assert_contains(Erl, <<"?CALL_RAW_FUNC(glVertexAttribL1d_raw(Index, Values))">>),
                s105_assert_contains(Erl, <<"?CALL_RAW_FUNC(glVertexAttribL4d_raw(Index, V1, V2, V3, V4))">>),
                s105_assert_contains(C, <<"glVertexAttribL1d(arg_0, arg_1);">>),
                s105_assert_contains(C, <<"glVertexAttribL4d(arg_0, arg_1, arg_2, arg_3, arg_4);">>);
            false ->
                s105_assert_not_contains(Erl, <<"-export([vertex_attrib_l/3]).">>),
                s105_assert_not_contains(C, <<"glVertexAttribL1d(">>)
        end,
        s105_assert_not_contains(Erl, <<"-export([vertex_attrib_l_1d/2]).">>),
        s105_assert_not_contains(Erl, <<"glVertexAttribL1dv_raw">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s105_gl_uint_spec() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s105_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s105_assert_before(First, Second, Values) ->
    ?assert(s105_index_of(First, Values) < s105_index_of(Second, Values)).

s105_index_of(Value, Values) ->
    s105_index_of(Value, Values, 1).

s105_index_of(Value, [Value | _], Index) ->
    Index;
s105_index_of(Value, [_ | Rest], Index) ->
    s105_index_of(Value, Rest, Index + 1).

s105_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s105_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 116.
s116_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s116_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s116_emitter_fixed_vector_pointer_test_() ->
    [
        {"gl 4.6", fun() -> s116_assert_emitted_surface({gl, {4, 6}}, true) end},
        {"gles 3.2", fun() -> s116_assert_emitted_surface({gles, {3, 2}}, false) end}
    ].

s116_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s116_is_desktop(Target) of
        true ->
            VertexAttrib = maps:get({"vertex_attrib", 3}, Functions),
            VertexAttribI = maps:get({"vertex_attrib_i", 3}, Functions),
            s116_assert_generic_fixed_vectors(VertexAttrib),
            s116_assert_integer_fixed_vectors(VertexAttribI);
        false ->
            s116_assert_commands_absent(Functions, s116_generic_commands() ++ s116_integer_commands())
    end,
    s116_assert_duplicate_pointer_spellings_absent(Functions),
    s116_assert_direct_wrappers_absent(Functions).

s116_assert_generic_fixed_vectors(VertexAttrib) ->
    s116_assert_type_atoms(VertexAttrib, [b, i, ub, ui, us]),
    s116_assert_vector4_only_specs(VertexAttrib, [
        {b, {gl, byte, []}},
        {i, {gl, int, []}},
        {ub, {gl, ubyte, []}},
        {ui, {gl, uint, []}},
        {us, {gl, ushort, []}}
    ]),
    s116_assert_single_vector_commands(VertexAttrib, s116_generic_command_specs()),
    s116_assert_single_vector_clauses(VertexAttrib, s116_generic_command_specs()),
    s116_assert_single_vector_nifs(VertexAttrib, s116_generic_command_specs()).

s116_assert_integer_fixed_vectors(VertexAttribI) ->
    s116_assert_type_atoms(VertexAttribI, [b, s, ub, us]),
    s116_assert_vector4_only_specs(VertexAttribI, [
        {b, {gl, byte, []}},
        {s, {gl, short, []}},
        {ub, {gl, ubyte, []}},
        {us, {gl, ushort, []}}
    ]),
    s116_assert_single_vector_commands(VertexAttribI, s116_integer_command_specs()),
    s116_assert_single_vector_clauses(VertexAttribI, s116_integer_command_specs()),
    s116_assert_single_vector_nifs(VertexAttribI, s116_integer_command_specs()).

s116_assert_type_atoms(FunctionData, Atoms) ->
    [{"Type", {set, TypeAtoms}}, {"Index", {gl, uint, []}}, {"Values", _}] =
        maps:get(specs_params, FunctionData),
    [?assert(lists:member(Atom, TypeAtoms)) || Atom <- Atoms],
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)).

s116_assert_vector4_only_specs(FunctionData, TypeSpecs) ->
    {_ExtraTypeName, {set, ExtraTypeVariants}} = maps:get(extra_type, FunctionData),
    [begin
        ?assert(lists:member({undefined, vector4, [Spec]}, ExtraTypeVariants)),
        ?assertNot(lists:member(Spec, ExtraTypeVariants)),
        ?assertNot(lists:member({undefined, vector2, [Spec]}, ExtraTypeVariants)),
        ?assertNot(lists:member({undefined, vector3, [Spec]}, ExtraTypeVariants))
    end || {_Atom, Spec} <- TypeSpecs].

s116_assert_single_vector_commands(FunctionData, CommandSpecs) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    Variants = maps:get(variants, FunctionData),
    [begin
        ?assert(lists:member({Command, {gl_vector, 4, GlType}, single_vector}, GlCommands)),
        ?assert(lists:member({{gl_vector, 4, GlType}, single_vector}, Variants))
    end || {Command, _Atom, GlType, _NifSpec} <- CommandSpecs].

s116_assert_single_vector_clauses(FunctionData, CommandSpecs) ->
    Clauses = maps:get(function_clauses, FunctionData),
    [begin
        Clause = s116_find_clause(Command, Clauses),
        ?assertEqual("Values", maps:get(guard_var, Clause)),
        ?assertEqual([{is_tuple, var}, {tuple_size, var, 4}], maps:get(guards, Clause)),
        ?assertEqual(
            [
                {atom_to_list(Atom), ignore},
                {"Index", do_nothing},
                {"Values", {gl_vector_to_pointer_list, 4}}
            ],
            maps:get(params, Clause)
        )
    end || {Command, Atom, _GlType, _NifSpec} <- CommandSpecs].

s116_assert_single_vector_nifs(FunctionData, CommandSpecs) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    [begin
        NifData = maps:get(Command, NifFunctions),
        ?assertEqual(2, maps:get(arity, NifData)),
        ?assertEqual(
            [
                {"Index", {gl_type, s116_gl_uint_spec()}},
                {"Values", {list_gl_type, NifSpec}}
            ],
            maps:get(params, NifData)
        ),
        ?assertEqual(void, maps:get(return, NifData))
    end || {Command, _Atom, _GlType, NifSpec} <- CommandSpecs].

s116_assert_duplicate_pointer_spellings_absent(Functions) ->
    s116_assert_commands_absent(Functions, [
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
        "glVertexAttribL4dv"
    ]).

s116_assert_direct_wrappers_absent(Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- [
        {"vertex_attrib_4bv", 2},
        {"vertex_attrib_4iv", 2},
        {"vertex_attrib_4ubv", 2},
        {"vertex_attrib_4uiv", 2},
        {"vertex_attrib_4usv", 2},
        {"vertex_attrib_i_4bv", 2},
        {"vertex_attrib_i_4sv", 2},
        {"vertex_attrib_i_4ubv", 2},
        {"vertex_attrib_i_4usv", 2}
    ]].

s116_assert_commands_absent(Functions, Commands) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- Commands].

s116_assert_emitted_surface(Target, ExpectedPresent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard116-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case ExpectedPresent of
            true ->
                s116_assert_contains(Erl, <<"glVertexAttrib4bv_raw(Index, NewValues)">>),
                s116_assert_contains(Erl, <<"glVertexAttribI4sv_raw(Index, NewValues)">>),
                s116_assert_contains(C, <<"GLbyte* arg_1_array = enif_alloc(sizeof(GLbyte) * arg_1_length);">>),
                s116_assert_contains(C, <<"GLshort* arg_1_array = enif_alloc(sizeof(GLshort) * arg_1_length);">>),
                s116_assert_contains(C, <<"glVertexAttrib4bv(arg_0, arg_1_array);">>),
                s116_assert_contains(C, <<"glVertexAttribI4sv(arg_0, arg_1_array);">>),
                s116_assert_contains(C, <<"{\"glVertexAttrib4bv_raw\", 2, nif_glVertexAttrib4bv, 0}">>),
                s116_assert_contains(C, <<"{\"glVertexAttribI4sv_raw\", 2, nif_glVertexAttribI4sv, 0}">>);
            false ->
                s116_assert_not_contains(Erl, <<"glVertexAttrib4bv_raw">>),
                s116_assert_not_contains(Erl, <<"glVertexAttribI4sv_raw">>),
                s116_assert_not_contains(C, <<"glVertexAttrib4bv(">>),
                s116_assert_not_contains(C, <<"glVertexAttribI4sv(">>)
        end,
        s116_assert_not_contains(Erl, <<"glVertexAttrib4fv_raw">>),
        s116_assert_not_contains(Erl, <<"glVertexAttribI4iv_raw">>),
        s116_assert_not_contains(Erl, <<"-export([vertex_attrib_4bv/2]).">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s116_generic_command_specs() ->
    [
        {"glVertexAttrib4bv", b, gl_byte, {"GLbyte", "int", "enif_get_int", "enif_make_int"}},
        {"glVertexAttrib4iv", i, gl_int, {"GLint", "int", "enif_get_int", "enif_make_int"}},
        {"glVertexAttrib4ubv", ub, gl_ubyte, {"GLubyte", "unsigned int", "enif_get_uint", "enif_make_uint"}},
        {"glVertexAttrib4uiv", ui, gl_uint, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}},
        {"glVertexAttrib4usv", us, gl_ushort, {"GLushort", "unsigned int", "enif_get_uint", "enif_make_uint"}}
    ].

s116_integer_command_specs() ->
    [
        {"glVertexAttribI4bv", b, gl_byte, {"GLbyte", "int", "enif_get_int", "enif_make_int"}},
        {"glVertexAttribI4sv", s, gl_short, {"GLshort", "int", "enif_get_int", "enif_make_int"}},
        {"glVertexAttribI4ubv", ub, gl_ubyte, {"GLubyte", "unsigned int", "enif_get_uint", "enif_make_uint"}},
        {"glVertexAttribI4usv", us, gl_ushort, {"GLushort", "unsigned int", "enif_get_uint", "enif_make_uint"}}
    ].

s116_generic_commands() ->
    [Command || {Command, _Atom, _GlType, _NifSpec} <- s116_generic_command_specs()].

s116_integer_commands() ->
    [Command || {Command, _Atom, _GlType, _NifSpec} <- s116_integer_command_specs()].

s116_is_desktop({gl, _Version}) ->
    true;
s116_is_desktop(_) ->
    false.

s116_gl_uint_spec() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s116_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s116_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s116_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 117.
s117_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s117_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s117_emitter_normalized_attribute_test_() ->
    [
        {"gl 4.6", fun() -> s117_assert_emitted_surface({gl, {4, 6}}, true) end},
        {"gles 3.2", fun() -> s117_assert_emitted_surface({gles, {3, 2}}, false) end}
    ].

s117_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s117_is_desktop(Target) of
        true ->
            VertexAttribN = maps:get({"vertex_attrib_n", 3}, Functions),
            s117_assert_specs(VertexAttribN),
            s117_assert_commands(VertexAttribN),
            s117_assert_clauses(VertexAttribN),
            s117_assert_nifs(VertexAttribN);
        false ->
            ?assertNot(maps:is_key({"vertex_attrib_n", 3}, Functions)),
            s117_assert_commands_absent(Functions, s117_enabled_commands())
    end,
    s117_assert_commands_absent(Functions, s117_duplicate_or_deferred_commands()),
    s117_assert_direct_wrappers_absent(Functions).

s117_assert_specs(FunctionData) ->
    {vertex_attrib_n_value, {set, ExtraTypeVariants}} = maps:get(extra_type, FunctionData),
    [{"Type", {set, TypeAtoms}}, {"Index", {gl, uint, []}}, {"Values", {undefined, vertex_attrib_n_value, []}}] =
        maps:get(specs_params, FunctionData),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),
    [
        begin
            ?assert(lists:member(Atom, TypeAtoms)),
            ?assert(lists:member({undefined, vector4, [Spec]}, ExtraTypeVariants)),
            ?assertNot(lists:member(Spec, ExtraTypeVariants)),
            ?assertNot(lists:member({undefined, vector2, [Spec]}, ExtraTypeVariants)),
            ?assertNot(lists:member({undefined, vector3, [Spec]}, ExtraTypeVariants))
        end
     || {_Command, Atom, _GlType, Spec, _NifSpec, _Form} <- s117_command_specs()
    ].

s117_assert_commands(FunctionData) ->
    GlCommands = maps:get(gl_commands, FunctionData),
    Variants = maps:get(variants, FunctionData),
    [
        begin
            ?assert(lists:member({Command, {gl_vector, 4, GlType}, Form}, GlCommands)),
            ?assert(lists:member({{gl_vector, 4, GlType}, Form}, Variants))
        end
     || {Command, _Atom, GlType, _Spec, _NifSpec, Form} <- s117_command_specs()
    ].

s117_assert_clauses(FunctionData) ->
    Clauses = maps:get(function_clauses, FunctionData),
    [
        s117_assert_clause(s117_find_clause(Command, Clauses), Atom, Form)
     || {Command, Atom, _GlType, _Spec, _NifSpec, Form} <- s117_command_specs()
    ].

s117_assert_clause(Clause, Atom, element) ->
    ?assertEqual("Values", maps:get(guard_var, Clause)),
    ?assertEqual([{is_tuple, var}, {tuple_size, var, 4}], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {atom_to_list(Atom), ignore},
            {"Index", do_nothing},
            {"Values", {gl_vector_to_list, 4}}
        ],
        maps:get(params, Clause)
    );
s117_assert_clause(Clause, Atom, single_vector) ->
    ?assertEqual("Values", maps:get(guard_var, Clause)),
    ?assertEqual([{is_tuple, var}, {tuple_size, var, 4}], maps:get(guards, Clause)),
    ?assertEqual(
        [
            {atom_to_list(Atom), ignore},
            {"Index", do_nothing},
            {"Values", {gl_vector_to_pointer_list, 4}}
        ],
        maps:get(params, Clause)
    ).

s117_assert_nifs(FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    [
        s117_assert_nif(maps:get(Command, NifFunctions), NifSpec, Form)
     || {Command, _Atom, _GlType, _Spec, NifSpec, Form} <- s117_command_specs()
    ].

s117_assert_nif(NifData, NifSpec, element) ->
    ?assertEqual(5, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Index", {gl_type, s117_gl_uint_spec()}}]
            ++ [{"V" ++ integer_to_list(N), {gl_type, NifSpec}} || N <- lists:seq(1, 4)],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData));
s117_assert_nif(NifData, NifSpec, single_vector) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Index", {gl_type, s117_gl_uint_spec()}},
            {"Values", {list_gl_type, NifSpec}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s117_assert_emitted_surface(Target, ExpectedPresent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard117-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        case ExpectedPresent of
            true ->
                s117_assert_contains(Erl, <<"-spec vertex_attrib_n(">>),
                s117_assert_contains(Erl, <<"glVertexAttrib4Nub_raw(Index, V1, V2, V3, V4)">>),
                s117_assert_contains(Erl, <<"glVertexAttrib4Nsv_raw(Index, NewValues)">>),
                s117_assert_contains(C, <<"glVertexAttrib4Nub(arg_0, arg_1, arg_2, arg_3, arg_4);">>),
                s117_assert_contains(C, <<"glVertexAttrib4Nsv(arg_0, arg_1_array);">>),
                s117_assert_contains(C, <<"{\"glVertexAttrib4Nub_raw\", 5, nif_glVertexAttrib4Nub, 0}">>),
                s117_assert_contains(C, <<"{\"glVertexAttrib4Nsv_raw\", 2, nif_glVertexAttrib4Nsv, 0}">>);
            false ->
                s117_assert_not_contains(Erl, <<"vertex_attrib_n(">>),
                s117_assert_not_contains(C, <<"glVertexAttrib4Nub(">>),
                s117_assert_not_contains(C, <<"glVertexAttrib4Nsv(">>)
        end,
        s117_assert_not_contains(Erl, <<"glVertexAttrib4Nubv_raw">>),
        s117_assert_not_contains(C, <<"glVertexAttrib4Nubv(">>),
        s117_assert_not_contains(Erl, <<"-export([vertex_attrib_n_4nub/5]).">>)
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s117_assert_direct_wrappers_absent(Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- [
        {"vertex_attrib_4_nbv", 2},
        {"vertex_attrib_4_niv", 2},
        {"vertex_attrib_4_nsv", 2},
        {"vertex_attrib_4_nub", 5},
        {"vertex_attrib_4_nubv", 2},
        {"vertex_attrib_4_nuiv", 2},
        {"vertex_attrib_4_nusv", 2}
    ]].

s117_assert_commands_absent(Functions, Commands) ->
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- Commands].

s117_command_specs() ->
    [
        {"glVertexAttrib4Nbv", b, gl_byte, {gl, byte, []}, {"GLbyte", "int", "enif_get_int", "enif_make_int"}, single_vector},
        {"glVertexAttrib4Niv", i, gl_int, {gl, int, []}, {"GLint", "int", "enif_get_int", "enif_make_int"}, single_vector},
        {"glVertexAttrib4Nsv", s, gl_short, {gl, short, []}, {"GLshort", "int", "enif_get_int", "enif_make_int"}, single_vector},
        {"glVertexAttrib4Nub", ub, gl_ubyte, {gl, ubyte, []}, {"GLubyte", "unsigned int", "enif_get_uint", "enif_make_uint"}, element},
        {"glVertexAttrib4Nuiv", ui, gl_uint, {gl, uint, []}, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}, single_vector},
        {"glVertexAttrib4Nusv", us, gl_ushort, {gl, ushort, []}, {"GLushort", "unsigned int", "enif_get_uint", "enif_make_uint"}, single_vector}
    ].

s117_enabled_commands() ->
    [Command || {Command, _Atom, _GlType, _Spec, _NifSpec, _Form} <- s117_command_specs()].

s117_duplicate_or_deferred_commands() ->
    [
        "glVertexAttrib4Nubv",
        "glVertexAttribP1uiv",
        "glVertexAttribP2uiv",
        "glVertexAttribP3uiv",
        "glVertexAttribP4uiv"
    ].

s117_is_desktop({gl, _Version}) ->
    true;
s117_is_desktop(_) ->
    false.

s117_gl_uint_spec() ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}.

s117_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s117_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s117_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 140.
s140_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s140_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s140_emitter_texture_upload_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s140_assert_emitted_surface(
                {gl, {4, 6}},
                s140_all_exports(),
                s140_all_c_calls(),
                []
            )
        end},
        {"gles 3.2", fun() ->
            s140_assert_emitted_surface(
                {gles, {3, 2}},
                s140_es3_exports(),
                s140_es3_c_calls(),
                s140_one_d_exports() ++ s140_one_d_c_calls()
            )
        end},
        {"gles 2.0", fun() ->
            s140_assert_emitted_surface(
                {gles, {2, 0}},
                s140_es2_exports(),
                s140_es2_c_calls(),
                s140_one_d_exports() ++ s140_one_d_c_calls() ++ s140_three_d_exports() ++ s140_three_d_c_calls()
            )
        end}
    ].

s140_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s140_assert_presence(Target, Functions),
    s140_assert_enum_types(BindingData),
    s140_assert_deferred_neighbors_absent(Functions),
    s140_assert_present_paths(Target, Functions).

s140_assert_presence(Target, Functions) ->
    Present = s140_present_functions(Target),
    Absent = s140_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s140_present_functions({gl, _Version}) ->
    s140_all_functions();
s140_present_functions({gles, {2, 0}}) ->
    s140_two_d_functions();
s140_present_functions({gles, _Version}) ->
    s140_two_d_functions() ++ s140_three_d_functions().

s140_all_functions() ->
    s140_one_d_functions() ++ s140_two_d_functions() ++ s140_three_d_functions().

s140_one_d_functions() ->
    [
        {"tex_image_1d", 8},
        {"tex_sub_image_1d", 7}
    ].

s140_two_d_functions() ->
    [
        {"tex_image_2d", 9},
        {"tex_sub_image_2d", 9}
    ].

s140_three_d_functions() ->
    [
        {"tex_image_3d", 10},
        {"tex_sub_image_3d", 11}
    ].

s140_assert_enum_types(BindingData) ->
    s140_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s140_assert_enum_contains(BindingData, "internal_format", "rgba"),
    s140_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s140_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s140_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s140_assert_present_paths(Target, Functions) ->
    [
        s140_assert_path(Function, Target, maps:get(Function, Functions))
     || Function <- s140_present_functions(Target)
    ].

s140_assert_path({"tex_image_1d", 8}, _Target, FunctionData) ->
    s140_assert_tex_image(
        FunctionData,
        "glTexImage1D",
        [{"Width", gl_sizei}]
    );
s140_assert_path({"tex_image_2d", 9}, _Target, FunctionData) ->
    s140_assert_tex_image(
        FunctionData,
        "glTexImage2D",
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s140_assert_path({"tex_image_3d", 10}, _Target, FunctionData) ->
    s140_assert_tex_image(
        FunctionData,
        "glTexImage3D",
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    );
s140_assert_path({"tex_sub_image_1d", 7}, _Target, FunctionData) ->
    s140_assert_tex_sub_image(
        FunctionData,
        "glTexSubImage1D",
        [{"Offset", gl_int}],
        [{"Width", gl_sizei}]
    );
s140_assert_path({"tex_sub_image_2d", 9}, _Target, FunctionData) ->
    s140_assert_tex_sub_image(
        FunctionData,
        "glTexSubImage2D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s140_assert_path({"tex_sub_image_3d", 11}, _Target, FunctionData) ->
    s140_assert_tex_sub_image(
        FunctionData,
        "glTexSubImage3D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}, {"OffsetZ", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    ).

s140_assert_tex_image(FunctionData, Command, Dimensions) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ExpectedParams =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}}
        ] ++
            [{in, Name, Type} || {Name, Type} <- Dimensions] ++
            [
                {in, "Border", gl_int},
                {in, "Format", {gl_enum, "PixelFormat"}},
                {in, "Type", {gl_enum, "PixelType"}},
                {in, "Pixels", byte_data_pointer_or_none}
            ],
    ?assertEqual(ExpectedParams, maps:get(params_specs, FunctionData)),
    s140_assert_specs_params(FunctionData, Dimensions, true),
    s140_assert_clause_params(FunctionData, byte_data_pointer_or_none),
    s140_assert_nif_params(FunctionData, Command, Dimensions, true).

s140_assert_tex_sub_image(FunctionData, Command, Offsets, Dimensions) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ExpectedParams =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int}
        ] ++
            [{in, Name, Type} || {Name, Type} <- Offsets] ++
            [{in, Name, Type} || {Name, Type} <- Dimensions] ++
            [
                {in, "Format", {gl_enum, "PixelFormat"}},
                {in, "Type", {gl_enum, "PixelType"}},
                {in, "Pixels", byte_data_pointer}
            ],
    ?assertEqual(ExpectedParams, maps:get(params_specs, FunctionData)),
    s140_assert_specs_params(FunctionData, Offsets ++ Dimensions, false),
    s140_assert_clause_params(FunctionData, byte_data_pointer),
    s140_assert_nif_params(FunctionData, Command, Offsets ++ Dimensions, false).

s140_assert_specs_params(FunctionData, MiddleParams, AllowsNone) ->
    PixelSpec = case AllowsNone of
        true -> {set, [{undefined, iodata, []}, none]};
        false -> {undefined, iodata, []}
    end,
    Expected =
        [
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}}
        ] ++
            s140_maybe_internal_format(FunctionData) ++
            [{Name, s140_type_spec(Type)} || {Name, Type} <- MiddleParams] ++
            s140_maybe_border(FunctionData) ++
            [
                {"Format", {undefined, pixel_format, []}},
                {"Type", {undefined, pixel_type, []}},
                {"Pixels", PixelSpec}
            ],
    ?assertEqual(Expected, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(Expected), maps:get(function_arity, FunctionData)).

s140_maybe_internal_format(FunctionData) ->
    case s140_is_tex_image_command(maps:get(gl_command, FunctionData)) of
        true -> [{"InternalFormat", {undefined, internal_format, []}}];
        false -> []
    end.

s140_maybe_border(FunctionData) ->
    case s140_is_tex_image_command(maps:get(gl_command, FunctionData)) of
        true -> [{"Border", {gl, int, []}}];
        false -> []
    end.

s140_assert_clause_params(FunctionData, PixelRule) ->
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assertEqual({"Pixels", PixelRule}, lists:last(Params)),
    ?assertEqual(maps:get(gl_command, FunctionData), maps:get(raw_function, Clause)).

s140_assert_nif_params(FunctionData, Command, MiddleParams, AllowsNone) ->
    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    Expected =
        [
            {"Target", s140_enum_nif_data()},
            {"Level", s140_int_nif_data()}
        ] ++
            s140_maybe_internal_format_nif(Command) ++
            [{Name, s140_nif_type(Type)} || {Name, Type} <- MiddleParams] ++
            s140_maybe_border_nif(Command) ++
            [
                {"Format", s140_enum_nif_data()},
                {"Type", s140_enum_nif_data()},
                {"Pixels", s140_pixel_nif_data(AllowsNone)}
            ],
    ?assertEqual(Expected, maps:get(params, NifData)),
    ?assertEqual(length(Expected), maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s140_maybe_internal_format_nif(Command) ->
    case s140_is_tex_image_command(Command) of
        true -> [{"InternalFormat", s140_enum_nif_data()}];
        false -> []
    end.

s140_maybe_border_nif(Command) ->
    case s140_is_tex_image_command(Command) of
        true -> [{"Border", s140_int_nif_data()}];
        false -> []
    end.

s140_is_tex_image_command(Command) ->
    lists:prefix("glTexImage", Command).

s140_type_spec(gl_int) -> {gl, int, []};
s140_type_spec(gl_sizei) -> {gl, sizei, []}.

s140_nif_type(gl_int) -> s140_int_nif_data();
s140_nif_type(gl_sizei) -> s140_sizei_nif_data().

s140_pixel_nif_data(true) -> in_gl_binary_or_null;
s140_pixel_nif_data(false) -> binary_to_glbinary.

s140_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s140_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s140_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s140_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard140-emitter-" ++
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

        [s140_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s140_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s140_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s140_assert_not_contains(C, Pattern) || Pattern <- Forbidden],

        s140_assert_contains(Erl, <<"Pixels0 = case Pixels of\n        none -> undefined;\n        _ -> iolist_to_binary(Pixels)\n    end,">>),
        s140_assert_contains(Erl, <<"Pixels0 = iolist_to_binary(Pixels),">>),
        s140_assert_contains(C, <<"if (enif_is_identical(argv[">>),
        s140_assert_contains(C, <<"enif_make_atom(env, \"undefined\")">>),
        s140_assert_contains(C, <<"if (!enif_inspect_binary(env, argv[">>)
    after
        ok = file:set_cwd(Cwd)
    end.

s140_all_exports() ->
    s140_one_d_exports() ++ s140_two_d_exports() ++ s140_three_d_exports().

s140_es3_exports() ->
    s140_two_d_exports() ++ s140_three_d_exports().

s140_es2_exports() ->
    s140_two_d_exports().

s140_one_d_exports() ->
    [
        <<"-export([tex_image_1d/8]).">>,
        <<"-export([tex_sub_image_1d/7]).">>
    ].

s140_two_d_exports() ->
    [
        <<"-export([tex_image_2d/9]).">>,
        <<"-export([tex_sub_image_2d/9]).">>,
        <<"-spec tex_image_2d(\n    Target :: texture_target(),\n    Level :: gl:int(),\n    InternalFormat :: internal_format(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Border :: gl:int(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Pixels :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"-spec tex_sub_image_2d(\n    Target :: texture_target(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Pixels :: iodata()\n) -> ok | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glTexImage2D_raw(NewTarget, Level, NewInternalFormat, Width, Height, Border, NewFormat, NewType, Pixels0)).">>,
        <<"?CALL_RAW_FUNC(glTexSubImage2D_raw(NewTarget, Level, OffsetX, OffsetY, Width, Height, NewFormat, NewType, Pixels0)).">>
    ].

s140_three_d_exports() ->
    [
        <<"-export([tex_image_3d/10]).">>,
        <<"-export([tex_sub_image_3d/11]).">>
    ].

s140_all_c_calls() ->
    s140_one_d_c_calls() ++ s140_two_d_c_calls() ++ s140_three_d_c_calls().

s140_es3_c_calls() ->
    s140_two_d_c_calls() ++ s140_three_d_c_calls().

s140_es2_c_calls() ->
    s140_two_d_c_calls().

s140_one_d_c_calls() ->
    [
        <<"glTexImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7);">>,
        <<"glTexSubImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, (void*)arg_6.data);">>
    ].

s140_two_d_c_calls() ->
    [
        <<"glTexImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8);">>,
        <<"glTexSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, (void*)arg_8.data);">>
    ].

s140_three_d_c_calls() ->
    [
        <<"glTexImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9);">>,
        <<"glTexSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, (void*)arg_10.data);">>
    ].

s140_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s140_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s140_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 141.
s141_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s141_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s141_emitter_dsa_texture_subimage_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s141_assert_emitted_surface(
                {gl, {4, 6}},
                s141_all_exports(),
                s141_all_c_calls(),
                []
            )
        end},
        {"gl 4.1", fun() ->
            s141_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                s141_all_exports() ++ s141_all_c_calls()
            )
        end},
        {"gles 3.2", fun() ->
            s141_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s141_all_exports() ++ s141_all_c_calls()
            )
        end}
    ].

s141_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s141_present_functions(Target),

    s141_assert_presence(Present, Functions),
    s141_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s141_assert_enum_types(BindingData),
            s141_assert_present_paths(Functions)
    end.

s141_present_functions({gl, {4, 6}}) ->
    s141_all_functions();
s141_present_functions(_) ->
    [].

s141_all_functions() ->
    [
        {"texture_sub_image_1d", 7},
        {"texture_sub_image_2d", 9},
        {"texture_sub_image_3d", 11}
    ].

s141_assert_presence(Present, Functions) ->
    Absent = s141_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s141_assert_enum_types(BindingData) ->
    s141_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s141_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s141_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s141_assert_present_paths(Functions) ->
    [
        s141_assert_path(Function, maps:get(Function, Functions))
     || Function <- s141_all_functions()
    ].

s141_assert_path({"texture_sub_image_1d", 7}, FunctionData) ->
    s141_assert_texture_sub_image(
        FunctionData,
        "glTextureSubImage1D",
        [{"Offset", gl_int}],
        [{"Width", gl_sizei}]
    );
s141_assert_path({"texture_sub_image_2d", 9}, FunctionData) ->
    s141_assert_texture_sub_image(
        FunctionData,
        "glTextureSubImage2D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s141_assert_path({"texture_sub_image_3d", 11}, FunctionData) ->
    s141_assert_texture_sub_image(
        FunctionData,
        "glTextureSubImage3D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}, {"OffsetZ", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    ).

s141_assert_texture_sub_image(FunctionData, Command, Offsets, Dimensions) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ExpectedParams =
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int}
        ] ++
            [{in, Name, Type} || {Name, Type} <- Offsets] ++
            [{in, Name, Type} || {Name, Type} <- Dimensions] ++
            [
                {in, "Format", {gl_enum, "PixelFormat"}},
                {in, "Type", {gl_enum, "PixelType"}},
                {in, "Pixels", byte_data_pointer}
            ],
    ?assertEqual(ExpectedParams, maps:get(params_specs, FunctionData)),
    s141_assert_specs_params(FunctionData, Offsets ++ Dimensions),
    s141_assert_clause_params(FunctionData),
    s141_assert_nif_params(FunctionData, Command, Offsets ++ Dimensions).

s141_assert_specs_params(FunctionData, MiddleParams) ->
    Expected =
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}}
        ] ++
            [{Name, s141_type_spec(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", {undefined, pixel_format, []}},
                {"Type", {undefined, pixel_type, []}},
                {"Pixels", {undefined, iodata, []}}
            ],
    ?assertEqual(Expected, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(Expected), maps:get(function_arity, FunctionData)).

s141_assert_clause_params(FunctionData) ->
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assertEqual({"Texture", do_nothing}, hd(Params)),
    ?assertEqual({"Pixels", byte_data_pointer}, lists:last(Params)),
    ?assertEqual(maps:get(gl_command, FunctionData), maps:get(raw_function, Clause)).

s141_assert_nif_params(FunctionData, Command, MiddleParams) ->
    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    Expected =
        [
            {"Texture", s141_uint_nif_data()},
            {"Level", s141_int_nif_data()}
        ] ++
            [{Name, s141_nif_type(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", s141_enum_nif_data()},
                {"Type", s141_enum_nif_data()},
                {"Pixels", binary_to_glbinary}
            ],
    ?assertEqual(Expected, maps:get(params, NifData)),
    ?assertEqual(length(Expected), maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s141_type_spec(gl_int) -> {gl, int, []};
s141_type_spec(gl_sizei) -> {gl, sizei, []}.

s141_nif_type(gl_int) -> s141_int_nif_data();
s141_nif_type(gl_sizei) -> s141_sizei_nif_data().

s141_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s141_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s141_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s141_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s141_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard141-emitter-" ++
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

        [s141_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s141_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s141_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s141_assert_not_contains(C, Pattern) || Pattern <- Forbidden],

        case ExpectedErl of
            [] ->
                ok;
            _ ->
                s141_assert_contains(Erl, <<"Pixels0 = iolist_to_binary(Pixels),">>),
                s141_assert_contains(C, <<"if (!enif_inspect_binary(env, argv[">>)
        end
    after
        ok = file:set_cwd(Cwd)
    end.

s141_all_exports() ->
    [
        <<"-export([texture_sub_image_1d/7]).">>,
        <<"-export([texture_sub_image_2d/9]).">>,
        <<"-export([texture_sub_image_3d/11]).">>,
        <<"-spec texture_sub_image_2d(\n    Texture :: texture(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Pixels :: iodata()\n) -> ok | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glTextureSubImage2D_raw(Texture, Level, OffsetX, OffsetY, Width, Height, NewFormat, NewType, Pixels0)).">>
    ].

s141_all_c_calls() ->
    [
        <<"glTextureSubImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, (void*)arg_6.data);">>,
        <<"glTextureSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, (void*)arg_8.data);">>,
        <<"glTextureSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, (void*)arg_10.data);">>
    ].

s141_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s141_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s141_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 142.
s142_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s142_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s142_emitter_compressed_texture_upload_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s142_assert_emitted_surface(
                {gl, {4, 6}},
                s142_all_exports(),
                s142_all_c_calls(),
                []
            )
        end},
        {"gles 3.2", fun() ->
            s142_assert_emitted_surface(
                {gles, {3, 2}},
                s142_es3_exports(),
                s142_es3_c_calls(),
                s142_one_d_exports() ++ s142_one_d_c_calls()
            )
        end},
        {"gles 2.0", fun() ->
            s142_assert_emitted_surface(
                {gles, {2, 0}},
                s142_es2_exports(),
                s142_es2_c_calls(),
                s142_one_d_exports() ++ s142_one_d_c_calls() ++ s142_three_d_exports() ++ s142_three_d_c_calls()
            )
        end}
    ].

s142_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s142_assert_presence(Target, Functions),
    s142_assert_enum_types(BindingData),
    s142_assert_deferred_neighbors_absent(Functions),
    s142_assert_present_paths(Target, Functions).

s142_assert_presence(Target, Functions) ->
    Present = s142_present_functions(Target),
    Absent = s142_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s142_present_functions({gl, _Version}) ->
    s142_all_functions();
s142_present_functions({gles, {2, 0}}) ->
    s142_two_d_functions();
s142_present_functions({gles, _Version}) ->
    s142_two_d_functions() ++ s142_three_d_functions().

s142_all_functions() ->
    s142_one_d_functions() ++ s142_two_d_functions() ++ s142_three_d_functions().

s142_one_d_functions() ->
    [
        {"compressed_tex_image_1d", 6},
        {"compressed_tex_sub_image_1d", 6}
    ].

s142_two_d_functions() ->
    [
        {"compressed_tex_image_2d", 7},
        {"compressed_tex_sub_image_2d", 8}
    ].

s142_three_d_functions() ->
    [
        {"compressed_tex_image_3d", 8},
        {"compressed_tex_sub_image_3d", 10}
    ].

s142_assert_enum_types(BindingData) ->
    s142_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s142_assert_enum_contains(BindingData, "internal_format", "rgba").

s142_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s142_assert_present_paths(Target, Functions) ->
    [
        s142_assert_path(Function, Target, maps:get(Function, Functions))
     || Function <- s142_present_functions(Target)
    ].

s142_assert_path({"compressed_tex_image_1d", 6}, _Target, FunctionData) ->
    s142_assert_compressed_tex_image(
        FunctionData,
        "glCompressedTexImage1D",
        [{"Width", gl_sizei}]
    );
s142_assert_path({"compressed_tex_image_2d", 7}, _Target, FunctionData) ->
    s142_assert_compressed_tex_image(
        FunctionData,
        "glCompressedTexImage2D",
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s142_assert_path({"compressed_tex_image_3d", 8}, _Target, FunctionData) ->
    s142_assert_compressed_tex_image(
        FunctionData,
        "glCompressedTexImage3D",
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    );
s142_assert_path({"compressed_tex_sub_image_1d", 6}, _Target, FunctionData) ->
    s142_assert_compressed_tex_sub_image(
        FunctionData,
        "glCompressedTexSubImage1D",
        [{"Offset", gl_int}],
        [{"Width", gl_sizei}]
    );
s142_assert_path({"compressed_tex_sub_image_2d", 8}, _Target, FunctionData) ->
    s142_assert_compressed_tex_sub_image(
        FunctionData,
        "glCompressedTexSubImage2D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s142_assert_path({"compressed_tex_sub_image_3d", 10}, _Target, FunctionData) ->
    s142_assert_compressed_tex_sub_image(
        FunctionData,
        "glCompressedTexSubImage3D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}, {"OffsetZ", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    ).

s142_assert_compressed_tex_image(FunctionData, Command, Dimensions) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ExpectedParams =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}}
        ] ++
            [{in, Name, Type} || {Name, Type} <- Dimensions] ++
            [
                {in, "Border", gl_int},
                {in, "ImageData", {byte_data, "ImageSize", gl_sizei}}
            ],
    ?assertEqual(ExpectedParams, maps:get(params_specs, FunctionData)),
    s142_assert_specs_params(FunctionData, Dimensions, true),
    s142_assert_clause_params(FunctionData),
    s142_assert_nif_params(FunctionData, Command, Dimensions, true).

s142_assert_compressed_tex_sub_image(FunctionData, Command, Offsets, Dimensions) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ExpectedParams =
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int}
        ] ++
            [{in, Name, Type} || {Name, Type} <- Offsets] ++
            [{in, Name, Type} || {Name, Type} <- Dimensions] ++
            [
                {in, "Format", {gl_enum, "InternalFormat"}},
                {in, "ImageData", {byte_data, "ImageSize", gl_sizei}}
            ],
    ?assertEqual(ExpectedParams, maps:get(params_specs, FunctionData)),
    s142_assert_specs_params(FunctionData, Offsets ++ Dimensions, false),
    s142_assert_clause_params(FunctionData),
    s142_assert_nif_params(FunctionData, Command, Offsets ++ Dimensions, false).

s142_assert_specs_params(FunctionData, MiddleParams, IsImage) ->
    Expected =
        [
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}}
        ] ++
            s142_maybe_internal_format(IsImage) ++
            [{Name, s142_type_spec(Type)} || {Name, Type} <- MiddleParams] ++
            s142_maybe_border(IsImage) ++
            s142_maybe_format(IsImage) ++
            [
                {"ImageData", {undefined, iodata, []}}
            ],
    ?assertEqual(Expected, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(Expected), maps:get(function_arity, FunctionData)).

s142_maybe_internal_format(true) -> [{"InternalFormat", {undefined, internal_format, []}}];
s142_maybe_internal_format(false) -> [].

s142_maybe_border(true) -> [{"Border", {gl, int, []}}];
s142_maybe_border(false) -> [].

s142_maybe_format(true) -> [];
s142_maybe_format(false) -> [{"Format", {undefined, internal_format, []}}].

s142_assert_clause_params(FunctionData) ->
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assertEqual({"ImageData", {byte_data, "ImageSize", gl_sizei}}, lists:last(Params)),
    ?assertEqual(maps:get(gl_command, FunctionData), maps:get(raw_function, Clause)).

s142_assert_nif_params(FunctionData, Command, MiddleParams, IsImage) ->
    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    Expected =
        [
            {"Target", s142_enum_nif_data()},
            {"Level", s142_int_nif_data()}
        ] ++
            s142_maybe_internal_format_nif(IsImage) ++
            [{Name, s142_nif_type(Type)} || {Name, Type} <- MiddleParams] ++
            s142_maybe_border_nif(IsImage) ++
            s142_maybe_format_nif(IsImage) ++
            [
                {"ImageSize", s142_sizei_nif_data()},
                {"ImageData", binary_to_glbinary}
            ],
    ?assertEqual(Expected, maps:get(params, NifData)),
    ?assertEqual(length(Expected), maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s142_maybe_internal_format_nif(true) -> [{"InternalFormat", s142_enum_nif_data()}];
s142_maybe_internal_format_nif(false) -> [].

s142_maybe_border_nif(true) -> [{"Border", s142_int_nif_data()}];
s142_maybe_border_nif(false) -> [].

s142_maybe_format_nif(true) -> [];
s142_maybe_format_nif(false) -> [{"Format", s142_enum_nif_data()}].

s142_type_spec(gl_int) -> {gl, int, []};
s142_type_spec(gl_sizei) -> {gl, sizei, []}.

s142_nif_type(gl_int) -> s142_int_nif_data();
s142_nif_type(gl_sizei) -> s142_sizei_nif_data().

s142_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s142_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s142_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s142_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard142-emitter-" ++
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

        [s142_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s142_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s142_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s142_assert_not_contains(C, Pattern) || Pattern <- Forbidden],

        case ExpectedErl of
            [] ->
                ok;
            _ ->
                s142_assert_contains(Erl, <<"ImageData0 = iolist_to_binary(ImageData),\n    ImageSize = byte_size(ImageData0),">>),
                s142_assert_contains(C, <<"if (!enif_inspect_binary(env, argv[">>)
        end
    after
        ok = file:set_cwd(Cwd)
    end.

s142_all_exports() ->
    s142_one_d_exports() ++ s142_two_d_exports() ++ s142_three_d_exports().

s142_es3_exports() ->
    s142_two_d_exports() ++ s142_three_d_exports().

s142_es2_exports() ->
    s142_two_d_exports().

s142_one_d_exports() ->
    [
        <<"-export([compressed_tex_image_1d/6]).">>,
        <<"-export([compressed_tex_sub_image_1d/6]).">>
    ].

s142_two_d_exports() ->
    [
        <<"-export([compressed_tex_image_2d/7]).">>,
        <<"-export([compressed_tex_sub_image_2d/8]).">>,
        <<"-spec compressed_tex_image_2d(\n    Target :: texture_target(),\n    Level :: gl:int(),\n    InternalFormat :: internal_format(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Border :: gl:int(),\n    ImageData :: iodata()\n) -> ok | {error, atom()}.">>,
        <<"-spec compressed_tex_sub_image_2d(\n    Target :: texture_target(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Format :: internal_format(),\n    ImageData :: iodata()\n) -> ok | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glCompressedTexImage2D_raw(NewTarget, Level, NewInternalFormat, Width, Height, Border, ImageSize, ImageData0)).">>,
        <<"?CALL_RAW_FUNC(glCompressedTexSubImage2D_raw(NewTarget, Level, OffsetX, OffsetY, Width, Height, NewFormat, ImageSize, ImageData0)).">>
    ].

s142_three_d_exports() ->
    [
        <<"-export([compressed_tex_image_3d/8]).">>,
        <<"-export([compressed_tex_sub_image_3d/10]).">>
    ].

s142_all_c_calls() ->
    s142_one_d_c_calls() ++ s142_two_d_c_calls() ++ s142_three_d_c_calls().

s142_es3_c_calls() ->
    s142_two_d_c_calls() ++ s142_three_d_c_calls().

s142_es2_c_calls() ->
    s142_two_d_c_calls().

s142_one_d_c_calls() ->
    [
        <<"glCompressedTexImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, (void*)arg_6.data);">>,
        <<"glCompressedTexSubImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, (void*)arg_6.data);">>
    ].

s142_two_d_c_calls() ->
    [
        <<"glCompressedTexImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, (void*)arg_7.data);">>,
        <<"glCompressedTexSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, (void*)arg_8.data);">>
    ].

s142_three_d_c_calls() ->
    [
        <<"glCompressedTexImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, (void*)arg_8.data);">>,
        <<"glCompressedTexSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, (void*)arg_10.data);">>
    ].

s142_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s142_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s142_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 143.
s143_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s143_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s143_emitter_compressed_dsa_texture_subimage_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s143_assert_emitted_surface(
                {gl, {4, 6}},
                s143_all_exports(),
                s143_all_c_calls(),
                []
            )
        end},
        {"gl 4.1", fun() ->
            s143_assert_emitted_surface(
                {gl, {4, 1}},
                [],
                [],
                s143_all_exports() ++ s143_all_c_calls()
            )
        end},
        {"gles 3.2", fun() ->
            s143_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                s143_all_exports() ++ s143_all_c_calls()
            )
        end}
    ].

s143_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s143_present_functions(Target),

    s143_assert_presence(Present, Functions),
    s143_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s143_assert_enum_types(BindingData),
            s143_assert_present_paths(Functions)
    end.

s143_present_functions({gl, {4, 6}}) ->
    s143_all_functions();
s143_present_functions(_) ->
    [].

s143_all_functions() ->
    [
        {"compressed_texture_sub_image_1d", 6},
        {"compressed_texture_sub_image_2d", 8},
        {"compressed_texture_sub_image_3d", 10}
    ].

s143_assert_presence(Present, Functions) ->
    Absent = s143_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s143_assert_enum_types(BindingData) ->
    s143_assert_enum_contains(BindingData, "internal_format", "compressed_rgba").

s143_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s143_assert_present_paths(Functions) ->
    [
        s143_assert_path(Function, maps:get(Function, Functions))
     || Function <- s143_all_functions()
    ].

s143_assert_path({"compressed_texture_sub_image_1d", 6}, FunctionData) ->
    s143_assert_compressed_texture_sub_image(
        FunctionData,
        "glCompressedTextureSubImage1D",
        [{"Offset", gl_int}],
        [{"Width", gl_sizei}]
    );
s143_assert_path({"compressed_texture_sub_image_2d", 8}, FunctionData) ->
    s143_assert_compressed_texture_sub_image(
        FunctionData,
        "glCompressedTextureSubImage2D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}]
    );
s143_assert_path({"compressed_texture_sub_image_3d", 10}, FunctionData) ->
    s143_assert_compressed_texture_sub_image(
        FunctionData,
        "glCompressedTextureSubImage3D",
        [{"OffsetX", gl_int}, {"OffsetY", gl_int}, {"OffsetZ", gl_int}],
        [{"Width", gl_sizei}, {"Height", gl_sizei}, {"Depth", gl_sizei}]
    ).

s143_assert_compressed_texture_sub_image(FunctionData, Command, Offsets, Dimensions) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ExpectedParams =
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int}
        ] ++
            [{in, Name, Type} || {Name, Type} <- Offsets] ++
            [{in, Name, Type} || {Name, Type} <- Dimensions] ++
            [
                {in, "Format", {gl_enum, "InternalFormat"}},
                {in, "ImageData", {byte_data, "ImageSize", gl_sizei}}
            ],
    ?assertEqual(ExpectedParams, maps:get(params_specs, FunctionData)),
    s143_assert_specs_params(FunctionData, Offsets ++ Dimensions),
    s143_assert_clause_params(FunctionData),
    s143_assert_nif_params(FunctionData, Command, Offsets ++ Dimensions).

s143_assert_specs_params(FunctionData, MiddleParams) ->
    Expected =
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}}
        ] ++
            [{Name, s143_type_spec(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", {undefined, internal_format, []}},
                {"ImageData", {undefined, iodata, []}}
            ],
    ?assertEqual(Expected, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(Expected), maps:get(function_arity, FunctionData)).

s143_assert_clause_params(FunctionData) ->
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assertEqual({"Texture", do_nothing}, hd(Params)),
    ?assertEqual({"ImageData", {byte_data, "ImageSize", gl_sizei}}, lists:last(Params)),
    ?assertEqual(maps:get(gl_command, FunctionData), maps:get(raw_function, Clause)).

s143_assert_nif_params(FunctionData, Command, MiddleParams) ->
    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    Expected =
        [
            {"Texture", s143_uint_nif_data()},
            {"Level", s143_int_nif_data()}
        ] ++
            [{Name, s143_nif_type(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", s143_enum_nif_data()},
                {"ImageSize", s143_sizei_nif_data()},
                {"ImageData", binary_to_glbinary}
            ],
    ?assertEqual(Expected, maps:get(params, NifData)),
    ?assertEqual(length(Expected), maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s143_type_spec(gl_int) -> {gl, int, []};
s143_type_spec(gl_sizei) -> {gl, sizei, []}.

s143_nif_type(gl_int) -> s143_int_nif_data();
s143_nif_type(gl_sizei) -> s143_sizei_nif_data().

s143_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s143_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s143_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s143_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s143_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard143-emitter-" ++
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

        [s143_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s143_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s143_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s143_assert_not_contains(C, Pattern) || Pattern <- Forbidden],

        case ExpectedErl of
            [] ->
                ok;
            _ ->
                s143_assert_contains(Erl, <<"ImageData0 = iolist_to_binary(ImageData),\n    ImageSize = byte_size(ImageData0),">>),
                s143_assert_contains(C, <<"if (!enif_inspect_binary(env, argv[">>)
        end
    after
        ok = file:set_cwd(Cwd)
    end.

s143_all_exports() ->
    [
        <<"-export([compressed_texture_sub_image_1d/6]).">>,
        <<"-export([compressed_texture_sub_image_2d/8]).">>,
        <<"-export([compressed_texture_sub_image_3d/10]).">>,
        <<"-spec compressed_texture_sub_image_2d(\n    Texture :: texture(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Format :: internal_format(),\n    ImageData :: iodata()\n) -> ok | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glCompressedTextureSubImage2D_raw(Texture, Level, OffsetX, OffsetY, Width, Height, NewFormat, ImageSize, ImageData0)).">>
    ].

s143_all_c_calls() ->
    [
        <<"glCompressedTextureSubImage1D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, (void*)arg_6.data);">>,
        <<"glCompressedTextureSubImage2D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, (void*)arg_8.data);">>,
        <<"glCompressedTextureSubImage3D(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, (void*)arg_10.data);">>
    ].

s143_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s143_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s143_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 151.
s151_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s151_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s151_emitter_read_n_pixels_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s151_assert_emitted_surface({gl, {4, 6}}, s151_expected_erlang(), s151_expected_c(), s151_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s151_assert_emitted_surface({gl, {4, 1}}, [], [], s151_absent_surface())
        end},
        {"gles 3.2", fun() ->
            s151_assert_emitted_surface({gles, {3, 2}}, s151_expected_erlang(), s151_expected_c(), s151_forbidden_surface())
        end},
        {"gles 3.1", fun() ->
            s151_assert_emitted_surface({gles, {3, 1}}, [], [], s151_absent_surface())
        end}
    ].

s151_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s151_present_functions(Target),

    s151_assert_presence(Present, Functions),
    s151_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s151_assert_read_n_pixels(maps:get({"read_n_pixels", 7}, Functions)),
            s151_assert_enum_contains(BindingData, "pixel_format", "rgba"),
            s151_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte")
    end.

s151_present_functions({gl, {4, 6}}) ->
    [{"read_n_pixels", 7}];
s151_present_functions({gles, {3, 2}}) ->
    [{"read_n_pixels", 7}];
s151_present_functions(_) ->
    [].

s151_assert_presence(Present, Functions) ->
    All = [{"read_n_pixels", 7}],
    Absent = All -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"read_n_pixels", 6}, Functions)),
    ?assertNot(maps:is_key({"read_n_pixels", 8}, Functions)).

s151_assert_read_n_pixels(FunctionData) ->
    ?assertEqual("glReadnPixels", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "X", gl_int},
            {in, "Y", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {out, "Pixels", {gl_binary, {explicit, "PixelsSize", gl_sizei}}}
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
    s151_assert_clause_params(
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
    ?assertEqual("glReadnPixels", maps:get(raw_function, Clause)),

    NifData = maps:get("glReadnPixels", maps:get(nif_functions, FunctionData)),
    ?assertEqual(7, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"X", s151_int_nif_data()},
            {"Y", s151_int_nif_data()},
            {"Width", s151_sizei_nif_data()},
            {"Height", s151_sizei_nif_data()},
            {"Format", s151_enum_nif_data()},
            {"Type", s151_enum_nif_data()},
            {"Pixels", {out_binary_explicit, gl_sizei}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s151_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnTexImage",
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s151_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard151-emitter-" ++
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

        [s151_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s151_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s151_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s151_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s151_expected_erlang() ->
    [
        <<"-export([read_n_pixels/7]).">>,
        <<"-spec read_n_pixels(\n    X :: gl:int(),\n    Y :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    PixelsSize :: non_neg_integer()\n) -> {ok, Pixels :: binary()} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glReadnPixels_raw(X, Y, Width, Height, NewFormat, NewType, PixelsSize)).">>
    ].

s151_expected_c() ->
    [
        <<"#include <limits.h>">>,
        <<"ErlNifUInt64 arg_6_size;">>,
        <<"if (!enif_get_uint64(env, argv[6], &arg_6_size)) {">>,
        <<"if (arg_6_size > (ErlNifUInt64)INT_MAX) {">>,
        <<"unsigned char* arg_6_bin = enif_make_new_binary(env, arg_6_size, &arg_6_term);">>,
        <<"if (arg_6_bin == NULL && arg_6_size > 0) {">>,
        <<"glReadnPixels(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, (GLsizei)arg_6_size, arg_6_bin);">>,
        <<"{\"glReadnPixels_raw\", 7, nif_glReadnPixels, 0}">>
    ].

s151_forbidden_surface() ->
    [
        <<"-export([get_n_tex_image/">>,
        <<"-export([getn_compressed_tex_image/">>,
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnTexImage(">>,
        <<"glGetnCompressedTexImage(">>
    ].

s151_absent_surface() ->
    s151_expected_erlang() ++
        [
            <<"glReadnPixels(">>,
            <<"{\"glReadnPixels_raw\", 7, nif_glReadnPixels, 0}">>
        ].

s151_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s151_assert_clause_param/1, lists:zip(Expected, Actual)).

s151_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s151_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s151_assert_enum_contains(BindingData, EnumName, Value) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumName, EnumTypes)),
    ?assert(lists:member(Value, maps:get(EnumName, EnumTypes))).

s151_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s151_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s151_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s151_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s151_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 153.
s153_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s153_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s153_emitter_clear_texture_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s153_assert_emitted_surface({gl, {4, 6}}, s153_expected_erlang(), s153_expected_c(), s153_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s153_assert_emitted_surface({gl, {4, 1}}, [], [], s153_absent_surface())
        end},
        {"gles 3.2", fun() ->
            s153_assert_emitted_surface({gles, {3, 2}}, [], [], s153_absent_surface())
        end}
    ].

s153_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s153_present_functions(Target),

    s153_assert_presence(Present, Functions),
    s153_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s153_assert_enum_types(BindingData),
            s153_assert_clear_tex_image(maps:get({"clear_tex_image", 5}, Functions)),
            s153_assert_clear_tex_sub_image(maps:get({"clear_tex_sub_image", 11}, Functions))
    end.

s153_present_functions({gl, {4, 6}}) ->
    s153_all_functions();
s153_present_functions(_) ->
    [].

s153_all_functions() ->
    [
        {"clear_tex_image", 5},
        {"clear_tex_sub_image", 11}
    ].

s153_assert_presence(Present, Functions) ->
    Absent = s153_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"clear_tex_image", 4}, Functions)),
    ?assertNot(maps:is_key({"clear_tex_image", 6}, Functions)),
    ?assertNot(maps:is_key({"clear_tex_sub_image", 10}, Functions)),
    ?assertNot(maps:is_key({"clear_tex_sub_image", 12}, Functions)).

s153_assert_enum_types(BindingData) ->
    s153_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s153_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s153_assert_clear_tex_image(FunctionData) ->
    ?assertEqual("glClearTexImage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {in, "Data", byte_data_pointer_or_none}
        ],
        maps:get(params_specs, FunctionData)
    ),
    s153_assert_common_clear_surface(FunctionData, "glClearTexImage", [], 5).

s153_assert_clear_tex_sub_image(FunctionData) ->
    ?assertEqual("glClearTexSubImage", maps:get(gl_command, FunctionData)),
    MiddleParams = [
        {"OffsetX", gl_int},
        {"OffsetY", gl_int},
        {"OffsetZ", gl_int},
        {"Width", gl_sizei},
        {"Height", gl_sizei},
        {"Depth", gl_sizei}
    ],
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int}
        ] ++
            [{in, Name, Type} || {Name, Type} <- MiddleParams] ++
            [
                {in, "Format", {gl_enum, "PixelFormat"}},
                {in, "Type", {gl_enum, "PixelType"}},
                {in, "Data", byte_data_pointer_or_none}
            ],
        maps:get(params_specs, FunctionData)
    ),
    s153_assert_common_clear_surface(FunctionData, "glClearTexSubImage", MiddleParams, 11).

s153_assert_common_clear_surface(FunctionData, Command, MiddleParams, Arity) ->
    ExpectedSpecs =
        [
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}}
        ] ++
            [{Name, s153_type_spec(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", {undefined, pixel_format, []}},
                {"Type", {undefined, pixel_type, []}},
                {"Data", {set, [{undefined, iodata, []}, none]}}
            ],
    ?assertEqual(ExpectedSpecs, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(Arity, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assertEqual({"Texture", do_nothing}, hd(Params)),
    ?assertEqual({"Data", byte_data_pointer_or_none}, lists:last(Params)),
    ?assertEqual(Command, maps:get(raw_function, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ExpectedNif =
        [
            {"Texture", s153_uint_nif_data()},
            {"Level", s153_int_nif_data()}
        ] ++
            [{Name, s153_nif_type(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", s153_enum_nif_data()},
                {"Type", s153_enum_nif_data()},
                {"Data", s153_binary_or_null_nif_data()}
            ],
    ?assertEqual(ExpectedNif, maps:get(params, NifData)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s153_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetnTexImage",
        "glGetnCompressedTexImage"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s153_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard153-emitter-" ++
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

        [s153_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s153_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s153_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s153_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s153_expected_erlang() ->
    [
        <<"-export([clear_tex_image/5]).">>,
        <<"-export([clear_tex_sub_image/11]).">>,
        <<"-spec clear_tex_image(\n    Texture :: texture(),\n    Level :: gl:int(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Data :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"-spec clear_tex_sub_image(\n    Texture :: texture(),\n    Level :: gl:int(),\n    OffsetX :: gl:int(),\n    OffsetY :: gl:int(),\n    OffsetZ :: gl:int(),\n    Width :: gl:sizei(),\n    Height :: gl:sizei(),\n    Depth :: gl:sizei(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Data :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"Data0 = case Data of\n        none -> undefined;\n        _ -> iolist_to_binary(Data)\n    end,">>,
        <<"?CALL_RAW_FUNC(glClearTexImage_raw(Texture, Level, NewFormat, NewType, Data0)).">>,
        <<"?CALL_RAW_FUNC(glClearTexSubImage_raw(Texture, Level, OffsetX, OffsetY, OffsetZ, Width, Height, Depth, NewFormat, NewType, Data0)).">>
    ].

s153_expected_c() ->
    [
        <<"const void* arg_4 = NULL;">>,
        <<"glClearTexImage(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"const void* arg_10 = NULL;">>,
        <<"glClearTexSubImage(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, arg_10);">>,
        <<"{\"glClearTexImage_raw\", 5, nif_glClearTexImage, 0}">>,
        <<"{\"glClearTexSubImage_raw\", 11, nif_glClearTexSubImage, 0}">>
    ].

s153_forbidden_surface() ->
    [
        <<"-export([get_n_tex_image/">>,
        <<"-export([get_n_compressed_tex_image/">>,
        <<"glGetnTexImage(">>,
        <<"glGetnCompressedTexImage(">>
    ].

s153_absent_surface() ->
    s153_expected_erlang() ++
        [
            <<"glClearTexImage(">>,
            <<"glClearTexSubImage(">>,
            <<"{\"glClearTexImage_raw\", 5, nif_glClearTexImage, 0}">>,
            <<"{\"glClearTexSubImage_raw\", 11, nif_glClearTexSubImage, 0}">>
        ].

s153_type_spec(gl_int) -> {gl, int, []};
s153_type_spec(gl_sizei) -> {gl, sizei, []}.

s153_nif_type(gl_int) -> s153_int_nif_data();
s153_nif_type(gl_sizei) -> s153_sizei_nif_data().

s153_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s153_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s153_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s153_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s153_binary_or_null_nif_data() ->
    in_gl_binary_or_null.

s153_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s153_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s153_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 154.
s154_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s154_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s154_emitter_buffer_clear_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s154_assert_emitted_surface({gl, {4, 6}}, s154_expected_erlang(), s154_expected_c(), s154_forbidden_surface())
        end},
        {"gl 4.1", fun() ->
            s154_assert_emitted_surface({gl, {4, 1}}, [], [], s154_absent_surface())
        end},
        {"gles 3.2", fun() ->
            s154_assert_emitted_surface({gles, {3, 2}}, [], [], s154_absent_surface())
        end}
    ].

s154_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    Present = s154_present_functions(Target),

    s154_assert_presence(Present, Functions),
    s154_assert_deferred_neighbors_absent(Functions),
    case Present of
        [] ->
            ok;
        _ ->
            s154_assert_enum_types(BindingData),
            s154_assert_clear_buffer_data(maps:get({"clear_buffer_data", 5}, Functions)),
            s154_assert_clear_buffer_sub_data(maps:get({"clear_buffer_sub_data", 7}, Functions)),
            s154_assert_clear_named_buffer_data(maps:get({"clear_named_buffer_data", 5}, Functions)),
            s154_assert_clear_named_buffer_sub_data(maps:get({"clear_named_buffer_sub_data", 7}, Functions))
    end.

s154_present_functions({gl, {4, 6}}) ->
    s154_all_functions();
s154_present_functions(_) ->
    [].

s154_all_functions() ->
    [
        {"clear_buffer_data", 5},
        {"clear_buffer_sub_data", 7},
        {"clear_named_buffer_data", 5},
        {"clear_named_buffer_sub_data", 7}
    ].

s154_assert_presence(Present, Functions) ->
    Absent = s154_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent],
    ?assertNot(maps:is_key({"clear_buffer_data", 4}, Functions)),
    ?assertNot(maps:is_key({"clear_buffer_data", 6}, Functions)),
    ?assertNot(maps:is_key({"clear_buffer_sub_data", 6}, Functions)),
    ?assertNot(maps:is_key({"clear_buffer_sub_data", 8}, Functions)),
    ?assertNot(maps:is_key({"clear_named_buffer_data", 4}, Functions)),
    ?assertNot(maps:is_key({"clear_named_buffer_data", 6}, Functions)),
    ?assertNot(maps:is_key({"clear_named_buffer_sub_data", 6}, Functions)),
    ?assertNot(maps:is_key({"clear_named_buffer_sub_data", 8}, Functions)).

s154_assert_enum_types(BindingData) ->
    s154_assert_enum_contains(BindingData, "buffer_target", "array_buffer"),
    s154_assert_enum_contains(BindingData, "sized_internal_format", "rgba8"),
    s154_assert_enum_contains(BindingData, "pixel_format", "rgba"),
    s154_assert_enum_contains(BindingData, "pixel_type", "unsigned_byte").

s154_assert_clear_buffer_data(FunctionData) ->
    ?assertEqual("glClearBufferData", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {in, "Data", byte_data_pointer_or_none}
        ],
        maps:get(params_specs, FunctionData)
    ),
    s154_assert_target_bound_clear_surface(FunctionData, "glClearBufferData", [], 5).

s154_assert_clear_buffer_sub_data(FunctionData) ->
    ?assertEqual("glClearBufferSubData", maps:get(gl_command, FunctionData)),
    MiddleParams = [{"Offset", gl_intptr}, {"Size", gl_sizeiptr}],
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}}
        ] ++
            [{in, Name, Type} || {Name, Type} <- MiddleParams] ++
            [
                {in, "Format", {gl_enum, "PixelFormat"}},
                {in, "Type", {gl_enum, "PixelType"}},
                {in, "Data", byte_data_pointer_or_none}
            ],
        maps:get(params_specs, FunctionData)
    ),
    s154_assert_target_bound_clear_surface(FunctionData, "glClearBufferSubData", MiddleParams, 7).

s154_assert_clear_named_buffer_data(FunctionData) ->
    ?assertEqual("glClearNamedBufferData", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}},
            {in, "Format", {gl_enum, "PixelFormat"}},
            {in, "Type", {gl_enum, "PixelType"}},
            {in, "Data", byte_data_pointer_or_none}
        ],
        maps:get(params_specs, FunctionData)
    ),
    s154_assert_named_clear_surface(FunctionData, "glClearNamedBufferData", [], 5).

s154_assert_clear_named_buffer_sub_data(FunctionData) ->
    ?assertEqual("glClearNamedBufferSubData", maps:get(gl_command, FunctionData)),
    MiddleParams = [{"Offset", gl_intptr}, {"Size", gl_sizeiptr}],
    ?assertEqual(
        [
            {in, "Buffer", {gl_object, buffer}},
            {in, "InternalFormat", {gl_enum, "SizedInternalFormat"}}
        ] ++
            [{in, Name, Type} || {Name, Type} <- MiddleParams] ++
            [
                {in, "Format", {gl_enum, "PixelFormat"}},
                {in, "Type", {gl_enum, "PixelType"}},
                {in, "Data", byte_data_pointer_or_none}
            ],
        maps:get(params_specs, FunctionData)
    ),
    s154_assert_named_clear_surface(FunctionData, "glClearNamedBufferSubData", MiddleParams, 7).

s154_assert_target_bound_clear_surface(FunctionData, Command, MiddleParams, Arity) ->
    s154_assert_common_clear_surface(
        FunctionData,
        Command,
        [{"Target", {undefined, buffer_target, []}}],
        MiddleParams,
        [{"Target", s154_enum_nif_data()}],
        Arity
    ).

s154_assert_named_clear_surface(FunctionData, Command, MiddleParams, Arity) ->
    s154_assert_common_clear_surface(
        FunctionData,
        Command,
        [{"Buffer", {undefined, buffer, []}}],
        MiddleParams,
        [{"Buffer", s154_uint_nif_data()}],
        Arity
    ).

s154_assert_common_clear_surface(FunctionData, Command, LeadingSpecs, MiddleParams, LeadingNif, Arity) ->
    ExpectedSpecs =
        LeadingSpecs ++
            [
                {"InternalFormat", {undefined, sized_internal_format, []}}
            ] ++
            [{Name, s154_type_spec(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", {undefined, pixel_format, []}},
                {"Type", {undefined, pixel_type, []}},
                {"Data", {set, [{undefined, iodata, []}, none]}}
            ],
    ?assertEqual(ExpectedSpecs, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(Arity, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assertEqual({"Data", byte_data_pointer_or_none}, lists:last(Params)),
    ?assertEqual(Command, maps:get(raw_function, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ExpectedNif =
        LeadingNif ++
            [
                {"InternalFormat", s154_enum_nif_data()}
            ] ++
            [{Name, s154_nif_type(Type)} || {Name, Type} <- MiddleParams] ++
            [
                {"Format", s154_enum_nif_data()},
                {"Type", s154_enum_nif_data()},
                {"Data", s154_binary_or_null_nif_data()}
            ],
    ?assertEqual(ExpectedNif, maps:get(params, NifData)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s154_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glClearNamedBufferDataEXT",
        "glClearNamedBufferSubDataEXT",
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s154_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard154-emitter-" ++
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

        [s154_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s154_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s154_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s154_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd)
    end.

s154_expected_erlang() ->
    [
        <<"-export([clear_buffer_data/5]).">>,
        <<"-export([clear_buffer_sub_data/7]).">>,
        <<"-export([clear_named_buffer_data/5]).">>,
        <<"-export([clear_named_buffer_sub_data/7]).">>,
        <<"-spec clear_buffer_data(\n    Target :: buffer_target(),\n    InternalFormat :: sized_internal_format(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Data :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"-spec clear_buffer_sub_data(\n    Target :: buffer_target(),\n    InternalFormat :: sized_internal_format(),\n    Offset :: gl:intptr(),\n    Size :: gl:sizeiptr(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Data :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"-spec clear_named_buffer_data(\n    Buffer :: buffer(),\n    InternalFormat :: sized_internal_format(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Data :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"-spec clear_named_buffer_sub_data(\n    Buffer :: buffer(),\n    InternalFormat :: sized_internal_format(),\n    Offset :: gl:intptr(),\n    Size :: gl:sizeiptr(),\n    Format :: pixel_format(),\n    Type :: pixel_type(),\n    Data :: iodata() | none\n) -> ok | {error, atom()}.">>,
        <<"Data0 = case Data of\n        none -> undefined;\n        _ -> iolist_to_binary(Data)\n    end,">>,
        <<"?CALL_RAW_FUNC(glClearBufferData_raw(NewTarget, NewInternalFormat, NewFormat, NewType, Data0)).">>,
        <<"?CALL_RAW_FUNC(glClearBufferSubData_raw(NewTarget, NewInternalFormat, Offset, Size, NewFormat, NewType, Data0)).">>,
        <<"?CALL_RAW_FUNC(glClearNamedBufferData_raw(Buffer, NewInternalFormat, NewFormat, NewType, Data0)).">>,
        <<"?CALL_RAW_FUNC(glClearNamedBufferSubData_raw(Buffer, NewInternalFormat, Offset, Size, NewFormat, NewType, Data0)).">>
    ].

s154_expected_c() ->
    [
        <<"const void* arg_4 = NULL;">>,
        <<"glClearBufferData(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"const void* arg_6 = NULL;">>,
        <<"glClearBufferSubData(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>,
        <<"glClearNamedBufferData(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
        <<"glClearNamedBufferSubData(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6);">>,
        <<"{\"glClearBufferData_raw\", 5, nif_glClearBufferData, 0}">>,
        <<"{\"glClearBufferSubData_raw\", 7, nif_glClearBufferSubData, 0}">>,
        <<"{\"glClearNamedBufferData_raw\", 5, nif_glClearNamedBufferData, 0}">>,
        <<"{\"glClearNamedBufferSubData_raw\", 7, nif_glClearNamedBufferSubData, 0}">>
    ].

s154_forbidden_surface() ->
    [
        <<"-export([map_buffer/">>,
        <<"-export([map_named_buffer/">>,
        <<"glClearNamedBufferDataEXT(">>,
        <<"glClearNamedBufferSubDataEXT(">>,
        <<"glMapBuffer(">>,
        <<"glMapNamedBuffer(">>
    ].

s154_absent_surface() ->
    s154_expected_erlang() ++
        [
            <<"glClearBufferData(">>,
            <<"glClearBufferSubData(">>,
            <<"glClearNamedBufferData(">>,
            <<"glClearNamedBufferSubData(">>,
            <<"{\"glClearBufferData_raw\", 5, nif_glClearBufferData, 0}">>,
            <<"{\"glClearBufferSubData_raw\", 7, nif_glClearBufferSubData, 0}">>,
            <<"{\"glClearNamedBufferData_raw\", 5, nif_glClearNamedBufferData, 0}">>,
            <<"{\"glClearNamedBufferSubData_raw\", 7, nif_glClearNamedBufferSubData, 0}">>
        ].

s154_type_spec(gl_intptr) -> {gl, intptr, []};
s154_type_spec(gl_sizeiptr) -> {gl, sizeiptr, []}.

s154_nif_type(gl_intptr) -> s154_intptr_nif_data();
s154_nif_type(gl_sizeiptr) -> s154_sizeiptr_nif_data().

s154_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s154_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s154_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s154_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

s154_binary_or_null_nif_data() ->
    in_gl_binary_or_null.

s154_assert_enum_contains(BindingData, TypeName, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(TypeName, EnumTypes))).

s154_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle)).

s154_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 163.
s163_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s163_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s163_emitter_tex_level_parameter_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s163_assert_emitted_present({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s163_assert_emitted_present({gles, {3, 2}})
        end},
        {"gles 3.0", fun() ->
            s163_assert_emitted_absent({gles, {3, 0}})
        end}
    ].

s163_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s163_supports_tex_level_parameter_readback(Target) of
        true ->
            ?assert(maps:is_key({"get_tex_level_parameter", 5}, Functions)),
            ?assertNot(maps:is_key({"get_tex_level_parameter", 4}, Functions)),
            GetTexLevelParameter = maps:get({"get_tex_level_parameter", 5}, Functions),
            s163_assert_specs(GetTexLevelParameter),
            s163_assert_family(GetTexLevelParameter, f, gl_float),
            s163_assert_family(GetTexLevelParameter, i, gl_int);
        false ->
            ?assertNot(maps:is_key({"get_tex_level_parameter", 5}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTexLevelParameterfv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTexLevelParameteriv", Functions))
    end,
    s163_assert_deferred_neighbors_absent(Functions).

s163_supports_tex_level_parameter_readback({gl, _Version}) ->
    true;
s163_supports_tex_level_parameter_readback({gles, {3, Minor}}) when Minor >= 1 ->
    true;
s163_supports_tex_level_parameter_readback(_) ->
    false.

s163_assert_specs(GetTexLevelParameter) ->
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Level", gl_int},
            {in, "ParamName", {gl_enum, "GetTextureParameter", texture_level_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetTexLevelParameter)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Target", {undefined, texture_target, []}},
            {"Level", {gl, int, []}},
            {"ParamName", {undefined, texture_level_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetTexLevelParameter)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_tex_level_parameter_value, []}}}],
        maps:get(specs_return, GetTexLevelParameter)
    ),
    ?assertEqual(5, maps:get(function_arity, GetTexLevelParameter)),
    {get_tex_level_parameter_value, {set, ValueTypes}} = maps:get(extra_type, GetTexLevelParameter),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)).

s163_assert_family(GetTexLevelParameter, TypeAtom, GlType) ->
    Command = s163_tex_level_parameter_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetTexLevelParameter))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetTexLevelParameter))),
    s163_assert_clause(GetTexLevelParameter, TypeAtom, Command),
    s163_assert_nif(GetTexLevelParameter, Command, GlType).

s163_assert_clause(GetTexLevelParameter, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s163_find_clause(Command, maps:get(function_clauses, GetTexLevelParameter)),
    [
        {Suffix, ignore},
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"Level", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TargetTransformMap)),
    ?assert(lists:keymember("texture_width", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_height", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s163_assert_nif(GetTexLevelParameter, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetTexLevelParameter)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s163_enum_nif_data()},
            {"Level", s163_int_nif_data()},
            {"ParamName", s163_enum_nif_data()},
            {"Values", {out_typed_value_list, s163_gl_ctype(GlType), s163_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s163_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s163_assert_emitted_present(Target) ->
    s163_assert_emitted_surface(
        Target,
        [
            <<"-export([get_tex_level_parameter/5]).">>,
            <<"-spec get_tex_level_parameter(\n    Type :: f | i,\n    Target :: texture_target(),\n    Level :: gl:int(),\n    ParamName :: texture_level_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_tex_level_parameter_value()]} | {error, atom()}.">>,
            <<"get_tex_level_parameter(f, Target, Level, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTexLevelParameterfv_raw(NewTarget, Level, NewParamName, Count))">>,
            <<"get_tex_level_parameter(i, Target, Level, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTexLevelParameteriv_raw(NewTarget, Level, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_3_count_tmp;">>,
            <<"GLfloat* arg_3_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_3_count);">>,
            <<"GLint* arg_3_values = enif_alloc(sizeof(GLint) * (size_t)arg_3_count);">>,
            <<"glGetTexLevelParameterfv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"glGetTexLevelParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetTexLevelParameterfv_raw\", 4, nif_glGetTexLevelParameterfv, 0}">>,
            <<"{\"glGetTexLevelParameteriv_raw\", 4, nif_glGetTexLevelParameteriv, 0}">>
        ],
        s163_forbidden_neighbors()
    ).

s163_assert_emitted_absent(Target) ->
    s163_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_tex_level_parameter">>,
            <<"glGetTexLevelParameterfv">>,
            <<"glGetTexLevelParameteriv">>
            | s163_forbidden_neighbors()
        ]
    ).

s163_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard163-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s163_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s163_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s163_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s163_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s163_tex_level_parameter_command(f) -> "glGetTexLevelParameterfv";
s163_tex_level_parameter_command(i) -> "glGetTexLevelParameteriv".

s163_gl_ctype(gl_float) -> "GLfloat";
s163_gl_ctype(gl_int) -> "GLint".

s163_term_function(gl_float) -> "enif_make_double";
s163_term_function(gl_int) -> "enif_make_int".

s163_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s163_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s163_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s163_forbidden_neighbors() ->
    [
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s163_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s163_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 164.
s164_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s164_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s164_emitter_texture_level_parameter_dsa_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s164_assert_emitted_present({gl, {4, 6}})
        end},
        {"gl 4.1", fun() ->
            s164_assert_emitted_absent({gl, {4, 1}})
        end},
        {"gles 3.2", fun() ->
            s164_assert_emitted_absent({gles, {3, 2}})
        end}
    ].

s164_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case Target of
        {gl, {4, 6}} ->
            ?assert(maps:is_key({"get_texture_level_parameter", 5}, Functions)),
            ?assertNot(maps:is_key({"get_texture_level_parameter", 4}, Functions)),
            GetTextureLevelParameter = maps:get({"get_texture_level_parameter", 5}, Functions),
            s164_assert_specs(GetTextureLevelParameter),
            s164_assert_family(GetTextureLevelParameter, f, gl_float),
            s164_assert_family(GetTextureLevelParameter, i, gl_int);
        _ ->
            ?assertNot(maps:is_key({"get_texture_level_parameter", 5}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTextureLevelParameterfv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glGetTextureLevelParameteriv", Functions))
    end,
    s164_assert_deferred_neighbors_absent(Functions).

s164_assert_specs(GetTextureLevelParameter) ->
    ?assertEqual(
        [
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "ParamName", {gl_enum, "GetTextureParameter", texture_level_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetTextureLevelParameter)
    ),
    ?assertEqual(
        [
            {"Type", {set, [f, i]}},
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"ParamName", {undefined, texture_level_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetTextureLevelParameter)
    ),
    ?assertEqual(
        [{"Values", {list, {undefined, get_texture_level_parameter_value, []}}}],
        maps:get(specs_return, GetTextureLevelParameter)
    ),
    ?assertEqual(5, maps:get(function_arity, GetTextureLevelParameter)),
    {get_texture_level_parameter_value, {set, ValueTypes}} =
        maps:get(extra_type, GetTextureLevelParameter),
    ?assert(lists:member({gl, float, []}, ValueTypes)),
    ?assert(lists:member({gl, int, []}, ValueTypes)).

s164_assert_family(GetTextureLevelParameter, TypeAtom, GlType) ->
    Command = s164_texture_level_parameter_command(TypeAtom),
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetTextureLevelParameter))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetTextureLevelParameter))),
    s164_assert_clause(GetTextureLevelParameter, TypeAtom, Command),
    s164_assert_nif(GetTextureLevelParameter, Command, GlType).

s164_assert_clause(GetTextureLevelParameter, TypeAtom, Command) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s164_find_clause(Command, maps:get(function_clauses, GetTextureLevelParameter)),
    [
        {Suffix, ignore},
        {"Texture", do_nothing},
        {"Level", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_width", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("texture_height", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s164_assert_nif(GetTextureLevelParameter, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetTextureLevelParameter)),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Texture", s164_texture_nif_data()},
            {"Level", s164_int_nif_data()},
            {"ParamName", s164_enum_nif_data()},
            {"Values", {out_typed_value_list, s164_gl_ctype(GlType), s164_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s164_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetTextureLevelParameterfvEXT",
        "glGetTextureLevelParameterivEXT",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s164_assert_emitted_present(Target) ->
    s164_assert_emitted_surface(
        Target,
        [
            <<"-export([get_texture_level_parameter/5]).">>,
            <<"-spec get_texture_level_parameter(\n    Type :: f | i,\n    Texture :: texture(),\n    Level :: gl:int(),\n    ParamName :: texture_level_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_texture_level_parameter_value()]} | {error, atom()}.">>,
            <<"get_texture_level_parameter(f, Texture, Level, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTextureLevelParameterfv_raw(Texture, Level, NewParamName, Count))">>,
            <<"get_texture_level_parameter(i, Texture, Level, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetTextureLevelParameteriv_raw(Texture, Level, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_3_count_tmp;">>,
            <<"if (arg_3_count_tmp == 0 || arg_3_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLfloat* arg_3_values = enif_alloc(sizeof(GLfloat) * (size_t)arg_3_count);">>,
            <<"GLint* arg_3_values = enif_alloc(sizeof(GLint) * (size_t)arg_3_count);">>,
            <<"glGetTextureLevelParameterfv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"glGetTextureLevelParameteriv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetTextureLevelParameterfv_raw\", 4, nif_glGetTextureLevelParameterfv, 0}">>,
            <<"{\"glGetTextureLevelParameteriv_raw\", 4, nif_glGetTextureLevelParameteriv, 0}">>
        ],
        s164_forbidden_neighbors()
    ).

s164_assert_emitted_absent(Target) ->
    s164_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_texture_level_parameter">>,
            <<"glGetTextureLevelParameterfv">>,
            <<"glGetTextureLevelParameteriv">>
            | s164_forbidden_neighbors()
        ]
    ).

s164_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard164-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s164_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s164_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s164_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s164_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s164_texture_level_parameter_command(f) -> "glGetTextureLevelParameterfv";
s164_texture_level_parameter_command(i) -> "glGetTextureLevelParameteriv".

s164_gl_ctype(gl_float) -> "GLfloat";
s164_gl_ctype(gl_int) -> "GLint".

s164_term_function(gl_float) -> "enif_make_double";
s164_term_function(gl_int) -> "enif_make_int".

s164_texture_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s164_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s164_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s164_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s164_forbidden_neighbors() ->
    [
        <<"glGetTextureLevelParameterfvEXT">>,
        <<"glGetTextureLevelParameterivEXT">>,
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s164_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s164_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 190.

s190_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s190_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s190_emitter_multi_bind_test_() ->
    [
        {"gl 4.6", fun() -> s190_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s190_assert_emitted_absent({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s190_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s190_assert_emitted_absent({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s190_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s190_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s190_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s190_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s190_supports_multi_bind(Target) of
        true ->
            s190_assert_enum_contains(BindingData, "buffer_target", "uniform_buffer"),
            s190_assert_path({"bind_buffers_base", 3}, maps:get({"bind_buffers_base", 3}, Functions)),
            s190_assert_path({"bind_buffers_range", 3}, maps:get({"bind_buffers_range", 3}, Functions)),
            s190_assert_path({"bind_textures", 2}, maps:get({"bind_textures", 2}, Functions)),
            s190_assert_path({"bind_samplers", 2}, maps:get({"bind_samplers", 2}, Functions)),
            s190_assert_path({"bind_image_textures", 2}, maps:get({"bind_image_textures", 2}, Functions)),
            s190_assert_path({"bind_vertex_buffers", 2}, maps:get({"bind_vertex_buffers", 2}, Functions)),
            s190_assert_path(
                {"vertex_array_vertex_buffers", 3},
                maps:get({"vertex_array_vertex_buffers", 3}, Functions)
            );
        false ->
            [
                ?assertNot(generator_test_support:has_gl_command(Command, Functions))
             || Command <- s190_multi_bind_commands()
            ],
            [
                ?assertNot(maps:is_key(Function, Functions))
             || Function <- [
                    {"bind_buffers_base", 3},
                    {"bind_buffers_range", 3},
                    {"bind_textures", 2},
                    {"bind_samplers", 2},
                    {"bind_image_textures", 2},
                    {"bind_vertex_buffers", 2},
                    {"vertex_array_vertex_buffers", 3}
                ]
            ]
    end,
    s190_assert_deferred_neighbors_absent(Functions).

s190_supports_multi_bind({gl, {4, 6}}) ->
    true;
s190_supports_multi_bind(_) ->
    false.

s190_assert_path({"bind_buffers_base", 3}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glBindBuffersBase",
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "First", gl_uint},
            {in, "Buffers", {multi_bind_object_list, buffer}}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"First", {gl, uint, []}},
            {"Buffers", s190_object_or_none_list(buffer)}
        ],
        [
            {"Target", s190_enum_nif_data()},
            {"First", s190_uint_nif_data()},
            {"Buffers", in_multi_bind_object_list}
        ]
    );
s190_assert_path({"bind_buffers_range", 3}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glBindBuffersRange",
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "First", gl_uint},
            {in, "Bindings", multi_bind_buffer_ranges}
        ],
        [
            {"Target", {undefined, buffer_target, []}},
            {"First", {gl, uint, []}},
            {"Bindings", s190_buffer_range_bindings()}
        ],
        [
            {"Target", s190_enum_nif_data()},
            {"First", s190_uint_nif_data()},
            {"Bindings", in_multi_bind_buffer_ranges}
        ]
    );
s190_assert_path({"bind_textures", 2}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glBindTextures",
        [
            {in, "First", gl_uint},
            {in, "Textures", {multi_bind_object_list, texture}}
        ],
        [
            {"First", {gl, uint, []}},
            {"Textures", s190_object_or_none_list(texture)}
        ],
        [
            {"First", s190_uint_nif_data()},
            {"Textures", in_multi_bind_object_list}
        ]
    );
s190_assert_path({"bind_samplers", 2}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glBindSamplers",
        [
            {in, "First", gl_uint},
            {in, "Samplers", {multi_bind_object_list, sampler}}
        ],
        [
            {"First", {gl, uint, []}},
            {"Samplers", s190_object_or_none_list(sampler)}
        ],
        [
            {"First", s190_uint_nif_data()},
            {"Samplers", in_multi_bind_object_list}
        ]
    );
s190_assert_path({"bind_image_textures", 2}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glBindImageTextures",
        [
            {in, "First", gl_uint},
            {in, "Textures", {multi_bind_object_list, texture}}
        ],
        [
            {"First", {gl, uint, []}},
            {"Textures", s190_object_or_none_list(texture)}
        ],
        [
            {"First", s190_uint_nif_data()},
            {"Textures", in_multi_bind_object_list}
        ]
    );
s190_assert_path({"bind_vertex_buffers", 2}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glBindVertexBuffers",
        [
            {in, "First", gl_uint},
            {in, "Bindings", multi_bind_vertex_buffers}
        ],
        [
            {"First", {gl, uint, []}},
            {"Bindings", s190_vertex_buffer_bindings()}
        ],
        [
            {"First", s190_uint_nif_data()},
            {"Bindings", in_multi_bind_vertex_buffers}
        ]
    );
s190_assert_path({"vertex_array_vertex_buffers", 3}, FunctionData) ->
    s190_assert_direct(
        FunctionData,
        "glVertexArrayVertexBuffers",
        [
            {in, "Array", {gl_object, vertex_array}},
            {in, "First", gl_uint},
            {in, "Bindings", multi_bind_vertex_buffers}
        ],
        [
            {"Array", {undefined, vertex_array, []}},
            {"First", {gl, uint, []}},
            {"Bindings", s190_vertex_buffer_bindings()}
        ],
        [
            {"Array", s190_uint_nif_data()},
            {"First", s190_uint_nif_data()},
            {"Bindings", in_multi_bind_vertex_buffers}
        ]
    ).

s190_assert_direct(FunctionData, Command, ParamsSpecs, SpecsParams, NifParams) ->
    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(Command, maps:get(raw_function, Clause)),
    s190_assert_clause_params(ParamsSpecs, maps:get(params, Clause)),

    NifData = maps:get(Command, maps:get(nif_functions, FunctionData)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s190_assert_clause_params(ParamsSpecs, ClauseParams) ->
    Expected =
        lists:map(
            fun
                ({in, Name, {gl_enum, _Group, _PublicName}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, {gl_enum, _Group}}) ->
                    {Name, {gl_enum_to_uint, enum_map}};
                ({in, Name, {multi_bind_object_list, _ObjectName}}) ->
                    {Name, multi_bind_object_list};
                ({in, Name, Type}) when
                    Type =:= multi_bind_buffer_ranges;
                    Type =:= multi_bind_vertex_buffers
                ->
                    {Name, Type};
                ({in, Name, _Type}) ->
                    {Name, do_nothing}
            end,
            ParamsSpecs
        ),
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(lists:keymember("uniform_buffer", 1, TransformMap)),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s190_assert_emitted_present(Target) ->
    {Erl, C} = s190_generate_surface(Target),
    s190_assert_export_and_call(Erl, <<"bind_buffers_base">>, 3, <<"glBindBuffersBase">>),
    s190_assert_export_and_call(Erl, <<"bind_buffers_range">>, 3, <<"glBindBuffersRange">>),
    s190_assert_export_and_call(Erl, <<"bind_textures">>, 2, <<"glBindTextures">>),
    s190_assert_export_and_call(Erl, <<"bind_samplers">>, 2, <<"glBindSamplers">>),
    s190_assert_export_and_call(Erl, <<"bind_image_textures">>, 2, <<"glBindImageTextures">>),
    s190_assert_export_and_call(Erl, <<"bind_vertex_buffers">>, 2, <<"glBindVertexBuffers">>),
    s190_assert_export_and_call(Erl, <<"vertex_array_vertex_buffers">>, 3, <<"glVertexArrayVertexBuffers">>),

    s190_assert_contains(Erl, <<"Buffers :: [buffer() | none]">>),
    s190_assert_contains(Erl, <<"Textures :: [texture() | none]">>),
    s190_assert_contains(Erl, <<"Samplers :: [sampler() | none]">>),
    s190_assert_contains(Erl, <<"Bindings :: [{buffer() | none, gl:intptr(), gl:sizeiptr()}]">>),
    s190_assert_contains(Erl, <<"Bindings :: [{buffer() | none, gl:intptr(), gl:sizei()}]">>),

    s190_assert_contains(C, <<"glBindBuffersBase(arg_0, arg_1, arg_2_count, (const GLuint*)arg_2_objects);">>),
    s190_assert_contains(
        C,
        <<"glBindBuffersRange(arg_0, arg_1, arg_2_count, (const GLuint*)arg_2_buffers, (const GLintptr*)arg_2_offsets, (const GLsizeiptr*)arg_2_sizes);">>
    ),
    s190_assert_contains(C, <<"glBindTextures(arg_0, arg_1_count, (const GLuint*)arg_1_objects);">>),
    s190_assert_contains(C, <<"glBindSamplers(arg_0, arg_1_count, (const GLuint*)arg_1_objects);">>),
    s190_assert_contains(C, <<"glBindImageTextures(arg_0, arg_1_count, (const GLuint*)arg_1_objects);">>),
    s190_assert_contains(
        C,
        <<"glBindVertexBuffers(arg_0, arg_1_count, (const GLuint*)arg_1_buffers, (const GLintptr*)arg_1_offsets, (const GLsizei*)arg_1_strides);">>
    ),
    s190_assert_contains(
        C,
        <<"glVertexArrayVertexBuffers(arg_0, arg_1, arg_2_count, (const GLuint*)arg_2_buffers, (const GLintptr*)arg_2_offsets, (const GLsizei*)arg_2_strides);">>
    ),
    s190_assert_contains(C, <<"enif_compare(arg_2_head, enif_make_atom(env, \"none\"))">>),
    s190_assert_contains(C, <<"arg_2_object_tmp == 0">>),
    s190_assert_deferred_emitted_absent(Erl, C).

s190_assert_emitted_absent(Target) ->
    {Erl, C} = s190_generate_surface(Target),
    [
        s190_assert_not_contains(Erl, Export)
     || Export <- [
            <<"-export([bind_buffers_base/3]).">>,
            <<"-export([bind_buffers_range/3]).">>,
            <<"-export([bind_textures/2]).">>,
            <<"-export([bind_samplers/2]).">>,
            <<"-export([bind_image_textures/2]).">>,
            <<"-export([bind_vertex_buffers/2]).">>,
            <<"-export([vertex_array_vertex_buffers/3]).">>
        ]
    ],
    [
        s190_assert_not_contains(C, Command)
     || Command <- [
            <<"glBindBuffersBase(">>,
            <<"glBindBuffersRange(">>,
            <<"glBindTextures(">>,
            <<"glBindSamplers(">>,
            <<"glBindImageTextures(">>,
            <<"glBindVertexBuffers(">>,
            <<"glVertexArrayVertexBuffers(">>
        ]
    ],
    s190_assert_deferred_emitted_absent(Erl, C).

s190_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard190-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s190_ensure_absent(Dir),
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

s190_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s190_object_or_none_list(ObjectName) ->
    {list, {set, [{undefined, ObjectName, []}, none]}}.

s190_buffer_range_bindings() ->
    {
        list,
        {tuple, [
            {set, [{undefined, buffer, []}, none]},
            {gl, intptr, []},
            {gl, sizeiptr, []}
        ]}
    }.

s190_vertex_buffer_bindings() ->
    {
        list,
        {tuple, [
            {set, [{undefined, buffer, []}, none]},
            {gl, intptr, []},
            {gl, sizei, []}
        ]}
    }.

s190_multi_bind_commands() ->
    [
        "glBindBuffersBase",
        "glBindBuffersRange",
        "glBindTextures",
        "glBindSamplers",
        "glBindImageTextures",
        "glBindVertexBuffers",
        "glVertexArrayVertexBuffers"
    ].

s190_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s190_deferred_neighbor_commands()
    ].

s190_assert_deferred_emitted_absent(Erl, C) ->
    [
        s190_assert_not_contains(Erl, Needle)
     || Needle <- [
        ]
    ],
    [
        s190_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glCopyImageSubDataEXT(">>,
            <<"glCopyImageSubDataNV(">>,
            <<"glCopyImageSubDataOES(">>
        ]
    ].

s190_deferred_neighbor_commands() ->
    [
        "glCopyImageSubDataEXT",
        "glCopyImageSubDataNV",
        "glCopyImageSubDataOES"
    ].

s190_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s190_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s190_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s190_assert_export_and_call(Erl, FunctionName, Arity, Command) ->
    s190_assert_contains(Erl, <<"-export([", FunctionName/binary, "/", (integer_to_binary(Arity))/binary, "]).">>),
    s190_assert_contains(Erl, <<"?CALL_RAW_FUNC(", Command/binary, "_raw(">>).

s190_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s190_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s190_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

%% Historical shard 191.

s191_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s191_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s191_emitter_copy_image_test_() ->
    [
        {"gl 4.6", fun() -> s191_assert_emitted_present({gl, {4, 6}}) end},
        {"gl 4.1", fun() -> s191_assert_emitted_absent({gl, {4, 1}}) end},
        {"gl 3.3", fun() -> s191_assert_emitted_absent({gl, {3, 3}}) end},
        {"gles 3.2", fun() -> s191_assert_emitted_present({gles, {3, 2}}) end},
        {"gles 3.1", fun() -> s191_assert_emitted_absent({gles, {3, 1}}) end},
        {"gles 3.0", fun() -> s191_assert_emitted_absent({gles, {3, 0}}) end},
        {"gles 2.0", fun() -> s191_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s191_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s191_supports_copy_image(Target) of
        true ->
            s191_assert_enum_contains(BindingData, "copy_image_sub_data_target", "texture_2d"),
            s191_assert_enum_contains(BindingData, "copy_image_sub_data_target", "renderbuffer"),
            s191_assert_path(maps:get({"copy_image_sub_data", 15}, Functions));
        false ->
            ?assertNot(maps:is_key({"copy_image_sub_data", 15}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glCopyImageSubData", Functions))
    end,
    s191_assert_deferred_neighbors_absent(Functions).

s191_supports_copy_image({gl, {4, 6}}) ->
    true;
s191_supports_copy_image({gles, {3, 2}}) ->
    true;
s191_supports_copy_image(_) ->
    false.

s191_assert_path(FunctionData) ->
    ParamsSpecs = s191_params_specs(),
    SpecsParams = s191_specs_params(),
    NifParams = s191_nif_params(),

    ?assertEqual("glCopyImageSubData", maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(15, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glCopyImageSubData", maps:get(raw_function, Clause)),
    s191_assert_clause_params(maps:get(params, Clause)),

    NifData = maps:get("glCopyImageSubData", maps:get(nif_functions, FunctionData)),
    ?assertEqual("glCopyImageSubData", maps:get(gl_command, NifData)),
    ?assertEqual(15, maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s191_params_specs() ->
    [
        {in, "SrcName", {gl_object_union, [texture, renderbuffer]}},
        {in, "SrcTarget", {gl_enum, "CopyImageSubDataTarget"}},
        {in, "SrcLevel", gl_int},
        {in, "SrcX", gl_int},
        {in, "SrcY", gl_int},
        {in, "SrcZ", gl_int},
        {in, "DstName", {gl_object_union, [texture, renderbuffer]}},
        {in, "DstTarget", {gl_enum, "CopyImageSubDataTarget"}},
        {in, "DstLevel", gl_int},
        {in, "DstX", gl_int},
        {in, "DstY", gl_int},
        {in, "DstZ", gl_int},
        {in, "Width", gl_sizei},
        {in, "Height", gl_sizei},
        {in, "Depth", gl_sizei}
    ].

s191_specs_params() ->
    [
        {"SrcName", s191_object_union_specs()},
        {"SrcTarget", {undefined, copy_image_sub_data_target, []}},
        {"SrcLevel", {gl, int, []}},
        {"SrcX", {gl, int, []}},
        {"SrcY", {gl, int, []}},
        {"SrcZ", {gl, int, []}},
        {"DstName", s191_object_union_specs()},
        {"DstTarget", {undefined, copy_image_sub_data_target, []}},
        {"DstLevel", {gl, int, []}},
        {"DstX", {gl, int, []}},
        {"DstY", {gl, int, []}},
        {"DstZ", {gl, int, []}},
        {"Width", {gl, sizei, []}},
        {"Height", {gl, sizei, []}},
        {"Depth", {gl, sizei, []}}
    ].

s191_object_union_specs() ->
    {set, [{undefined, texture, []}, {undefined, renderbuffer, []}]}.

s191_nif_params() ->
    [
        {"SrcName", s191_uint_nif_data()},
        {"SrcTarget", s191_enum_nif_data()},
        {"SrcLevel", s191_int_nif_data()},
        {"SrcX", s191_int_nif_data()},
        {"SrcY", s191_int_nif_data()},
        {"SrcZ", s191_int_nif_data()},
        {"DstName", s191_uint_nif_data()},
        {"DstTarget", s191_enum_nif_data()},
        {"DstLevel", s191_int_nif_data()},
        {"DstX", s191_int_nif_data()},
        {"DstY", s191_int_nif_data()},
        {"DstZ", s191_int_nif_data()},
        {"Width", s191_sizei_nif_data()},
        {"Height", s191_sizei_nif_data()},
        {"Depth", s191_sizei_nif_data()}
    ].

s191_assert_clause_params(ClauseParams) ->
    Expected =
        [
            {"SrcName", do_nothing},
            {"SrcTarget", {gl_enum_to_uint, enum_map}},
            {"SrcLevel", do_nothing},
            {"SrcX", do_nothing},
            {"SrcY", do_nothing},
            {"SrcZ", do_nothing},
            {"DstName", do_nothing},
            {"DstTarget", {gl_enum_to_uint, enum_map}},
            {"DstLevel", do_nothing},
            {"DstX", do_nothing},
            {"DstY", do_nothing},
            {"DstZ", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing},
            {"Depth", do_nothing}
        ],
    Actual =
        lists:map(
            fun
                ({Name, {gl_enum_to_uint, TransformMap}}) ->
                    ?assert(lists:keymember("texture_2d", 1, TransformMap)),
                    ?assert(lists:keymember("renderbuffer", 1, TransformMap)),
                    {Name, {gl_enum_to_uint, enum_map}};
                (Other) ->
                    Other
            end,
            ClauseParams
        ),
    ?assertEqual(Expected, Actual).

s191_assert_emitted_present(Target) ->
    {Erl, C} = s191_generate_surface(Target),
    s191_assert_contains(Erl, <<"-export([copy_image_sub_data/15]).">>),
    s191_assert_contains(Erl, <<"SrcName :: texture() | renderbuffer()">>),
    s191_assert_contains(Erl, <<"DstName :: texture() | renderbuffer()">>),
    s191_assert_contains(Erl, <<"SrcTarget :: copy_image_sub_data_target()">>),
    s191_assert_contains(Erl, <<"DstTarget :: copy_image_sub_data_target()">>),
    s191_assert_contains(Erl, <<"?CALL_RAW_FUNC(glCopyImageSubData_raw(">>),
    s191_assert_contains(
        C,
        <<"glCopyImageSubData(arg_0, arg_1, arg_2, arg_3, arg_4, arg_5, arg_6, arg_7, arg_8, arg_9, arg_10, arg_11, arg_12, arg_13, arg_14);">>
    ),
    s191_assert_deferred_emitted_absent(Erl, C).

s191_assert_emitted_absent(Target) ->
    {Erl, C} = s191_generate_surface(Target),
    s191_assert_not_contains(Erl, <<"-export([copy_image_sub_data/15]).">>),
    s191_assert_not_contains(C, <<"glCopyImageSubData(">>),
    s191_assert_deferred_emitted_absent(Erl, C).

s191_generate_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard191-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s191_ensure_absent(Dir),
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

s191_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s191_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s191_deferred_neighbor_commands()
    ].

s191_assert_deferred_emitted_absent(Erl, C) ->
    [
        s191_assert_not_contains(Erl, Needle)
     || Needle <- [
        ]
    ],
    [
        s191_assert_not_contains(C, Needle)
     || Needle <- [
            <<"glCopyImageSubDataEXT(">>,
            <<"glCopyImageSubDataNV(">>,
            <<"glCopyImageSubDataOES(">>
        ]
    ].

s191_deferred_neighbor_commands() ->
    [
        "glCopyImageSubDataEXT",
        "glCopyImageSubDataNV",
        "glCopyImageSubDataOES"
    ].

s191_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(EnumType, EnumTypes)),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s191_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s191_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s191_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s191_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s191_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s191_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s191_assert_not_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertEqual(nomatch, string:find(Haystack, Needle));
s191_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
