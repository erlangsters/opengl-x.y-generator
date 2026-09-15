-module(resolver_state_tests).
-include_lib("eunit/include/eunit.hrl").

%% State and scalar command resolver contracts.

%% Historical shard 1.
s001_gl_4_6_resolver_test() ->
    s001_assert_target({gl, {4, 6}}).

s001_gles_3_2_resolver_test() ->
    s001_assert_target({gles, {3, 2}}).

s001_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s001_assert_function(Functions, {"cull_face", 1}, "glCullFace"),
    s001_assert_function(Functions, {"front_face", 1}, "glFrontFace"),
    s001_assert_function(Functions, {"line_width", 1}, "glLineWidth"),
    s001_assert_function(Functions, {"scissor", 4}, "glScissor"),

    s001_assert_enum_path(
        maps:get({"front_face", 1}, Functions),
        "FrontFaceDirection"
    ),
    s001_assert_float_path(maps:get({"line_width", 1}, Functions)),
    s001_assert_int_sizei_path(maps:get({"scissor", 4}, Functions)),
    ok.

s001_assert_function(Functions, Key, GlCommand) ->
    ?assert(maps:is_key(Key, Functions)),
    ?assertEqual(GlCommand, maps:get(gl_command, maps:get(Key, Functions))).

s001_assert_enum_path(FunctionData, EnumGroup) ->
    ?assertEqual(
        [{in, "Mode", {gl_enum, EnumGroup}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertMatch(
        [{"Mode", {undefined, _EnumTypeName, []}}],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Mode", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(length(TransformMap) > 0),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(maps:get(gl_command, FunctionData), NifFunctions),
    ?assertEqual(
        [{"Mode", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ).

s001_assert_float_path(FunctionData) ->
    ?assertEqual(
        [{in, "Width", gl_float}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Width", {gl, float, []}}],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Width", do_nothing}], maps:get(params, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glLineWidth", NifFunctions),
    ?assertEqual(
        [{"Width", {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}}],
        maps:get(params, NifData)
    ).

s001_assert_int_sizei_path(FunctionData) ->
    ?assertEqual(
        [
            {in, "X", gl_int},
            {in, "Y", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"X", {gl, int, []}},
            {"Y", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"X", do_nothing},
            {"Y", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        maps:get(params, Clause)
    ),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glScissor", NifFunctions),
    ?assertEqual(
        [
            {"X", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
            {"Y", {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}},
            {"Width", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {"Height", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}}
        ],
        maps:get(params, NifData)
    ).

%% Historical shard 2.
s002_gl_4_6_resolver_test() ->
    s002_assert_target({gl, {4, 6}}).

s002_gles_3_2_resolver_test() ->
    s002_assert_target({gles, {3, 2}}).

s002_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s002_assert_function(Functions, {"clear", 1}, "glClear"),
    s002_assert_function(Functions, {"clear_color", 4}, "glClearColor"),
    s002_assert_function(Functions, {"color_mask", 4}, "glColorMask"),

    s002_assert_bitfield_path(maps:get({"clear", 1}, Functions)),
    s002_assert_clear_color_path(maps:get({"clear_color", 4}, Functions)),
    s002_assert_bool_path(maps:get({"color_mask", 4}, Functions)),
    ok.

s002_assert_function(Functions, Key, GlCommand) ->
    ?assert(maps:is_key(Key, Functions)),
    ?assertEqual(GlCommand, maps:get(gl_command, maps:get(Key, Functions))).

s002_assert_bitfield_path(FunctionData) ->
    ?assertEqual(
        [{in, "Mask", {gl_bitfield, "ClearBufferMask"}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Mask", {undefined, clear_buffer_mask, []}}],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Mask", {gl_bitfield_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(length(TransformMap) > 0),
    ?assert(lists:keymember("color_buffer_bit", 1, TransformMap)),
    ?assert(lists:keymember("GL_COLOR_BUFFER_BIT", 2, TransformMap)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glClear", NifFunctions),
    ?assertEqual(
        [{"Mask", {gl_type, {"GLbitfield", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ).

s002_assert_clear_color_path(FunctionData) ->
    ?assertEqual(
        [
            {in, "Red", gl_float},
            {in, "Green", gl_float},
            {in, "Blue", gl_float},
            {in, "Alpha", gl_float}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Red", {gl, float, []}},
            {"Green", {gl, float, []}},
            {"Blue", {gl, float, []}},
            {"Alpha", {gl, float, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Red", do_nothing},
            {"Green", do_nothing},
            {"Blue", do_nothing},
            {"Alpha", do_nothing}
        ],
        maps:get(params, Clause)
    ),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glClearColor", NifFunctions),
    ?assertEqual(
        [
            {"Red", s002_float_nif_data()},
            {"Green", s002_float_nif_data()},
            {"Blue", s002_float_nif_data()},
            {"Alpha", s002_float_nif_data()}
        ],
        maps:get(params, NifData)
    ).

s002_assert_bool_path(FunctionData) ->
    ?assertEqual(
        [
            {in, "Red", gl_bool},
            {in, "Green", gl_bool},
            {in, "Blue", gl_bool},
            {in, "Alpha", gl_bool}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Red", {gl, boolean, []}},
            {"Green", {gl, boolean, []}},
            {"Blue", {gl, boolean, []}},
            {"Alpha", {gl, boolean, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Red", do_nothing},
            {"Green", do_nothing},
            {"Blue", do_nothing},
            {"Alpha", do_nothing}
        ],
        maps:get(params, Clause)
    ),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glColorMask", NifFunctions),
    ?assertEqual(
        [
            {"Red", boolean_to_glbool},
            {"Green", boolean_to_glbool},
            {"Blue", boolean_to_glbool},
            {"Alpha", boolean_to_glbool}
        ],
        maps:get(params, NifData)
    ).

s002_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 3.
s003_gl_4_6_resolver_test() ->
    s003_assert_target({gl, {4, 6}}).

s003_gles_3_2_resolver_test() ->
    s003_assert_target({gles, {3, 2}}).

s003_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s003_assert_function(Functions, {"clear_stencil", 1}, "glClearStencil"),
    s003_assert_function(Functions, {"depth_mask", 1}, "glDepthMask"),
    s003_assert_function(Functions, {"stencil_mask", 1}, "glStencilMask"),

    s003_assert_clear_stencil_path(maps:get({"clear_stencil", 1}, Functions)),
    s003_assert_depth_mask_path(maps:get({"depth_mask", 1}, Functions)),
    s003_assert_stencil_mask_path(maps:get({"stencil_mask", 1}, Functions)),
    ok.

s003_assert_function(Functions, Key, GlCommand) ->
    ?assert(maps:is_key(Key, Functions)),
    ?assertEqual(GlCommand, maps:get(gl_command, maps:get(Key, Functions))).

s003_assert_clear_stencil_path(FunctionData) ->
    ?assertEqual(
        [{in, "Value", gl_int}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Value", {gl, int, []}}],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Value", do_nothing}], maps:get(params, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glClearStencil", NifFunctions),
    ?assertEqual(
        [{"Value", s003_int_nif_data()}],
        maps:get(params, NifData)
    ).

s003_assert_depth_mask_path(FunctionData) ->
    ?assertEqual(
        [{in, "Flag", gl_bool}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Flag", {gl, boolean, []}}],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Flag", do_nothing}], maps:get(params, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDepthMask", NifFunctions),
    ?assertEqual(
        [{"Flag", boolean_to_glbool}],
        maps:get(params, NifData)
    ).

s003_assert_stencil_mask_path(FunctionData) ->
    ?assertEqual(
        [{in, "Mask", gl_uint}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Mask", {gl, uint, []}}],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Mask", do_nothing}], maps:get(params, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glStencilMask", NifFunctions),
    ?assertEqual(
        [{"Mask", s003_uint_nif_data()}],
        maps:get(params, NifData)
    ).

s003_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s003_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 5.
s005_desktop_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s005_assert_desktop_target(Target) end}
     || Target <- generator_test_support:desktop_targets()].

s005_es_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s005_assert_es_target(Target) end}
     || Target <- generator_test_support:es_targets()].

s005_assert_desktop_target(Target) ->
    FunctionData = s005_clear_depth_function_data(Target),
    s005_assert_clear_depth_path(FunctionData, #{
        command => "glClearDepth",
        absent_command => "glClearDepthf",
        param_spec => gl_double,
        public_spec => {gl, double, []},
        nif_data => {gl_type, {"GLdouble", "double", "enif_get_double", "enif_make_double"}}
    }).

s005_assert_es_target(Target) ->
    FunctionData = s005_clear_depth_function_data(Target),
    s005_assert_clear_depth_path(FunctionData, #{
        command => "glClearDepthf",
        absent_command => "glClearDepth",
        param_spec => gl_float,
        public_spec => {gl, float, []},
        nif_data => {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}
    }).

s005_clear_depth_function_data(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    ?assert(maps:is_key({"clear_depth", 1}, Functions)),
    maps:get({"clear_depth", 1}, Functions).

s005_assert_clear_depth_path(FunctionData, Expected) ->
    Command = maps:get(command, Expected),
    AbsentCommand = maps:get(absent_command, Expected),

    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Depth", maps:get(param_spec, Expected)}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Depth", maps:get(public_spec, Expected)}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Depth", do_nothing}], maps:get(params, Clause)),
    ?assertEqual(Command, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key(Command, NifFunctions)),
    ?assertNot(maps:is_key(AbsentCommand, NifFunctions)),

    NifData = maps:get(Command, NifFunctions),
    ?assertEqual([{"Depth", maps:get(nif_data, Expected)}], maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 6.
s006_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s006_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s006_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s006_assert_capability_function(
        maps:get({"enable", 1}, Functions),
        "glEnable"
    ),
    s006_assert_capability_function(
        maps:get({"disable", 1}, Functions),
        "glDisable"
    ).

s006_assert_capability_function(FunctionData, RawCommand) ->
    ?assertEqual(RawCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Cap", {gl_enum, "EnableCap"}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Cap", {undefined, enable_cap, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Cap", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(lists:keymember("scissor_test", 1, TransformMap)),
    ?assert(lists:keymember("blend", 1, TransformMap)),
    ?assert(lists:keymember("cull_face", 1, TransformMap)),
    ?assertEqual(RawCommand, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key(RawCommand, NifFunctions)),
    NifData = maps:get(RawCommand, NifFunctions),
    ?assertEqual(
        [{"Cap", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 14.
s014_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s014_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s014_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"is_enabled", 1}, Functions)),

    s014_assert_is_enabled(maps:get({"is_enabled", 1}, Functions)).

s014_assert_is_enabled(FunctionData) ->
    ?assertEqual("glIsEnabled", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Capability", {gl_enum, "EnableCap"}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"IsEnabled", gl_bool}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Capability", {undefined, enable_cap, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"IsEnabled", {gl, boolean, []}}],
        maps:get(specs_return, FunctionData)
    ),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Capability", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(lists:keymember("scissor_test", 1, TransformMap)),
    ?assert(lists:keymember("blend", 1, TransformMap)),
    ?assertEqual("glIsEnabled", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glIsEnabled", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Capability", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

%% Historical shard 15.
s015_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s015_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s015_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"viewport", 4}, Functions)),

    s015_assert_viewport(maps:get({"viewport", 4}, Functions)).

s015_assert_viewport(FunctionData) ->
    ?assertEqual("glViewport", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "X", gl_int},
            {in, "Y", gl_int},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"X", {gl, int, []}},
            {"Y", {gl, int, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"X", do_nothing},
            {"Y", do_nothing},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glViewport", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glViewport", NifFunctions),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"X", s015_int_nif_data()},
            {"Y", s015_int_nif_data()},
            {"Width", s015_sizei_nif_data()},
            {"Height", s015_sizei_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s015_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s015_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 16.
s016_desktop_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s016_assert_desktop_target(Target) end}
     || Target <- generator_test_support:desktop_targets()].

s016_es_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s016_assert_es_target(Target) end}
     || Target <- generator_test_support:es_targets()].

s016_assert_desktop_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s016_assert_deferred_absences(Functions),
    s016_assert_depth_range_path(maps:get({"depth_range", 2}, Functions), #{
        command => "glDepthRange",
        absent_command => "glDepthRangef",
        param_spec => gl_double,
        public_spec => {gl, double, []},
        nif_data => {gl_type, {"GLdouble", "double", "enif_get_double", "enif_make_double"}}
    }).

s016_assert_es_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s016_assert_deferred_absences(Functions),
    s016_assert_depth_range_path(maps:get({"depth_range", 2}, Functions), #{
        command => "glDepthRangef",
        absent_command => "glDepthRange",
        param_spec => gl_float,
        public_spec => {gl, float, []},
        nif_data => {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}
    }).

s016_assert_deferred_absences(Functions) ->
    ?assert(maps:is_key({"depth_range", 2}, Functions)).

s016_assert_depth_range_path(FunctionData, Expected) ->
    Command = maps:get(command, Expected),
    AbsentCommand = maps:get(absent_command, Expected),
    ParamSpec = maps:get(param_spec, Expected),
    PublicSpec = maps:get(public_spec, Expected),
    NifData = maps:get(nif_data, Expected),

    ?assertEqual(Command, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Near", ParamSpec},
            {in, "Far", ParamSpec}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Near", PublicSpec},
            {"Far", PublicSpec}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Near", do_nothing},
            {"Far", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual(Command, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key(Command, NifFunctions)),
    ?assertNot(maps:is_key(AbsentCommand, NifFunctions)),

    RawNifData = maps:get(Command, NifFunctions),
    ?assertEqual(2, maps:get(arity, RawNifData)),
    ?assertEqual(
        [
            {"Near", NifData},
            {"Far", NifData}
        ],
        maps:get(params, RawNifData)
    ),
    ?assertEqual(void, maps:get(return, RawNifData)).

%% Historical shard 17.
s017_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s017_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s017_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"polygon_offset", 2}, Functions)),

    s017_assert_polygon_offset_path(maps:get({"polygon_offset", 2}, Functions)).

s017_assert_polygon_offset_path(FunctionData) ->
    ?assertEqual("glPolygonOffset", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Factor", gl_float},
            {in, "Units", gl_float}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Factor", {gl, float, []}},
            {"Units", {gl, float, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Factor", do_nothing},
            {"Units", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glPolygonOffset", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glPolygonOffset", NifFunctions)),
    NifData = maps:get("glPolygonOffset", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Factor", s017_float_nif_data()},
            {"Units", s017_float_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s017_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 18.
s018_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s018_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s018_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"depth_func", 1}, Functions)),

    s018_assert_depth_function_enum(BindingData),
    s018_assert_depth_func_path(maps:get({"depth_func", 1}, Functions)).

s018_assert_depth_function_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("depth_function", EnumTypes)),
    DepthFunctions = maps:get("depth_function", EnumTypes),
    ?assert(lists:member("less", DepthFunctions)),
    ?assert(lists:member("lequal", DepthFunctions)),
    ?assert(lists:member("always", DepthFunctions)).

s018_assert_depth_func_path(FunctionData) ->
    ?assertEqual("glDepthFunc", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Function", {gl_enum, "DepthFunction"}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Function", {undefined, depth_function, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Function", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    ?assert(lists:keymember("less", 1, TransformMap)),
    ?assert(lists:keymember("lequal", 1, TransformMap)),
    ?assert(lists:keymember("always", 1, TransformMap)),
    ?assertEqual("glDepthFunc", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glDepthFunc", NifFunctions)),
    NifData = maps:get("glDepthFunc", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Function",
                {gl_type,
                    {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 19.
s019_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s019_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s019_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"stencil_func", 3}, Functions)),

    s019_assert_stencil_function_enum(BindingData),
    s019_assert_stencil_func_path(maps:get({"stencil_func", 3}, Functions)).

s019_assert_stencil_function_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("stencil_function", EnumTypes)),
    StencilFunctions = maps:get("stencil_function", EnumTypes),
    ?assert(lists:member("always", StencilFunctions)),
    ?assert(lists:member("equal", StencilFunctions)),
    ?assert(lists:member("less", StencilFunctions)).

s019_assert_stencil_func_path(FunctionData) ->
    ?assertEqual("glStencilFunc", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Function", {gl_enum, "StencilFunction"}},
            {in, "Ref", gl_int},
            {in, "Mask", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Function", {undefined, stencil_function, []}},
            {"Ref", {gl, int, []}},
            {"Mask", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Function", {gl_enum_to_uint, TransformMap}},
        {"Ref", do_nothing},
        {"Mask", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("always", 1, TransformMap)),
    ?assert(lists:keymember("equal", 1, TransformMap)),
    ?assert(lists:keymember("less", 1, TransformMap)),
    ?assertEqual("glStencilFunc", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glStencilFunc", NifFunctions)),
    NifData = maps:get("glStencilFunc", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Function", s019_enum_nif_data()},
            {"Ref", s019_int_nif_data()},
            {"Mask", s019_uint_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s019_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s019_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s019_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 20.
s020_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s020_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s020_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"stencil_op", 3}, Functions)),

    s020_assert_stencil_op_enum(BindingData),
    s020_assert_stencil_op_path(maps:get({"stencil_op", 3}, Functions)).

s020_assert_stencil_op_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("stencil_op", EnumTypes)),
    StencilOps = maps:get("stencil_op", EnumTypes),
    ?assert(lists:member("keep", StencilOps)),
    ?assert(lists:member("zero", StencilOps)),
    ?assert(lists:member("replace", StencilOps)),
    ?assert(lists:member("incr", StencilOps)),
    ?assert(lists:member("decr", StencilOps)),
    ?assert(lists:member("invert", StencilOps)),
    ?assert(lists:member("incr_wrap", StencilOps)),
    ?assert(lists:member("decr_wrap", StencilOps)).

s020_assert_stencil_op_path(FunctionData) ->
    ?assertEqual("glStencilOp", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Fail", {gl_enum, "StencilOp"}},
            {in, "ZFail", {gl_enum, "StencilOp"}},
            {in, "ZPass", {gl_enum, "StencilOp"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Fail", {undefined, stencil_op, []}},
            {"ZFail", {undefined, stencil_op, []}},
            {"ZPass", {undefined, stencil_op, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Fail", {gl_enum_to_uint, FailTransformMap}},
        {"ZFail", {gl_enum_to_uint, ZFailTransformMap}},
        {"ZPass", {gl_enum_to_uint, ZPassTransformMap}}
    ] = maps:get(params, Clause),
    s020_assert_stencil_op_transform(FailTransformMap),
    s020_assert_stencil_op_transform(ZFailTransformMap),
    s020_assert_stencil_op_transform(ZPassTransformMap),
    ?assertEqual("glStencilOp", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glStencilOp", NifFunctions)),
    NifData = maps:get("glStencilOp", NifFunctions),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Fail", s020_enum_nif_data()},
            {"ZFail", s020_enum_nif_data()},
            {"ZPass", s020_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s020_assert_stencil_op_transform(TransformMap) ->
    ?assert(lists:keymember("keep", 1, TransformMap)),
    ?assert(lists:keymember("zero", 1, TransformMap)),
    ?assert(lists:keymember("replace", 1, TransformMap)),
    ?assert(lists:keymember("incr", 1, TransformMap)),
    ?assert(lists:keymember("decr", 1, TransformMap)),
    ?assert(lists:keymember("invert", 1, TransformMap)),
    ?assert(lists:keymember("incr_wrap", 1, TransformMap)),
    ?assert(lists:keymember("decr_wrap", 1, TransformMap)).

s020_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 21.
s021_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s021_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s021_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"blend_func", 2}, Functions)),

    s021_assert_blending_factor_enum(BindingData),
    s021_assert_blend_func_path(maps:get({"blend_func", 2}, Functions)).

s021_assert_blending_factor_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("blending_factor", EnumTypes)),
    BlendingFactors = maps:get("blending_factor", EnumTypes),
    ?assert(lists:member("zero", BlendingFactors)),
    ?assert(lists:member("one", BlendingFactors)),
    ?assert(lists:member("src_alpha", BlendingFactors)),
    ?assert(lists:member("one_minus_src_alpha", BlendingFactors)),
    ?assert(lists:member("dst_alpha", BlendingFactors)),
    ?assert(lists:member("one_minus_dst_alpha", BlendingFactors)).

s021_assert_blend_func_path(FunctionData) ->
    ?assertEqual("glBlendFunc", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "SourceFactor", {gl_enum, "BlendingFactor"}},
            {in, "DestinationFactor", {gl_enum, "BlendingFactor"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"SourceFactor", {undefined, blending_factor, []}},
            {"DestinationFactor", {undefined, blending_factor, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"SourceFactor", {gl_enum_to_uint, SourceTransformMap}},
        {"DestinationFactor", {gl_enum_to_uint, DestinationTransformMap}}
    ] = maps:get(params, Clause),
    s021_assert_blending_factor_transform(SourceTransformMap),
    s021_assert_blending_factor_transform(DestinationTransformMap),
    ?assertEqual("glBlendFunc", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBlendFunc", NifFunctions)),
    NifData = maps:get("glBlendFunc", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"SourceFactor", s021_enum_nif_data()},
            {"DestinationFactor", s021_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s021_assert_blending_factor_transform(TransformMap) ->
    ?assert(lists:keymember("zero", 1, TransformMap)),
    ?assert(lists:keymember("one", 1, TransformMap)),
    ?assert(lists:keymember("src_alpha", 1, TransformMap)),
    ?assert(lists:keymember("one_minus_src_alpha", 1, TransformMap)),
    ?assert(lists:keymember("dst_alpha", 1, TransformMap)),
    ?assert(lists:keymember("one_minus_dst_alpha", 1, TransformMap)).

s021_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 22.
s022_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s022_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s022_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"blend_func", 2}, Functions)),
    ?assert(maps:is_key({"blend_equation", 1}, Functions)),

    s022_assert_blend_equation_mode_enum(BindingData),
    s022_assert_blend_equation_path(maps:get({"blend_equation", 1}, Functions)).

s022_assert_blend_equation_mode_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("blend_equation_mode", EnumTypes)),
    ?assertNot(maps:is_key("blend_equation_mode_ext", EnumTypes)),
    BlendEquationModes = maps:get("blend_equation_mode", EnumTypes),
    ?assert(lists:member("func_add", BlendEquationModes)),
    ?assert(lists:member("func_subtract", BlendEquationModes)),
    ?assert(lists:member("func_reverse_subtract", BlendEquationModes)).

s022_assert_blend_equation_path(FunctionData) ->
    ?assertEqual("glBlendEquation", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Mode", {gl_enum, "BlendEquationModeEXT", blend_equation_mode}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Mode", {undefined, blend_equation_mode, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Mode", {gl_enum_to_uint, TransformMap}}] = maps:get(params, Clause),
    s022_assert_blend_equation_mode_transform(TransformMap),
    ?assertEqual("glBlendEquation", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBlendEquation", NifFunctions)),
    NifData = maps:get("glBlendEquation", NifFunctions),
    ?assertEqual(1, maps:get(arity, NifData)),
    ?assertEqual([{"Mode", s022_enum_nif_data()}], maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s022_assert_blend_equation_mode_transform(TransformMap) ->
    ?assert(lists:keymember("func_add", 1, TransformMap)),
    ?assert(lists:keymember("func_subtract", 1, TransformMap)),
    ?assert(lists:keymember("func_reverse_subtract", 1, TransformMap)).

s022_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 23.
s023_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s023_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s023_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"blend_func", 2}, Functions)),
    ?assert(maps:is_key({"blend_equation", 1}, Functions)),
    ?assert(maps:is_key({"blend_color", 4}, Functions)),

    s023_assert_blend_color_path(maps:get({"blend_color", 4}, Functions)).

s023_assert_blend_color_path(FunctionData) ->
    ?assertEqual("glBlendColor", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Red", gl_float},
            {in, "Green", gl_float},
            {in, "Blue", gl_float},
            {in, "Alpha", gl_float}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Red", {gl, float, []}},
            {"Green", {gl, float, []}},
            {"Blue", {gl, float, []}},
            {"Alpha", {gl, float, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Red", do_nothing},
            {"Green", do_nothing},
            {"Blue", do_nothing},
            {"Alpha", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glBlendColor", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBlendColor", NifFunctions)),
    NifData = maps:get("glBlendColor", NifFunctions),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Red", s023_float_nif_data()},
            {"Green", s023_float_nif_data()},
            {"Blue", s023_float_nif_data()},
            {"Alpha", s023_float_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s023_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 24.
s024_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s024_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s024_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"blend_func", 2}, Functions)),
    ?assert(maps:is_key({"blend_equation", 1}, Functions)),
    ?assert(maps:is_key({"blend_color", 4}, Functions)),
    ?assert(maps:is_key({"blend_equation_separate", 2}, Functions)),

    s024_assert_blend_equation_mode_enum(BindingData),
    s024_assert_blend_equation_separate_path(
        maps:get({"blend_equation_separate", 2}, Functions)
    ).

s024_assert_blend_equation_mode_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("blend_equation_mode", EnumTypes)),
    ?assertNot(maps:is_key("blend_equation_mode_ext", EnumTypes)),
    BlendEquationModes = maps:get("blend_equation_mode", EnumTypes),
    ?assert(lists:member("func_add", BlendEquationModes)),
    ?assert(lists:member("func_subtract", BlendEquationModes)),
    ?assert(lists:member("func_reverse_subtract", BlendEquationModes)).

s024_assert_blend_equation_separate_path(FunctionData) ->
    ?assertEqual("glBlendEquationSeparate", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "ModeRGB", {gl_enum, "BlendEquationModeEXT", blend_equation_mode}},
            {in, "ModeAlpha", {gl_enum, "BlendEquationModeEXT", blend_equation_mode}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"ModeRGB", {undefined, blend_equation_mode, []}},
            {"ModeAlpha", {undefined, blend_equation_mode, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"ModeRGB", {gl_enum_to_uint, ModeRgbTransformMap}},
        {"ModeAlpha", {gl_enum_to_uint, ModeAlphaTransformMap}}
    ] = maps:get(params, Clause),
    s024_assert_blend_equation_mode_transform(ModeRgbTransformMap),
    s024_assert_blend_equation_mode_transform(ModeAlphaTransformMap),
    ?assertEqual("glBlendEquationSeparate", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBlendEquationSeparate", NifFunctions)),
    NifData = maps:get("glBlendEquationSeparate", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"ModeRGB", s024_enum_nif_data()},
            {"ModeAlpha", s024_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s024_assert_blend_equation_mode_transform(TransformMap) ->
    ?assert(lists:keymember("func_add", 1, TransformMap)),
    ?assert(lists:keymember("func_subtract", 1, TransformMap)),
    ?assert(lists:keymember("func_reverse_subtract", 1, TransformMap)).

s024_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 25.
s025_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s025_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s025_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"blend_func", 2}, Functions)),
    ?assert(maps:is_key({"blend_func_separate", 4}, Functions)),
    ?assert(maps:is_key({"blend_equation", 1}, Functions)),
    ?assert(maps:is_key({"blend_equation_separate", 2}, Functions)),
    ?assert(maps:is_key({"blend_color", 4}, Functions)),
    ?assertEqual(Target =:= {gles, {3, 2}}, generator_test_support:has_gl_command("glBlendBarrier", Functions)),

    s025_assert_blending_factor_enum(BindingData),
    s025_assert_blend_func_separate_path(
        maps:get({"blend_func_separate", 4}, Functions)
    ).

s025_assert_blending_factor_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("blending_factor", EnumTypes)),
    BlendingFactors = maps:get("blending_factor", EnumTypes),
    ?assert(lists:member("zero", BlendingFactors)),
    ?assert(lists:member("one", BlendingFactors)),
    ?assert(lists:member("src_alpha", BlendingFactors)),
    ?assert(lists:member("one_minus_src_alpha", BlendingFactors)),
    ?assert(lists:member("dst_alpha", BlendingFactors)),
    ?assert(lists:member("one_minus_dst_alpha", BlendingFactors)).

s025_assert_blend_func_separate_path(FunctionData) ->
    ?assertEqual("glBlendFuncSeparate", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "SourceRGB", {gl_enum, "BlendingFactor"}},
            {in, "DestinationRGB", {gl_enum, "BlendingFactor"}},
            {in, "SourceAlpha", {gl_enum, "BlendingFactor"}},
            {in, "DestinationAlpha", {gl_enum, "BlendingFactor"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"SourceRGB", {undefined, blending_factor, []}},
            {"DestinationRGB", {undefined, blending_factor, []}},
            {"SourceAlpha", {undefined, blending_factor, []}},
            {"DestinationAlpha", {undefined, blending_factor, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"SourceRGB", {gl_enum_to_uint, SourceRgbTransformMap}},
        {"DestinationRGB", {gl_enum_to_uint, DestinationRgbTransformMap}},
        {"SourceAlpha", {gl_enum_to_uint, SourceAlphaTransformMap}},
        {"DestinationAlpha", {gl_enum_to_uint, DestinationAlphaTransformMap}}
    ] = maps:get(params, Clause),
    s025_assert_blending_factor_transform(SourceRgbTransformMap),
    s025_assert_blending_factor_transform(DestinationRgbTransformMap),
    s025_assert_blending_factor_transform(SourceAlphaTransformMap),
    s025_assert_blending_factor_transform(DestinationAlphaTransformMap),
    ?assertEqual("glBlendFuncSeparate", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glBlendFuncSeparate", NifFunctions)),
    NifData = maps:get("glBlendFuncSeparate", NifFunctions),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"SourceRGB", s025_enum_nif_data()},
            {"DestinationRGB", s025_enum_nif_data()},
            {"SourceAlpha", s025_enum_nif_data()},
            {"DestinationAlpha", s025_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s025_assert_blending_factor_transform(TransformMap) ->
    ?assert(lists:keymember("zero", 1, TransformMap)),
    ?assert(lists:keymember("one", 1, TransformMap)),
    ?assert(lists:keymember("src_alpha", 1, TransformMap)),
    ?assert(lists:keymember("one_minus_src_alpha", 1, TransformMap)),
    ?assert(lists:keymember("dst_alpha", 1, TransformMap)),
    ?assert(lists:keymember("one_minus_dst_alpha", 1, TransformMap)).

s025_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 26.
s026_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s026_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s026_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"stencil_func", 3}, Functions)),
    ?assert(maps:is_key({"stencil_func_separate", 4}, Functions)),
    ?assert(maps:is_key({"stencil_op", 3}, Functions)),
    ?assert(maps:is_key({"stencil_op_separate", 4}, Functions)),
    ?assert(maps:is_key({"stencil_mask", 1}, Functions)),
    ?assert(maps:is_key({"stencil_mask_separate", 2}, Functions)),

    s026_assert_triangle_face_enum(BindingData),
    s026_assert_stencil_function_enum(BindingData),
    s026_assert_stencil_op_enum(BindingData),
    s026_assert_stencil_func_separate_path(
        maps:get({"stencil_func_separate", 4}, Functions)
    ),
    s026_assert_stencil_op_separate_path(
        maps:get({"stencil_op_separate", 4}, Functions)
    ),
    s026_assert_stencil_mask_separate_path(
        maps:get({"stencil_mask_separate", 2}, Functions)
    ).

s026_assert_triangle_face_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("triangle_face", EnumTypes)),
    TriangleFaces = maps:get("triangle_face", EnumTypes),
    ?assert(lists:member("front", TriangleFaces)),
    ?assert(lists:member("back", TriangleFaces)),
    ?assert(lists:member("front_and_back", TriangleFaces)).

s026_assert_stencil_function_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("stencil_function", EnumTypes)),
    StencilFunctions = maps:get("stencil_function", EnumTypes),
    ?assert(lists:member("always", StencilFunctions)),
    ?assert(lists:member("equal", StencilFunctions)),
    ?assert(lists:member("less", StencilFunctions)).

s026_assert_stencil_op_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("stencil_op", EnumTypes)),
    StencilOps = maps:get("stencil_op", EnumTypes),
    ?assert(lists:member("keep", StencilOps)),
    ?assert(lists:member("zero", StencilOps)),
    ?assert(lists:member("replace", StencilOps)),
    ?assert(lists:member("incr", StencilOps)),
    ?assert(lists:member("decr", StencilOps)),
    ?assert(lists:member("invert", StencilOps)),
    ?assert(lists:member("incr_wrap", StencilOps)),
    ?assert(lists:member("decr_wrap", StencilOps)).

s026_assert_stencil_func_separate_path(FunctionData) ->
    ?assertEqual("glStencilFuncSeparate", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Face", {gl_enum, "TriangleFace"}},
            {in, "Function", {gl_enum, "StencilFunction"}},
            {in, "Ref", gl_int},
            {in, "Mask", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Face", {undefined, triangle_face, []}},
            {"Function", {undefined, stencil_function, []}},
            {"Ref", {gl, int, []}},
            {"Mask", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Face", {gl_enum_to_uint, FaceTransformMap}},
        {"Function", {gl_enum_to_uint, FunctionTransformMap}},
        {"Ref", do_nothing},
        {"Mask", do_nothing}
    ] = maps:get(params, Clause),
    s026_assert_triangle_face_transform(FaceTransformMap),
    ?assert(lists:keymember("always", 1, FunctionTransformMap)),
    ?assert(lists:keymember("equal", 1, FunctionTransformMap)),
    ?assert(lists:keymember("less", 1, FunctionTransformMap)),
    ?assertEqual("glStencilFuncSeparate", maps:get(raw_function, Clause)),

    NifData = s026_nif_data("glStencilFuncSeparate", FunctionData),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Face", s026_enum_nif_data()},
            {"Function", s026_enum_nif_data()},
            {"Ref", s026_int_nif_data()},
            {"Mask", s026_uint_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s026_assert_stencil_op_separate_path(FunctionData) ->
    ?assertEqual("glStencilOpSeparate", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Face", {gl_enum, "TriangleFace"}},
            {in, "StencilFail", {gl_enum, "StencilOp"}},
            {in, "DepthPassFail", {gl_enum, "StencilOp"}},
            {in, "DepthPassPass", {gl_enum, "StencilOp"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Face", {undefined, triangle_face, []}},
            {"StencilFail", {undefined, stencil_op, []}},
            {"DepthPassFail", {undefined, stencil_op, []}},
            {"DepthPassPass", {undefined, stencil_op, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(4, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Face", {gl_enum_to_uint, FaceTransformMap}},
        {"StencilFail", {gl_enum_to_uint, StencilFailTransformMap}},
        {"DepthPassFail", {gl_enum_to_uint, DepthPassFailTransformMap}},
        {"DepthPassPass", {gl_enum_to_uint, DepthPassPassTransformMap}}
    ] = maps:get(params, Clause),
    s026_assert_triangle_face_transform(FaceTransformMap),
    s026_assert_stencil_op_transform(StencilFailTransformMap),
    s026_assert_stencil_op_transform(DepthPassFailTransformMap),
    s026_assert_stencil_op_transform(DepthPassPassTransformMap),
    ?assertEqual("glStencilOpSeparate", maps:get(raw_function, Clause)),

    NifData = s026_nif_data("glStencilOpSeparate", FunctionData),
    ?assertEqual(4, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Face", s026_enum_nif_data()},
            {"StencilFail", s026_enum_nif_data()},
            {"DepthPassFail", s026_enum_nif_data()},
            {"DepthPassPass", s026_enum_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s026_assert_stencil_mask_separate_path(FunctionData) ->
    ?assertEqual("glStencilMaskSeparate", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Face", {gl_enum, "TriangleFace"}},
            {in, "Mask", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Face", {undefined, triangle_face, []}},
            {"Mask", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Face", {gl_enum_to_uint, FaceTransformMap}},
        {"Mask", do_nothing}
    ] = maps:get(params, Clause),
    s026_assert_triangle_face_transform(FaceTransformMap),
    ?assertEqual("glStencilMaskSeparate", maps:get(raw_function, Clause)),

    NifData = s026_nif_data("glStencilMaskSeparate", FunctionData),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Face", s026_enum_nif_data()},
            {"Mask", s026_uint_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s026_assert_triangle_face_transform(TransformMap) ->
    ?assert(lists:keymember("front", 1, TransformMap)),
    ?assert(lists:keymember("back", 1, TransformMap)),
    ?assert(lists:keymember("front_and_back", 1, TransformMap)).

s026_assert_stencil_op_transform(TransformMap) ->
    ?assert(lists:keymember("keep", 1, TransformMap)),
    ?assert(lists:keymember("zero", 1, TransformMap)),
    ?assert(lists:keymember("replace", 1, TransformMap)),
    ?assert(lists:keymember("incr", 1, TransformMap)),
    ?assert(lists:keymember("decr", 1, TransformMap)),
    ?assert(lists:keymember("invert", 1, TransformMap)),
    ?assert(lists:keymember("incr_wrap", 1, TransformMap)),
    ?assert(lists:keymember("decr_wrap", 1, TransformMap)).

s026_nif_data(Command, FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key(Command, NifFunctions)),
    maps:get(Command, NifFunctions).

s026_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s026_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s026_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 28.
s028_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s028_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s028_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"pixel_store", 2}, Functions)),
    ?assertNot(maps:is_key({"pixel_store", 3}, Functions)),

    s028_assert_pixel_store_parameter_enum(BindingData),
    case Target of
        {gl, _} ->
            ?assert(generator_test_support:has_gl_command("glPixelStoref", Functions));
        {gles, _} ->
            ?assertNot(generator_test_support:has_gl_command("glPixelStoref", Functions)),
            s028_assert_pixel_store_path(maps:get({"pixel_store", 2}, Functions))
    end.

s028_assert_pixel_store_parameter_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("pixel_store_parameter", EnumTypes)),
    PixelStoreParameters = maps:get("pixel_store_parameter", EnumTypes),
    ?assert(lists:member("pack_alignment", PixelStoreParameters)),
    ?assert(lists:member("unpack_alignment", PixelStoreParameters)).

s028_assert_pixel_store_path(FunctionData) ->
    ?assertEqual("glPixelStorei", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Name", {gl_enum, "PixelStoreParameter"}},
            {in, "Param", gl_int}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Name", {undefined, pixel_store_parameter, []}},
            {"Param", {gl, int, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Name", {gl_enum_to_uint, NameTransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("pack_alignment", 1, NameTransformMap)),
    ?assert(lists:keymember("unpack_alignment", 1, NameTransformMap)),
    ?assertEqual("glPixelStorei", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glPixelStorei", NifFunctions)),
    NifData = maps:get("glPixelStorei", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Name", s028_enum_nif_data()},
            {"Param", s028_int_nif_data()}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s028_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s028_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 33.
s033_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s033_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s033_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"sample_coverage", 2}, Functions)),
    ?assertNot(maps:is_key({"sample_coverage", 3}, Functions)),

    s033_assert_sample_coverage_path(maps:get({"sample_coverage", 2}, Functions)).

s033_assert_sample_coverage_path(FunctionData) ->
    ?assertEqual("glSampleCoverage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Value", gl_float},
            {in, "Invert", gl_bool}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Value", {gl, float, []}},
            {"Invert", {gl, boolean, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [
            {"Value", do_nothing},
            {"Invert", do_nothing}
        ],
        maps:get(params, Clause)
    ),
    ?assertEqual("glSampleCoverage", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    ?assert(maps:is_key("glSampleCoverage", NifFunctions)),
    NifData = maps:get("glSampleCoverage", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Value", s033_float_nif_data()},
            {"Invert", boolean_to_glbool}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s033_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 55.
s055_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s055_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s055_emitter_direct_state_surface_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard55-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        ?assertMatch({_, _}, binary:match(Erl, <<"-export([point_size/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([polygon_mode/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([logic_op/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([color_mask/5]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([enable/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([disable/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([is_enabled/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([draw_arrays_instanced/4]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([primitive_restart_index/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([min_sample_shading/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([blend_equation/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([blend_equation_separate/3]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([blend_func/3]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([blend_func_separate/5]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"Mode :: blend_equation_mode()">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"blend_equation_mode_ext()">>)),

        ?assertMatch({_, _}, binary:match(C, <<"glPointSize(arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glPolygonMode(arg_0, arg_1);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glLogicOp(arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glColorMaski(arg_0, arg_1, arg_2, arg_3, arg_4);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glDrawArraysInstanced(arg_0, arg_1, arg_2, arg_3);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glBlendFuncSeparatei(arg_0, arg_1, arg_2, arg_3, arg_4);">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s055_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s055_assert_presence(Target, Functions),
    s055_assert_deferred_neighbors_absent(Functions),
    s055_assert_present_paths(Target, BindingData, Functions).

s055_assert_presence(Target, Functions) ->
    Present = s055_present_functions(Target),
    Absent = s055_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s055_present_functions({gl, {3, 3}}) ->
    [
        {"point_size", 1},
        {"polygon_mode", 2},
        {"logic_op", 1},
        {"color_mask", 5},
        {"enable", 2},
        {"disable", 2},
        {"is_enabled", 2},
        {"draw_arrays_instanced", 4},
        {"primitive_restart_index", 1}
    ];
s055_present_functions({gl, {4, 1}}) ->
    s055_all_functions();
s055_present_functions({gl, {4, 6}}) ->
    s055_all_functions();
s055_present_functions({gles, {2, 0}}) ->
    [];
s055_present_functions({gles, {3, 0}}) ->
    [{"draw_arrays_instanced", 4}];
s055_present_functions({gles, {3, 1}}) ->
    [{"draw_arrays_instanced", 4}];
s055_present_functions({gles, {3, 2}}) ->
    [
        {"color_mask", 5},
        {"enable", 2},
        {"disable", 2},
        {"is_enabled", 2},
        {"draw_arrays_instanced", 4},
        {"min_sample_shading", 1},
        {"blend_equation", 2},
        {"blend_equation_separate", 3},
        {"blend_func", 3},
        {"blend_func_separate", 5}
    ].

s055_all_functions() ->
    [
        {"point_size", 1},
        {"polygon_mode", 2},
        {"logic_op", 1},
        {"color_mask", 5},
        {"enable", 2},
        {"disable", 2},
        {"is_enabled", 2},
        {"draw_arrays_instanced", 4},
        {"primitive_restart_index", 1},
        {"min_sample_shading", 1},
        {"blend_equation", 2},
        {"blend_equation_separate", 3},
        {"blend_func", 3},
        {"blend_func_separate", 5}
    ].

s055_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s055_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s055_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s055_present_functions(Target)
    ).

s055_assert_path({"point_size", 1}, _Target, _BindingData, FunctionData) ->
    s055_assert_direct(
        FunctionData,
        "glPointSize",
        [{in, "Size", gl_float}],
        [{"Size", {gl, float, []}}],
        [{"Size", do_nothing}],
        [{"Size", s055_float_nif_data()}],
        void
    );
s055_assert_path({"polygon_mode", 2}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "polygon_mode", "fill"),
    s055_assert_direct(
        FunctionData,
        "glPolygonMode",
        [
            {in, "Face", {gl_enum, "TriangleFace"}},
            {in, "Mode", {gl_enum, "PolygonMode"}}
        ],
        [{"Face", {undefined, triangle_face, []}}, {"Mode", {undefined, polygon_mode, []}}],
        s055_enum_clause_params(["front_and_back", "fill"]),
        [{"Face", s055_enum_nif_data()}, {"Mode", s055_enum_nif_data()}],
        void
    );
s055_assert_path({"logic_op", 1}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "logic_op", "copy"),
    s055_assert_direct(
        FunctionData,
        "glLogicOp",
        [{in, "OpCode", {gl_enum, "LogicOp"}}],
        [{"OpCode", {undefined, logic_op, []}}],
        s055_enum_clause_params(["copy"]),
        [{"OpCode", s055_enum_nif_data()}],
        void
    );
s055_assert_path({"color_mask", 5}, _Target, _BindingData, FunctionData) ->
    s055_assert_direct(
        FunctionData,
        "glColorMaski",
        [
            {in, "Index", gl_uint},
            {in, "Red", gl_bool},
            {in, "Green", gl_bool},
            {in, "Blue", gl_bool},
            {in, "Alpha", gl_bool}
        ],
        [
            {"Index", {gl, uint, []}},
            {"Red", {gl, boolean, []}},
            {"Green", {gl, boolean, []}},
            {"Blue", {gl, boolean, []}},
            {"Alpha", {gl, boolean, []}}
        ],
        [
            {"Index", do_nothing},
            {"Red", do_nothing},
            {"Green", do_nothing},
            {"Blue", do_nothing},
            {"Alpha", do_nothing}
        ],
        [
            {"Index", s055_uint_nif_data()},
            {"Red", boolean_to_glbool},
            {"Green", boolean_to_glbool},
            {"Blue", boolean_to_glbool},
            {"Alpha", boolean_to_glbool}
        ],
        void
    );
s055_assert_path({"enable", 2}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "enable_cap", "blend"),
    s055_assert_direct(
        FunctionData,
        "glEnablei",
        [{in, "Capability", {gl_enum, "EnableCap"}}, {in, "Index", gl_uint}],
        [{"Capability", {undefined, enable_cap, []}}, {"Index", {gl, uint, []}}],
        s055_enum_plus_plain_clause_params(["blend"], "Index"),
        [{"Capability", s055_enum_nif_data()}, {"Index", s055_uint_nif_data()}],
        void
    );
s055_assert_path({"disable", 2}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "enable_cap", "blend"),
    s055_assert_direct(
        FunctionData,
        "glDisablei",
        [{in, "Capability", {gl_enum, "EnableCap"}}, {in, "Index", gl_uint}],
        [{"Capability", {undefined, enable_cap, []}}, {"Index", {gl, uint, []}}],
        s055_enum_plus_plain_clause_params(["blend"], "Index"),
        [{"Capability", s055_enum_nif_data()}, {"Index", s055_uint_nif_data()}],
        void
    );
s055_assert_path({"is_enabled", 2}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "enable_cap", "blend"),
    ?assertEqual("glIsEnabledi", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Capability", {gl_enum, "EnableCap"}}, {in, "Index", gl_uint}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"IsEnabled", gl_bool}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [{"Capability", {undefined, enable_cap, []}}, {"Index", {gl, uint, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"IsEnabled", {gl, boolean, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    [{"Capability", {gl_enum_to_uint, TransformMap}}, {"Index", do_nothing}] = maps:get(params, Clause),
    ?assert(lists:keymember("blend", 1, TransformMap)),
    NifData = maps:get("glIsEnabledi", maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [{"Capability", s055_enum_nif_data()}, {"Index", s055_uint_nif_data()}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData));
s055_assert_path({"draw_arrays_instanced", 4}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "primitive_type", "triangles"),
    s055_assert_direct(
        FunctionData,
        "glDrawArraysInstanced",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "First", gl_int},
            {in, "Count", gl_sizei},
            {in, "InstanceCount", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"First", {gl, int, []}},
            {"Count", {gl, sizei, []}},
            {"InstanceCount", {gl, sizei, []}}
        ],
        s055_enum_plus_plain_clause_params(["triangles"], ["First", "Count", "InstanceCount"]),
        [
            {"Mode", s055_enum_nif_data()},
            {"First", s055_int_nif_data()},
            {"Count", s055_sizei_nif_data()},
            {"InstanceCount", s055_sizei_nif_data()}
        ],
        void
    );
s055_assert_path({"primitive_restart_index", 1}, _Target, _BindingData, FunctionData) ->
    s055_assert_direct(
        FunctionData,
        "glPrimitiveRestartIndex",
        [{in, "Index", gl_uint}],
        [{"Index", {gl, uint, []}}],
        [{"Index", do_nothing}],
        [{"Index", s055_uint_nif_data()}],
        void
    );
s055_assert_path({"min_sample_shading", 1}, _Target, _BindingData, FunctionData) ->
    s055_assert_direct(
        FunctionData,
        "glMinSampleShading",
        [{in, "Value", gl_float}],
        [{"Value", {gl, float, []}}],
        [{"Value", do_nothing}],
        [{"Value", s055_float_nif_data()}],
        void
    );
s055_assert_path({"blend_equation", 2}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "blend_equation_mode", "func_add"),
    s055_assert_direct(
        FunctionData,
        "glBlendEquationi",
        [
            {in, "Buffer", gl_uint},
            {in, "Mode", {gl_enum, "BlendEquationModeEXT", blend_equation_mode}}
        ],
        [{"Buffer", {gl, uint, []}}, {"Mode", {undefined, blend_equation_mode, []}}],
        s055_plain_plus_enum_clause_params("Buffer", ["func_add"]),
        [{"Buffer", s055_uint_nif_data()}, {"Mode", s055_enum_nif_data()}],
        void
    );
s055_assert_path({"blend_equation_separate", 3}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "blend_equation_mode", "func_add"),
    s055_assert_direct(
        FunctionData,
        "glBlendEquationSeparatei",
        [
            {in, "Buffer", gl_uint},
            {in, "ModeRGB", {gl_enum, "BlendEquationModeEXT", blend_equation_mode}},
            {in, "ModeAlpha", {gl_enum, "BlendEquationModeEXT", blend_equation_mode}}
        ],
        [
            {"Buffer", {gl, uint, []}},
            {"ModeRGB", {undefined, blend_equation_mode, []}},
            {"ModeAlpha", {undefined, blend_equation_mode, []}}
        ],
        s055_plain_plus_enums_clause_params("Buffer", [{"ModeRGB", "func_add"}, {"ModeAlpha", "func_add"}]),
        [{"Buffer", s055_uint_nif_data()}, {"ModeRGB", s055_enum_nif_data()}, {"ModeAlpha", s055_enum_nif_data()}],
        void
    );
s055_assert_path({"blend_func", 3}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "blending_factor", "src_alpha"),
    s055_assert_direct(
        FunctionData,
        "glBlendFunci",
        [
            {in, "Buffer", gl_uint},
            {in, "SourceFactor", {gl_enum, "BlendingFactor"}},
            {in, "DestinationFactor", {gl_enum, "BlendingFactor"}}
        ],
        [
            {"Buffer", {gl, uint, []}},
            {"SourceFactor", {undefined, blending_factor, []}},
            {"DestinationFactor", {undefined, blending_factor, []}}
        ],
        s055_plain_plus_enums_clause_params(
            "Buffer",
            [{"SourceFactor", "src_alpha"}, {"DestinationFactor", "one_minus_src_alpha"}]
        ),
        [{"Buffer", s055_uint_nif_data()}, {"SourceFactor", s055_enum_nif_data()}, {"DestinationFactor", s055_enum_nif_data()}],
        void
    );
s055_assert_path({"blend_func_separate", 5}, _Target, BindingData, FunctionData) ->
    s055_assert_enum_contains(BindingData, "blending_factor", "src_alpha"),
    s055_assert_direct(
        FunctionData,
        "glBlendFuncSeparatei",
        [
            {in, "Buffer", gl_uint},
            {in, "SourceRGB", {gl_enum, "BlendingFactor"}},
            {in, "DestinationRGB", {gl_enum, "BlendingFactor"}},
            {in, "SourceAlpha", {gl_enum, "BlendingFactor"}},
            {in, "DestinationAlpha", {gl_enum, "BlendingFactor"}}
        ],
        [
            {"Buffer", {gl, uint, []}},
            {"SourceRGB", {undefined, blending_factor, []}},
            {"DestinationRGB", {undefined, blending_factor, []}},
            {"SourceAlpha", {undefined, blending_factor, []}},
            {"DestinationAlpha", {undefined, blending_factor, []}}
        ],
        s055_plain_plus_enums_clause_params(
            "Buffer",
            [
                {"SourceRGB", "src_alpha"},
                {"DestinationRGB", "one_minus_src_alpha"},
                {"SourceAlpha", "one"},
                {"DestinationAlpha", "zero"}
            ]
        ),
        [
            {"Buffer", s055_uint_nif_data()},
            {"SourceRGB", s055_enum_nif_data()},
            {"DestinationRGB", s055_enum_nif_data()},
            {"SourceAlpha", s055_enum_nif_data()},
            {"DestinationAlpha", s055_enum_nif_data()}
        ],
        void
    ).

s055_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams, NifReturn) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s055_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(NifReturn, maps:get(return, NifData)).

s055_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s055_assert_clause_param/1, lists:zip(Expected, Actual)).

s055_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s055_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s055_enum_clause_params(AtomsByPosition) ->
    lists:map(
        fun({Name, Atom}) -> {Name, {gl_enum_to_uint, [Atom]}} end,
        case AtomsByPosition of
            ["front_and_back", "fill"] -> [{"Face", "front_and_back"}, {"Mode", "fill"}];
            ["copy"] -> [{"OpCode", "copy"}]
        end
    ).

s055_enum_plus_plain_clause_params(RequiredAtoms, PlainName) when is_list(PlainName), is_integer(hd(PlainName)) ->
    [{"Capability", {gl_enum_to_uint, RequiredAtoms}}, {PlainName, do_nothing}];
s055_enum_plus_plain_clause_params(RequiredAtoms, PlainNames) ->
    [{"Mode", {gl_enum_to_uint, RequiredAtoms}}] ++ [{Name, do_nothing} || Name <- PlainNames].

s055_plain_plus_enum_clause_params(PlainName, RequiredAtoms) ->
    [{PlainName, do_nothing}, {"Mode", {gl_enum_to_uint, RequiredAtoms}}].

s055_plain_plus_enums_clause_params(PlainName, EnumParams) ->
    [{PlainName, do_nothing}] ++
        [{Name, {gl_enum_to_uint, [RequiredAtom]}} || {Name, RequiredAtom} <- EnumParams].

s055_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s055_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s055_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s055_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s055_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s055_float_nif_data() ->
    {gl_type, {"GLfloat", "double", "enif_get_double", "enif_make_double"}}.

%% Historical shard 76.
s076_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s076_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s076_emitter_patch_parameter_test_() ->
    [
        {"gl 4.6", fun() ->
            s076_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([patch_parameter/3]).">>,
                    <<"-type patch_parameter_value() ::">>,
                    <<"-spec patch_parameter(\n    Type :: f | i,">>,
                    <<"patch_default_outer_level">>,
                    <<"patch_parameter(f, ParamName, Param) when is_list(Param) ->">>,
                    <<"patch_parameter(i, ParamName, Param) ->">>,
                    <<"?CALL_RAW_FUNC(glPatchParameterfv_raw(NewParamName, Param))">>,
                    <<"?CALL_RAW_FUNC(glPatchParameteri_raw(NewParamName, Param))">>
                ],
                [
                    <<"glPatchParameterfv(arg_0, arg_1_array);">>,
                    <<"glPatchParameteri(arg_0, arg_1);">>
                ],
                [
                    <<"-export([patch_parameteri/2]).">>,
                    <<"-export([patch_parameterfv/2]).">>,
                    <<"patch_parameter_name_arb">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s076_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([patch_parameter/3]).">>,
                    <<"-spec patch_parameter(\n    Type :: i,">>,
                    <<"Param :: gl:int()">>,
                    <<"patch_vertices">>,
                    <<"patch_parameter(i, ParamName, Param) ->">>,
                    <<"?CALL_RAW_FUNC(glPatchParameteri_raw(NewParamName, Param))">>
                ],
                [
                    <<"glPatchParameteri(arg_0, arg_1);">>
                ],
                [
                    <<"-type patch_parameter_value() ::">>,
                    <<"patch_parameter_value()">>,
                    <<"patch_default_outer_level">>,
                    <<"glPatchParameterfv_raw">>,
                    <<"glPatchParameterfv(arg_0, arg_1_array);">>,
                    <<"patch_parameter_name_arb">>
                ]
            )
        end},
        {"gl 3.3", fun() ->
            s076_assert_emitted_surface(
                {gl, {3, 3}},
                [],
                [],
                [
                    <<"-export([patch_parameter/3]).">>,
                    <<"glPatchParameteri_raw">>,
                    <<"glPatchParameterfv_raw">>
                ]
            )
        end}
    ].

s076_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s076_supports_patch_parameter(Target) of
        none ->
            ?assertNot(maps:is_key({"patch_parameter", 3}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glPatchParameteri", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glPatchParameterfv", Functions));
        integer_only ->
            ?assert(maps:is_key({"patch_parameter", 3}, Functions)),
            s076_assert_patch_parameter_integer_only(BindingData, maps:get({"patch_parameter", 3}, Functions));
        full ->
            ?assert(maps:is_key({"patch_parameter", 3}, Functions)),
            s076_assert_patch_parameter_full(BindingData, maps:get({"patch_parameter", 3}, Functions))
    end,
    s076_assert_direct_wrappers_absent(Functions).

s076_supports_patch_parameter({gl, {3, 3}}) ->
    none;
s076_supports_patch_parameter({gl, _Version}) ->
    full;
s076_supports_patch_parameter({gles, {3, 2}}) ->
    integer_only;
s076_supports_patch_parameter({gles, _Version}) ->
    none.

s076_assert_direct_wrappers_absent(Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- [
        {"patch_parameteri", 2},
        {"patch_parameterfv", 2}
    ]].

s076_assert_patch_parameter_full(BindingData, FunctionData) ->
    s076_assert_common_patch_parameter(BindingData, FunctionData, [f, i], {undefined, patch_parameter_value, []}),
    s076_assert_enum_contains(BindingData, "patch_parameter_name", "patch_default_outer_level"),
    s076_assert_enum_contains(BindingData, "patch_parameter_name", "patch_default_inner_level"),
    {patch_parameter_value, {set, Variants}} = maps:get(extra_type, FunctionData),
    ?assertEqual(lists:sort([{gl, int, []}, {list, {gl, float, []}}]), lists:sort(Variants)),
    ?assert(lists:member({"glPatchParameterfv", gl_float, array}, maps:get(gl_commands, FunctionData))),
    ?assert(lists:member({"glPatchParameteri", gl_int, element}, maps:get(gl_commands, FunctionData))),
    ?assertEqual(lists:sort([{gl_float, array}, {gl_int, element}]), lists:sort(maps:get(variants, FunctionData))),
    s076_assert_array_float_clause(FunctionData),
    s076_assert_integer_clause(FunctionData),
    s076_assert_float_array_nif(FunctionData),
    s076_assert_integer_nif(FunctionData).

s076_assert_patch_parameter_integer_only(BindingData, FunctionData) ->
    s076_assert_common_patch_parameter(BindingData, FunctionData, [i], {gl, int, []}),
    s076_assert_enum_not_contains(BindingData, "patch_parameter_name", "patch_default_outer_level"),
    s076_assert_enum_not_contains(BindingData, "patch_parameter_name", "patch_default_inner_level"),
    ?assertEqual(undefined, maps:get(extra_type, FunctionData)),
    ?assert(lists:member({"glPatchParameteri", gl_int, element}, maps:get(gl_commands, FunctionData))),
    ?assertNot(lists:member({"glPatchParameterfv", gl_float, array}, maps:get(gl_commands, FunctionData))),
    ?assertEqual([{gl_int, element}], maps:get(variants, FunctionData)),
    s076_assert_integer_clause(FunctionData),
    s076_assert_integer_nif(FunctionData),
    ?assertNot(maps:is_key("glPatchParameterfv", maps:get(nif_functions, FunctionData))).

s076_assert_common_patch_parameter(BindingData, FunctionData, TypeAtoms, ParamType) ->
    s076_assert_enum_contains(BindingData, "patch_parameter_name", "patch_vertices"),
    ?assertNot(maps:is_key("patch_parameter_name_arb", maps:get(enum_types, BindingData))),
    ?assertEqual(
        [
            {in, "ParamName", {gl_enum, "PatchParameterName", patch_parameter_name}},
            {in, "Param", gl_x}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Type", {set, TypeAtoms}},
            {"ParamName", {undefined, patch_parameter_name, []}},
            {"Param", ParamType}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(3, maps:get(function_arity, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)).

s076_assert_array_float_clause(FunctionData) ->
    Clause = s076_find_clause("glPatchParameterfv", maps:get(function_clauses, FunctionData)),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([{is_list, var}], maps:get(guards, Clause)),
    [
        {"f", ignore},
        {"ParamName", {gl_enum_to_uint, TransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("patch_default_outer_level", 1, TransformMap)).

s076_assert_integer_clause(FunctionData) ->
    Clause = s076_find_clause("glPatchParameteri", maps:get(function_clauses, FunctionData)),
    ?assertEqual("Param", maps:get(guard_var, Clause)),
    ?assertEqual([], maps:get(guards, Clause)),
    [
        {"i", ignore},
        {"ParamName", {gl_enum_to_uint, TransformMap}},
        {"Param", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("patch_vertices", 1, TransformMap)).

s076_assert_float_array_nif(FunctionData) ->
    s076_assert_nif(
        maps:get("glPatchParameterfv", maps:get(nif_functions, FunctionData)),
        [{"ParamName", s076_enum_nif_data()}, {"Param", {list_gl_type, s076_float_raw_nif_data()}}]
    ).

s076_assert_integer_nif(FunctionData) ->
    s076_assert_nif(
        maps:get("glPatchParameteri", maps:get(nif_functions, FunctionData)),
        [{"ParamName", s076_enum_nif_data()}, {"Param", s076_int_nif_data()}]
    ).

s076_assert_nif(NifData, Params) ->
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s076_find_clause(RawFunction, Clauses) ->
    [Clause] = [Clause0 || Clause0 <- Clauses, maps:get(raw_function, Clause0) =:= RawFunction],
    Clause.

s076_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s076_assert_enum_not_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assertNot(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s076_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard76-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s076_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s076_assert_contains(C, Needle) || Needle <- RequiredC],
        [s076_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s076_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s076_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s076_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s076_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s076_float_raw_nif_data() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s076_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 126.
s126_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s126_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s126_emitter_indexed_vector_state_test_() ->
    [
        {"gl 4.6", fun() -> s126_assert_emitted_surface({gl, {4, 6}}, true) end},
        {"gl 4.1", fun() -> s126_assert_emitted_surface({gl, {4, 1}}, true) end},
        {"gl 3.3", fun() -> s126_assert_emitted_surface({gl, {3, 3}}, false) end},
        {"gles 3.2", fun() -> s126_assert_emitted_surface({gles, {3, 2}}, false) end}
    ].

s126_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    Expected = s126_supports_indexed_vector_state(Target),
    [s126_assert_presence(Expected, Function, Functions) || Function <- s126_functions()],
    s126_assert_raw_public_wrappers_absent(Functions),
    case Expected of
        true ->
            s126_assert_viewport_indexedfv(maps:get({"viewport", 2}, Functions)),
            s126_assert_viewport_arrayv(maps:get({"viewport_array", 2}, Functions)),
            s126_assert_scissor_indexedv(maps:get({"scissor_indexed", 2}, Functions)),
            s126_assert_scissor_arrayv(maps:get({"scissor_array", 2}, Functions)),
            s126_assert_depth_range_arrayv(maps:get({"depth_range_array", 2}, Functions));
        false ->
            s126_assert_commands_absent(Functions)
    end.

s126_supports_indexed_vector_state({gl, {4, 1}}) -> true;
s126_supports_indexed_vector_state({gl, {4, 6}}) -> true;
s126_supports_indexed_vector_state(_) -> false.

s126_functions() ->
    [
        {"viewport", 2},
        {"viewport_array", 2},
        {"scissor_indexed", 2},
        {"scissor_array", 2},
        {"depth_range_array", 2}
    ].

s126_assert_presence(true, Function, Functions) ->
    ?assert(maps:is_key(Function, Functions));
s126_assert_presence(false, Function, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)).

s126_assert_raw_public_wrappers_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- [
            {"viewport_indexedfv", 2},
            {"viewport_arrayv", 3},
            {"scissor_indexedv", 2},
            {"scissor_arrayv", 3},
            {"depth_range_arrayv", 3}
        ]
    ].

s126_assert_commands_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s126_commands()
    ].

s126_commands() ->
    [
        "glViewportIndexedfv",
        "glViewportArrayv",
        "glScissorIndexedv",
        "glScissorArrayv",
        "glDepthRangeArrayv"
    ].

s126_assert_viewport_indexedfv(FunctionData) ->
    s126_assert_single_vector_direct(
        FunctionData,
        "glViewportIndexedfv",
        "Values",
        gl_float,
        {gl, float, []},
        s126_float_convert_data()
    ).

s126_assert_viewport_arrayv(FunctionData) ->
    s126_assert_counted_vector_direct(
        FunctionData,
        "glViewportArrayv",
        "Viewports",
        4,
        gl_float,
        {gl, float, []},
        s126_float_convert_data()
    ).

s126_assert_scissor_indexedv(FunctionData) ->
    s126_assert_single_vector_direct(
        FunctionData,
        "glScissorIndexedv",
        "Box",
        gl_int,
        {gl, int, []},
        s126_int_convert_data()
    ).

s126_assert_scissor_arrayv(FunctionData) ->
    s126_assert_counted_vector_direct(
        FunctionData,
        "glScissorArrayv",
        "Boxes",
        4,
        gl_int,
        {gl, int, []},
        s126_int_convert_data()
    ).

s126_assert_depth_range_arrayv(FunctionData) ->
    s126_assert_counted_vector_direct(
        FunctionData,
        "glDepthRangeArrayv",
        "Ranges",
        2,
        gl_double,
        {gl, double, []},
        s126_double_convert_data()
    ).

s126_assert_single_vector_direct(FunctionData, GlCommand, ValueName, GlType, PublicType, NifType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Index", gl_uint}, {in, ValueName, {single_vector, 4, GlType}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Index", {gl, uint, []}},
            {ValueName, {undefined, vector4, [PublicType]}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    ?assertEqual(
        [
            {"Index", do_nothing},
            {ValueName, {gl_vector_to_pointer_list, 4}}
        ],
        maps:get(params, Clause)
    ),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Index", s126_uint_nif_data()},
            {ValueName, {list_gl_type, NifType}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s126_assert_counted_vector_direct(FunctionData, GlCommand, ValueName, N, GlType, PublicType, NifType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "First", gl_uint},
            {in, ValueName, {counted_list, "Count", {gl_vector, N, GlType}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"First", {gl, uint, []}},
            {ValueName, {list, {undefined, list_to_atom("vector" ++ integer_to_list(N)), [PublicType]}}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(2, maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    ?assertEqual(
        [
            {"First", do_nothing},
            {ValueName, {counted_list_gl_vectors_to_list, "Count", N}}
        ],
        maps:get(params, Clause)
    ),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"First", s126_uint_nif_data()},
            {"Count", s126_sizei_nif_data()},
            {ValueName, {list_gl_type, NifType}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s126_assert_emitted_surface(Target, ExpectedPresent) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard126-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
                s126_assert_contains(Erl, <<"-export([viewport/2]).">>),
                s126_assert_contains(Erl, <<"-export([viewport_array/2]).">>),
                s126_assert_contains(Erl, <<"-export([scissor_indexed/2]).">>),
                s126_assert_contains(Erl, <<"-export([scissor_array/2]).">>),
                s126_assert_contains(Erl, <<"-export([depth_range_array/2]).">>),
                s126_assert_contains(Erl, <<"NewValues = ?GL_PACK_VECTOR_4(Values)">>),
                s126_assert_contains(Erl, <<"Count = length(Viewports)">>),
                s126_assert_contains(Erl, <<"NewViewports = lists:foldl(fun(Vector, Acc) -> Acc ++ ?GL_PACK_VECTOR_4(Vector) end, [], Viewports)">>),
                s126_assert_contains(Erl, <<"NewBox = ?GL_PACK_VECTOR_4(Box)">>),
                s126_assert_contains(Erl, <<"NewRanges = lists:foldl(fun(Vector, Acc) -> Acc ++ ?GL_PACK_VECTOR_2(Vector) end, [], Ranges)">>),
                s126_assert_contains(C, <<"glViewportIndexedfv(arg_0, arg_1_array);">>),
                s126_assert_contains(C, <<"glViewportArrayv(arg_0, arg_1, arg_2_array);">>),
                s126_assert_contains(C, <<"glScissorIndexedv(arg_0, arg_1_array);">>),
                s126_assert_contains(C, <<"glScissorArrayv(arg_0, arg_1, arg_2_array);">>),
                s126_assert_contains(C, <<"glDepthRangeArrayv(arg_0, arg_1, arg_2_array);">>),
                s126_assert_contains(C, <<"{\"glViewportIndexedfv_raw\", 2, nif_glViewportIndexedfv, 0}">>),
                s126_assert_contains(C, <<"{\"glDepthRangeArrayv_raw\", 3, nif_glDepthRangeArrayv, 0}">>);
            false ->
                s126_assert_not_contains(Erl, <<"-export([viewport/2]).">>),
                s126_assert_not_contains(Erl, <<"-export([viewport_array/2]).">>),
                s126_assert_not_contains(Erl, <<"-export([scissor_indexed/2]).">>),
                s126_assert_not_contains(Erl, <<"-export([scissor_array/2]).">>),
                s126_assert_not_contains(Erl, <<"-export([depth_range_array/2]).">>),
                s126_assert_not_contains(C, <<"glViewportIndexedfv(">>),
                s126_assert_not_contains(C, <<"glViewportArrayv(">>),
                s126_assert_not_contains(C, <<"glScissorIndexedv(">>),
                s126_assert_not_contains(C, <<"glScissorArrayv(">>),
                s126_assert_not_contains(C, <<"glDepthRangeArrayv(">>)
        end
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s126_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s126_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s126_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s126_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s126_int_convert_data() ->
    {"GLint", "int", "enif_get_int", "enif_make_int"}.

s126_float_convert_data() ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"}.

s126_double_convert_data() ->
    {"GLdouble", "double", "enif_get_double", "enif_make_double"}.
