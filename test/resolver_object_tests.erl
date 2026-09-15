-module(resolver_object_tests).
-include_lib("eunit/include/eunit.hrl").

%% Object lifecycle and object binding resolver contracts.

%% Historical shard 4.
s004_gl_4_6_resolver_test() ->
    s004_assert_target({gl, {4, 6}}).

s004_gles_3_2_resolver_test() ->
    s004_assert_target({gles, {3, 2}}).

s004_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s004_assert_function(Functions, {"finish", 0}, "glFinish"),
    s004_assert_function(Functions, {"flush", 0}, "glFlush"),

    s004_assert_void_no_arg_path(maps:get({"finish", 0}, Functions), "glFinish"),
    s004_assert_void_no_arg_path(maps:get({"flush", 0}, Functions), "glFlush"),
    ok.

s004_assert_function(Functions, Key, GlCommand) ->
    ?assert(maps:is_key(Key, Functions)),
    ?assertEqual(GlCommand, maps:get(gl_command, maps:get(Key, Functions))).

s004_assert_void_no_arg_path(FunctionData, GlCommand) ->
    ?assertEqual([], maps:get(params_specs, FunctionData)),
    ?assertEqual([], maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([], maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(GlCommand, NifFunctions),
    ?assertEqual([], maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 7.
s007_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s007_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s007_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"gen_textures", 1}, Functions)),
    ?assert(maps:is_key({"bind_texture", 2}, Functions)),
    ?assert(maps:is_key({"is_texture", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_textures", 2}, Functions)),

    s007_assert_gen_textures(maps:get({"gen_textures", 1}, Functions)),
    s007_assert_bind_texture(maps:get({"bind_texture", 2}, Functions)),
    s007_assert_is_texture(maps:get({"is_texture", 1}, Functions)).

s007_assert_gen_textures(FunctionData) ->
    ?assertEqual("glGenTextures", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{out, "Textures", {{list, 2, "N"}, {gl_object, texture}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"N", {undefined, pos_integer, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"Textures", {list, {undefined, texture, []}}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"N", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glGenTextures", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGenTextures", NifFunctions),
    ?assertEqual(
        [{"Textures", {return_list_terms_alloc, "GLuint", "enif_make_uint"}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s007_assert_bind_texture(FunctionData) ->
    ?assertEqual("glBindTexture", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Texture", {gl_object, texture, [none]}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Texture", {set, [{undefined, texture, []}, none]}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TransformMap}},
        {"Texture", {gl_object_to_uint, [{none, 0}]}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TransformMap)),
    ?assertEqual("glBindTexture", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glBindTexture", NifFunctions),
    ?assertEqual(
        [
            {"Target", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Texture", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s007_assert_is_texture(FunctionData) ->
    ?assertEqual("glIsTexture", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Texture", {gl_object, texture}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Texture", {undefined, texture, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"IsTexture", {gl, boolean, []}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Texture", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glIsTexture", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glIsTexture", NifFunctions),
    ?assertEqual(
        [{"Texture", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

%% Historical shard 8.
s008_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s008_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s008_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"delete_textures", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_textures", 2}, Functions)),

    s008_assert_delete_textures(maps:get({"delete_textures", 1}, Functions)).

s008_assert_delete_textures(FunctionData) ->
    ?assertEqual("glDeleteTextures", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Textures", {counted_list, "N", {gl_object, texture}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Textures", {list, {undefined, texture, []}}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Textures", {counted_list_gl_objects_to_binary, "N"}}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glDeleteTextures", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDeleteTextures", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"N", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {"Textures", binary_to_glbinary}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 9.
s009_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s009_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s009_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"gen_buffers", 1}, Functions)),
    ?assert(maps:is_key({"bind_buffer", 2}, Functions)),
    ?assert(maps:is_key({"is_buffer", 1}, Functions)),
    ?assert(maps:is_key({"delete_buffers", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_buffers", 2}, Functions)),

    s009_assert_enum_types(BindingData),
    s009_assert_gen_buffers(maps:get({"gen_buffers", 1}, Functions)),
    s009_assert_bind_buffer(maps:get({"bind_buffer", 2}, Functions)),
    s009_assert_is_buffer(maps:get({"is_buffer", 1}, Functions)),
    s009_assert_delete_buffers(maps:get({"delete_buffers", 1}, Functions)).

s009_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("buffer_target", EnumTypes)),
    ?assertNot(maps:is_key("buffer_target_arb", EnumTypes)),
    ?assert(lists:member("array_buffer", maps:get("buffer_target", EnumTypes))).

s009_assert_gen_buffers(FunctionData) ->
    ?assertEqual("glGenBuffers", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{out, "Buffers", {{list, 2, "N"}, {gl_object, buffer}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"N", {undefined, pos_integer, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"Buffers", {list, {undefined, buffer, []}}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"N", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glGenBuffers", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGenBuffers", NifFunctions),
    ?assertEqual(
        [{"Buffers", {return_list_terms_alloc, "GLuint", "enif_make_uint"}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s009_assert_bind_buffer(FunctionData) ->
    ?assertEqual("glBindBuffer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "Buffer", {gl_object, buffer, [none]}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, buffer_target, []}},
            {"Buffer", {set, [{undefined, buffer, []}, none]}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TransformMap}},
        {"Buffer", {gl_object_to_uint, [{none, 0}]}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("array_buffer", 1, TransformMap)),
    ?assertEqual("glBindBuffer", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glBindBuffer", NifFunctions),
    ?assertEqual(
        [
            {"Target", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {"Buffer", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s009_assert_is_buffer(FunctionData) ->
    ?assertEqual("glIsBuffer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Buffer", {gl_object, buffer}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Buffer", {undefined, buffer, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"IsBuffer", {gl, boolean, []}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Buffer", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glIsBuffer", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glIsBuffer", NifFunctions),
    ?assertEqual(
        [{"Buffer", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s009_assert_delete_buffers(FunctionData) ->
    ?assertEqual("glDeleteBuffers", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Buffers", {counted_list, "N", {gl_object, buffer}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Buffers", {list, {undefined, buffer, []}}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Buffers", {counted_list_gl_objects_to_binary, "N"}}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glDeleteBuffers", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDeleteBuffers", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"N", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {"Buffers", binary_to_glbinary}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 12.
s012_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s012_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()].

s012_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"gen_textures", 1}, Functions)),
    ?assert(maps:is_key({"bind_texture", 2}, Functions)),
    ?assert(maps:is_key({"is_texture", 1}, Functions)),
    ?assert(maps:is_key({"delete_textures", 1}, Functions)),
    ?assert(maps:is_key({"gen_buffers", 1}, Functions)),
    ?assert(maps:is_key({"bind_buffer", 2}, Functions)),
    ?assert(maps:is_key({"is_buffer", 1}, Functions)),
    ?assert(maps:is_key({"delete_buffers", 1}, Functions)),

    s012_assert_bind_texture(maps:get({"bind_texture", 2}, Functions)),
    s012_assert_bind_buffer(maps:get({"bind_buffer", 2}, Functions)),
    s012_assert_object_functions_unchanged(Functions).

s012_assert_bind_texture(FunctionData) ->
    ?assertEqual("glBindTexture", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {in, "Texture", {gl_object, texture, [none]}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"Texture", {set, [{undefined, texture, []}, none]}}
        ],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TransformMap}},
        {"Texture", {gl_object_to_uint, [{none, 0}]}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("texture_2d", 1, TransformMap)),
    ?assertEqual("glBindTexture", maps:get(raw_function, Clause)),

    s012_assert_uint_object_nif_param("glBindTexture", "Texture", FunctionData).

s012_assert_bind_buffer(FunctionData) ->
    ?assertEqual("glBindBuffer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "BufferTargetARB", buffer_target}},
            {in, "Buffer", {gl_object, buffer, [none]}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, buffer_target, []}},
            {"Buffer", {set, [{undefined, buffer, []}, none]}}
        ],
        maps:get(specs_params, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    [
        {"Target", {gl_enum_to_uint, TransformMap}},
        {"Buffer", {gl_object_to_uint, [{none, 0}]}}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("array_buffer", 1, TransformMap)),
    ?assertEqual("glBindBuffer", maps:get(raw_function, Clause)),

    s012_assert_uint_object_nif_param("glBindBuffer", "Buffer", FunctionData).

s012_assert_uint_object_nif_param(Command, ObjectParamName, FunctionData) ->
    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get(Command, NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}},
            {ObjectParamName, {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s012_assert_object_functions_unchanged(Functions) ->
    s012_assert_function_command({"gen_textures", 1}, "glGenTextures", Functions),
    s012_assert_function_command({"is_texture", 1}, "glIsTexture", Functions),
    s012_assert_function_command({"delete_textures", 1}, "glDeleteTextures", Functions),
    s012_assert_function_command({"gen_buffers", 1}, "glGenBuffers", Functions),
    s012_assert_function_command({"is_buffer", 1}, "glIsBuffer", Functions),
    s012_assert_function_command({"delete_buffers", 1}, "glDeleteBuffers", Functions).

s012_assert_function_command(Function, Command, Functions) ->
    FunctionData = maps:get(Function, Functions),
    ?assertEqual(Command, maps:get(gl_command, FunctionData)).

%% Historical shard 43.
s043_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s043_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s043_assert_target({gles, {2, 0}} = Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    ?assertNot(maps:is_key({"gen_vertex_arrays", 1}, Functions)),
    ?assertNot(maps:is_key({"bind_vertex_array", 1}, Functions)),
    ?assertNot(maps:is_key({"is_vertex_array", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_vertex_arrays", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_vertex_arrays", 2}, Functions));
s043_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    ?assert(maps:is_key({"gen_vertex_arrays", 1}, Functions)),
    ?assert(maps:is_key({"bind_vertex_array", 1}, Functions)),
    ?assert(maps:is_key({"is_vertex_array", 1}, Functions)),
    ?assert(maps:is_key({"delete_vertex_arrays", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_vertex_arrays", 2}, Functions)),

    s043_assert_gen_vertex_arrays(maps:get({"gen_vertex_arrays", 1}, Functions)),
    s043_assert_bind_vertex_array(maps:get({"bind_vertex_array", 1}, Functions)),
    s043_assert_is_vertex_array(maps:get({"is_vertex_array", 1}, Functions)),
    s043_assert_delete_vertex_arrays(maps:get({"delete_vertex_arrays", 1}, Functions)).

s043_assert_gen_vertex_arrays(FunctionData) ->
    ?assertEqual("glGenVertexArrays", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{out, "Arrays", {{list, 2, "N"}, {gl_object, vertex_array}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"N", {undefined, pos_integer, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"Arrays", {list, {undefined, vertex_array, []}}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"N", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glGenVertexArrays", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glGenVertexArrays", NifFunctions),
    ?assertEqual(
        [{"Arrays", {return_list_terms_alloc, "GLuint", "enif_make_uint"}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s043_assert_bind_vertex_array(FunctionData) ->
    ?assertEqual("glBindVertexArray", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Array", {gl_object, vertex_array, [none]}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Array", {set, [{undefined, vertex_array, []}, none]}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Array", {gl_object_to_uint, [{none, 0}]}}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glBindVertexArray", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glBindVertexArray", NifFunctions),
    ?assertEqual(
        [{"Array", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s043_assert_is_vertex_array(FunctionData) ->
    ?assertEqual("glIsVertexArray", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Array", {gl_object, vertex_array}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Array", {undefined, vertex_array, []}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"IsArray", {gl, boolean, []}}],
        maps:get(specs_return, FunctionData)
    ),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{"Array", do_nothing}], maps:get(params, Clause)),
    ?assertEqual("glIsVertexArray", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glIsVertexArray", NifFunctions),
    ?assertEqual(
        [{"Array", {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}}],
        maps:get(params, NifData)
    ),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s043_assert_delete_vertex_arrays(FunctionData) ->
    ?assertEqual("glDeleteVertexArrays", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Arrays", {counted_list, "N", {gl_object, vertex_array}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [{"Arrays", {list, {undefined, vertex_array, []}}}],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(1, maps:get(function_arity, FunctionData)),

    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(
        [{"Arrays", {counted_list_gl_objects_to_binary, "N"}}],
        maps:get(params, Clause)
    ),
    ?assertEqual("glDeleteVertexArrays", maps:get(raw_function, Clause)),

    NifFunctions = maps:get(nif_functions, FunctionData),
    NifData = maps:get("glDeleteVertexArrays", NifFunctions),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"N", {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}},
            {"Arrays", binary_to_glbinary}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

%% Historical shard 54.
s054_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s054_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s054_emitter_object_namespace_surface_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard54-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([query/0]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([sampler/0]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([gen_queries/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([delete_queries/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([begin_query/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([end_query/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([gen_samplers/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([delete_samplers/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_buffers/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_renderbuffers/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_textures/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_vertex_arrays/1]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_queries/2]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([create_samplers/1]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([delete_queries/2]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([delete_samplers/2]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([get_query/2]).">>)),

        ?assertMatch({_, _}, binary:match(C, <<"glBeginQuery(arg_0, arg_1);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glEndQuery(arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glGenSamplers(arg_0_n, arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glCreateBuffers(arg_0_n, arg_0);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glCreateTextures(arg_0, arg_1_n, arg_1);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glCreateQueries(arg_0, arg_1_n, arg_1);">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s054_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s054_assert_query_surface(Target, BindingData, Functions),
    s054_assert_sampler_surface(Target, BindingData, Functions),
    s054_assert_dsa_creation_surface(Target, BindingData, Functions),
    s054_assert_deferred_neighbors_absent(Functions).

s054_assert_query_surface({gles, {2, 0}}, BindingData, Functions) ->
    s054_assert_object_type_absent(query, BindingData),
    s054_assert_functions_absent(
        [
            {"gen_queries", 1},
            {"delete_queries", 1},
            {"delete_queries", 2},
            {"is_query", 1},
            {"begin_query", 2},
            {"end_query", 1}
        ],
        Functions
    );
s054_assert_query_surface(_Target, BindingData, Functions) ->
    s054_assert_object_type_present(query, BindingData),
    s054_assert_enum_contains(BindingData, "query_target", "any_samples_passed"),
    Expected = [
        {"gen_queries", 1},
        {"delete_queries", 1},
        {"is_query", 1},
        {"begin_query", 2},
        {"end_query", 1}
    ],
    [?assert(maps:is_key(Function, Functions)) || Function <- Expected],
    ?assertNot(maps:is_key({"delete_queries", 2}, Functions)),
    s054_assert_gen_object(maps:get({"gen_queries", 1}, Functions), "glGenQueries", "Queries", query),
    s054_assert_delete_object_list(
        maps:get({"delete_queries", 1}, Functions),
        "glDeleteQueries",
        "Queries",
        query
    ),
    s054_assert_is_object(maps:get({"is_query", 1}, Functions), "glIsQuery", "Query", "IsQuery", query),
    s054_assert_begin_query(maps:get({"begin_query", 2}, Functions)),
    s054_assert_end_query(maps:get({"end_query", 1}, Functions)).

s054_assert_sampler_surface({gles, {2, 0}}, BindingData, Functions) ->
    s054_assert_object_type_absent(sampler, BindingData),
    s054_assert_functions_absent(
        [
            {"gen_samplers", 1},
            {"delete_samplers", 1},
            {"delete_samplers", 2},
            {"is_sampler", 1}
        ],
        Functions
    );
s054_assert_sampler_surface(_Target, BindingData, Functions) ->
    s054_assert_object_type_present(sampler, BindingData),
    Expected = [
        {"gen_samplers", 1},
        {"delete_samplers", 1},
        {"is_sampler", 1}
    ],
    [?assert(maps:is_key(Function, Functions)) || Function <- Expected],
    ?assertNot(maps:is_key({"delete_samplers", 2}, Functions)),
    s054_assert_gen_object(maps:get({"gen_samplers", 1}, Functions), "glGenSamplers", "Samplers", sampler),
    s054_assert_delete_object_list(
        maps:get({"delete_samplers", 1}, Functions),
        "glDeleteSamplers",
        "Samplers",
        sampler
    ),
    s054_assert_is_object(
        maps:get({"is_sampler", 1}, Functions),
        "glIsSampler",
        "Sampler",
        "IsSampler",
        sampler
    ).

s054_assert_dsa_creation_surface({gl, {4, 6}}, BindingData, Functions) ->
    s054_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s054_assert_enum_contains(BindingData, "query_target", "any_samples_passed"),
    Expected = [
        {"create_buffers", 1},
        {"create_renderbuffers", 1},
        {"create_textures", 2},
        {"create_vertex_arrays", 1},
        {"create_queries", 2},
        {"create_samplers", 1}
    ],
    [?assert(maps:is_key(Function, Functions)) || Function <- Expected],
    s054_assert_gen_object(maps:get({"create_buffers", 1}, Functions), "glCreateBuffers", "Buffers", buffer),
    s054_assert_gen_object(
        maps:get({"create_renderbuffers", 1}, Functions),
        "glCreateRenderbuffers",
        "Renderbuffers",
        renderbuffer
    ),
    s054_assert_create_textures(maps:get({"create_textures", 2}, Functions)),
    s054_assert_gen_object(
        maps:get({"create_vertex_arrays", 1}, Functions),
        "glCreateVertexArrays",
        "Arrays",
        vertex_array
    ),
    s054_assert_create_queries(maps:get({"create_queries", 2}, Functions)),
    s054_assert_gen_object(maps:get({"create_samplers", 1}, Functions), "glCreateSamplers", "Samplers", sampler);
s054_assert_dsa_creation_surface(_Target, _BindingData, Functions) ->
    s054_assert_functions_absent(
        [
            {"create_buffers", 1},
            {"create_renderbuffers", 1},
            {"create_textures", 2},
            {"create_vertex_arrays", 1},
            {"create_queries", 2},
            {"create_samplers", 1}
        ],
        Functions
    ).

s054_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s054_assert_functions_absent(FunctionsToCheck, Functions) ->
    [?assertNot(maps:is_key(Function, Functions)) || Function <- FunctionsToCheck].

s054_assert_object_type_present(ObjectType, BindingData) ->
    ?assert(lists:keymember(ObjectType, 1, maps:get(object_types, BindingData))).

s054_assert_object_type_absent(ObjectType, BindingData) ->
    ?assertNot(lists:keymember(ObjectType, 1, maps:get(object_types, BindingData))).

s054_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s054_assert_gen_object(FunctionData, GlCommand, ParamName, ObjectType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{out, ParamName, {{list, 2, "N"}, {gl_object, ObjectType}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual([{"N", {undefined, pos_integer, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual(
        [{ParamName, {list, {undefined, ObjectType, []}}}],
        maps:get(specs_return, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    s054_assert_return_list_nif(FunctionData, GlCommand, ParamName, 1).

s054_assert_delete_object_list(FunctionData, GlCommand, ParamName, ObjectType) ->
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

s054_assert_is_object(FunctionData, GlCommand, ParamName, ReturnName, ObjectType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual([{in, ParamName, {gl_object, ObjectType}}], maps:get(params_specs, FunctionData)),
    ?assertEqual([{ParamName, {undefined, ObjectType, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual([{ReturnName, {gl, boolean, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{ParamName, do_nothing}], maps:get(params, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s054_assert_begin_query(FunctionData) ->
    ?assertEqual("glBeginQuery", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "QueryTarget"}},
            {in, "Query", {gl_object, query}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, query_target, []}},
            {"Query", {undefined, query, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s054_assert_enum_param("Target", "any_samples_passed", "GL_ANY_SAMPLES_PASSED", Params),
    ?assert(lists:member({"Query", do_nothing}, Params)),
    s054_assert_nif_params(FunctionData, "glBeginQuery", [gl_enum, gl_uint]).

s054_assert_end_query(FunctionData) ->
    ?assertEqual("glEndQuery", maps:get(gl_command, FunctionData)),
    ?assertEqual([{in, "Target", {gl_enum, "QueryTarget"}}], maps:get(params_specs, FunctionData)),
    ?assertEqual([{"Target", {undefined, query_target, []}}], maps:get(specs_params, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s054_assert_enum_param("Target", "any_samples_passed", "GL_ANY_SAMPLES_PASSED", maps:get(params, Clause)),
    s054_assert_nif_params(FunctionData, "glEndQuery", [gl_enum]).

s054_assert_create_textures(FunctionData) ->
    ?assertEqual("glCreateTextures", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget"}},
            {out, "Textures", {{list, 2, "N"}, {gl_object, texture}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, texture_target, []}},
            {"N", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"Textures", {list, {undefined, texture, []}}}],
        maps:get(specs_return, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    s054_assert_enum_param("Target", "texture_2d", "GL_TEXTURE_2D", maps:get(params, Clause)),
    s054_assert_return_list_nif(FunctionData, "glCreateTextures", "Textures", 2).

s054_assert_create_queries(FunctionData) ->
    ?assertEqual("glCreateQueries", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "QueryTarget"}},
            {out, "Queries", {{list, 2, "N"}, {gl_object, query}}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, query_target, []}},
            {"N", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual(
        [{"Queries", {list, {undefined, query, []}}}],
        maps:get(specs_return, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    s054_assert_enum_param("Target", "any_samples_passed", "GL_ANY_SAMPLES_PASSED", maps:get(params, Clause)),
    s054_assert_return_list_nif(FunctionData, "glCreateQueries", "Queries", 2).

s054_assert_return_list_nif(FunctionData, GlCommand, ParamName, ExpectedArity) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(ExpectedArity, maps:get(arity, NifData)),
    ?assert(
        lists:member(
            {ParamName, {return_list_terms_alloc, "GLuint", "enif_make_uint"}},
            maps:get(params, NifData)
        )
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s054_assert_nif_params(FunctionData, GlCommand, ParamKinds) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    Actual = [s054_classify_nif_param(Param) || {_Name, Param} <- maps:get(params, NifData)],
    ?assertEqual(ParamKinds, Actual),
    ?assertEqual(void, maps:get(return, NifData)).

s054_classify_nif_param({gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}) ->
    gl_enum;
s054_classify_nif_param({gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}) ->
    gl_uint.

s054_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).
