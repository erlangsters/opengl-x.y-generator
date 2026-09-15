-module(resolver_pixel_framebuffer_tests).
-include_lib("eunit/include/eunit.hrl").

%% Pixel, framebuffer, renderbuffer, and clear resolver contracts.

%% Historical shard 53.
s053_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s053_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s053_emitter_framebuffer_renderbuffer_surface_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard53-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, "OpenGL 4.6", generator_test_support:resolve_target({gl, {4, 6}})),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate({gl, {4, 6}}, BindingData),
        gl_nif_module_generator:generate({gl, {4, 6}}, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([renderbuffer/0]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export_type([framebuffer/0]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export_type([render_buffer/0]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export_type([frame_buffer/0]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([renderbuffer_storage/4]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([framebuffer_renderbuffer/4]).">>)),
        ?assertMatch({_, _}, binary:match(Erl, <<"-export([check_framebuffer_status/1]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([delete_renderbuffers/2]).">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"-export([delete_framebuffers/2]).">>)),

        ?assertMatch({_, _}, binary:match(C, <<"glRenderbufferStorage(arg_0, arg_1, arg_2, arg_3);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glFramebufferRenderbuffer(arg_0, arg_1, arg_2, arg_3);">>)),
        ?assertMatch({_, _}, binary:match(C, <<"glCheckFramebufferStatus(arg_0);">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s053_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    Expected = [
        {"gen_renderbuffers", 1},
        {"bind_renderbuffer", 2},
        {"is_renderbuffer", 1},
        {"delete_renderbuffers", 1},
        {"renderbuffer_storage", 4},
        {"gen_framebuffers", 1},
        {"bind_framebuffer", 2},
        {"is_framebuffer", 1},
        {"delete_framebuffers", 1},
        {"check_framebuffer_status", 1},
        {"framebuffer_renderbuffer", 4}
    ],
    [?assert(maps:is_key(Function, Functions)) || Function <- Expected],
    ?assertNot(maps:is_key({"delete_renderbuffers", 2}, Functions)),
    ?assertNot(maps:is_key({"delete_framebuffers", 2}, Functions)),
    s053_assert_deferred_neighbors_absent(Functions),

    s053_assert_enum_types(BindingData),
    s053_assert_gen_object(
        maps:get({"gen_renderbuffers", 1}, Functions),
        "glGenRenderbuffers",
        "Renderbuffers",
        renderbuffer
    ),
    s053_assert_bind_renderbuffer(maps:get({"bind_renderbuffer", 2}, Functions)),
    s053_assert_is_object(
        maps:get({"is_renderbuffer", 1}, Functions),
        "glIsRenderbuffer",
        "Renderbuffer",
        "IsRenderbuffer",
        renderbuffer
    ),
    s053_assert_delete_object_list(
        maps:get({"delete_renderbuffers", 1}, Functions),
        "glDeleteRenderbuffers",
        "Renderbuffers",
        renderbuffer
    ),
    s053_assert_renderbuffer_storage(maps:get({"renderbuffer_storage", 4}, Functions)),
    s053_assert_gen_object(
        maps:get({"gen_framebuffers", 1}, Functions),
        "glGenFramebuffers",
        "Framebuffers",
        framebuffer
    ),
    s053_assert_bind_framebuffer(maps:get({"bind_framebuffer", 2}, Functions)),
    s053_assert_is_object(
        maps:get({"is_framebuffer", 1}, Functions),
        "glIsFramebuffer",
        "Framebuffer",
        "IsFramebuffer",
        framebuffer
    ),
    s053_assert_delete_object_list(
        maps:get({"delete_framebuffers", 1}, Functions),
        "glDeleteFramebuffers",
        "Framebuffers",
        framebuffer
    ),
    s053_assert_check_framebuffer_status(maps:get({"check_framebuffer_status", 1}, Functions)),
    s053_assert_framebuffer_renderbuffer(maps:get({"framebuffer_renderbuffer", 4}, Functions)).

s053_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s053_assert_enum_types(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member("renderbuffer", maps:get("renderbuffer_target", EnumTypes))),
    ?assert(lists:member("framebuffer", maps:get("framebuffer_target", EnumTypes))),
    ?assert(lists:member("color_attachment0", maps:get("framebuffer_attachment", EnumTypes))),
    ?assert(lists:member("rgba4", maps:get("internal_format", EnumTypes))),
    ?assert(lists:member("framebuffer_complete", maps:get("framebuffer_status", EnumTypes))).

s053_assert_gen_object(FunctionData, GlCommand, ParamName, ObjectType) ->
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
    ?assertEqual([{ParamName, {list, {undefined, ObjectType, []}}}], maps:get(specs_return, FunctionData)),
    ?assertEqual([{ParamName, {return_list_terms_alloc, "GLuint", "enif_make_uint"}}],
        maps:get(params, maps:get(GlCommand, maps:get(nif_functions, FunctionData)))
    ),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)).

s053_assert_bind_renderbuffer(FunctionData) ->
    ?assertEqual("glBindRenderbuffer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "RenderbufferTarget"}},
            {in, "Renderbuffer", {gl_object, renderbuffer}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, renderbuffer_target, []}},
            {"Renderbuffer", {undefined, renderbuffer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s053_assert_enum_param("Target", "renderbuffer", "GL_RENDERBUFFER", Params),
    ?assert(lists:member({"Renderbuffer", do_nothing}, Params)),
    s053_assert_nif_params(FunctionData, "glBindRenderbuffer", [gl_enum, gl_uint]).

s053_assert_bind_framebuffer(FunctionData) ->
    ?assertEqual("glBindFramebuffer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Framebuffer", {gl_object, framebuffer}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Framebuffer", {undefined, framebuffer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s053_assert_enum_param("Target", "framebuffer", "GL_FRAMEBUFFER", Params),
    ?assert(lists:member({"Framebuffer", do_nothing}, Params)),
    s053_assert_nif_params(FunctionData, "glBindFramebuffer", [gl_enum, gl_uint]).

s053_assert_is_object(FunctionData, GlCommand, ParamName, ReturnName, ObjectType) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual([{in, ParamName, {gl_object, ObjectType}}], maps:get(params_specs, FunctionData)),
    ?assertEqual([{ParamName, {undefined, ObjectType, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual([{ReturnName, {gl, boolean, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual([{ParamName, do_nothing}], maps:get(params, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(glbool_to_boolean, maps:get(return, NifData)).

s053_assert_delete_object_list(FunctionData, GlCommand, ParamName, ObjectType) ->
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
    ).

s053_assert_renderbuffer_storage(FunctionData) ->
    ?assertEqual("glRenderbufferStorage", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "RenderbufferTarget"}},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, renderbuffer_target, []}},
            {"InternalFormat", {undefined, internal_format, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s053_assert_enum_param("Target", "renderbuffer", "GL_RENDERBUFFER", Params),
    s053_assert_enum_param("InternalFormat", "rgba4", "GL_RGBA4", Params),
    ?assert(lists:member({"Width", do_nothing}, Params)),
    ?assert(lists:member({"Height", do_nothing}, Params)),
    s053_assert_nif_params(FunctionData, "glRenderbufferStorage", [gl_enum, gl_enum, gl_sizei, gl_sizei]).

s053_assert_check_framebuffer_status(FunctionData) ->
    ?assertEqual("glCheckFramebufferStatus", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{in, "Target", {gl_enum, "FramebufferTarget"}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"Status", {gl_enum, "FramebufferStatus"}}, maps:get(return_specs, FunctionData)),
    ?assertEqual([{"Status", {undefined, framebuffer_status, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s053_assert_enum_param("Target", "framebuffer", "GL_FRAMEBUFFER", maps:get(params, Clause)),
    NifData = maps:get("glCheckFramebufferStatus", maps:get(nif_functions, FunctionData)),
    {glenum_to_atom, TransformMap} = maps:get(return, NifData),
    ?assert(lists:member({"GL_FRAMEBUFFER_COMPLETE", "framebuffer_complete"}, TransformMap)).

s053_assert_framebuffer_renderbuffer(FunctionData) ->
    ?assertEqual("glFramebufferRenderbuffer", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
            {in, "RenderbufferTarget", {gl_enum, "RenderbufferTarget"}},
            {in, "Renderbuffer", {gl_object, renderbuffer}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Attachment", {undefined, framebuffer_attachment, []}},
            {"RenderbufferTarget", {undefined, renderbuffer_target, []}},
            {"Renderbuffer", {undefined, renderbuffer, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s053_assert_enum_param("Target", "framebuffer", "GL_FRAMEBUFFER", Params),
    s053_assert_enum_param("Attachment", "color_attachment0", "GL_COLOR_ATTACHMENT0", Params),
    s053_assert_enum_param("RenderbufferTarget", "renderbuffer", "GL_RENDERBUFFER", Params),
    ?assert(lists:member({"Renderbuffer", do_nothing}, Params)),
    s053_assert_nif_params(FunctionData, "glFramebufferRenderbuffer", [gl_enum, gl_enum, gl_enum, gl_uint]).

s053_assert_nif_params(FunctionData, GlCommand, ParamKinds) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    Actual = [s053_classify_nif_param(Param) || {_Name, Param} <- maps:get(params, NifData)],
    ?assertEqual(ParamKinds, Actual),
    ?assertEqual(void, maps:get(return, NifData)).

s053_classify_nif_param({gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}) ->
    gl_enum;
s053_classify_nif_param({gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}) ->
    gl_uint;
s053_classify_nif_param({gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}) ->
    gl_sizei.

s053_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).

%% Historical shard 59.
s059_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s059_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s059_emitter_framebuffer_copy_surface_test_() ->
    [
        {"gl 4.6", fun() -> s059_assert_emitted_surface({gl, {4, 6}}, "OpenGL 4.6", s059_gl_46_exports(), s059_gl_46_c_calls()) end},
        {"gles 3.2", fun() ->
            s059_assert_emitted_surface({gles, {3, 2}}, "OpenGL ES 3.2", s059_gles_32_exports(), s059_gles_32_c_calls())
        end}
    ].

s059_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s059_assert_presence(Target, Functions),
    s059_assert_deferred_neighbors_absent(Functions),
    s059_assert_present_paths(Target, BindingData, Functions).

s059_assert_presence(Target, Functions) ->
    Present = s059_present_functions(Target),
    Absent = s059_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s059_present_functions({gl, {3, 3}}) ->
    s059_non_dsa_framebuffer_copy();
s059_present_functions({gl, {4, 1}}) ->
    s059_non_dsa_framebuffer_copy();
s059_present_functions({gl, {4, 6}}) ->
    s059_all_functions();
s059_present_functions({gles, {2, 0}}) ->
    [{"framebuffer_texture_2d", 5}];
s059_present_functions({gles, {3, 0}}) ->
    s059_es3_framebuffer_copy();
s059_present_functions({gles, {3, 1}}) ->
    s059_es3_framebuffer_copy();
s059_present_functions({gles, {3, 2}}) ->
    s059_es3_framebuffer_copy() ++ [{"framebuffer_texture", 4}].

s059_all_functions() ->
    s059_non_dsa_framebuffer_copy() ++ s059_dsa_framebuffer_copy().

s059_non_dsa_framebuffer_copy() ->
    [
        {"framebuffer_texture_1d", 5},
        {"framebuffer_texture_2d", 5},
        {"framebuffer_texture_3d", 6},
        {"framebuffer_texture", 4},
        {"framebuffer_texture_layer", 5},
        {"blit_framebuffer", 10},
        {"copy_buffer_sub_data", 5}
    ].

s059_es3_framebuffer_copy() ->
    [
        {"framebuffer_texture_2d", 5},
        {"framebuffer_texture_layer", 5},
        {"blit_framebuffer", 10},
        {"copy_buffer_sub_data", 5}
    ].

s059_dsa_framebuffer_copy() ->
    [
        {"copy_named_buffer_sub_data", 5},
        {"create_framebuffers", 1},
        {"named_framebuffer_renderbuffer", 4},
        {"named_framebuffer_texture", 4},
        {"named_framebuffer_texture_layer", 5},
        {"check_named_framebuffer_status", 2},
        {"named_renderbuffer_storage", 4},
        {"blit_named_framebuffer", 12}
    ].

s059_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands
    ].

s059_assert_present_paths(Target, BindingData, Functions) ->
    lists:foreach(
        fun(Function) -> s059_assert_path(Function, Target, BindingData, maps:get(Function, Functions)) end,
        s059_present_functions(Target)
    ).

s059_assert_path({"framebuffer_texture_1d", 5}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "texture_target", "texture_1d"),
    s059_assert_framebuffer_texture_target_direct(FunctionData, "glFramebufferTexture1D", "texture_1d");
s059_assert_path({"framebuffer_texture_2d", 5}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "texture_target", "texture_2d"),
    s059_assert_framebuffer_texture_target_direct(FunctionData, "glFramebufferTexture2D", "texture_2d");
s059_assert_path({"framebuffer_texture_3d", 6}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "texture_target", "texture_3d"),
    s059_assert_direct(
        FunctionData,
        "glFramebufferTexture3D",
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
            {in, "TextureTarget", {gl_enum, "TextureTarget"}},
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int},
            {in, "Layer", gl_int}
        ],
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Attachment", {undefined, framebuffer_attachment, []}},
            {"TextureTarget", {undefined, texture_target, []}},
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}},
            {"Layer", {gl, int, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["framebuffer"]}},
            {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
            {"TextureTarget", {gl_enum_to_uint, ["texture_3d"]}},
            {"Texture", do_nothing},
            {"Level", do_nothing},
            {"Layer", do_nothing}
        ],
        [
            {"Target", s059_enum_nif_data()},
            {"Attachment", s059_enum_nif_data()},
            {"TextureTarget", s059_enum_nif_data()},
            {"Texture", s059_uint_nif_data()},
            {"Level", s059_int_nif_data()},
            {"Layer", s059_int_nif_data()}
        ]
    );
s059_assert_path({"framebuffer_texture", 4}, _Target, _BindingData, FunctionData) ->
    s059_assert_framebuffer_texture_object_direct(FunctionData, "glFramebufferTexture");
s059_assert_path({"framebuffer_texture_layer", 5}, _Target, _BindingData, FunctionData) ->
    s059_assert_framebuffer_texture_layer_direct(FunctionData, "glFramebufferTextureLayer");
s059_assert_path({"blit_framebuffer", 10}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "blit_framebuffer_filter", "nearest"),
    s059_assert_blit_direct(FunctionData, "glBlitFramebuffer", []);
s059_assert_path({"copy_buffer_sub_data", 5}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "copy_buffer_sub_data_target", "copy_read_buffer"),
    s059_assert_copy_buffer_target_direct(FunctionData);
s059_assert_path({"copy_named_buffer_sub_data", 5}, _Target, _BindingData, FunctionData) ->
    s059_assert_named_buffer_copy_direct(FunctionData);
s059_assert_path({"create_framebuffers", 1}, _Target, _BindingData, FunctionData) ->
    s059_assert_create_framebuffers(FunctionData);
s059_assert_path({"named_framebuffer_renderbuffer", 4}, _Target, _BindingData, FunctionData) ->
    s059_assert_named_framebuffer_renderbuffer(FunctionData);
s059_assert_path({"named_framebuffer_texture", 4}, _Target, _BindingData, FunctionData) ->
    s059_assert_framebuffer_texture_object_direct(FunctionData, "glNamedFramebufferTexture");
s059_assert_path({"named_framebuffer_texture_layer", 5}, _Target, _BindingData, FunctionData) ->
    s059_assert_framebuffer_texture_layer_direct(FunctionData, "glNamedFramebufferTextureLayer");
s059_assert_path({"check_named_framebuffer_status", 2}, _Target, _BindingData, FunctionData) ->
    s059_assert_check_named_framebuffer_status(FunctionData);
s059_assert_path({"named_renderbuffer_storage", 4}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "internal_format", "rgba4"),
    s059_assert_named_renderbuffer_storage(FunctionData);
s059_assert_path({"blit_named_framebuffer", 12}, _Target, BindingData, FunctionData) ->
    s059_assert_enum_contains(BindingData, "blit_framebuffer_filter", "nearest"),
    s059_assert_blit_direct(
        FunctionData,
        "glBlitNamedFramebuffer",
        [
            {"ReadFramebuffer", {undefined, framebuffer, []}},
            {"DrawFramebuffer", {undefined, framebuffer, []}}
        ]
    ).

s059_assert_framebuffer_texture_target_direct(FunctionData, GlCommand, TextureTargetAtom) ->
    s059_assert_direct(
        FunctionData,
        GlCommand,
        [
            {in, "Target", {gl_enum, "FramebufferTarget"}},
            {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
            {in, "TextureTarget", {gl_enum, "TextureTarget"}},
            {in, "Texture", {gl_object, texture}},
            {in, "Level", gl_int}
        ],
        [
            {"Target", {undefined, framebuffer_target, []}},
            {"Attachment", {undefined, framebuffer_attachment, []}},
            {"TextureTarget", {undefined, texture_target, []}},
            {"Texture", {undefined, texture, []}},
            {"Level", {gl, int, []}}
        ],
        [
            {"Target", {gl_enum_to_uint, ["framebuffer"]}},
            {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
            {"TextureTarget", {gl_enum_to_uint, [TextureTargetAtom]}},
            {"Texture", do_nothing},
            {"Level", do_nothing}
        ],
        [
            {"Target", s059_enum_nif_data()},
            {"Attachment", s059_enum_nif_data()},
            {"TextureTarget", s059_enum_nif_data()},
            {"Texture", s059_uint_nif_data()},
            {"Level", s059_int_nif_data()}
        ]
    ).

s059_assert_framebuffer_texture_object_direct(FunctionData, GlCommand) ->
    {ParamsSpecs, SpecsParams, ClauseParams, NifParams} =
        case GlCommand of
            "glFramebufferTexture" ->
                {
                    [
                        {in, "Target", {gl_enum, "FramebufferTarget"}},
                        {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
                        {in, "Texture", {gl_object, texture}},
                        {in, "Level", gl_int}
                    ],
                    [
                        {"Target", {undefined, framebuffer_target, []}},
                        {"Attachment", {undefined, framebuffer_attachment, []}},
                        {"Texture", {undefined, texture, []}},
                        {"Level", {gl, int, []}}
                    ],
                    [
                        {"Target", {gl_enum_to_uint, ["framebuffer"]}},
                        {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
                        {"Texture", do_nothing},
                        {"Level", do_nothing}
                    ],
                    [
                        {"Target", s059_enum_nif_data()},
                        {"Attachment", s059_enum_nif_data()},
                        {"Texture", s059_uint_nif_data()},
                        {"Level", s059_int_nif_data()}
                    ]
                };
            "glNamedFramebufferTexture" ->
                {
                    [
                        {in, "Framebuffer", {gl_object, framebuffer}},
                        {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
                        {in, "Texture", {gl_object, texture}},
                        {in, "Level", gl_int}
                    ],
                    [
                        {"Framebuffer", {undefined, framebuffer, []}},
                        {"Attachment", {undefined, framebuffer_attachment, []}},
                        {"Texture", {undefined, texture, []}},
                        {"Level", {gl, int, []}}
                    ],
                    [
                        {"Framebuffer", do_nothing},
                        {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
                        {"Texture", do_nothing},
                        {"Level", do_nothing}
                    ],
                    [
                        {"Framebuffer", s059_uint_nif_data()},
                        {"Attachment", s059_enum_nif_data()},
                        {"Texture", s059_uint_nif_data()},
                        {"Level", s059_int_nif_data()}
                    ]
                }
        end,
    s059_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s059_assert_framebuffer_texture_layer_direct(FunctionData, GlCommand) ->
    {ParamsSpecs, SpecsParams, ClauseParams, NifParams} =
        case GlCommand of
            "glFramebufferTextureLayer" ->
                {
                    [
                        {in, "Target", {gl_enum, "FramebufferTarget"}},
                        {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
                        {in, "Texture", {gl_object, texture}},
                        {in, "Level", gl_int},
                        {in, "Layer", gl_int}
                    ],
                    [
                        {"Target", {undefined, framebuffer_target, []}},
                        {"Attachment", {undefined, framebuffer_attachment, []}},
                        {"Texture", {undefined, texture, []}},
                        {"Level", {gl, int, []}},
                        {"Layer", {gl, int, []}}
                    ],
                    [
                        {"Target", {gl_enum_to_uint, ["framebuffer"]}},
                        {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
                        {"Texture", do_nothing},
                        {"Level", do_nothing},
                        {"Layer", do_nothing}
                    ],
                    [
                        {"Target", s059_enum_nif_data()},
                        {"Attachment", s059_enum_nif_data()},
                        {"Texture", s059_uint_nif_data()},
                        {"Level", s059_int_nif_data()},
                        {"Layer", s059_int_nif_data()}
                    ]
                };
            "glNamedFramebufferTextureLayer" ->
                {
                    [
                        {in, "Framebuffer", {gl_object, framebuffer}},
                        {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
                        {in, "Texture", {gl_object, texture}},
                        {in, "Level", gl_int},
                        {in, "Layer", gl_int}
                    ],
                    [
                        {"Framebuffer", {undefined, framebuffer, []}},
                        {"Attachment", {undefined, framebuffer_attachment, []}},
                        {"Texture", {undefined, texture, []}},
                        {"Level", {gl, int, []}},
                        {"Layer", {gl, int, []}}
                    ],
                    [
                        {"Framebuffer", do_nothing},
                        {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
                        {"Texture", do_nothing},
                        {"Level", do_nothing},
                        {"Layer", do_nothing}
                    ],
                    [
                        {"Framebuffer", s059_uint_nif_data()},
                        {"Attachment", s059_enum_nif_data()},
                        {"Texture", s059_uint_nif_data()},
                        {"Level", s059_int_nif_data()},
                        {"Layer", s059_int_nif_data()}
                    ]
                }
        end,
    s059_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams).

s059_assert_blit_direct(FunctionData, GlCommand, LeadingSpecsParams) ->
    LeadingParamsSpecs =
        case GlCommand of
            "glBlitFramebuffer" -> [];
            "glBlitNamedFramebuffer" ->
                [
                    {in, "ReadFramebuffer", {gl_object, framebuffer}},
                    {in, "DrawFramebuffer", {gl_object, framebuffer}}
                ]
        end,
    LeadingClauseParams =
        [{Name, do_nothing} || {Name, _Spec} <- LeadingSpecsParams],
    LeadingNifParams =
        [{Name, s059_uint_nif_data()} || {Name, _Spec} <- LeadingSpecsParams],
    IntParams = [
        "SrcX0",
        "SrcY0",
        "SrcX1",
        "SrcY1",
        "DstX0",
        "DstY0",
        "DstX1",
        "DstY1"
    ],
    s059_assert_direct(
        FunctionData,
        GlCommand,
        LeadingParamsSpecs
            ++ [{in, Name, gl_int} || Name <- IntParams]
            ++ [
                {in, "Mask", {gl_bitfield, "ClearBufferMask"}},
                {in, "Filter", {gl_enum, "BlitFramebufferFilter"}}
            ],
        LeadingSpecsParams
            ++ [{Name, {gl, int, []}} || Name <- IntParams]
            ++ [
                {"Mask", {undefined, clear_buffer_mask, []}},
                {"Filter", {undefined, blit_framebuffer_filter, []}}
            ],
        LeadingClauseParams
            ++ [{Name, do_nothing} || Name <- IntParams]
            ++ [
                {"Mask", {gl_bitfield_to_uint, ["color_buffer_bit"]}},
                {"Filter", {gl_enum_to_uint, ["nearest"]}}
            ],
        LeadingNifParams
            ++ [{Name, s059_int_nif_data()} || Name <- IntParams]
            ++ [
                {"Mask", s059_bitfield_nif_data()},
                {"Filter", s059_enum_nif_data()}
            ]
    ).

s059_assert_copy_buffer_target_direct(FunctionData) ->
    s059_assert_direct(
        FunctionData,
        "glCopyBufferSubData",
        [
            {in, "ReadTarget", {gl_enum, "CopyBufferSubDataTarget"}},
            {in, "WriteTarget", {gl_enum, "CopyBufferSubDataTarget"}},
            {in, "ReadOffset", gl_intptr},
            {in, "WriteOffset", gl_intptr},
            {in, "Size", gl_sizeiptr}
        ],
        [
            {"ReadTarget", {undefined, copy_buffer_sub_data_target, []}},
            {"WriteTarget", {undefined, copy_buffer_sub_data_target, []}},
            {"ReadOffset", {gl, intptr, []}},
            {"WriteOffset", {gl, intptr, []}},
            {"Size", {gl, sizeiptr, []}}
        ],
        [
            {"ReadTarget", {gl_enum_to_uint, ["copy_read_buffer"]}},
            {"WriteTarget", {gl_enum_to_uint, ["copy_write_buffer"]}},
            {"ReadOffset", do_nothing},
            {"WriteOffset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"ReadTarget", s059_enum_nif_data()},
            {"WriteTarget", s059_enum_nif_data()},
            {"ReadOffset", s059_intptr_nif_data()},
            {"WriteOffset", s059_intptr_nif_data()},
            {"Size", s059_sizeiptr_nif_data()}
        ]
    ).

s059_assert_named_buffer_copy_direct(FunctionData) ->
    s059_assert_direct(
        FunctionData,
        "glCopyNamedBufferSubData",
        [
            {in, "ReadBuffer", {gl_object, buffer}},
            {in, "WriteBuffer", {gl_object, buffer}},
            {in, "ReadOffset", gl_intptr},
            {in, "WriteOffset", gl_intptr},
            {in, "Size", gl_sizeiptr}
        ],
        [
            {"ReadBuffer", {undefined, buffer, []}},
            {"WriteBuffer", {undefined, buffer, []}},
            {"ReadOffset", {gl, intptr, []}},
            {"WriteOffset", {gl, intptr, []}},
            {"Size", {gl, sizeiptr, []}}
        ],
        [
            {"ReadBuffer", do_nothing},
            {"WriteBuffer", do_nothing},
            {"ReadOffset", do_nothing},
            {"WriteOffset", do_nothing},
            {"Size", do_nothing}
        ],
        [
            {"ReadBuffer", s059_uint_nif_data()},
            {"WriteBuffer", s059_uint_nif_data()},
            {"ReadOffset", s059_intptr_nif_data()},
            {"WriteOffset", s059_intptr_nif_data()},
            {"Size", s059_sizeiptr_nif_data()}
        ]
    ).

s059_assert_create_framebuffers(FunctionData) ->
    ?assertEqual("glCreateFramebuffers", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [{out, "Framebuffers", {{list, 2, "N"}, {gl_object, framebuffer}}}],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual([{"N", {undefined, pos_integer, []}}], maps:get(specs_params, FunctionData)),
    ?assertEqual([{"Framebuffers", {list, {undefined, framebuffer, []}}}], maps:get(specs_return, FunctionData)),
    NifData = maps:get("glCreateFramebuffers", maps:get(nif_functions, FunctionData)),
    ?assertEqual([{"Framebuffers", {return_list_terms_alloc, "GLuint", "enif_make_uint"}}], maps:get(params, NifData)).

s059_assert_named_framebuffer_renderbuffer(FunctionData) ->
    s059_assert_direct(
        FunctionData,
        "glNamedFramebufferRenderbuffer",
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Attachment", {gl_enum, "FramebufferAttachment"}},
            {in, "RenderbufferTarget", {gl_enum, "RenderbufferTarget"}},
            {in, "Renderbuffer", {gl_object, renderbuffer}}
        ],
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Attachment", {undefined, framebuffer_attachment, []}},
            {"RenderbufferTarget", {undefined, renderbuffer_target, []}},
            {"Renderbuffer", {undefined, renderbuffer, []}}
        ],
        [
            {"Framebuffer", do_nothing},
            {"Attachment", {gl_enum_to_uint, ["color_attachment0"]}},
            {"RenderbufferTarget", {gl_enum_to_uint, ["renderbuffer"]}},
            {"Renderbuffer", do_nothing}
        ],
        [
            {"Framebuffer", s059_uint_nif_data()},
            {"Attachment", s059_enum_nif_data()},
            {"RenderbufferTarget", s059_enum_nif_data()},
            {"Renderbuffer", s059_uint_nif_data()}
        ]
    ).

s059_assert_check_named_framebuffer_status(FunctionData) ->
    ?assertEqual("glCheckNamedFramebufferStatus", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Framebuffer", {gl_object, framebuffer}},
            {in, "Target", {gl_enum, "FramebufferTarget"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual({"Status", {gl_enum, "FramebufferStatus"}}, maps:get(return_specs, FunctionData)),
    ?assertEqual(
        [
            {"Framebuffer", {undefined, framebuffer, []}},
            {"Target", {undefined, framebuffer_target, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    ?assertEqual([{"Status", {undefined, framebuffer_status, []}}], maps:get(specs_return, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s059_assert_clause_params(
        [
            {"Framebuffer", do_nothing},
            {"Target", {gl_enum_to_uint, ["framebuffer"]}}
        ],
        maps:get(params, Clause)
    ),
    NifData = maps:get("glCheckNamedFramebufferStatus", maps:get(nif_functions, FunctionData)),
    ?assertEqual([{"Framebuffer", s059_uint_nif_data()}, {"Target", s059_enum_nif_data()}], maps:get(params, NifData)),
    {glenum_to_atom, TransformMap} = maps:get(return, NifData),
    ?assert(lists:member({"GL_FRAMEBUFFER_COMPLETE", "framebuffer_complete"}, TransformMap)).

s059_assert_named_renderbuffer_storage(FunctionData) ->
    s059_assert_direct(
        FunctionData,
        "glNamedRenderbufferStorage",
        [
            {in, "Renderbuffer", {gl_object, renderbuffer}},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}},
            {in, "Width", gl_sizei},
            {in, "Height", gl_sizei}
        ],
        [
            {"Renderbuffer", {undefined, renderbuffer, []}},
            {"InternalFormat", {undefined, internal_format, []}},
            {"Width", {gl, sizei, []}},
            {"Height", {gl, sizei, []}}
        ],
        [
            {"Renderbuffer", do_nothing},
            {"InternalFormat", {gl_enum_to_uint, ["rgba4"]}},
            {"Width", do_nothing},
            {"Height", do_nothing}
        ],
        [
            {"Renderbuffer", s059_uint_nif_data()},
            {"InternalFormat", s059_enum_nif_data()},
            {"Width", s059_sizei_nif_data()},
            {"Height", s059_sizei_nif_data()}
        ]
    ).

s059_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s059_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s059_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s059_assert_clause_param/1, lists:zip(Expected, Actual)).

s059_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s059_assert_clause_param({{Name, {gl_bitfield_to_uint, RequiredAtoms}}, {Name, {gl_bitfield_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s059_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s059_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s059_assert_emitted_surface(Target, ApiName, ExpectedExports, ExpectedCalls) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard59-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        [?assertEqual(nomatch, binary:match(Erl, Export)) || Export <- s059_deferred_exports()],
        [?assertEqual(nomatch, binary:match(C, Call)) || Call <- s059_deferred_c_calls()],
        ?assertEqual(nomatch, binary:match(Erl, <<"frame_buffer()">>)),
        ?assertEqual(nomatch, binary:match(Erl, <<"render_buffer()">>))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s059_gl_46_exports() ->
    [
        <<"-export([framebuffer_texture_1d/5]).">>,
        <<"-export([framebuffer_texture_2d/5]).">>,
        <<"-export([framebuffer_texture_3d/6]).">>,
        <<"-export([framebuffer_texture/4]).">>,
        <<"-export([framebuffer_texture_layer/5]).">>,
        <<"-export([blit_framebuffer/10]).">>,
        <<"-export([copy_buffer_sub_data/5]).">>,
        <<"-export([copy_named_buffer_sub_data/5]).">>,
        <<"-export([create_framebuffers/1]).">>,
        <<"-export([named_framebuffer_renderbuffer/4]).">>,
        <<"-export([named_framebuffer_texture/4]).">>,
        <<"-export([named_framebuffer_texture_layer/5]).">>,
        <<"-export([check_named_framebuffer_status/2]).">>,
        <<"-export([named_renderbuffer_storage/4]).">>,
        <<"-export([blit_named_framebuffer/12]).">>
    ].

s059_gles_32_exports() ->
    [
        <<"-export([framebuffer_texture_2d/5]).">>,
        <<"-export([framebuffer_texture/4]).">>,
        <<"-export([framebuffer_texture_layer/5]).">>,
        <<"-export([blit_framebuffer/10]).">>,
        <<"-export([copy_buffer_sub_data/5]).">>
    ].

s059_gl_46_c_calls() ->
    [
        <<"glFramebufferTexture1D(">>,
        <<"glFramebufferTexture2D(">>,
        <<"glFramebufferTexture3D(">>,
        <<"glFramebufferTexture(">>,
        <<"glFramebufferTextureLayer(">>,
        <<"glBlitFramebuffer(">>,
        <<"glCopyBufferSubData(">>,
        <<"glCopyNamedBufferSubData(">>,
        <<"glCreateFramebuffers(">>,
        <<"glNamedFramebufferRenderbuffer(">>,
        <<"glNamedFramebufferTexture(">>,
        <<"glNamedFramebufferTextureLayer(">>,
        <<"glCheckNamedFramebufferStatus(">>,
        <<"glNamedRenderbufferStorage(">>,
        <<"glBlitNamedFramebuffer(">>
    ].

s059_gles_32_c_calls() ->
    [
        <<"glFramebufferTexture2D(">>,
        <<"glFramebufferTexture(">>,
        <<"glFramebufferTextureLayer(">>,
        <<"glBlitFramebuffer(">>,
        <<"glCopyBufferSubData(">>
    ].

s059_deferred_exports() ->
    [
        <<"-export([get_framebuffer_attachment_parameter/3]).">>
    ].

s059_deferred_c_calls() ->
    [].

s059_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s059_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s059_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s059_intptr_nif_data() ->
    {gl_type, {"GLintptr", "int", "enif_get_int", "enif_make_int"}}.

s059_sizeiptr_nif_data() ->
    {gl_type, {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"}}.

s059_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

s059_bitfield_nif_data() ->
    {gl_type, {"GLbitfield", "unsigned int", "enif_get_uint", "enif_make_uint"}}.
