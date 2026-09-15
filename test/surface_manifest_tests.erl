-module(surface_manifest_tests).
-include_lib("eunit/include/eunit.hrl").

%% Surface inventory and manifest guardrails.

%% Historical shard 171.
s171_duplicate_not_active_specs_test() ->
    Active = generator_test_support:active_spec_commands(),
    Commands = s171_duplicate_vertex_attrib_pointer_commands(),
    ?assertEqual(28, length(Commands)),
    [
        ?assertNot(lists:member(Command, Active))
     || Command <- Commands
    ].

s171_duplicate_vertex_attrib_surface_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s171_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s171_emitted_target_absence_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s171_assert_emitted_surface(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s171_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    s171_assert_active_aggregate_wrappers(Target, Functions),
    s171_assert_commands_absent(Functions, s171_duplicate_vertex_attrib_pointer_commands()),
    s171_assert_direct_wrappers_absent(Functions).

s171_assert_active_aggregate_wrappers(Target, Functions) ->
    ?assert(maps:is_key({"vertex_attrib", 3}, Functions)),
    case s171_supports_integer_vertex_attrib(Target) of
        true -> ?assert(maps:is_key({"vertex_attrib_i", 3}, Functions));
        false -> ?assertNot(maps:is_key({"vertex_attrib_i", 3}, Functions))
    end,
    case s171_supports_vertex_attrib_l(Target) of
        true -> ?assert(maps:is_key({"vertex_attrib_l", 3}, Functions));
        false -> ?assertNot(maps:is_key({"vertex_attrib_l", 3}, Functions))
    end,
    case s171_is_desktop(Target) of
        true -> ?assert(maps:is_key({"vertex_attrib_p", 5}, Functions));
        false -> ?assertNot(maps:is_key({"vertex_attrib_p", 5}, Functions))
    end.

s171_assert_emitted_surface(Target) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard171-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
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
        s171_assert_not_contains_any(Erl, s171_duplicate_raw_needles()),
        s171_assert_not_contains_any(C, s171_duplicate_call_needles())
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s171_supports_integer_vertex_attrib({gles, {2, 0}}) ->
    false;
s171_supports_integer_vertex_attrib(_) ->
    true.

s171_supports_vertex_attrib_l({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s171_supports_vertex_attrib_l({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s171_supports_vertex_attrib_l(_) ->
    false.

s171_is_desktop({gl, _Version}) ->
    true;
s171_is_desktop(_) ->
    false.

s171_assert_direct_wrappers_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- s171_duplicate_public_wrappers()
    ].

s171_assert_commands_absent(Functions, Commands) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- Commands
    ].

s171_duplicate_vertex_attrib_pointer_commands() ->
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
        "glVertexAttribP1uiv",
        "glVertexAttribP2uiv",
        "glVertexAttribP3uiv",
        "glVertexAttribP4uiv"
    ].

s171_duplicate_public_wrappers() ->
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
        {"vertex_attrib_p1uiv", 4},
        {"vertex_attrib_p2uiv", 4},
        {"vertex_attrib_p3uiv", 4},
        {"vertex_attrib_p4uiv", 4}
    ].

s171_duplicate_raw_needles() ->
    [list_to_binary(Command ++ "_raw") || Command <- s171_duplicate_vertex_attrib_pointer_commands()].

s171_duplicate_call_needles() ->
    [list_to_binary(Command ++ "(") || Command <- s171_duplicate_vertex_attrib_pointer_commands()].

s171_assert_not_contains_any(Haystack, Needles) ->
    [
        ?assertEqual(nomatch, binary:match(Haystack, Needle))
     || Needle <- Needles
    ].

%% Historical shard 180.
s180_active_depth_stencil_clear_surface_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s180_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s180_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    s180_assert_presence(
        s180_supports_clear_buffer_depth_stencil(Target),
        {"clear_buffer_depth_stencil", 3},
        "glClearBufferfi",
        Functions
    ),
    s180_assert_presence(
        s180_supports_named_clear_buffer_depth_stencil(Target),
        {"clear_named_framebuffer_depth_stencil", 4},
        "glClearNamedFramebufferfi",
        Functions
    ).

s180_supports_clear_buffer_depth_stencil({gles, {2, 0}}) ->
    false;
s180_supports_clear_buffer_depth_stencil(_Target) ->
    true.

s180_supports_named_clear_buffer_depth_stencil({gl, {4, 6}}) ->
    true;
s180_supports_named_clear_buffer_depth_stencil(_Target) ->
    false.

s180_assert_presence(true, Function, Command, Functions) ->
    ?assert(maps:is_key(Function, Functions)),
    FunctionData = maps:get(Function, Functions),
    ?assertEqual(Command, maps:get(gl_command, FunctionData));
s180_assert_presence(false, Function, _Command, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)).
