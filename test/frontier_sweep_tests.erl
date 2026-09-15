-module(frontier_sweep_tests).

-include_lib("eunit/include/eunit.hrl").

frontier_generic_state_readback_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_generic_state_readback(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

frontier_unsafe_boundaries_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_unsafe_boundaries(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

frontier_gles32_direct_controls_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_gles32_direct_controls(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

frontier_depth_stencil_clear_wrappers_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_depth_stencil_clear_wrappers(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

frontier_design_boundary_commands_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_design_boundary_commands_absent(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

assert_generic_state_readback(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    [
        assert_presence(generic_state_reader_supported(Target, Function), Function, Functions)
     || Function <- generic_state_readers()
    ],
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- fixed_state_helpers()
    ],
    [
        assert_state_parameter(BindingData, Atom)
     || Atom <- required_state_parameters(Target)
    ],
    ok.

generic_state_readers() ->
    [
        {"get_boolean", 2},
        {"get_boolean", 3},
        {"get_float", 2},
        {"get_float", 3},
        {"get_double", 2},
        {"get_double", 3},
        {"get_integer", 2},
        {"get_integer", 3},
        {"get_integer64", 2},
        {"get_integer64", 3}
    ].

generic_state_reader_supported(_Target, {"get_boolean", 2}) -> true;
generic_state_reader_supported({gles, {2, 0}}, {"get_boolean", 3}) -> false;
generic_state_reader_supported({gles, {3, 0}}, {"get_boolean", 3}) -> false;
generic_state_reader_supported(_Target, {"get_boolean", 3}) -> true;
generic_state_reader_supported(_Target, {"get_float", 2}) -> true;
generic_state_reader_supported({gl, {4, Minor}}, {"get_float", 3}) when Minor >= 1 -> true;
generic_state_reader_supported({gl, {Major, _Minor}}, {"get_float", 3}) when Major > 4 -> true;
generic_state_reader_supported(_Target, {"get_float", 3}) -> false;
generic_state_reader_supported({gl, _Version}, {"get_double", 2}) -> true;
generic_state_reader_supported(_Target, {"get_double", 2}) -> false;
generic_state_reader_supported({gl, {4, Minor}}, {"get_double", 3}) when Minor >= 1 -> true;
generic_state_reader_supported({gl, {Major, _Minor}}, {"get_double", 3}) when Major > 4 -> true;
generic_state_reader_supported(_Target, {"get_double", 3}) -> false;
generic_state_reader_supported(_Target, {"get_integer", 2}) -> true;
generic_state_reader_supported({gles, {2, 0}}, {"get_integer", 3}) -> false;
generic_state_reader_supported(_Target, {"get_integer", 3}) -> true;
generic_state_reader_supported({gles, {2, 0}}, {"get_integer64", 2}) -> false;
generic_state_reader_supported(_Target, {"get_integer64", 2}) -> true;
generic_state_reader_supported({gles, {2, 0}}, {"get_integer64", 3}) -> false;
generic_state_reader_supported(_Target, {"get_integer64", 3}) -> true.

fixed_state_helpers() ->
    [
        {"get_integer", 1},
        {"get_integer64", 1},
        {"get_max_texture_size", 0},
        {"get_pack_alignment", 0},
        {"get_max_element_index", 0},
        {"get_shader_compiler_supported", 0},
        {"get_max_debug_message_length", 0},
        {"get_debug_next_logged_message_length", 0},
        {"get_max_combined_vertex_uniform_components", 0},
        {"get_max_compute_uniform_blocks", 0}
    ].

required_state_parameters(Target) ->
    Base = ["max_texture_size", "pack_alignment", "depth_writemask", "blend"],
    MaybeMaxElementIndex =
        case supports_max_element_index(Target) of
            true -> ["max_element_index" | Base];
            false -> Base
        end,
    case lists:member(Target, [{gl, {4, 6}}, {gles, {3, 2}}]) of
        true -> ["max_debug_message_length" | MaybeMaxElementIndex];
        false -> MaybeMaxElementIndex
    end.

supports_max_element_index({gl, {4, 6}}) -> true;
supports_max_element_index({gles, {3, _Minor}}) -> true;
supports_max_element_index(_) -> false.

assert_state_parameter(BindingData, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get("state_parameter_name", EnumTypes))).

assert_presence(true, Function, Functions) ->
    ?assert(maps:is_key(Function, Functions));
assert_presence(false, Function, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)).

assert_unsafe_boundaries(Target) ->
    Functions = generator_test_support:functions(Target),
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- unsafe_public_functions()
    ].

assert_design_boundary_commands_absent(Target) ->
    Functions = generator_test_support:functions(Target),
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- parked_design_boundary_commands()
    ].

assert_gles32_direct_controls(Target) ->
    Functions = generator_test_support:functions(Target),
    Expected = Target =:= {gles, {3, 2}},
    assert_presence(Expected, {"blend_barrier", 0}, Functions),
    assert_presence(Expected, {"primitive_bounding_box", 8}, Functions).

assert_depth_stencil_clear_wrappers(Target) ->
    Functions = generator_test_support:functions(Target),
    assert_presence(Target =/= {gles, {2, 0}}, {"clear_buffer_depth_stencil", 3}, Functions),
    assert_presence(Target =:= {gl, {4, 6}}, {"clear_named_framebuffer_depth_stencil", 4}, Functions),
    ?assertNot(maps:is_key({"clear_buffer", 5}, Functions)),
    ?assertNot(maps:is_key({"clear_named_framebuffer", 6}, Functions)).

unsafe_public_functions() ->
    [
        {"map_buffer", 2},
        {"map_buffer_range", 4},
        {"map_named_buffer", 2},
        {"map_named_buffer_range", 4},
        {"unmap_buffer", 1},
        {"unmap_named_buffer", 1},
        {"fence_sync", 2},
        {"client_wait_sync", 3},
        {"wait_sync", 3},
        {"delete_sync", 1},
        {"is_sync", 1},
        {"debug_message_callback", 1}
    ].

parked_design_boundary_commands() ->
    texture_upload_and_readback_commands()
        ++ element_and_indirect_draw_commands()
        ++ generic_query_and_readback_commands()
        ++ uniform_array_and_matrix_commands()
        ++ fixed_vector_input_commands()
        ++ compatibility_profile_packed_fixed_function_commands()
        ++ mapped_buffer_and_sync_commands()
        ++ debug_pointer_and_multibind_commands().

texture_upload_and_readback_commands() ->
    [].

element_and_indirect_draw_commands() ->
    [].

generic_query_and_readback_commands() ->
    [].

uniform_array_and_matrix_commands() ->
    [].

fixed_vector_input_commands() ->
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
        "glVertexAttrib4Nubv",
        "glVertexAttribP1uiv",
        "glVertexAttribP2uiv",
        "glVertexAttribP3uiv",
        "glVertexAttribP4uiv"
    ].

compatibility_profile_packed_fixed_function_commands() ->
    [
        "glVertexP2ui",
        "glVertexP2uiv",
        "glVertexP3ui",
        "glVertexP3uiv",
        "glVertexP4ui",
        "glVertexP4uiv",
        "glTexCoordP1ui",
        "glTexCoordP1uiv",
        "glTexCoordP2ui",
        "glTexCoordP2uiv",
        "glTexCoordP3ui",
        "glTexCoordP3uiv",
        "glTexCoordP4ui",
        "glTexCoordP4uiv",
        "glMultiTexCoordP1ui",
        "glMultiTexCoordP1uiv",
        "glMultiTexCoordP2ui",
        "glMultiTexCoordP2uiv",
        "glMultiTexCoordP3ui",
        "glMultiTexCoordP3uiv",
        "glMultiTexCoordP4ui",
        "glMultiTexCoordP4uiv",
        "glNormalP3ui",
        "glNormalP3uiv",
        "glColorP3ui",
        "glColorP3uiv",
        "glColorP4ui",
        "glColorP4uiv",
        "glSecondaryColorP3ui",
        "glSecondaryColorP3uiv"
    ].

mapped_buffer_and_sync_commands() ->
    [
        "glMapBuffer",
        "glMapBufferRange",
        "glFlushMappedBufferRange",
        "glUnmapBuffer",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glFlushMappedNamedBufferRange",
        "glUnmapNamedBuffer",
        "glGetNamedBufferPointerv",
        "glFenceSync",
        "glIsSync",
        "glDeleteSync",
        "glClientWaitSync",
        "glWaitSync",
        "glGetSynciv"
    ].

debug_pointer_and_multibind_commands() ->
    [
        "glDebugMessageCallback",
        "glObjectPtrLabel",
        "glGetObjectPtrLabel",
        "glGetPointerv"
    ].
