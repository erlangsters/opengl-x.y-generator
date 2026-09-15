-module(direction_checkpoint_tests).

-include_lib("eunit/include/eunit.hrl").

aggregate_uniform_direction_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_uniform_direction(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

aggregate_program_uniform_direction_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_program_uniform_direction(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

indexed_vector_state_direction_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> assert_indexed_vector_state_direction(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

public_api_policy_examples_test() ->
    Functions = generator_test_support:functions({gl, {4, 6}}),

    ?assert(maps:is_key({"delete_textures", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_textures", 2}, Functions)),
    ?assert(maps:is_key({"delete_buffers", 1}, Functions)),
    ?assertNot(maps:is_key({"delete_buffers", 2}, Functions)),
    ?assert(maps:is_key({"buffer_data", 3}, Functions)),
    ?assertNot(maps:is_key({"buffer_data", 4}, Functions)),
    ?assert(maps:is_key({"buffer_sub_data", 3}, Functions)),
    ?assertNot(maps:is_key({"buffer_sub_data", 4}, Functions)),

    assert_none_unbind(maps:get({"bind_texture", 2}, Functions), texture),
    assert_none_unbind(maps:get({"bind_buffer", 2}, Functions), buffer),
    assert_location_query(maps:get({"get_attrib_location", 2}, Functions)),
    assert_location_query(maps:get({"get_uniform_location", 2}, Functions)).

assert_uniform_direction(Target) ->
    Functions = generator_test_support:functions(Target),
    ?assert(maps:is_key({"uniform", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_1f", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_1i", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_2f", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_2i", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_3f", 4}, Functions)),
    ?assertNot(maps:is_key({"uniform_3i", 4}, Functions)),
    ?assertNot(maps:is_key({"uniform_4f", 5}, Functions)),
    ?assertNot(maps:is_key({"uniform_4i", 5}, Functions)),
    ?assertNot(maps:is_key({"uniform_1ui", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_2ui", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_3ui", 4}, Functions)),
    ?assertNot(maps:is_key({"uniform_4ui", 5}, Functions)),
    ?assertNot(maps:is_key({"uniform_1fv", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_1iv", 3}, Functions)),
    assert_matrix_uniform_direction(Target, Functions),
    assert_scalar_uniform(maps:get({"uniform", 3}, Functions)).

assert_matrix_uniform_direction(Target, Functions) ->
    ?assert(maps:is_key({"uniform_matrix", 3}, Functions)),
    ?assertNot(maps:is_key({"uniform_matrix_2x3fv", 2}, Functions)),
    ?assertNot(maps:is_key({"uniform_matrix_2dv", 2}, Functions)),
    case supports_non_square_uniform_matrix(Target) of
        true ->
            ?assert(generator_test_support:has_gl_command("glUniformMatrix2x3fv", Functions)),
            ?assert(generator_test_support:has_gl_command("glUniformMatrix4x3fv", Functions));
        false ->
            ?assertNot(generator_test_support:has_gl_command("glUniformMatrix2x3fv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glUniformMatrix4x3fv", Functions))
    end,
    case supports_double_uniform_matrix(Target) of
        true ->
            ?assert(generator_test_support:has_gl_command("glUniformMatrix2dv", Functions)),
            ?assert(generator_test_support:has_gl_command("glUniformMatrix4x3dv", Functions));
        false ->
            ?assertNot(generator_test_support:has_gl_command("glUniformMatrix2dv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glUniformMatrix4x3dv", Functions))
    end.

supports_non_square_uniform_matrix({gles, {2, 0}}) -> false;
supports_non_square_uniform_matrix(_) -> true.

supports_double_uniform_matrix({gl, {4, _}}) -> true;
supports_double_uniform_matrix(_) -> false.

assert_program_uniform_direction(Target) ->
    Functions = generator_test_support:functions(Target),
    case supports_program_uniform(Target) of
        true ->
            ?assert(maps:is_key({"program_uniform", 4}, Functions)),
            ?assertNot(maps:is_key({"program_uniform_1fv", 4}, Functions)),
            ?assertNot(maps:is_key({"program_uniform_4iv", 4}, Functions)),
            ?assertNot(maps:is_key({"program_uniform_1uiv", 4}, Functions)),
            ?assertNot(maps:is_key({"program_uniform_4dv", 4}, Functions)),
            assert_program_uniform_matrix_direction(Target, Functions),
            assert_scalar_program_uniform(maps:get({"program_uniform", 4}, Functions));
        false ->
            ?assertNot(maps:is_key({"program_uniform", 4}, Functions)),
            ?assertNot(maps:is_key({"program_uniform_matrix", 4}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glProgramUniform1fv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glProgramUniformMatrix2fv", Functions))
    end.

supports_program_uniform({gl, {4, 1}}) -> true;
supports_program_uniform({gl, {4, 6}}) -> true;
supports_program_uniform({gles, {3, 1}}) -> true;
supports_program_uniform({gles, {3, 2}}) -> true;
supports_program_uniform(_) -> false.

assert_program_uniform_matrix_direction(Target, Functions) ->
    ?assert(maps:is_key({"program_uniform_matrix", 4}, Functions)),
    ?assertNot(maps:is_key({"program_uniform_matrix_2fv", 5}, Functions)),
    ?assertNot(maps:is_key({"program_uniform_matrix_4x3dv", 5}, Functions)),
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, program_uniform_matrix_value, []}}
    ] = maps:get(specs_params, maps:get({"program_uniform_matrix", 4}, Functions)),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(generator_test_support:has_gl_command("glProgramUniformMatrix2fv", Functions)),
    ?assert(generator_test_support:has_gl_command("glProgramUniformMatrix4x3fv", Functions)),
    case supports_double_program_uniform_matrix(Target) of
        true ->
            ?assert(lists:member(d, TypeAtoms)),
            ?assert(generator_test_support:has_gl_command("glProgramUniformMatrix2dv", Functions)),
            ?assert(generator_test_support:has_gl_command("glProgramUniformMatrix4x3dv", Functions));
        false ->
            ?assertNot(lists:member(d, TypeAtoms)),
            ?assertNot(generator_test_support:has_gl_command("glProgramUniformMatrix2dv", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glProgramUniformMatrix4x3dv", Functions))
    end.

supports_double_program_uniform_matrix({gl, {4, _}}) -> true;
supports_double_program_uniform_matrix(_) -> false.

assert_scalar_uniform(FunctionData) ->
    [{"Type", {set, TypeAtoms}}, {"Location", {gl, int, []}}, {"Value", {undefined, uniform_value, []}}] =
        maps:get(specs_params, FunctionData),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    GlCommands = maps:get(gl_commands, FunctionData),
    ?assert(lists:member({"glUniform1i", gl_int, element}, GlCommands)),
    ?assert(lists:member({"glUniform1f", gl_float, element}, GlCommands)).

assert_scalar_program_uniform(FunctionData) ->
    [
        {"Type", {set, TypeAtoms}},
        {"Program", {undefined, program, []}},
        {"Location", {gl, int, []}},
        {"Value", {undefined, program_uniform_value, []}}
    ] = maps:get(specs_params, FunctionData),
    ?assert(lists:member(f, TypeAtoms)),
    ?assert(lists:member(i, TypeAtoms)),
    ?assert(lists:member(ui, TypeAtoms)),
    GlCommands = maps:get(gl_commands, FunctionData),
    ?assert(lists:member({"glProgramUniform1fv", gl_float, counted_array}, GlCommands)),
    ?assert(lists:member({"glProgramUniform1iv", gl_int, counted_array}, GlCommands)).

assert_none_unbind(FunctionData, ObjectType) ->
    ?assert(lists:member(
        {"Target", {undefined, maps:get("Target", param_type_names(FunctionData)), []}},
        maps:get(specs_params, FunctionData)
    )),
    ?assert(lists:member(
        {"Texture", {set, [{undefined, ObjectType, []}, none]}},
        maps:get(specs_params, FunctionData)
    )
        orelse lists:member(
            {"Buffer", {set, [{undefined, ObjectType, []}, none]}},
            maps:get(specs_params, FunctionData)
        )),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    ?assert(lists:member({"Texture", {gl_object_to_uint, [{none, 0}]}}, Params)
        orelse lists:member({"Buffer", {gl_object_to_uint, [{none, 0}]}}, Params)).

param_type_names(FunctionData) ->
    maps:from_list([
        {ParamName, TypeName}
     || {ParamName, {undefined, TypeName, []}} <- maps:get(specs_params, FunctionData)
    ]).

assert_location_query(FunctionData) ->
    ?assertEqual([{"Location", {gl, int, []}}], maps:get(specs_return, FunctionData)).

assert_indexed_vector_state_direction(Target) ->
    Functions = generator_test_support:functions(Target),
    Expected = lists:member(Target, [{gl, {4, 1}}, {gl, {4, 6}}]),
    [
        assert_indexed_vector_presence(Expected, Function, Functions)
     || Function <- [
            {"viewport", 2},
            {"viewport_array", 2},
            {"scissor_indexed", 2},
            {"scissor_array", 2},
            {"depth_range_array", 2}
        ]
    ],
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

assert_indexed_vector_presence(true, Function, Functions) ->
    ?assert(maps:is_key(Function, Functions));
assert_indexed_vector_presence(false, Function, Functions) ->
    ?assertNot(maps:is_key(Function, Functions)).
