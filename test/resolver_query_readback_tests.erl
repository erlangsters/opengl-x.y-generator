-module(resolver_query_readback_tests).
-include_lib("eunit/include/eunit.hrl").

%% Query, readback, parameter, and reflection readback contracts.

%% Historical shard 77.
s077_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s077_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s077_emitter_indexed_query_test_() ->
    [
        {"gl 4.6", fun() ->
            s077_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([begin_query/2]).">>,
                    <<"-export([begin_query/3]).">>,
                    <<"-export([end_query/1]).">>,
                    <<"-export([end_query/2]).">>,
                    <<"-spec begin_query(\n    Target :: query_target(),\n    Index :: gl:uint(),">>,
                    <<"-spec end_query(\n    Target :: query_target(),\n    Index :: gl:uint()">>,
                    <<"ok = gl:begin_query(primitives_generated, 0, Query).">>,
                    <<"ok = gl:end_query(primitives_generated, 0).">>,
                    <<"?CALL_RAW_FUNC(glBeginQueryIndexed_raw(NewTarget, Index, Query))">>,
                    <<"?CALL_RAW_FUNC(glEndQueryIndexed_raw(NewTarget, Index))">>
                ],
                [
                    <<"glBeginQueryIndexed(arg_0, arg_1, arg_2);">>,
                    <<"glEndQueryIndexed(arg_0, arg_1);">>,
                    <<"{\"glBeginQueryIndexed_raw\", 3, nif_glBeginQueryIndexed, 0}">>,
                    <<"{\"glEndQueryIndexed_raw\", 2, nif_glEndQueryIndexed, 0}">>
                ],
                [
                    <<"-export([begin_query_indexed/3]).">>,
                    <<"-export([end_query_indexed/2]).">>
                ]
            )
        end},
        {"gl 3.3", fun() ->
            s077_assert_emitted_surface(
                {gl, {3, 3}},
                [],
                [],
                [
                    <<"-export([begin_query/3]).">>,
                    <<"-export([end_query/2]).">>,
                    <<"glBeginQueryIndexed_raw">>,
                    <<"glEndQueryIndexed_raw">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s077_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                [
                    <<"-export([begin_query/3]).">>,
                    <<"-export([end_query/2]).">>,
                    <<"glBeginQueryIndexed_raw">>,
                    <<"glEndQueryIndexed_raw">>
                ]
            )
        end}
    ].

s077_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s077_supports_indexed_queries(Target) of
        true ->
            ?assert(maps:is_key({"begin_query", 2}, Functions)),
            ?assert(maps:is_key({"begin_query", 3}, Functions)),
            ?assert(maps:is_key({"end_query", 1}, Functions)),
            ?assert(maps:is_key({"end_query", 2}, Functions)),
            ?assertNot(maps:is_key({"begin_query_indexed", 3}, Functions)),
            ?assertNot(maps:is_key({"end_query_indexed", 2}, Functions)),
            s077_assert_indexed_begin_query(BindingData, maps:get({"begin_query", 3}, Functions)),
            s077_assert_indexed_end_query(BindingData, maps:get({"end_query", 2}, Functions));
        false ->
            ?assertNot(maps:is_key({"begin_query", 3}, Functions)),
            ?assertNot(maps:is_key({"end_query", 2}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glBeginQueryIndexed", Functions)),
            ?assertNot(generator_test_support:has_gl_command("glEndQueryIndexed", Functions))
    end.

s077_supports_indexed_queries({gl, {4, Minor}}) when Minor >= 1 ->
    true;
s077_supports_indexed_queries({gl, {Major, _Minor}}) when Major > 4 ->
    true;
s077_supports_indexed_queries(_) ->
    false.

s077_assert_indexed_begin_query(BindingData, FunctionData) ->
    s077_assert_enum_contains(BindingData, "query_target", "primitives_generated"),
    ?assertEqual("glBeginQueryIndexed", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "QueryTarget"}},
            {in, "Index", gl_uint},
            {in, "Query", {gl_object, query}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, query_target, []}},
            {"Index", {gl, uint, []}},
            {"Query", {undefined, query, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s077_assert_enum_param("Target", "primitives_generated", "GL_PRIMITIVES_GENERATED", Params),
    ?assert(lists:member({"Index", do_nothing}, Params)),
    ?assert(lists:member({"Query", do_nothing}, Params)),
    s077_assert_nif_params(FunctionData, "glBeginQueryIndexed", [s077_enum_nif_data(), s077_uint_nif_data(), s077_uint_nif_data()]).

s077_assert_indexed_end_query(BindingData, FunctionData) ->
    s077_assert_enum_contains(BindingData, "query_target", "primitives_generated"),
    ?assertEqual("glEndQueryIndexed", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "QueryTarget"}},
            {in, "Index", gl_uint}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, query_target, []}},
            {"Index", {gl, uint, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    Params = maps:get(params, Clause),
    s077_assert_enum_param("Target", "primitives_generated", "GL_PRIMITIVES_GENERATED", Params),
    ?assert(lists:member({"Index", do_nothing}, Params)),
    s077_assert_nif_params(FunctionData, "glEndQueryIndexed", [s077_enum_nif_data(), s077_uint_nif_data()]).

s077_assert_nif_params(FunctionData, GlCommand, ExpectedParams) ->
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(ExpectedParams), maps:get(arity, NifData)),
    ?assertEqual(ExpectedParams, [Param || {_Name, Param} <- maps:get(params, NifData)]),
    ?assertEqual(void, maps:get(return, NifData)).

s077_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).

s077_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s077_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard77-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s077_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s077_assert_contains(C, Needle) || Needle <- RequiredC],
        [s077_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s077_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s077_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s077_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s077_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s077_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 78.
s078_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s078_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s078_emitter_query_counter_test_() ->
    [
        {"gl 4.6", fun() ->
            s078_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([query_counter/2]).">>,
                    <<"-export_type([query_counter_target/0]).">>,
                    <<"-type query_counter_target() ::\n    timestamp">>,
                    <<"-spec query_counter(\n    Query :: query(),\n    Target :: query_counter_target()">>,
                    <<"ok = gl:query_counter(Query, timestamp).">>,
                    <<"?CALL_RAW_FUNC(glQueryCounter_raw(Query, NewTarget))">>
                ],
                [
                    <<"glQueryCounter(arg_0, arg_1);">>,
                    <<"{\"glQueryCounter_raw\", 2, nif_glQueryCounter, 0}">>
                ],
                []
            )
        end},
        {"gles 3.2", fun() ->
            s078_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                [
                    <<"-export([query_counter/2]).">>,
                    <<"-export_type([query_counter_target/0]).">>,
                    <<"glQueryCounter_raw">>
                ]
            )
        end}
    ].

s078_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s078_supports_query_counter(Target) of
        true ->
            ?assert(maps:is_key({"query_counter", 2}, Functions)),
            s078_assert_query_counter_target_enum(BindingData),
            s078_assert_query_counter(maps:get({"query_counter", 2}, Functions));
        false ->
            ?assertNot(maps:is_key({"query_counter", 2}, Functions)),
            ?assertNot(generator_test_support:has_gl_command("glQueryCounter", Functions))
    end.

s078_supports_query_counter({gl, _Version}) ->
    true;
s078_supports_query_counter(_) ->
    false.

s078_assert_query_counter_target_enum(BindingData) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key("query_counter_target", EnumTypes)),
    QueryCounterTargets = maps:get("query_counter_target", EnumTypes),
    ?assertEqual(["timestamp"], QueryCounterTargets),
    ?assertNot(lists:member("time_elapsed", QueryCounterTargets)).

s078_assert_query_counter(FunctionData) ->
    ?assertEqual("glQueryCounter", maps:get(gl_command, FunctionData)),
    ?assertEqual(
        [
            {in, "Query", {gl_object, query}},
            {in, "Target", {gl_enum, "QueryCounterTarget"}}
        ],
        maps:get(params_specs, FunctionData)
    ),
    ?assertEqual(
        [
            {"Query", {undefined, query, []}},
            {"Target", {undefined, query_counter_target, []}}
        ],
        maps:get(specs_params, FunctionData)
    ),
    [Clause] = maps:get(function_clauses, FunctionData),
    ?assertEqual("glQueryCounter", maps:get(raw_function, Clause)),
    Params = maps:get(params, Clause),
    ?assert(lists:member({"Query", do_nothing}, Params)),
    s078_assert_enum_param("Target", "timestamp", "GL_TIMESTAMP", Params),
    s078_assert_nif_params(FunctionData).

s078_assert_nif_params(FunctionData) ->
    NifData = maps:get("glQueryCounter", maps:get(nif_functions, FunctionData)),
    ?assertEqual(2, maps:get(arity, NifData)),
    ?assertEqual(
        [s078_uint_nif_data(), s078_enum_nif_data()],
        [Param || {_Name, Param} <- maps:get(params, NifData)]
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s078_assert_enum_param(ParamName, Atom, Constant, Params) ->
    {ParamName, {gl_enum_to_uint, TransformMap}} = lists:keyfind(ParamName, 1, Params),
    ?assert(lists:member({Atom, Constant}, TransformMap)).

s078_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard78-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s078_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s078_assert_contains(C, Needle) || Needle <- RequiredC],
        [s078_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s078_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s078_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s078_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s078_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s078_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 86.
s086_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s086_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s086_emitter_transform_feedback_operation_surface_test_() ->
    [
        {"gl 4.6", fun() ->
            s086_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([begin_transform_feedback/1]).">>,
                    <<"-export([end_transform_feedback/0]).">>,
                    <<"-export([pause_transform_feedback/0]).">>,
                    <<"-export([resume_transform_feedback/0]).">>,
                    <<"-spec begin_transform_feedback(PrimitiveMode :: primitive_type()) -> ok | {error, atom()}.">>,
                    <<"glBeginTransformFeedback_raw(NewPrimitiveMode)">>,
                    <<"glPauseTransformFeedback_raw()">>
                ],
                [
                    <<"glBeginTransformFeedback(arg_0);">>,
                    <<"glEndTransformFeedback();">>,
                    <<"glPauseTransformFeedback();">>,
                    <<"glResumeTransformFeedback();">>
                ],
                []
            )
        end},
        {"gl 3.3", fun() ->
            s086_assert_emitted_surface(
                {gl, {3, 3}},
                [
                    <<"-export([begin_transform_feedback/1]).">>,
                    <<"-export([end_transform_feedback/0]).">>,
                    <<"glBeginTransformFeedback_raw(NewPrimitiveMode)">>
                ],
                [
                    <<"glBeginTransformFeedback(arg_0);">>,
                    <<"glEndTransformFeedback();">>
                ],
                [
                    <<"-export([pause_transform_feedback/0]).">>,
                    <<"-export([resume_transform_feedback/0]).">>,
                    <<"glPauseTransformFeedback_raw">>,
                    <<"glResumeTransformFeedback_raw">>
                ]
            )
        end},
        {"gles 3.2", fun() ->
            s086_assert_emitted_surface(
                {gles, {3, 2}},
                [
                    <<"-export([begin_transform_feedback/1]).">>,
                    <<"-export([end_transform_feedback/0]).">>,
                    <<"-export([pause_transform_feedback/0]).">>,
                    <<"-export([resume_transform_feedback/0]).">>
                ],
                [
                    <<"glBeginTransformFeedback(arg_0);">>,
                    <<"glEndTransformFeedback();">>,
                    <<"glPauseTransformFeedback();">>,
                    <<"glResumeTransformFeedback();">>
                ],
                []
            )
        end}
    ].

s086_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),

    s086_assert_presence(Target, Functions),
    s086_assert_deferred_neighbors_absent(Functions),
    [s086_assert_path(Function, BindingData, maps:get(Function, Functions)) || Function <- s086_present_functions(Target)].

s086_assert_presence(Target, Functions) ->
    Present = s086_present_functions(Target),
    Absent = s086_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s086_present_functions({gl, {3, 3}}) ->
    s086_begin_end();
s086_present_functions({gl, {4, 1}}) ->
    s086_all_functions();
s086_present_functions({gl, {4, 6}}) ->
    s086_all_functions();
s086_present_functions({gles, {2, 0}}) ->
    [];
s086_present_functions({gles, _}) ->
    s086_all_functions().

s086_all_functions() ->
    s086_begin_end() ++ s086_pause_resume().

s086_begin_end() ->
    [
        {"begin_transform_feedback", 1},
        {"end_transform_feedback", 0}
    ].

s086_pause_resume() ->
    [
        {"pause_transform_feedback", 0},
        {"resume_transform_feedback", 0}
    ].

s086_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s086_assert_path({"begin_transform_feedback", 1}, BindingData, FunctionData) ->
    s086_assert_enum_contains(BindingData, "primitive_type", "points"),
    s086_assert_direct(
        FunctionData,
        "glBeginTransformFeedback",
        [{in, "PrimitiveMode", {gl_enum, "PrimitiveType"}}],
        [{"PrimitiveMode", {undefined, primitive_type, []}}],
        [{"PrimitiveMode", {gl_enum_to_uint, ["points"]}}],
        [{"PrimitiveMode", s086_enum_nif_data()}]
    );
s086_assert_path({"end_transform_feedback", 0}, _BindingData, FunctionData) ->
    s086_assert_zero_arg_direct(FunctionData, "glEndTransformFeedback");
s086_assert_path({"pause_transform_feedback", 0}, _BindingData, FunctionData) ->
    s086_assert_zero_arg_direct(FunctionData, "glPauseTransformFeedback");
s086_assert_path({"resume_transform_feedback", 0}, _BindingData, FunctionData) ->
    s086_assert_zero_arg_direct(FunctionData, "glResumeTransformFeedback").

s086_assert_zero_arg_direct(FunctionData, GlCommand) ->
    s086_assert_direct(FunctionData, GlCommand, [], [], [], []).

s086_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s086_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s086_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s086_assert_clause_param/1, lists:zip(Expected, Actual)).

s086_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s086_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s086_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s086_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard86-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s086_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s086_assert_contains(C, Needle) || Needle <- RequiredC],
        [s086_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s086_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s086_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s086_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s086_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

%% Historical shard 95.
s095_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s095_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s095_emitter_non_indexed_draw_followups_test_() ->
    [
        {"gl 4.6", fun() ->
            s095_assert_emitted_surface(
                {gl, {4, 6}},
                [
                    <<"-export([draw_arrays_instanced_base_instance/5]).">>,
                    <<"-export([draw_transform_feedback/2]).">>,
                    <<"-export([draw_transform_feedback_stream/3]).">>,
                    <<"-export([draw_transform_feedback_instanced/3]).">>,
                    <<"-export([draw_transform_feedback_stream_instanced/4]).">>,
                    <<"draw_arrays_instanced_base_instance(Mode, First, Count, InstanceCount, BaseInstance) ->">>,
                    <<"draw_transform_feedback(Mode, Feedback) ->">>,
                    <<"draw_transform_feedback_stream_instanced(Mode, Feedback, Stream, InstanceCount) ->">>,
                    <<"glDrawArraysInstancedBaseInstance_raw(NewMode, First, Count, InstanceCount, BaseInstance)">>,
                    <<"glDrawTransformFeedback_raw(NewMode, Feedback)">>,
                    <<"glDrawTransformFeedbackStreamInstanced_raw(NewMode, Feedback, Stream, InstanceCount)">>
                ],
                [
                    <<"glDrawArraysInstancedBaseInstance(arg_0, arg_1, arg_2, arg_3, arg_4);">>,
                    <<"glDrawTransformFeedback(arg_0, arg_1);">>,
                    <<"glDrawTransformFeedbackStream(arg_0, arg_1, arg_2);">>,
                    <<"glDrawTransformFeedbackInstanced(arg_0, arg_1, arg_2);">>,
                    <<"glDrawTransformFeedbackStreamInstanced(arg_0, arg_1, arg_2, arg_3);">>
                ],
                s095_forbidden_needles()
            )
        end},
        {"gl 4.1", fun() ->
            s095_assert_emitted_surface(
                {gl, {4, 1}},
                [
                    <<"-export([draw_transform_feedback/2]).">>,
                    <<"-export([draw_transform_feedback_stream/3]).">>,
                    <<"glDrawTransformFeedback_raw(NewMode, Feedback)">>,
                    <<"glDrawTransformFeedbackStream_raw(NewMode, Feedback, Stream)">>
                ],
                [
                    <<"glDrawTransformFeedback(arg_0, arg_1);">>,
                    <<"glDrawTransformFeedbackStream(arg_0, arg_1, arg_2);">>
                ],
                [
                    <<"-export([draw_arrays_instanced_base_instance/5]).">>,
                    <<"-export([draw_transform_feedback_instanced/3]).">>,
                    <<"-export([draw_transform_feedback_stream_instanced/4]).">>,
                    <<"glDrawArraysInstancedBaseInstance">>,
                    <<"glDrawTransformFeedbackInstanced">>,
                    <<"glDrawTransformFeedbackStreamInstanced">>
                ] ++ s095_forbidden_needles()
            )
        end},
        {"gles 3.2", fun() ->
            s095_assert_emitted_surface(
                {gles, {3, 2}},
                [],
                [],
                [
                    <<"-export([draw_arrays_instanced_base_instance/5]).">>,
                    <<"-export([draw_transform_feedback/2]).">>,
                    <<"-export([draw_transform_feedback_stream/3]).">>,
                    <<"-export([draw_transform_feedback_instanced/3]).">>,
                    <<"-export([draw_transform_feedback_stream_instanced/4]).">>,
                    <<"glDrawArraysInstancedBaseInstance">>,
                    <<"glDrawTransformFeedback">>
                ] ++ s095_forbidden_needles()
            )
        end}
    ].

s095_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    s095_assert_presence(Target, Functions),
    s095_assert_deferred_neighbors_absent(Functions),
    [s095_assert_path(Function, BindingData, maps:get(Function, Functions))
     || Function <- s095_present_functions(Target)].

s095_assert_presence(Target, Functions) ->
    Present = s095_present_functions(Target),
    Absent = s095_all_functions() -- Present,
    [?assert(maps:is_key(Function, Functions)) || Function <- Present],
    [?assertNot(maps:is_key(Function, Functions)) || Function <- Absent].

s095_present_functions({gl, {4, 1}}) ->
    s095_transform_feedback_draws();
s095_present_functions({gl, {4, 6}}) ->
    s095_all_functions();
s095_present_functions(_) ->
    [].

s095_all_functions() ->
    [{"draw_arrays_instanced_base_instance", 5}] ++
        s095_transform_feedback_draws() ++
        s095_transform_feedback_instanced_draws().

s095_transform_feedback_draws() ->
    [
        {"draw_transform_feedback", 2},
        {"draw_transform_feedback_stream", 3}
    ].

s095_transform_feedback_instanced_draws() ->
    [
        {"draw_transform_feedback_instanced", 3},
        {"draw_transform_feedback_stream_instanced", 4}
    ].

s095_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s095_assert_path({"draw_arrays_instanced_base_instance", 5}, BindingData, FunctionData) ->
    s095_assert_enum_contains(BindingData, "primitive_type", "triangles"),
    s095_assert_direct(
        FunctionData,
        "glDrawArraysInstancedBaseInstance",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "First", gl_int},
            {in, "Count", gl_sizei},
            {in, "InstanceCount", gl_sizei},
            {in, "BaseInstance", gl_uint}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"First", {gl, int, []}},
            {"Count", {gl, sizei, []}},
            {"InstanceCount", {gl, sizei, []}},
            {"BaseInstance", {gl, uint, []}}
        ],
        s095_enum_plus_plain_clause_params(["triangles"], ["First", "Count", "InstanceCount", "BaseInstance"]),
        [
            {"Mode", s095_enum_nif_data()},
            {"First", s095_int_nif_data()},
            {"Count", s095_sizei_nif_data()},
            {"InstanceCount", s095_sizei_nif_data()},
            {"BaseInstance", s095_uint_nif_data()}
        ]
    );
s095_assert_path({"draw_transform_feedback", 2}, BindingData, FunctionData) ->
    s095_assert_enum_contains(BindingData, "primitive_type", "points"),
    s095_assert_direct(
        FunctionData,
        "glDrawTransformFeedback",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Feedback", {gl_object, transform_feedback}}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Feedback", {undefined, transform_feedback, []}}
        ],
        s095_enum_plus_plain_clause_params(["points"], ["Feedback"]),
        [{"Mode", s095_enum_nif_data()}, {"Feedback", s095_uint_nif_data()}]
    );
s095_assert_path({"draw_transform_feedback_stream", 3}, BindingData, FunctionData) ->
    s095_assert_enum_contains(BindingData, "primitive_type", "points"),
    s095_assert_direct(
        FunctionData,
        "glDrawTransformFeedbackStream",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "Stream", gl_uint}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Feedback", {undefined, transform_feedback, []}},
            {"Stream", {gl, uint, []}}
        ],
        s095_enum_plus_plain_clause_params(["points"], ["Feedback", "Stream"]),
        [
            {"Mode", s095_enum_nif_data()},
            {"Feedback", s095_uint_nif_data()},
            {"Stream", s095_uint_nif_data()}
        ]
    );
s095_assert_path({"draw_transform_feedback_instanced", 3}, BindingData, FunctionData) ->
    s095_assert_enum_contains(BindingData, "primitive_type", "points"),
    s095_assert_direct(
        FunctionData,
        "glDrawTransformFeedbackInstanced",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "InstanceCount", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Feedback", {undefined, transform_feedback, []}},
            {"InstanceCount", {gl, sizei, []}}
        ],
        s095_enum_plus_plain_clause_params(["points"], ["Feedback", "InstanceCount"]),
        [
            {"Mode", s095_enum_nif_data()},
            {"Feedback", s095_uint_nif_data()},
            {"InstanceCount", s095_sizei_nif_data()}
        ]
    );
s095_assert_path({"draw_transform_feedback_stream_instanced", 4}, BindingData, FunctionData) ->
    s095_assert_enum_contains(BindingData, "primitive_type", "points"),
    s095_assert_direct(
        FunctionData,
        "glDrawTransformFeedbackStreamInstanced",
        [
            {in, "Mode", {gl_enum, "PrimitiveType"}},
            {in, "Feedback", {gl_object, transform_feedback}},
            {in, "Stream", gl_uint},
            {in, "InstanceCount", gl_sizei}
        ],
        [
            {"Mode", {undefined, primitive_type, []}},
            {"Feedback", {undefined, transform_feedback, []}},
            {"Stream", {gl, uint, []}},
            {"InstanceCount", {gl, sizei, []}}
        ],
        s095_enum_plus_plain_clause_params(["points"], ["Feedback", "Stream", "InstanceCount"]),
        [
            {"Mode", s095_enum_nif_data()},
            {"Feedback", s095_uint_nif_data()},
            {"Stream", s095_uint_nif_data()},
            {"InstanceCount", s095_sizei_nif_data()}
        ]
    ).

s095_assert_direct(FunctionData, GlCommand, ParamsSpecs, SpecsParams, ClauseParams, NifParams) ->
    ?assertEqual(GlCommand, maps:get(gl_command, FunctionData)),
    ?assertEqual(ParamsSpecs, maps:get(params_specs, FunctionData)),
    ?assertEqual(gl_void, maps:get(return_specs, FunctionData)),
    ?assertEqual(SpecsParams, maps:get(specs_params, FunctionData)),
    ?assertEqual([], maps:get(specs_return, FunctionData)),
    ?assertEqual(length(SpecsParams), maps:get(function_arity, FunctionData)),
    [Clause] = maps:get(function_clauses, FunctionData),
    s095_assert_clause_params(ClauseParams, maps:get(params, Clause)),
    ?assertEqual(GlCommand, maps:get(raw_function, Clause)),
    NifData = maps:get(GlCommand, maps:get(nif_functions, FunctionData)),
    ?assertEqual(length(NifParams), maps:get(arity, NifData)),
    ?assertEqual(NifParams, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s095_assert_clause_params(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun s095_assert_clause_param/1, lists:zip(Expected, Actual)).

s095_assert_clause_param({{Name, {gl_enum_to_uint, RequiredAtoms}}, {Name, {gl_enum_to_uint, TransformMap}}}) ->
    [?assert(lists:keymember(Atom, 1, TransformMap)) || Atom <- RequiredAtoms];
s095_assert_clause_param({Expected, Actual}) ->
    ?assertEqual(Expected, Actual).

s095_enum_plus_plain_clause_params(RequiredAtoms, PlainNames) ->
    [{"Mode", {gl_enum_to_uint, RequiredAtoms}}] ++ [{Name, do_nothing} || Name <- PlainNames].

s095_assert_enum_contains(BindingData, EnumType, Atom) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(lists:member(Atom, maps:get(EnumType, EnumTypes))).

s095_assert_emitted_surface(Target, RequiredErl, RequiredC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard95-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),
        [s095_assert_contains(Erl, Needle) || Needle <- RequiredErl],
        [s095_assert_contains(C, Needle) || Needle <- RequiredC],
        [s095_assert_not_contains(Erl, Needle) || Needle <- Forbidden],
        [s095_assert_not_contains(C, Needle) || Needle <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s095_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s095_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).

s095_forbidden_needles() ->
    [].

s095_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s095_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s095_int_nif_data() ->
    {gl_type, {"GLint", "int", "enif_get_int", "enif_make_int"}}.

s095_sizei_nif_data() ->
    {gl_type, {"GLsizei", "int", "enif_get_int", "enif_make_int"}}.

%% Historical shard 165.
s165_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s165_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s165_emitter_query_object_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s165_assert_emitted_gl_present({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s165_assert_emitted_es_present({gles, {3, 2}})
        end},
        {"gles 2.0", fun() ->
            s165_assert_emitted_absent({gles, {2, 0}})
        end}
    ].

s165_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    Expected = s165_expected_variants(Target),
    case Expected of
        [] ->
            ?assertNot(maps:is_key({"get_query_object", 4}, Functions)),
            [
                s165_assert_command_absent(Command, Functions)
             || {Command, _TypeAtom, _GlType} <- s165_all_variants()
            ];
        _ ->
            ?assert(maps:is_key({"get_query_object", 4}, Functions)),
            ?assertNot(maps:is_key({"get_query_object", 3}, Functions)),
            GetQueryObject = maps:get({"get_query_object", 4}, Functions),
            s165_assert_specs(GetQueryObject, Expected),
            [s165_assert_family(GetQueryObject, Variant) || Variant <- Expected],
            [
                s165_assert_command_absent(Command, Functions)
             || {Command, _TypeAtom, _GlType} <- s165_all_variants() -- Expected
            ]
    end,
    s165_assert_deferred_neighbors_absent(Functions).

s165_assert_command_absent(Command, Functions) ->
    ?assertNot(generator_test_support:has_gl_command(Command, Functions)),
    ok.

s165_assert_specs(GetQueryObject, Expected) ->
    ?assertEqual(
        [
            {in, "Query", {gl_object, query}},
            {in, "ParamName", {gl_enum, "QueryObjectParameterName", query_object_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_x}}
        ],
        maps:get(params_specs, GetQueryObject)
    ),
    ExpectedTypeAtoms = [TypeAtom || {_Command, TypeAtom, _GlType} <- Expected],
    ?assertEqual(
        [
            {"Type", {set, ExpectedTypeAtoms}},
            {"Query", {undefined, query, []}},
            {"ParamName", {undefined, query_object_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetQueryObject)
    ),
    ExpectedValueTypes = [s165_type_spec(GlType) || {_Command, _TypeAtom, GlType} <- Expected],
    case ExpectedValueTypes of
        [_Single] ->
            ?assertEqual([{"Values", {list, hd(ExpectedValueTypes)}}], maps:get(specs_return, GetQueryObject)),
            ?assertEqual(undefined, maps:get(extra_type, GetQueryObject));
        _ ->
            ?assertEqual(
                [{"Values", {list, {undefined, get_query_object_value, []}}}],
                maps:get(specs_return, GetQueryObject)
            ),
            {get_query_object_value, {set, ValueTypes}} = maps:get(extra_type, GetQueryObject),
            [?assert(lists:member(ValueType, ValueTypes)) || ValueType <- ExpectedValueTypes]
    end,
    ?assertEqual(4, maps:get(function_arity, GetQueryObject)).

s165_assert_family(GetQueryObject, {Command, TypeAtom, GlType}) ->
    ?assert(lists:member({Command, GlType, typed_value_list}, maps:get(gl_commands, GetQueryObject))),
    ?assert(lists:member({GlType, typed_value_list}, maps:get(variants, GetQueryObject))),
    s165_assert_clause(GetQueryObject, Command, TypeAtom),
    s165_assert_nif(GetQueryObject, Command, GlType).

s165_assert_clause(GetQueryObject, Command, TypeAtom) ->
    Suffix = atom_to_list(TypeAtom),
    Clause = s165_find_clause(Command, maps:get(function_clauses, GetQueryObject)),
    [
        {Suffix, ignore},
        {"Query", do_nothing},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("query_result", 1, ParamNameTransformMap)),
    ?assert(lists:keymember("query_result_available", 1, ParamNameTransformMap)),
    ?assertEqual(Command, maps:get(raw_function, Clause)).

s165_assert_nif(GetQueryObject, Command, GlType) ->
    NifData = maps:get(Command, maps:get(nif_functions, GetQueryObject)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Query", s165_query_nif_data()},
            {"ParamName", s165_enum_nif_data()},
            {"Values", {out_typed_value_list, s165_gl_ctype(GlType), s165_term_function(GlType)}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s165_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetQueryObjectivARB",
        "glGetQueryObjectuivARB",
        "glGetQueryObjectivEXT",
        "glGetQueryObjectuivEXT",
        "glGetQueryObjecti64vEXT",
        "glGetQueryObjectui64vEXT",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s165_assert_emitted_gl_present(Target) ->
    s165_assert_emitted_surface(
        Target,
        [
            <<"-export([get_query_object/4]).">>,
            <<"-export_type([query_object_parameter_name/0]).">>,
            <<"-spec get_query_object(\n    Type :: i | i64 | ui | ui64,\n    Query :: query(),\n    ParamName :: query_object_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [get_query_object_value()]} | {error, atom()}.">>,
            <<"get_query_object(i, Query, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryObjectiv_raw(Query, NewParamName, Count))">>,
            <<"get_query_object(ui, Query, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryObjectuiv_raw(Query, NewParamName, Count))">>,
            <<"get_query_object(i64, Query, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryObjecti64v_raw(Query, NewParamName, Count))">>,
            <<"get_query_object(ui64, Query, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryObjectui64v_raw(Query, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"GLuint* arg_2_values = enif_alloc(sizeof(GLuint) * (size_t)arg_2_count);">>,
            <<"GLint64* arg_2_values = enif_alloc(sizeof(GLint64) * (size_t)arg_2_count);">>,
            <<"GLuint64* arg_2_values = enif_alloc(sizeof(GLuint64) * (size_t)arg_2_count);">>,
            <<"glGetQueryObjectiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetQueryObjectuiv(arg_0, arg_1, arg_2_values);">>,
            <<"glGetQueryObjecti64v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetQueryObjectui64v(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetQueryObjectiv_raw\", 3, nif_glGetQueryObjectiv, 0}">>,
            <<"{\"glGetQueryObjectuiv_raw\", 3, nif_glGetQueryObjectuiv, 0}">>,
            <<"{\"glGetQueryObjecti64v_raw\", 3, nif_glGetQueryObjecti64v, 0}">>,
            <<"{\"glGetQueryObjectui64v_raw\", 3, nif_glGetQueryObjectui64v, 0}">>
        ],
        s165_forbidden_neighbors()
    ).

s165_assert_emitted_es_present(Target) ->
    s165_assert_emitted_surface(
        Target,
        [
            <<"-export([get_query_object/4]).">>,
            <<"-export_type([query_object_parameter_name/0]).">>,
            <<"-spec get_query_object(\n    Type :: ui,\n    Query :: query(),\n    ParamName :: query_object_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:uint()]} | {error, atom()}.">>,
            <<"get_query_object(ui, Query, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryObjectuiv_raw(Query, NewParamName, Count))">>
        ],
        [
            <<"GLuint* arg_2_values = enif_alloc(sizeof(GLuint) * (size_t)arg_2_count);">>,
            <<"glGetQueryObjectuiv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetQueryObjectuiv_raw\", 3, nif_glGetQueryObjectuiv, 0}">>
        ],
        [
            <<"glGetQueryObjectiv">>,
            <<"glGetQueryObjecti64v">>,
            <<"glGetQueryObjectui64v">>
            | s165_forbidden_neighbors()
        ]
    ).

s165_assert_emitted_absent(Target) ->
    s165_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_query_object">>,
            <<"query_object_parameter_name">>,
            <<"glGetQueryObjectiv">>,
            <<"glGetQueryObjectuiv">>,
            <<"glGetQueryObjecti64v">>,
            <<"glGetQueryObjectui64v">>
            | s165_forbidden_neighbors()
        ]
    ).

s165_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard165-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s165_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s165_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s165_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s165_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s165_expected_variants({gl, _Version}) ->
    [
        {"glGetQueryObjectiv", i, gl_int},
        {"glGetQueryObjecti64v", i64, gl_int64},
        {"glGetQueryObjectuiv", ui, gl_uint},
        {"glGetQueryObjectui64v", ui64, gl_uint64}
    ];
s165_expected_variants({gles, {3, _Minor}}) ->
    [{"glGetQueryObjectuiv", ui, gl_uint}];
s165_expected_variants(_) ->
    [].

s165_all_variants() ->
    [
        {"glGetQueryObjectiv", i, gl_int},
        {"glGetQueryObjecti64v", i64, gl_int64},
        {"glGetQueryObjectuiv", ui, gl_uint},
        {"glGetQueryObjectui64v", ui64, gl_uint64}
    ].

s165_type_spec(gl_int) -> {gl, int, []};
s165_type_spec(gl_uint) -> {gl, uint, []};
s165_type_spec(gl_int64) -> {gl, int64, []};
s165_type_spec(gl_uint64) -> {gl, uint64, []}.

s165_gl_ctype(gl_int) -> "GLint";
s165_gl_ctype(gl_uint) -> "GLuint";
s165_gl_ctype(gl_int64) -> "GLint64";
s165_gl_ctype(gl_uint64) -> "GLuint64".

s165_term_function(gl_int) -> "enif_make_int";
s165_term_function(gl_uint) -> "enif_make_uint";
s165_term_function(gl_int64) -> "enif_make_int64";
s165_term_function(gl_uint64) -> "enif_make_uint64".

s165_query_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s165_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s165_find_clause(Command, Clauses) ->
    hd([Clause || Clause <- Clauses, maps:get(raw_function, Clause) =:= Command]).

s165_forbidden_neighbors() ->
    [
        <<"glGetQueryObjectivARB">>,
        <<"glGetQueryObjectuivARB">>,
        <<"glGetQueryObjectivEXT">>,
        <<"glGetQueryObjectuivEXT">>,
        <<"glGetQueryObjecti64vEXT">>,
        <<"glGetQueryObjectui64vEXT">>,
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s165_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s165_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 166.
s166_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s166_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s166_emitter_query_target_readback_test_() ->
    [
        {"gl 4.6", fun() ->
            s166_assert_emitted_present({gl, {4, 6}})
        end},
        {"gles 3.2", fun() ->
            s166_assert_emitted_present({gles, {3, 2}})
        end},
        {"gles 2.0", fun() ->
            s166_assert_emitted_absent({gles, {2, 0}})
        end}
    ].

s166_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    case s166_supports_get_query(Target) of
        true ->
            ?assert(maps:is_key({"get_query", 3}, Functions)),
            GetQuery = maps:get({"get_query", 3}, Functions),
            s166_assert_specs(GetQuery),
            s166_assert_clause(GetQuery),
            s166_assert_nif(GetQuery);
        false ->
            ?assertNot(maps:is_key({"get_query", 3}, Functions)),
            s166_assert_command_absent("glGetQueryiv", Functions)
    end,
    s166_assert_deferred_neighbors_absent(Functions).

s166_supports_get_query({gles, {2, 0}}) ->
    false;
s166_supports_get_query(_) ->
    true.

s166_assert_command_absent(Command, Functions) ->
    ?assertNot(generator_test_support:has_gl_command(Command, Functions)),
    ok.

s166_assert_specs(GetQuery) ->
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "QueryTarget"}},
            {in, "ParamName", {gl_enum, "QueryParameterName", query_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, GetQuery)
    ),
    ?assertEqual(
        [
            {"Target", {undefined, query_target, []}},
            {"ParamName", {undefined, query_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, GetQuery)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, GetQuery)),
    ?assertEqual(undefined, maps:get(extra_type, GetQuery)),
    ?assertEqual(3, maps:get(function_arity, GetQuery)).

s166_assert_clause(GetQuery) ->
    [Clause] = maps:get(function_clauses, GetQuery),
    [
        {"Target", {gl_enum_to_uint, TargetTransformMap}},
        {"ParamName", {gl_enum_to_uint, ParamNameTransformMap}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    ?assert(lists:keymember("any_samples_passed", 1, TargetTransformMap)),
    ?assert(lists:keymember("current_query", 1, ParamNameTransformMap)),
    ?assertEqual("glGetQueryiv", maps:get(raw_function, Clause)).

s166_assert_nif(GetQuery) ->
    NifData = maps:get("glGetQueryiv", maps:get(nif_functions, GetQuery)),
    ?assertEqual(3, maps:get(arity, NifData)),
    ?assertEqual(
        [
            {"Target", s166_enum_nif_data()},
            {"ParamName", s166_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ],
        maps:get(params, NifData)
    ),
    ?assertEqual(void, maps:get(return, NifData)).

s166_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetQueryivARB",
        "glGetQueryivEXT",
        "glGetQueryObjectivARB",
        "glGetQueryObjectuivARB",
        "glGetQueryObjectivEXT",
        "glGetQueryObjectuivEXT",
        "glGetQueryObjecti64vEXT",
        "glGetQueryObjectui64vEXT",
        "glGetBufferPointerv",
        "glGetVertexAttribPointerv"
    ],
    [?assertNot(generator_test_support:has_gl_command(Command, Functions)) || Command <- DeferredCommands].

s166_assert_emitted_present(Target) ->
    s166_assert_emitted_surface(
        Target,
        [
            <<"-export([get_query/3]).">>,
            <<"-export_type([query_parameter_name/0]).">>,
            <<"-spec get_query(\n    Target :: query_target(),\n    ParamName :: query_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>,
            <<"get_query(Target, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetQueryiv_raw(NewTarget, NewParamName, Count))">>
        ],
        [
            <<"ErlNifUInt64 arg_2_count_tmp;">>,
            <<"if (arg_2_count_tmp == 0 || arg_2_count_tmp > (ErlNifUInt64)INT_MAX) {">>,
            <<"GLint* arg_2_values = enif_alloc(sizeof(GLint) * (size_t)arg_2_count);">>,
            <<"glGetQueryiv(arg_0, arg_1, arg_2_values);">>,
            <<"{\"glGetQueryiv_raw\", 3, nif_glGetQueryiv, 0}">>
        ],
        s166_forbidden_neighbors()
    ).

s166_assert_emitted_absent(Target) ->
    s166_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_query">>,
            <<"query_parameter_name">>,
            <<"glGetQueryiv">>
            | s166_forbidden_neighbors()
        ]
    ).

s166_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard166-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s166_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s166_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s166_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s166_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s166_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s166_forbidden_neighbors() ->
    [
        <<"glGetQueryivARB">>,
        <<"glGetQueryivEXT">>,
        <<"glGetQueryObjectivARB">>,
        <<"glGetQueryObjectuivARB">>,
        <<"glGetQueryObjectivEXT">>,
        <<"glGetQueryObjectuivEXT">>,
        <<"glGetQueryObjecti64vEXT">>,
        <<"glGetQueryObjectui64vEXT">>,
        <<"glGetBufferPointerv">>,
        <<"glGetVertexAttribPointerv">>
    ].

s166_assert_contains(Binary, Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern)).

s166_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).


%% Historical shard 179.

s179_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s179_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s179_emitter_command_state_readback_test_() ->
    [
        {"gl 4.6", fun() -> s179_assert_emitted_gl46() end},
        {"gl 4.1", fun() -> s179_assert_emitted_gl41() end},
        {"gl 3.3", fun() -> s179_assert_emitted_gl33() end},
        {"gles 3.2", fun() -> s179_assert_emitted_es32() end},
        {"gles 3.0", fun() -> s179_assert_emitted_es30() end},
        {"gles 2.0", fun() -> s179_assert_emitted_es20() end}
    ].

s179_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    [s179_assert_reader(Functions, Reader) || Reader <- s179_expected_readers(Target)],
    [s179_assert_reader_absent(Functions, Reader) || Reader <- s179_absent_readers(Target)],
    s179_assert_fixed_state_helpers_absent(Functions),
    s179_assert_enum_contains(BindingData, state_parameter_name, "depth_writemask"),
    s179_assert_enum_contains(BindingData, state_parameter_name, "line_width"),
    s179_assert_enum_contains(BindingData, state_parameter_name, "pack_alignment"),
    s179_assert_enum_contains(BindingData, state_parameter_name, "blend"),
    s179_assert_enum_contains(BindingData, state_parameter_name, "max_texture_size"),
    case s179_supports_max_element_index(Target) of
        true -> s179_assert_enum_contains(BindingData, state_parameter_name, "max_element_index");
        false -> ok
    end,
    case s179_supports_multisample(Target) of
        true -> s179_assert_enum_contains(BindingData, multisample_parameter_name, "sample_position");
        false -> ok
    end,
    ?assertNot(maps:is_key({"get_parameter", 3}, Functions)),
    ?assertNot(maps:is_key({"get_parameter", 4}, Functions)),
    s179_assert_selector_getters_absent(Functions),
    s179_assert_deferred_neighbors_absent(Functions).

s179_all_readers() ->
    [
        {"glGetBooleanv", {"get_boolean", 2}, "glGetBooleanv", gl_bool, 2, non_indexed_state},
        {"glGetBooleani_v", {"get_boolean", 3}, "glGetBooleani_v", gl_bool, 3, indexed_state},
        {"glGetFloatv", {"get_float", 2}, "glGetFloatv", gl_float, 2, non_indexed_state},
        {"glGetFloati_v", {"get_float", 3}, "glGetFloati_v", gl_float, 3, indexed_state},
        {"glGetDoublev", {"get_double", 2}, "glGetDoublev", gl_double, 2, non_indexed_state},
        {"glGetDoublei_v", {"get_double", 3}, "glGetDoublei_v", gl_double, 3, indexed_state},
        {"glGetIntegerv", {"get_integer", 2}, "glGetIntegervValues", gl_int, 2, non_indexed_state},
        {"glGetIntegeri_v", {"get_integer", 3}, "glGetIntegeri_v", gl_int, 3, indexed_state},
        {"glGetInteger64v", {"get_integer64", 2}, "glGetInteger64vValues", gl_int64, 2, non_indexed_state},
        {"glGetInteger64i_v", {"get_integer64", 3}, "glGetInteger64i_v", gl_int64, 3, indexed_state},
        {"glGetMultisamplefv", {"get_multisample", 3}, "glGetMultisamplefv", gl_float, 3, multisample}
    ].

s179_expected_readers(Target) ->
    [Reader || Reader <- s179_all_readers(), s179_reader_supported(Target, Reader)].

s179_absent_readers(Target) ->
    s179_all_readers() -- s179_expected_readers(Target).

s179_reader_supported(_Target, {"glGetBooleanv", _, _, _, _, _}) -> true;
s179_reader_supported({gles, {2, 0}}, {"glGetBooleani_v", _, _, _, _, _}) -> false;
s179_reader_supported({gles, {3, 0}}, {"glGetBooleani_v", _, _, _, _, _}) -> false;
s179_reader_supported(_Target, {"glGetBooleani_v", _, _, _, _, _}) -> true;
s179_reader_supported(_Target, {"glGetFloatv", _, _, _, _, _}) -> true;
s179_reader_supported({gl, {4, Minor}}, {"glGetFloati_v", _, _, _, _, _}) when Minor >= 1 -> true;
s179_reader_supported({gl, {Major, _Minor}}, {"glGetFloati_v", _, _, _, _, _}) when Major > 4 -> true;
s179_reader_supported(_Target, {"glGetFloati_v", _, _, _, _, _}) -> false;
s179_reader_supported({gl, _Version}, {"glGetDoublev", _, _, _, _, _}) -> true;
s179_reader_supported(_Target, {"glGetDoublev", _, _, _, _, _}) -> false;
s179_reader_supported({gl, {4, Minor}}, {"glGetDoublei_v", _, _, _, _, _}) when Minor >= 1 -> true;
s179_reader_supported({gl, {Major, _Minor}}, {"glGetDoublei_v", _, _, _, _, _}) when Major > 4 -> true;
s179_reader_supported(_Target, {"glGetDoublei_v", _, _, _, _, _}) -> false;
s179_reader_supported(_Target, {"glGetIntegerv", _, _, _, _, _}) -> true;
s179_reader_supported({gles, {2, 0}}, {"glGetIntegeri_v", _, _, _, _, _}) -> false;
s179_reader_supported(_Target, {"glGetIntegeri_v", _, _, _, _, _}) -> true;
s179_reader_supported({gles, {2, 0}}, {"glGetInteger64v", _, _, _, _, _}) -> false;
s179_reader_supported(_Target, {"glGetInteger64v", _, _, _, _, _}) -> true;
s179_reader_supported({gles, {2, 0}}, {"glGetInteger64i_v", _, _, _, _, _}) -> false;
s179_reader_supported(_Target, {"glGetInteger64i_v", _, _, _, _, _}) -> true;
s179_reader_supported(Target, {"glGetMultisamplefv", _, _, _, _, _}) -> s179_supports_multisample(Target).

s179_supports_multisample({gl, _Version}) -> true;
s179_supports_multisample({gles, {3, Minor}}) when Minor >= 1 -> true;
s179_supports_multisample(_) -> false.

s179_supports_max_element_index({gl, {4, 6}}) -> true;
s179_supports_max_element_index({gles, {3, _Minor}}) -> true;
s179_supports_max_element_index(_) -> false.

s179_assert_reader(Functions, {Command, Key = {_Name, Arity}, RawName, GlType, Arity, Shape}) ->
    Function = maps:get(Key, Functions),
    ?assertEqual(Command, maps:get(gl_command, Function)),
    ?assertEqual(s179_params_specs(Shape, GlType), maps:get(params_specs, Function)),
    ?assertEqual(s179_specs_params(Shape), maps:get(specs_params, Function)),
    ?assertEqual([{"Values", {list, s179_type_spec(GlType)}}], maps:get(specs_return, Function)),
    ?assertEqual(undefined, maps:get(extra_type, Function)),
    ?assertEqual(Arity, maps:get(function_arity, Function)),

    [Clause] = maps:get(function_clauses, Function),
    s179_assert_clause_params(Shape, maps:get(params, Clause)),
    ?assertEqual(RawName, maps:get(raw_function, Clause)),
    s179_assert_nif(Function, RawName, Command, GlType, Arity, Shape).

s179_assert_reader_absent(Functions, {Command, Key, _RawName, _GlType, _Arity, _Shape}) ->
    ?assertNot(maps:is_key(Key, Functions)),
    ?assertNot(generator_test_support:has_gl_command(Command, Functions)).

s179_params_specs(multisample, GlType) ->
    [
        {in, "ParamName", {gl_enum, "GetMultisamplePNameNV", multisample_parameter_name}},
        {in, "Index", gl_uint},
        {out, "Values", {typed_value_list, "Count", GlType}}
    ];
s179_params_specs(non_indexed_state, GlType) ->
    [
        {in, "ParamName", {gl_enum, "GetPName", state_parameter_name}},
        {out, "Values", {typed_value_list, "Count", GlType}}
    ];
s179_params_specs(indexed_state, GlType) ->
    [
        {in, "ParamName", {gl_enum, "GetPName", state_parameter_name}},
        {in, "Index", gl_uint},
        {out, "Values", {typed_value_list, "Count", GlType}}
    ].

s179_specs_params(non_indexed_state) ->
    [
        {"ParamName", {undefined, state_parameter_name, []}},
        {"Count", {undefined, pos_integer, []}}
    ];
s179_specs_params(indexed_state) ->
    [
        {"ParamName", {undefined, state_parameter_name, []}},
        {"Index", {gl, uint, []}},
        {"Count", {undefined, pos_integer, []}}
    ];
s179_specs_params(multisample) ->
    [
        {"ParamName", {undefined, multisample_parameter_name, []}},
        {"Index", {gl, uint, []}},
        {"Count", {undefined, pos_integer, []}}
    ].

s179_assert_clause_params(non_indexed_state, Params) ->
    [
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Count", do_nothing}
    ] = Params,
    s179_assert_state_parameter_transform(ParamNameMap);
s179_assert_clause_params(indexed_state, Params) ->
    [
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Index", do_nothing},
        {"Count", do_nothing}
    ] = Params,
    s179_assert_state_parameter_transform(ParamNameMap);
s179_assert_clause_params(multisample, Params) ->
    [
        {"ParamName", {gl_enum_to_uint, ParamNameMap}},
        {"Index", do_nothing},
        {"Count", do_nothing}
    ] = Params,
    ?assert(lists:keymember("sample_position", 1, ParamNameMap)).

s179_assert_state_parameter_transform(ParamNameMap) ->
    ?assert(lists:keymember("depth_writemask", 1, ParamNameMap)),
    ?assert(lists:keymember("line_width", 1, ParamNameMap)),
    ?assert(lists:keymember("pack_alignment", 1, ParamNameMap)),
    ?assert(lists:keymember("blend", 1, ParamNameMap)),
    ?assert(lists:keymember("max_texture_size", 1, ParamNameMap)).

s179_assert_nif(Function, RawName, Command, GlType, Arity, Shape) ->
    NifData = maps:get(RawName, maps:get(nif_functions, Function)),
    ?assertEqual(Command, maps:get(gl_command, NifData)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(void, maps:get(return, NifData)),
    ?assertEqual(s179_nif_params(Shape, GlType), maps:get(params, NifData)).

s179_nif_params(non_indexed_state, GlType) ->
    [
        {"ParamName", s179_enum_nif_data()},
        {"Values", {out_typed_value_list, s179_gl_ctype(GlType), s179_term_function(GlType)}}
    ];
s179_nif_params(indexed_state, GlType) ->
    [
        {"ParamName", s179_enum_nif_data()},
        {"Index", s179_uint_nif_data()},
        {"Values", {out_typed_value_list, s179_gl_ctype(GlType), s179_term_function(GlType)}}
    ];
s179_nif_params(multisample, GlType) ->
    s179_nif_params(indexed_state, GlType).

s179_assert_emitted_gl46() ->
    s179_assert_emitted_surface(
        {gl, {4, 6}},
        s179_all_export_needles(),
        [
            <<"glGetBooleanv(arg_0, arg_1_values);">>,
            <<"glGetBooleani_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetFloatv(arg_0, arg_1_values);">>,
            <<"glGetFloati_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetDoublev(arg_0, arg_1_values);">>,
            <<"glGetDoublei_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetIntegerv(arg_0, arg_1_values);">>,
            <<"glGetIntegeri_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetInteger64v(arg_0, arg_1_values);">>,
            <<"glGetInteger64i_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetMultisamplefv(arg_0, arg_1, arg_2_values);">>,
            <<"GLboolean* arg_1_values = enif_alloc(sizeof(GLboolean) * (size_t)arg_1_count);">>,
            <<"arg_1_ret = enif_make_list_cell(env, custom_enif_make_bool(env, arg_1_values[i]), arg_1_ret);">>,
            <<"{\"glGetIntegervValues_raw\", 2, nif_glGetIntegervValues, 0}">>,
            <<"{\"glGetInteger64vValues_raw\", 2, nif_glGetInteger64vValues, 0}">>
        ],
        s179_forbidden_needles()
    ).

s179_assert_emitted_gl41() ->
    s179_assert_emitted_surface(
        {gl, {4, 1}},
        s179_all_export_needles(),
        [
            <<"glGetFloati_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetDoublei_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetMultisamplefv(arg_0, arg_1, arg_2_values);">>
        ],
        s179_forbidden_needles()
    ).

s179_assert_emitted_gl33() ->
    s179_assert_emitted_surface(
        {gl, {3, 3}},
        [
            <<"-export([get_boolean/2]).">>,
            <<"-export([get_boolean/3]).">>,
            <<"-export([get_float/2]).">>,
            <<"-export([get_double/2]).">>,
            <<"-export([get_integer/2]).">>,
            <<"-export([get_integer/3]).">>,
            <<"-export([get_integer64/2]).">>,
            <<"-export([get_integer64/3]).">>,
            <<"-export([get_multisample/3]).">>,
            <<"?CALL_RAW_FUNC(glGetIntegervValues_raw(NewParamName, Count)).">>
        ],
        [
            <<"glGetBooleani_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetDoublev(arg_0, arg_1_values);">>,
            <<"glGetMultisamplefv(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"-export([get_float/3]).">>,
            <<"-export([get_double/3]).">>,
            <<"glGetFloati_v(">>,
            <<"glGetDoublei_v(">>
            | s179_forbidden_needles()
        ]
    ).

s179_assert_emitted_es32() ->
    s179_assert_emitted_surface(
        {gles, {3, 2}},
        [
            <<"-export([get_boolean/2]).">>,
            <<"-export([get_boolean/3]).">>,
            <<"-export([get_float/2]).">>,
            <<"-export([get_integer/2]).">>,
            <<"-export([get_integer/3]).">>,
            <<"-export([get_integer64/2]).">>,
            <<"-export([get_integer64/3]).">>,
            <<"-export([get_multisample/3]).">>,
            <<"?CALL_RAW_FUNC(glGetIntegervValues_raw(NewParamName, Count)).">>,
            <<"?CALL_RAW_FUNC(glGetMultisamplefv_raw(NewParamName, Index, Count)).">>
        ],
        [
            <<"glGetBooleani_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetInteger64i_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetMultisamplefv(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"-export([get_float/3]).">>,
            <<"-export([get_double/2]).">>,
            <<"-export([get_double/3]).">>,
            <<"glGetDoublev(">>,
            <<"glGetDoublei_v(">>,
            <<"glGetFloati_v(">>
            | s179_forbidden_needles()
        ]
    ).

s179_assert_emitted_es30() ->
    s179_assert_emitted_surface(
        {gles, {3, 0}},
        [
            <<"-export([get_boolean/2]).">>,
            <<"-export([get_float/2]).">>,
            <<"-export([get_integer/2]).">>,
            <<"-export([get_integer/3]).">>,
            <<"-export([get_integer64/2]).">>,
            <<"-export([get_integer64/3]).">>
        ],
        [
            <<"glGetIntegeri_v(arg_0, arg_1, arg_2_values);">>,
            <<"glGetInteger64i_v(arg_0, arg_1, arg_2_values);">>
        ],
        [
            <<"-export([get_boolean/3]).">>,
            <<"-export([get_multisample/3]).">>,
            <<"-export([get_double/2]).">>,
            <<"-export([get_double/3]).">>,
            <<"glGetBooleani_v(">>,
            <<"glGetMultisamplefv(">>,
            <<"glGetDoublev(">>,
            <<"glGetDoublei_v(">>,
            <<"glGetFloati_v(">>
            | s179_forbidden_needles()
        ]
    ).

s179_assert_emitted_es20() ->
    s179_assert_emitted_surface(
        {gles, {2, 0}},
        [
            <<"-export([get_boolean/2]).">>,
            <<"-export([get_float/2]).">>,
            <<"-export([get_integer/2]).">>,
            <<"?CALL_RAW_FUNC(glGetBooleanv_raw(NewParamName, Count)).">>,
            <<"?CALL_RAW_FUNC(glGetIntegervValues_raw(NewParamName, Count)).">>
        ],
        [
            <<"glGetBooleanv(arg_0, arg_1_values);">>,
            <<"glGetFloatv(arg_0, arg_1_values);">>,
            <<"glGetIntegerv(arg_0, arg_1_values);">>
        ],
        [
            <<"-export([get_boolean/3]).">>,
            <<"-export([get_double/2]).">>,
            <<"-export([get_double/3]).">>,
            <<"-export([get_float/3]).">>,
            <<"-export([get_integer/3]).">>,
            <<"-export([get_integer64/2]).">>,
            <<"-export([get_integer64/3]).">>,
            <<"-export([get_multisample/3]).">>,
            <<"glGetBooleani_v(">>,
            <<"glGetDoublev(">>,
            <<"glGetDoublei_v(">>,
            <<"glGetFloati_v(">>,
            <<"glGetIntegeri_v(">>,
            <<"glGetInteger64v(">>,
            <<"glGetInteger64i_v(">>,
            <<"glGetMultisamplefv(">>
            | s179_forbidden_needles()
        ]
    ).

s179_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard179-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s179_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s179_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s179_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s179_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s179_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s179_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s179_all_export_needles() ->
    [
        <<"-export([get_boolean/2]).">>,
        <<"-export([get_boolean/3]).">>,
        <<"-export([get_float/2]).">>,
        <<"-export([get_float/3]).">>,
        <<"-export([get_double/2]).">>,
        <<"-export([get_double/3]).">>,
        <<"-export([get_integer/2]).">>,
        <<"-export([get_integer/3]).">>,
        <<"-export([get_integer64/2]).">>,
        <<"-export([get_integer64/3]).">>,
        <<"-export([get_multisample/3]).">>,
        <<"-export_type([state_parameter_name/0]).">>,
        <<"-export_type([multisample_parameter_name/0]).">>,
        <<"-spec get_boolean(\n    ParamName :: state_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:boolean()]} | {error, atom()}.">>,
        <<"-spec get_integer(\n    ParamName :: state_parameter_name(),\n    Count :: pos_integer()\n) -> {ok, Values :: [gl:int()]} | {error, atom()}.">>,
        <<"?CALL_RAW_FUNC(glGetBooleanv_raw(NewParamName, Count)).">>,
        <<"?CALL_RAW_FUNC(glGetIntegervValues_raw(NewParamName, Count)).">>,
        <<"?CALL_RAW_FUNC(glGetInteger64vValues_raw(NewParamName, Count)).">>,
        <<"?CALL_RAW_FUNC(glGetMultisamplefv_raw(NewParamName, Index, Count)).">>
    ].

s179_assert_selector_getters_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- [
            {"get_depth_writemask", 0},
            {"get_line_width", 0},
            {"get_pack_alignment_generic", 0},
            {"get_viewport", 0},
            {"get_viewport", 1},
            {"get_blend_enabled", 1},
            {"get_sample_position", 1}
        ]
    ].

s179_assert_fixed_state_helpers_absent(Functions) ->
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- [
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
        ]
    ].

s179_assert_deferred_neighbors_absent(Functions) ->
    DeferredCommands = [
        "glGetProgramPipelineiv"
    ],
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- DeferredCommands,
        not s179_already_settled_neighbor(Command, Functions)
    ].

s179_already_settled_neighbor("glGetProgramPipelineiv", Functions) ->
    maps:is_key({"get_program_pipeline", 3}, Functions);
s179_already_settled_neighbor(_Command, _Functions) ->
    false.

s179_forbidden_needles() ->
    [
        <<"glGetIntegervInteger_raw">>,
        <<"glGetInteger64vInteger_raw">>,
        <<"glGetIntegervBoolean_raw">>,
        <<"-export([get_integer/1]).">>,
        <<"-export([get_integer64/1]).">>,
        <<"-export([get_max_">>,
        <<"get_shader_compiler_supported">>,
        <<"get_debug_next_logged_message_length">>
    ].

s179_assert_enum_contains(BindingData, TypeName, Atom) ->
    ?assert(lists:member(Atom, maps:get(atom_to_list(TypeName), maps:get(enum_types, BindingData)))).

s179_type_spec(gl_bool) -> {gl, boolean, []};
s179_type_spec(gl_float) -> {gl, float, []};
s179_type_spec(gl_int) -> {gl, int, []};
s179_type_spec(gl_double) -> {gl, double, []};
s179_type_spec(gl_int64) -> {gl, int64, []}.

s179_gl_ctype(gl_bool) -> "GLboolean";
s179_gl_ctype(gl_float) -> "GLfloat";
s179_gl_ctype(gl_int) -> "GLint";
s179_gl_ctype(gl_double) -> "GLdouble";
s179_gl_ctype(gl_int64) -> "GLint64".

s179_term_function(gl_bool) -> "custom_enif_make_bool";
s179_term_function(gl_float) -> "enif_make_double";
s179_term_function(gl_int) -> "enif_make_int";
s179_term_function(gl_double) -> "enif_make_double";
s179_term_function(gl_int64) -> "enif_make_int64".

s179_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s179_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s179_assert_contains(Binary, Pattern) when is_binary(Binary), is_binary(Pattern) ->
    ?assertMatch({_, _}, binary:match(Binary, Pattern));
s179_assert_contains(String, Pattern) when is_list(String), is_list(Pattern) ->
    ?assertNotEqual(nomatch, string:find(String, Pattern)).

s179_assert_not_contains(Binary, Pattern) ->
    ?assertEqual(nomatch, binary:match(Binary, Pattern)).

%% Historical shard 181.

s181_resolver_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s181_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s181_emitter_reflection_query_test_() ->
    [
        {"gl 4.6", fun() -> s181_assert_emitted_gl46() end},
        {"gl 4.1", fun() -> s181_assert_emitted_absent({gl, {4, 1}}) end},
        {"gles 3.2", fun() -> s181_assert_emitted_es32() end},
        {"gles 2.0", fun() -> s181_assert_emitted_absent({gles, {2, 0}}) end}
    ].

s181_assert_target(Target) ->
    BindingData = generator_test_support:resolve_target(Target),
    Functions = maps:get(functions, BindingData),
    case s181_supports_internal_format_i(Target) of
        true ->
            s181_assert_internal_format(maps:get({"get_internal_format", 5}, Functions), Target),
            s181_assert_enum_contains(BindingData, internal_format_target, "renderbuffer"),
            s181_assert_enum_contains(BindingData, internal_format_parameter_name, "num_sample_counts");
        false ->
            s181_assert_internal_format_absent(Functions)
    end,
    case s181_supports_atomic_counter_buffer(Target) of
        true ->
            s181_assert_atomic_counter_buffer(maps:get({"get_active_atomic_counter_buffer", 4}, Functions)),
            s181_assert_enum_contains(BindingData, atomic_counter_buffer_parameter_name, "atomic_counter_buffer_active_atomic_counters");
        false ->
            s181_assert_atomic_counter_buffer_absent(Functions)
    end,
    s181_assert_deferred_neighbors_absent(Functions).

s181_supports_internal_format_i({gl, {4, 6}}) -> true;
s181_supports_internal_format_i({gles, {3, _Minor}}) -> true;
s181_supports_internal_format_i(_) -> false.

s181_supports_internal_format_i64({gl, {4, 6}}) -> true;
s181_supports_internal_format_i64(_) -> false.

s181_supports_atomic_counter_buffer({gl, {4, 6}}) -> true;
s181_supports_atomic_counter_buffer(_) -> false.

s181_assert_internal_format(Function, Target) ->
    ?assertEqual(
        lists:sort(s181_internal_format_commands(Target)),
        lists:sort(maps:get(gl_commands, Function))
    ),
    ?assertEqual(
        [
            {in, "Target", {gl_enum, "TextureTarget", internal_format_target}},
            {in, "InternalFormat", {gl_enum, "InternalFormat"}},
            {in, "ParamName", {gl_enum, "InternalFormatPName", internal_format_parameter_name}},
            {out, "Values", {typed_value_list_with_size, "Count", gl_x}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Type", {set, [i] ++ s181_i64_atom(Target)}},
            {"Target", {undefined, internal_format_target, []}},
            {"InternalFormat", {undefined, internal_format, []}},
            {"ParamName", {undefined, internal_format_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual(s181_internal_format_return_specs(Target), maps:get(specs_return, Function)),
    ?assertEqual(5, maps:get(function_arity, Function)),
    s181_assert_internal_format_clauses(Function, Target),
    s181_assert_internal_format_nifs(Function, Target).

s181_i64_command(Target) ->
    case s181_supports_internal_format_i64(Target) of
        true -> ["glGetInternalformati64v"];
        false -> []
    end.

s181_internal_format_commands(Target) ->
    [
        {"glGetInternalformativ", gl_int, typed_value_list_with_size}
     | s181_i64_internal_format_commands(Target)
    ].

s181_i64_internal_format_commands(Target) ->
    case s181_supports_internal_format_i64(Target) of
        true -> [{"glGetInternalformati64v", gl_int64, typed_value_list_with_size}];
        false -> []
    end.

s181_i64_atom(Target) ->
    case s181_supports_internal_format_i64(Target) of
        true -> [i64];
        false -> []
    end.

s181_internal_format_return_specs(Target) ->
    case s181_supports_internal_format_i64(Target) of
        true -> [{"Values", {list, {undefined, get_internal_format_value, []}}}];
        false -> [{"Values", {list, {gl, int, []}}}]
    end.

s181_assert_internal_format_clauses(Function, Target) ->
    Clauses = maps:get(function_clauses, Function),
    ?assertEqual(1 + length(s181_i64_command(Target)), length(Clauses)),
    s181_assert_internal_format_clause("glGetInternalformativ", i, Clauses),
    case s181_supports_internal_format_i64(Target) of
        true -> s181_assert_internal_format_clause("glGetInternalformati64v", i64, Clauses);
        false -> ok
    end.

s181_assert_internal_format_clause(RawFunction, TypeAtom, Clauses) ->
    [Clause] = [
        Clause0
     || Clause0 <- Clauses,
        maps:get(raw_function, Clause0) =:= RawFunction
    ],
    TypeString = atom_to_list(TypeAtom),
    [
        {TypeString, ignore},
        {"Target", {gl_enum_to_uint, _}},
        {"InternalFormat", {gl_enum_to_uint, _}},
        {"ParamName", {gl_enum_to_uint, _}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause).

s181_assert_internal_format_nifs(Function, Target) ->
    Nifs = maps:get(nif_functions, Function),
    s181_assert_nif(
        maps:get("glGetInternalformativ", Nifs),
        "glGetInternalformativ",
        4,
        [
            {"Target", s181_enum_nif_data()},
            {"InternalFormat", s181_enum_nif_data()},
            {"ParamName", s181_enum_nif_data()},
            {"Values", {out_typed_value_list_with_size, "GLint", "enif_make_int"}}
        ]
    ),
    case s181_supports_internal_format_i64(Target) of
        true ->
            s181_assert_nif(
                maps:get("glGetInternalformati64v", Nifs),
                "glGetInternalformati64v",
                4,
                [
                    {"Target", s181_enum_nif_data()},
                    {"InternalFormat", s181_enum_nif_data()},
                    {"ParamName", s181_enum_nif_data()},
                    {"Values", {out_typed_value_list_with_size, "GLint64", "enif_make_int64"}}
                ]
            );
        false ->
            ?assertNot(maps:is_key("glGetInternalformati64v", Nifs))
    end.

s181_assert_internal_format_absent(Functions) ->
    ?assertNot(maps:is_key({"get_internal_format", 5}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetInternalformativ", Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetInternalformati64v", Functions)).

s181_assert_atomic_counter_buffer(Function) ->
    ?assertEqual("glGetActiveAtomicCounterBufferiv", maps:get(gl_command, Function)),
    ?assertEqual(
        [
            {in, "Program", {gl_object, program}},
            {in, "BufferIndex", gl_uint},
            {in, "ParamName", {gl_enum, "AtomicCounterBufferPName", atomic_counter_buffer_parameter_name}},
            {out, "Values", {typed_value_list, "Count", gl_int}}
        ],
        maps:get(params_specs, Function)
    ),
    ?assertEqual(
        [
            {"Program", {undefined, program, []}},
            {"BufferIndex", {gl, uint, []}},
            {"ParamName", {undefined, atomic_counter_buffer_parameter_name, []}},
            {"Count", {undefined, pos_integer, []}}
        ],
        maps:get(specs_params, Function)
    ),
    ?assertEqual([{"Values", {list, {gl, int, []}}}], maps:get(specs_return, Function)),
    ?assertEqual(4, maps:get(function_arity, Function)),
    [Clause] = maps:get(function_clauses, Function),
    [
        {"Program", do_nothing},
        {"BufferIndex", do_nothing},
        {"ParamName", {gl_enum_to_uint, _}},
        {"Count", do_nothing}
    ] = maps:get(params, Clause),
    s181_assert_nif(
        maps:get("glGetActiveAtomicCounterBufferiv", maps:get(nif_functions, Function)),
        "glGetActiveAtomicCounterBufferiv",
        4,
        [
            {"Program", s181_uint_nif_data()},
            {"BufferIndex", s181_uint_nif_data()},
            {"ParamName", s181_enum_nif_data()},
            {"Values", {out_typed_value_list, "GLint", "enif_make_int"}}
        ]
    ).

s181_assert_atomic_counter_buffer_absent(Functions) ->
    ?assertNot(maps:is_key({"get_active_atomic_counter_buffer", 4}, Functions)),
    ?assertNot(generator_test_support:has_gl_command("glGetActiveAtomicCounterBufferiv", Functions)).

s181_assert_nif(NifData, Command, Arity, Params) ->
    ?assertEqual(Command, maps:get(gl_command, NifData, Command)),
    ?assertEqual(Arity, maps:get(arity, NifData)),
    ?assertEqual(Params, maps:get(params, NifData)),
    ?assertEqual(void, maps:get(return, NifData)).

s181_assert_emitted_gl46() ->
    s181_assert_emitted_surface(
        {gl, {4, 6}},
        [
            <<"-export([get_internal_format/5]).">>,
            <<"-export([get_active_atomic_counter_buffer/4]).">>,
            <<"-export_type([internal_format_target/0]).">>,
            <<"-export_type([internal_format_parameter_name/0]).">>,
            <<"-export_type([atomic_counter_buffer_parameter_name/0]).">>,
            <<"get_internal_format(i, Target, InternalFormat, ParamName, Count) ->">>,
            <<"get_internal_format(i64, Target, InternalFormat, ParamName, Count) ->">>,
            <<"get_active_atomic_counter_buffer(Program, BufferIndex, ParamName, Count) ->">>,
            <<"?CALL_RAW_FUNC(glGetInternalformativ_raw(">>,
            <<"?CALL_RAW_FUNC(glGetInternalformati64v_raw(">>
        ],
        [
            <<"glGetInternalformativ(arg_0, arg_1, arg_2, arg_3_count, arg_3_values);">>,
            <<"glGetInternalformati64v(arg_0, arg_1, arg_2, arg_3_count, arg_3_values);">>,
            <<"glGetActiveAtomicCounterBufferiv(arg_0, arg_1, arg_2, arg_3_values);">>,
            <<"{\"glGetInternalformativ_raw\", 4, nif_glGetInternalformativ, 0}">>,
            <<"{\"glGetInternalformati64v_raw\", 4, nif_glGetInternalformati64v, 0}">>,
            <<"{\"glGetActiveAtomicCounterBufferiv_raw\", 4, nif_glGetActiveAtomicCounterBufferiv, 0}">>
        ],
        s181_forbidden_needles()
    ).

s181_assert_emitted_es32() ->
    s181_assert_emitted_surface(
        {gles, {3, 2}},
        [
            <<"-export([get_internal_format/5]).">>,
            <<"get_internal_format(i, Target, InternalFormat, ParamName, Count) ->">>
        ],
        [
            <<"glGetInternalformativ(arg_0, arg_1, arg_2, arg_3_count, arg_3_values);">>,
            <<"{\"glGetInternalformativ_raw\", 4, nif_glGetInternalformativ, 0}">>
        ],
        [
            <<"-export([get_active_atomic_counter_buffer/4]).">>,
            <<"get_internal_format(i64,">>,
            <<"glGetInternalformati64v(">>,
            <<"glGetActiveAtomicCounterBufferiv(">>
            | s181_forbidden_needles()
        ]
    ).

s181_assert_emitted_absent(Target) ->
    s181_assert_emitted_surface(
        Target,
        [],
        [],
        [
            <<"get_internal_format">>,
            <<"get_active_atomic_counter_buffer">>,
            <<"glGetInternalformativ(">>,
            <<"glGetInternalformati64v(">>,
            <<"glGetActiveAtomicCounterBufferiv(">>
            | s181_forbidden_needles()
        ]
    ).

s181_assert_emitted_surface(Target, ExpectedErl, ExpectedC, Forbidden) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(
        generator_test_support:tmp_root(),
        "opengl-shard181-emitter-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    BindingData = maps:put(api_name, generator_test_support:target_name(Target), generator_test_support:resolve_target(Target)),
    ok = s181_ensure_absent(Dir),
    ok = file:make_dir(Dir),
    try
        ok = file:set_cwd(Dir),
        gl_module_generator:generate(Target, BindingData),
        gl_header_generator:generate(Target, BindingData),
        gl_nif_module_generator:generate(Target, BindingData),
        {ok, Erl} = file:read_file("gl.erl"),
        {ok, C} = file:read_file("gl.c"),

        [s181_assert_contains(Erl, Pattern) || Pattern <- ExpectedErl],
        [s181_assert_contains(C, Pattern) || Pattern <- ExpectedC],
        [s181_assert_not_contains(Erl, Pattern) || Pattern <- Forbidden],
        [s181_assert_not_contains(C, Pattern) || Pattern <- Forbidden]
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(Dir)
    end.

s181_ensure_absent(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

s181_assert_deferred_neighbors_absent(Functions) ->
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- [
            "glCopyImageSubDataEXT",
            "glCopyImageSubDataNV",
            "glCopyImageSubDataOES"
        ]
    ].

s181_forbidden_needles() ->
    [
        <<"get_parameter(">>,
        <<"glCopyImageSubDataEXT(">>,
        <<"glCopyImageSubDataNV(">>,
        <<"glCopyImageSubDataOES(">>
    ].

s181_assert_enum_contains(BindingData, TypeName, AtomName) ->
    EnumTypes = maps:get(enum_types, BindingData),
    ?assert(maps:is_key(atom_to_list(TypeName), EnumTypes)),
    EnumData = maps:get(atom_to_list(TypeName), EnumTypes),
    case EnumData of
        {_Groups, Values} ->
            ?assert(lists:keymember(AtomName, 1, Values));
        AtomNames when is_list(AtomNames) ->
            ?assert(lists:member(AtomName, AtomNames))
    end.

s181_enum_nif_data() ->
    {gl_type, {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s181_uint_nif_data() ->
    {gl_type, {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"}}.

s181_assert_contains(Haystack, Needle) when is_list(Haystack) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle));
s181_assert_contains(Haystack, Needle) ->
    ?assertMatch({_, _}, binary:match(Haystack, Needle)).

s181_assert_not_contains(Haystack, Needle) ->
    ?assertEqual(nomatch, binary:match(Haystack, Needle)).
