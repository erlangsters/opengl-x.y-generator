-module(omission_policy_tests).
-include_lib("eunit/include/eunit.hrl").

%% Omission and unsafe-surface policy guardrails.

%% Historical shard 177.
s177_omission_ledger_test_() ->
    [
        {"omitted commands are not active specs", fun s177_assert_omissions_not_active_specs/0},
        {"omission ledger records design-gated families", fun s177_assert_omission_ledger/0}
    ].

s177_omitted_surface_absence_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s177_assert_target(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s177_missing_commands_are_unspecced_test_() ->
    [
        {generator_test_support:target_name(Target), fun() -> s177_assert_missing_commands(Target) end}
     || Target <- generator_test_support:supported_targets()
    ].

s177_assert_omissions_not_active_specs() ->
    Active = generator_test_support:active_spec_commands(),
    [
        ?assertNot(lists:member(Command, Active))
     || Command <- s177_omitted_commands() ++ s177_robust_texture_blocker_commands()
    ].

s177_assert_omission_ledger() ->
    {ok, Contents} = file:read_file("docs/omissions.md"),
    [
        s177_assert_contains(Contents, Needle)
     || Needle <- [
        <<"Omissions And Design-Gated Families">>,
        <<"glMapBuffer">>,
        <<"glFenceSync">>,
        <<"glDebugMessageCallback">>,
        <<"glGetPointerv">>,
        <<"glGetnTexImage">>,
        <<"glVertexAttrib*dv">>,
        <<"Client-Side Element Indexes And Indirect Payloads">>
    ]].

s177_assert_target(Target) ->
    Functions = generator_test_support:functions(Target),
    [
        ?assertNot(generator_test_support:has_gl_command(Command, Functions))
     || Command <- s177_omitted_commands() ++ s177_robust_texture_blocker_commands()
    ],
    [
        ?assertNot(maps:is_key(Function, Functions))
     || Function <- s177_omitted_public_wrappers()
    ].

s177_assert_missing_commands(Target) ->
    Missing = generator_test_support:missing_commands(Target),
    {_Enums, Commands} = generator_test_support:gl_items(Target),
    [
        ?assert(lists:member(Command, Missing))
     || Command <- s177_omitted_commands() ++ s177_robust_texture_blocker_commands(),
        lists:member(Command, Commands)
    ].

s177_omitted_commands() ->
    [
        "glMapBuffer",
        "glMapBufferRange",
        "glMapNamedBuffer",
        "glMapNamedBufferRange",
        "glUnmapBuffer",
        "glUnmapNamedBuffer",
        "glFlushMappedBufferRange",
        "glFlushMappedNamedBufferRange",
        "glGetBufferPointerv",
        "glGetNamedBufferPointerv",
        "glGetVertexAttribPointerv",
        "glFenceSync",
        "glIsSync",
        "glDeleteSync",
        "glClientWaitSync",
        "glWaitSync",
        "glGetSynciv",
        "glGetSync",
        "glDebugMessageCallback",
        "glObjectPtrLabel",
        "glGetObjectPtrLabel",
        "glGetPointerv"
    ].

s177_robust_texture_blocker_commands() ->
    [
        "glGetnTexImage",
        "glGetnCompressedTexImage"
    ].

s177_omitted_public_wrappers() ->
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

s177_assert_contains(Haystack, Needle) when is_binary(Haystack), is_binary(Needle) ->
    ?assertNotEqual(nomatch, binary:match(Haystack, Needle));
s177_assert_contains(Haystack, Needle) ->
    ?assertNotEqual(nomatch, string:find(Haystack, Needle)).
