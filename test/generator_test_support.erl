-module(generator_test_support).

-export([
    active_spec_commands/0,
    binding_specs/0,
    desktop_targets/0,
    es_targets/0,
    functions/1,
    gl_items/1,
    gl_specs/0,
    has_gl_command/2,
    missing_commands/1,
    resolve_target/1,
    supported_targets/0,
    target_name/1,
    tmp_root/0
]).

supported_targets() ->
    desktop_targets() ++ es_targets().

desktop_targets() ->
    [
        {gl, {3, 3}},
        {gl, {4, 1}},
        {gl, {4, 6}}
    ].

es_targets() ->
    [
        {gles, {2, 0}},
        {gles, {3, 0}},
        {gles, {3, 1}},
        {gles, {3, 2}}
    ].

target_name({Api, {Major, Minor}}) ->
    lists:flatten(io_lib:format("~p ~p.~p", [Api, Major, Minor])).

tmp_root() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        Dir -> Dir
    end.

resolve_target(Target) ->
    get_or_put({?MODULE, resolve_target, Target}, fun() ->
        GlSpecs = gl_specs(),
        binding_resolver:resolve(Target, gl_items(Target), GlSpecs, binding_specs())
    end).

functions(Target) ->
    maps:get(functions, resolve_target(Target)).

missing_commands(Target) ->
    maps:get(missing_commands, resolve_target(Target)).

active_spec_commands() ->
    {ok, Contents} = file:read_file("binding-specs.conf"),
    Lines = binary:split(Contents, <<"\n">>, [global]),
    lists:usort([
        Command
     || Line <- Lines,
        Command <- [active_spec_command(Line)],
        Command =/= undefined
    ]).

active_spec_command(Line) ->
    case re:run(Line, "^[ \\t]+\\{\"gl([A-Za-z0-9_]+)\"", [{capture, all_but_first, list}]) of
        {match, [Name]} ->
            "gl" ++ Name;
        nomatch ->
            undefined
    end.

gl_specs() ->
    get_or_put({?MODULE, gl_specs}, fun() ->
        {GlSpecs, _} = xmerl_scan:file("gl-specs.xml"),
        GlSpecs
    end).

binding_specs() ->
    get_or_put({?MODULE, binding_specs}, fun() ->
        {ok, [BindingSpecs]} = file:consult("binding-specs.conf"),
        BindingSpecs
    end).

gl_items(Target) ->
    get_or_put({?MODULE, gl_items, Target}, fun() ->
        gl_specs:determine_api_items(Target, gl_specs())
    end).

has_gl_command(Command, Functions) ->
    lists:any(fun({_Function, FunctionData}) ->
        case maps:get(gl_commands, FunctionData, undefined) of
            undefined ->
                maps:get(gl_command, FunctionData, undefined) =:= Command;
            GlCommands ->
                lists:any(fun
                    ({GlCommand, _Type, _Form}) ->
                        GlCommand =:= Command;
                    (GlCommand) ->
                        GlCommand =:= Command
                end, GlCommands)
            end
    end, maps:to_list(Functions)).

get_or_put(Key, Fun) ->
    case persistent_term:get(Key, undefined) of
        undefined ->
            Value = Fun(),
            persistent_term:put(Key, Value),
            Value;
        Value ->
            Value
    end.
