%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It does this and that...
%%
-module(indirect_function_resolver).
-export([resolve/4]).

resolve(Functions, EnumTypes, BitfieldTypes, EnumToConstantMap) ->
    % Resolving a function consists of augmenting the existing "function data"
    % with more data.
    maps:fold(fun(Name, Data0, ResolvedFunctions) ->
        ResolvedIndirectFunctionData = resolve_function(
            Name,
            Data0,
            EnumTypes,
            BitfieldTypes,
            EnumToConstantMap
        ),
        Data1 = maps:merge(Data0, ResolvedIndirectFunctionData),
        maps:put(Name,  Data1, ResolvedFunctions)
    end, #{}, Functions).

resolve_function(
    FunctionName,
    FunctionData,
    _EnumTypes,
    _BitfieldTypes,
    _EnumToConstantMap
) ->
    {ExtraType, SpecsParams, SpecsReturn} =
        resolve_function_specs(FunctionName, FunctionData),
    #{
        extra_type => ExtraType,
        specs_params => SpecsParams,
        specs_return => SpecsReturn,
        function_arity => length(SpecsParams),
        function_clauses => resolve_function_clauses(FunctionData),
        nif_functions => resolve_nif_functions(FunctionData)
    }.

resolve_function_specs({FunctionName, _FunctionArity}, FunctionData) ->

    % Define an extra type (named `<function-name>_val()`) if there is more
    % than one variant (to improve readability of the function -specs()
    % attribute).
    Variants = maps:get(variants, FunctionData),
    io:format(user, "variants: ~p~n", [Variants]),
    ExtraType = case Variants of
        [_] ->
            % We do not define an extra type if there is only one variant.
            undefined;
        _ ->
            TypeName = list_to_atom(FunctionName ++ "_value"),
            % XXX: Rework this.
            SetValues = lists:foldl(fun
                ({{gl_vector, N, Type}, element}, Acc) ->
                    Name = lists:flatten(io_lib:format("vector~p", [N])),
                    Node = {undefined, list_to_atom(Name), [{undefined, Type, []}]},
                    [Node|Acc];
                ({{gl_vector, N, Type}, array}, Acc) ->
                    Name = lists:flatten(io_lib:format("vector~p", [N])),
                    Node = {undefined, list_to_atom(Name), [{undefined, Type, []}]},
                    [{list, Node}|Acc];
                ({{gl_matrix, N, N, Type}, array}, Acc) ->
                    Name = lists:flatten(io_lib:format("matrix~p", [N])),
                    Node = {undefined, list_to_atom(Name), [{undefined, Type, []}]},
                    [Node|Acc];
                ({{gl_matrix, N, M, Type}, array}, Acc) ->
                    Name = lists:flatten(io_lib:format("matrix~px~p", [N, M])),
                    Node = {undefined, list_to_atom(Name), [{undefined, Type, []}]},
                    [Node|Acc];
                ({Type, element}, Acc) ->
                    Node = {undefined, Type, []},
                    [Node|Acc];
                ({Type, array}, Acc) ->
                    Node = {undefined, Type, []},
                    [{list, Node}|Acc]
            end, [], Variants),
            TypeSpecs = {set, SetValues},
            {TypeName, TypeSpecs}
    end,

    % Compute the first "suffix" specs parameter (which contributes to picking
    % the right OpenGL function to use).
    AvailableTypes = gather_available_types(maps:get(gl_commands, FunctionData)),
    NormalizedAvailableTypes = specs_normalize_available_types(AvailableTypes),

    io:format(user, "test: ~p~n", [AvailableTypes]),
    io:format(user, "test: ~p~n", [NormalizedAvailableTypes]),
    % Make "type" type specs.


    % SpecsParamXName = "Type",
    AtomsSpecs = lists:map(fun gl_type_to_suffix_atom/1, AvailableTypes),
    SpecsFirstParam = {"Type", {set, AtomsSpecs}},

    % We compute all data to generate the "-specs" attribute in the ".erl"
    % module.
    % TypeSpecsX = {custom, list_to_atom(FunctionName ++ "_value")},
    TypeSpecsX = {undefined, list_to_atom(FunctionName ++ "_value"), []},
    {SpecsParams, SpecsReturn} =
        function_resolver_common:resolve_function_specs(FunctionData, TypeSpecsX),

    {ExtraType, [SpecsFirstParam] ++ SpecsParams, SpecsReturn}.

gl_type_to_suffix_atom(gl_byte) -> b;
gl_type_to_suffix_atom(gl_ubyte) -> ub;
gl_type_to_suffix_atom(gl_short) -> s;
gl_type_to_suffix_atom(gl_ushort) -> us;
gl_type_to_suffix_atom(gl_int) -> i;
gl_type_to_suffix_atom(gl_uint) -> ui;
gl_type_to_suffix_atom(gl_int64) -> i64;
gl_type_to_suffix_atom(gl_uint64) -> ui64;
gl_type_to_suffix_atom(gl_float) -> f;
gl_type_to_suffix_atom(gl_double) -> d.


% -type guard() ::
%     var |
%     {var_element, Index :: pos_integer()} |
%     {is_tuple, [guard_param()]} |
%     {tuple_size, [guard_param()], N :: non_neg_integer()} |
%     {is_integer, [guard_param()]} |
%     {is_float, [guard_param()]}
% .

% resolve_function_clause_guards_2(Leaf, {gl_matrix, M, N, X}) ->
%     ok;

% resolve_function_clause_guards_2(Leaf, {gl_vector, N, X}) ->

% resolve_function_clause_guards_2(Leaf, integer) ->
%     [{is_integer, [Leaf]}];
% resolve_function_clause_guards_2(Leaf, float) ->
%     [{is_float, [Leaf]}].

% resolve_function_clause_guards(TypeX, element) ->
%     resolve_function_clause_guards_2(var, TypeX);
% resolve_function_clause_guards(TypeX, array) ->
%     % To resolve an array, it's the same as resolving an element, except that
%     % the leaf must be `head_var` instead of `var`, and with the additional
%     % "is_list(Val)" guard.
%     Guards = resolve_function_clause_guards_2(head_var, TypeX),
%     [{is_list, var}] ++ Guards.

resolve_function_clause_guards(_, _) ->
    [
        {is_list, var},
        {is_tuple, head_var},
        {tuple_size, head_var, 4},
        {is_tuple, {element, 1, head_var}},
        {tuple_size, {element, 1, head_var}, 4},
        {is_integer, {element, 1, {element, 1, head_var}}}
    ].

resolve_function_clauses(FunctionData) ->
    lists:foldl(fun({GlCommand, TypeX, FormX}, Clauses) ->
        % Suffix = case TypeX of
        %     {gl_vector, _, GlType} ->
        %         gl_type_to_suffix_atom(GlType);
        %     {gl_matrix, _, _, GlType} ->
        %         gl_type_to_suffix_atom(GlType);
        %     _ ->
        %         gl_type_to_suffix_atom(TypeX)
        % end,

        Guards = resolve_function_clause_guards(TypeX, FormX),

        Clause = #{
            guards => Guards,
            params => [
            ],
            raw_function => GlCommand
        },
        [Clause|Clauses]
    end, [], maps:get(gl_commands, FunctionData)).

    % [#{
    %     guards => [],
    %     params => [],
    %     raw_function => "glFooBar"
    % }].

resolve_nif_functions(FunctionData) ->

    % % Needed by all NIF functions (we pre-compute).
    % NifParams1 = lists:map(
    %     fun resolve_nif_parameters/1,
    %     maps:get(params_specs, FunctionData)
    % ),
    % NifFunctions = lists:foldl(fun({Name, Type, Form}, Acc) ->
    %     NifParams2 = resolve_indirect_nif_params(Type, Form),
    %     Arity = length(NifParams1) + length(NifParams2),
    %     Data = #{
    %         arity => Arity,
    %         params => NifParams1 ++ NifParams2,
    %         return => gl_void
    %     },
    %     maps:put(Name, Data, Acc)
    % end, #{}, maps:get(gl_commands, FunctionData)),

    % There is one NIF function per OpenGL function.
    NifFunctions = lists:foldl(fun({NifName, XType, XForm}, Acc) ->
        SolvedParamsSpecs =
            solve_params_specs(maps:get(params_specs, FunctionData), XType, XForm),
        SolvedReturnSpecs =
            solve_return_specs(maps:get(return_specs, FunctionData), XType, XForm),
        {NifParams, NifReturn} = function_resolver_common:resolve_nif_function(
            SolvedParamsSpecs,
            SolvedReturnSpecs
        ),
        NifArity = length(NifParams),

        maps:put(
            NifName,
            #{
                arity => NifArity,
                params => NifParams,
                return => NifReturn
            },
            Acc
        )
    end, #{}, maps:get(gl_commands, FunctionData)),

    NifFunctions.

solve_params_specs(ParamsSpecs, XType, XForm) ->
    lists:map(fun
        ({Direction, Name, gl_x}) ->
            {Direction, Name, solve_gl_x(XForm, XType)};
        (ParamsSpec) ->
            ParamsSpec
    end, ParamsSpecs).

solve_return_specs(gl_x, XType, XForm) ->
    solve_gl_x(XForm, XType);
solve_return_specs(ReturnSpecs, _, _) ->
    ReturnSpecs.

solve_gl_x(element, XType) ->
    XType;
solve_gl_x(array, XType) ->
    {list, XType}.

gather_available_types(GlCommands) ->
    Types = lists:foldl(fun
        ({_GlCommand, {gl_vector, _N, Type}, _Form}, Acc) ->
            sets:add_element(Type, Acc);
        ({_GlCommand, {gl_matrix, _N, _M, Type}, _Form}, Acc) ->
            sets:add_element(Type, Acc);
        ({_GlCommand, Type, _Form}, Acc) ->
            sets:add_element(Type, Acc)
    end, sets:new(), GlCommands),
    sets:to_list(Types).

specs_normalize_available_types(Types) ->
    Tmp1 = lists:foldl(fun
        (gl_short, Acc) ->
            sets:add_element({undefined, integer, []}, Acc);
        (gl_ushort, Acc) ->
            sets:add_element({undefined, pos_integer, []}, Acc);
        (gl_int, Acc) ->
            sets:add_element({undefined, integer, []}, Acc);
        (gl_uint, Acc) ->
            sets:add_element({undefined, pos_integer, []}, Acc);
        (gl_float, Acc) ->
            sets:add_element({undefined, float, []}, Acc);
        (gl_double, Acc) ->
            sets:add_element({undefined, float, []}, Acc);
        (_, Acc) ->
            % XXX: Should not exist.
            sets:add_element({undefined, term, []}, Acc)
    end, sets:new(), Types),

    % XXX: Further remove multually exclusive type specs (like pos_integer() and
    %      integer(), if integer() is allowed, is implies pos_integer()...).
    Tmp2 = Tmp1,


    sets:to_list(Tmp2).