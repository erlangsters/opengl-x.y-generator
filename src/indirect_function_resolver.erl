%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It resolves the "indirect" OpenGL function (the one that have a 1:many
%% mapping), with the help of the binding specs, to a data structure that can
%% be used to generate the binding.
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
    EnumTypes,
    BitfieldTypes,
    EnumToConstantMap
) ->
    {ExtraType, SpecsParams, SpecsReturn} =
        resolve_function_specs(FunctionName, FunctionData),
    #{
        extra_type => ExtraType,
        specs_params => SpecsParams,
        specs_return => SpecsReturn,
        function_arity => length(SpecsParams),
        function_clauses => resolve_function_clauses(
            FunctionData,
            EnumTypes,
            BitfieldTypes,
            EnumToConstantMap
        ),
        nif_functions => resolve_nif_functions(FunctionData)
    }.

resolve_function_specs({FunctionName, _FunctionArity}, FunctionData) ->
    % Define an extra type (named `<function-name>_value()`) if there is more
    % than one variant (to improve readability of the function -specs()
    % attribute).
    Variants = maps:get(variants, FunctionData),
    {ExtraType, TypeSpecsX} = case Variants of
        [Variant] ->
            % We do not define an extra type if there is only one variant.
            {undefined, variant_specs(Variant)};
        _ ->
            TypeName = list_to_atom(FunctionName ++ "_value"),
            SetValues = lists:foldl(fun(Variant, Acc) ->
                [variant_specs(Variant)|Acc]
            end, [], Variants),
            TypeSpecs = {set, SetValues},
            {{TypeName, TypeSpecs}, {undefined, list_to_atom(FunctionName ++ "_value"), []}}
    end,

    % Compute the specs of the first "suffix" parameter (which contributes to
    % picking the right OpenGL function to use).
    AvailableTypes = gather_available_types(maps:get(gl_commands, FunctionData)),

    AtomsSpecs = lists:map(fun gl_type_to_suffix_atom/1, AvailableTypes),
    SpecsFirstParam = {"Type", {set, AtomsSpecs}},

    % We compute all data to generate the "-specs" attribute in the ".erl"
    % file.
    ParamsSpecs = maps:get(params_specs, FunctionData),
    ReturnSpecs = maps:get(return_specs, FunctionData),
    {SpecsParams, SpecsReturn} =
        common_specs_resolver:resolve_function_specs(ParamsSpecs, ReturnSpecs, TypeSpecsX),

    {ExtraType, [SpecsFirstParam] ++ SpecsParams, SpecsReturn}.

variant_specs({{gl_vector, N, Type}, element}) ->
    Name = lists:flatten(io_lib:format("vector~p", [N])),
    TypeNode = specs_resolve_type(Type),
    {undefined, list_to_atom(Name), [TypeNode]};
variant_specs({{gl_vector, N, Type}, single_vector}) ->
    Name = lists:flatten(io_lib:format("vector~p", [N])),
    TypeNode = specs_resolve_type(Type),
    {undefined, list_to_atom(Name), [TypeNode]};
variant_specs({{gl_vector, N, Type}, array}) ->
    Name = lists:flatten(io_lib:format("vector~p", [N])),
    TypeNode = specs_resolve_type(Type),
    Node = {undefined, list_to_atom(Name), [TypeNode]},
    {list, Node};
variant_specs({{gl_vector, N, Type}, counted_array}) ->
    variant_specs({{gl_vector, N, Type}, array});
variant_specs({{gl_matrix, N, N, Type}, element}) ->
    Name = lists:flatten(io_lib:format("matrix~p", [N])),
    TypeNode = specs_resolve_type(Type),
    {undefined, list_to_atom(Name), [TypeNode]};
variant_specs({{gl_matrix, N, M, Type}, element}) ->
    Name = lists:flatten(io_lib:format("matrix~px~p", [N, M])),
    TypeNode = specs_resolve_type(Type),
    {undefined, list_to_atom(Name), [TypeNode]};
variant_specs({{gl_matrix, N, N, Type}, array}) ->
    Name = lists:flatten(io_lib:format("matrix~p", [N])),
    TypeNode = specs_resolve_type(Type),
    Node = {undefined, list_to_atom(Name), [TypeNode]},
    {list, Node};
variant_specs({{gl_matrix, N, M, Type}, array}) ->
    Name = lists:flatten(io_lib:format("matrix~px~p", [N, M])),
    TypeNode = specs_resolve_type(Type),
    Node = {undefined, list_to_atom(Name), [TypeNode]},
    {list, Node};
variant_specs({{gl_matrix, N, M, Type}, counted_array}) ->
    variant_specs({{gl_matrix, N, M, Type}, array});
variant_specs({Type, element}) ->
    specs_resolve_type(Type);
variant_specs({Type, array}) ->
    {list, specs_resolve_type(Type)};
variant_specs({Type, counted_array}) ->
    variant_specs({Type, array});
variant_specs({Type, typed_value_list}) ->
    specs_resolve_type(Type);
variant_specs({Type, typed_value_list_with_size}) ->
    specs_resolve_type(Type);
variant_specs({Type, typed_value_list_with_byte_size}) ->
    specs_resolve_type(Type).

resolve_function_clause_guards_leaf(Leaf, {gl_matrix, M, N, _}) ->
    [
        {is_tuple, Leaf},
        {tuple_size, Leaf, M},
        {is_tuple, {element, 1, Leaf}},
        {tuple_size, {element, 1, Leaf}, N}
    ];
resolve_function_clause_guards_leaf(Leaf, {gl_vector, N, _}) ->
    [
        {is_tuple, Leaf},
        {tuple_size, Leaf, N}
    ];
resolve_function_clause_guards_leaf(_Leaf, _) ->
    [].

resolve_function_clause_guards(Type, element) ->
    resolve_function_clause_guards_leaf(var, Type);
resolve_function_clause_guards(Type, single_vector) ->
    resolve_function_clause_guards_leaf(var, Type);
resolve_function_clause_guards(Type, array) ->
    % To resolve an array, it's the same as resolving an element, except that
    % the leaf must be `head_var` instead of `var`, and with the additional
    % "is_list(Val)" guard.
    Guards = resolve_function_clause_guards_leaf(head_var, Type),
    [{is_list, var}] ++ Guards;
resolve_function_clause_guards(Type, counted_array) ->
    resolve_function_clause_guards(Type, array);
resolve_function_clause_guards(_Type, typed_value_list) ->
    [];
resolve_function_clause_guards(_Type, typed_value_list_with_size) ->
    [];
resolve_function_clause_guards(_Type, typed_value_list_with_byte_size) ->
    [].


is_out_param(ParamsSpecs) ->
    lists:any(fun
        ({out, _Name, gl_x}) ->
            true;
        ({out, _Name, _Type}) ->
            false;
        ({in, _Name, _Type}) ->
            false;
        ({const, _Name, _Type}) ->
            false
    end, ParamsSpecs).

resolve_function_clauses(FunctionData, EnumTypes, BitfieldTypes, EnumToConstantMap) ->
    % There is one function clause per variant of the OpenGL function.
    OrderedCommands = maps:get(gl_commands, FunctionData), % XXX: Need ordering ?

    lists:map(fun({GlCommand, Type, Form}) ->
        % We resolve the guards for the function clause. We need to retrieve
        % the name of the X parameter too (it's used by the guards).
        XName = lists:foldl(fun
            ({_Direction, Name, gl_x}, _Acc) ->
                Name;
            (_, Acc) ->
                Acc
        end, undefined, maps:get(params_specs, FunctionData)),
        HasConstCount = has_const_count(maps:get(params_specs, FunctionData)),

        Guards = case is_out_param(maps:get(params_specs, FunctionData)) of
            true ->
                [];
            false ->
                resolve_function_clause_guards(Type, Form)
        end,

        % We resolve the parameters. In an indirect function, there is the X
        % parameter that needs to be augmented with the type and form so the
        % resolver can know what to do. (The resolver is designed to call the
        % local `resolve_x_param/4` function).


        XParamSpec = case Type of
            {gl_matrix, M, N, GlType} ->
                case Form of
                    element ->
                        {gl_matrix, M, N, GlType};
                    array ->
                        {list, {gl_matrix, M, N, GlType}};
                    counted_array ->
                        {list, {gl_matrix, M, N, GlType}}
                end;
            {gl_vector, N, GlType} ->
                case Form of
                    element ->
                        {gl_vector, N, GlType};
                    single_vector ->
                        {single_vector, N, GlType};
                    array ->
                        {list, {gl_vector, N, GlType}};
                    counted_array ->
                        {list, {gl_vector, N, GlType}}
                end;
            GlType ->
                case Form of
                    element ->
                        GlType;
                    array ->
                        {list, GlType};
                    counted_array ->
                        {list, GlType};
                    typed_value_list ->
                        GlType;
                    typed_value_list_with_size ->
                        GlType;
                    typed_value_list_with_byte_size ->
                        GlType
                end
        end,
        ParamsSpecs = lists:flatmap(fun
            ({out, Name, {typed_value_list, CountName, gl_x}}) ->
                [{out, Name, {typed_value_list, CountName, Type}}];
            ({out, Name, {typed_value_list_with_size, CountName, gl_x}}) ->
                [{out, Name, {typed_value_list_with_size, CountName, Type}}];
            ({out, Name, {typed_value_list_with_byte_size, CountName, gl_x}}) ->
                [{out, Name, {typed_value_list_with_byte_size, CountName, Type}}];
            ({out, Name, gl_x}) ->
                % gl_int and {{list,1},gl_int}
                true = Form =/= element,
                {list, TmpX} = XParamSpec,
                AdjustedXPAramSpec = {{list, 1, "N"}, TmpX},
                % io:format(user, "~p and ~p~n", [TmpX,AdjustedXPAramSpec]),
                % foo = bar,
                [{out, Name, AdjustedXPAramSpec}];
            ({const, "Count", {gl_sizei_constant, _Value}}) when Form =:= counted_array ->
                [{in, "Count", {derived_count, XName}}];
            ({in, Name, gl_x}) when Form =:= counted_array, HasConstCount ->
                [{in, Name, XParamSpec}];
            ({in, Name, gl_x}) when Form =:= counted_array ->
                [
                    {in, "Count", {derived_count, Name}},
                    {in, Name, XParamSpec}
                ];
            ({Direction, Name, gl_x}) ->
                [{Direction, Name, XParamSpec}];
            (Any) ->
                [Any]
        end, maps:get(params_specs, FunctionData)),
        Params = common_clauses_resolver:resolve_params(
            ParamsSpecs,
            EnumTypes,
            BitfieldTypes,
            EnumToConstantMap
        ),

        % We also need to add the first "suffix" parameter (the 'ignore' rule
        % means it won't be passed to the NIF function).
        SuffixName = suffix_name(Type),
        SuffixParam = {atom_to_list(SuffixName), ignore},

        #{
            guard_var => XName,
            guards => Guards,
            params => [SuffixParam] ++ Params,
            raw_function => GlCommand
        }
    end, OrderedCommands).

has_const_count(ParamsSpecs) ->
    lists:any(fun
        ({const, "Count", {gl_sizei_constant, _Value}}) -> true;
        (_) -> false
    end, ParamsSpecs).

suffix_name(Type) ->
    case Type of
        {gl_matrix, _M, _N, GlType} ->
            gl_type_to_suffix_atom(GlType);
        {gl_vector, _N, GlType} ->
            gl_type_to_suffix_atom(GlType);
        (GlType) ->
            gl_type_to_suffix_atom(GlType)
    end.

        % ParamsSpecs = lists:map(fun
        %     ({out, Name, gl_x}) ->
        %         true = Form =/= element,
        %         {list, TmpX} = XParamSpec,
        %         AdjustedXPAramSpec = {{list, 1}, TmpX},
        %         {out, Name, AdjustedXPAramSpec};
        %     ({Direction, Name, gl_x}) ->
        %         {Direction, Name, XParamSpec};
        %     (Any) ->
        %         Any
        % end, maps:get(params_specs, FunctionData)),


solve_params_specs(ParamsSpecs, Type, Form) ->
    HasConstCount = has_const_count(ParamsSpecs),
    XName = gl_x_param_name(ParamsSpecs),
    lists:flatmap(fun
        ({out, Name, {typed_value_list, CountName, gl_x}}) ->
            [{out, Name, {typed_value_list, CountName, Type}}];
        ({out, Name, {typed_value_list_with_size, CountName, gl_x}}) ->
            [{out, Name, {typed_value_list_with_size, CountName, Type}}];
        ({out, Name, {typed_value_list_with_byte_size, CountName, gl_x}}) ->
            [{out, Name, {typed_value_list_with_byte_size, CountName, Type}}];
        ({out, Name, gl_x}) when Form =:= array; Form =:= counted_array ->
            [{out, Name, {{list, 1}, Type}}];
        ({const, "Count", {gl_sizei_constant, _Value}}) when Form =:= counted_array ->
            [{in, "Count", {derived_count, XName}}];
        ({in, Name, gl_x}) when Form =:= counted_array, HasConstCount ->
            [{in, Name, solve_gl_x(Form, Type)}];
        ({in, Name, gl_x}) when Form =:= counted_array ->
            [
                {in, "Count", {derived_count, Name}},
                {in, Name, solve_gl_x(Form, Type)}
            ];
        ({Direction, Name, gl_x}) ->
            [{Direction, Name, solve_gl_x(Form, Type)}];
        (ParamsSpec) ->
            [ParamsSpec]
    end, ParamsSpecs).

gl_x_param_name(ParamsSpecs) ->
    lists:foldl(fun
        ({_Direction, Name, gl_x}, _Acc) -> Name;
        (_, Acc) -> Acc
    end, undefined, ParamsSpecs).

solve_return_specs(gl_x, Type, Form) ->
    solve_gl_x(Form, Type);
solve_return_specs(ReturnSpecs, _, _) ->
    ReturnSpecs.

solve_gl_x(element, Type) ->
    Type;
solve_gl_x(single_vector, {gl_vector, _N, Type}) ->
    {list, Type};
solve_gl_x(array, Type) ->
    {list, Type};
solve_gl_x(counted_array, Type) ->
    {list, Type};
solve_gl_x(typed_value_list, Type) ->
    Type;
solve_gl_x(typed_value_list_with_size, Type) ->
    Type;
solve_gl_x(typed_value_list_with_byte_size, Type) ->
    Type.

resolve_nif_functions(FunctionData) ->
    % There is one NIF function per OpenGL function.
    lists:foldl(fun({GlCommand, Type, Form}, NifFunctions) ->
        NifName = GlCommand,

        % We replace the "gl_x" parameter with the type and form, and we
        % resolve like a direct function.
        % io:format(user, "before params: ~p~n", [maps:get(params_specs, FunctionData)]),
        SolvedParamsSpecs =
            solve_params_specs(maps:get(params_specs, FunctionData), Type, Form),
        % io:format(user, "solved params: ~p~n", [SolvedParamsSpecs]),
        SolvedReturnSpecs =
            solve_return_specs(maps:get(return_specs, FunctionData), Type, Form),
        {NifParams, NifReturn} = common_nifs_resolver:resolve_nif_function(
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
            NifFunctions
        )
    end, #{}, maps:get(gl_commands, FunctionData)).

% ---

gather_available_types(GlCommands) ->
    Types = lists:foldl(fun
        ({_GlCommand, {gl_vector, _N, Type}, _Form}, Acc) ->
            sets:add_element(Type, Acc);
        ({_GlCommand, {gl_matrix, _N, _M, Type}, _Form}, Acc) ->
            sets:add_element(Type, Acc);
        ({_GlCommand, Type, _Form}, Acc) ->
            sets:add_element(Type, Acc)
    end, sets:new(), GlCommands),
    lists:sort(sets:to_list(Types)).


specs_resolve_type(Type) ->
    case Type of
        gl_byte -> {gl, byte, []};
        gl_ubyte -> {gl, ubyte, []};
        gl_short -> {gl, short, []};
        gl_ushort -> {gl, ushort, []};
        gl_int -> {gl, int, []};
        gl_uint -> {gl, uint, []};
        gl_int64 -> {gl, int64, []};
        gl_uint64 -> {gl, uint64, []};
        gl_float -> {gl, float, []};
        gl_double -> {gl, double, []}
    end.

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
