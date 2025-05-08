%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It does this and that...
%%
-module(binding_resolver).
-export([resolve/4]).

% XXX
-type data() :: #{
    enum_integers_map => #{string() => string()},
    enums_map => #{string() => string()},
    object_types => [string()],
    enum_types => #{string() => [string()]},
    bitfield_types => #{string() => [string()]},
    extra_types => term(),
    functions => maps:map()
}.

-spec resolve(term(), term(), xmerl:element(), term()) -> ok.
resolve(Target, {GlEnums, GlCommands}, GlSpecs, BindingSpecs) ->
    % To help with resolving, we first extract infos from the OpenGL specs
    % (irrelevantly to what need to be resolved) and place them in data
    % structure easy to work with.
    GlSpecsEnums = gl_specs:read_enums(GlSpecs),
    GlSpecsEnumsValuesMap = gl_specs:enums_values_map(GlSpecsEnums),
    GlSpecsGroupNamesMap = gl_specs:enums_group_names_map(GlSpecsEnums),
    % XXX: GlSpecsCommands = gl_specs:read_commands(GlSpecs),

    % Compute the list of BEAM functions. So far we have the list of OpenGL
    % functions, but some will be segregated into one BEAM function. It
    % basically reworks the data structure of the binding specs so for each
    % BEAM function to be included in the binding, is associated all the data
    % needed to resolve them on a one-to-one basis.
    FunctionsSpecs = proplists:get_value(functions, BindingSpecs),
    AggregateFunctionsSpecs = proplists:get_value(aggregate_functions, BindingSpecs),
    {DirectFunctions, IndirectFunctions0} = lists:foldl(fun(GlCommand, {DirectFunctions0, IndirectFunctions0}) ->
        case proplists:get_value(GlCommand, FunctionsSpecs, undefined) of
            undefined ->
                io:format(
                    "/!\\ No function spec for '~s' OpenGL command~n",
                    [GlCommand]
                ),
                {DirectFunctions0, IndirectFunctions0};
            {direct, FunctionSpec} ->
                % It's a "direct" function (it's a 1:1 mapping).
                FunctionName = proplists:get_value(name, FunctionSpec),
                FunctionParams = proplists:get_value(params, FunctionSpec),
                FunctionReturn = proplists:get_value(return, FunctionSpec),

                % Note that it's the pre-resolved arity of the function (it's
                % just a mean to distinguish different functions that share the
                % same name).
                FunctionArity = length(FunctionParams),

                FunctionDoc = proplists:get_value(doc, FunctionSpec),
                FunctionExample = proplists:get_value(example, FunctionSpec),

                % While we're at it, we resolve the documentation-related part
                % of the function.
                FunctionUrl = resolve_doc_url(Target, GlCommand),

                % If binding specs is correct, there should be no
                % duplicate.
                % false = maps:is_key(FunctionName, DirectFunctions0),
                % false = maps:is_key(FunctionName, IndirectFunctions0),


                DirectFunctions1 = maps:put({FunctionName, FunctionArity}, #{
                    gl_command => GlCommand,
                    params_specs => FunctionParams,
                    return_specs => FunctionReturn,
                    doc_description => FunctionDoc,
                    doc_example => FunctionExample,
                    doc_url => FunctionUrl
                }, DirectFunctions0),
                {DirectFunctions1, IndirectFunctions0};
            {indirect, {GlCommandX, TypeX, FormX}} ->
                % It's an "indirect" function (it's a 1:many mapping).
                case proplists:get_value(GlCommandX, AggregateFunctionsSpecs, undefined) of
                    undefined ->
                        io:format(
                            "/!\\ No aggregate function spec for '~s' OpenGL command~n",
                            [GlCommand]
                        ),
                        {DirectFunctions0, IndirectFunctions0};
                    AggregateFunctionSpec ->
                        FunctionName = proplists:get_value(name, AggregateFunctionSpec),
                        FunctionParams = proplists:get_value(params, AggregateFunctionSpec),
                        FunctionReturn = proplists:get_value(return, AggregateFunctionSpec),

                        % Note that it's the pre-resolved arity of the function
                        % (it's just a mean to distinguish different functions
                        % that share the same name).
                        FunctionArity = length(FunctionParams),

                        FunctionDoc = proplists:get_value(doc, AggregateFunctionSpec),
                        FunctionExample = proplists:get_value(example, AggregateFunctionSpec),

                        % % While we're at it, we resolve the documentation-related part
                        % % of the function.
                        % FunctionUrl = resolve_doc_url(Target, GlCommand),
                        FunctionUrl = "https://docs.gl/gl4/glFoobar",

                        IndirectFunctions1 = case maps:get({FunctionName, FunctionArity}, IndirectFunctions0, undefined) of
                            undefined ->
                                % First time we see this function.
                                maps:put({FunctionName, FunctionArity}, #{
                                    gl_commands => [{GlCommand, TypeX, FormX}],
                                    variants => [{TypeX, FormX}],
                                    params_specs => FunctionParams,
                                    return_specs => FunctionReturn,
                                    doc_description => FunctionDoc,
                                    doc_example => FunctionExample,
                                    doc_url => FunctionUrl
                                }, IndirectFunctions0);
                            FunctionData0 ->
                                #{
                                    gl_commands := GlCommandXs,
                                    variants := Variants
                                } = FunctionData0,
                                FunctionData1 = maps:merge(FunctionData0, #{
                                    gl_commands => [{GlCommand, TypeX, FormX} | GlCommandXs],
                                    variants => [{TypeX, FormX}|Variants]
                                }),
                                maps:put({FunctionName, FunctionArity}, FunctionData1, IndirectFunctions0)
                        end,
                        {DirectFunctions0, IndirectFunctions1}
                end
        end
    end, {#{}, #{}}, GlCommands),
    io:format(user, "DirectFunctions:~n~p~n", [DirectFunctions]),
    % io:format(user, "IndirectFunctions:~n~p~n", [IndirectFunctions]),
    IndirectFunctions = #{},

    Constants = resolve_constants(GlEnums, GlSpecsEnumsValuesMap),
    % io:format(user, "Constants:~n~p~n", [Constants]),

    % We augment the OpenGL API with an extra type for each "OpenGL object".
    % Note that not all versions support those, so we adjust according to the
    % targeted API.
    ObjectTypes0 = [
        {shader,             "An OpenGL shader object."},
        {program,            "An OpenGL program object."},
        {buffer,             "An OpenGL buffer object."},
        {render_buffer,      "An OpenGL render buffer object."},
        {frame_buffer,       "An OpenGL frame buffer object."},
        {vertex_array,       "An OpenGL vertex array object."},
        {texture,            "An OpenGL texture object."},
        {query,              "An OpenGL query object."},
        {sync,               "An OpenGL sync object."},
        {sampler,            "An OpenGL sampler object."},
        {transform_feedback, "An OpenGL transform feedback object."},
        {program_pipeline,   "An OpenGL program pipeline object."}
    ],
    % (Adjust this if you're modifying the generator to support earlier versions
    % of OpenGL and OpenGL ES.)
    ObjectsToRemove = case Target of
        {gl, {3, 0}} ->
            [sync, sampler, program_pipeline];
        {gl, {3, 1}} ->
            [sync, sampler, program_pipeline];
        {gl, {3, 2}} ->
            [sampler, program_pipeline];
        {gl, {3, 3}} ->
            [program_pipeline];
        {gl, {4, 0}} ->
            [program_pipeline];
        {gles, {2, 0}} ->
            [vertex_array, query, sync, sampler, transform_feedback, program_pipeline];
        {gles, {3, 0}} ->
            [program_pipeline];
        _ ->
            []
    end,
    ObjectTypes1 = lists:foldl(fun(ObjectToRemove, ObjectTypes) ->
        proplists:delete(ObjectToRemove, ObjectTypes)
    end, ObjectTypes0, ObjectsToRemove),

    % XXX: Add location too ?

    % We compute the list of "OpenGL enum" types. For instance, a shader type
    % is a "GLenum" (an integer). We augment the API again by exposing this as
    % an atom to choose from a list of atoms.
    % As a byproduct of this operation, we also construct a map that correlates
    % the Erlang atom name to its "constant" version.
    {EnumTypes, EnumToConstantMap1} = compute_enum_types(
        DirectFunctions,
        IndirectFunctions,
        GlEnums,
        GlSpecsGroupNamesMap
    ),
    % io:format(user, "EnumTypes:~n~p~n", [EnumTypes]),
    % io:format(user, "EnumToConstantMap1:~n~p~n", [EnumToConstantMap1]),

    {BitfieldTypes, EnumToConstantMap2} = compute_bitfield_types(
        DirectFunctions,
        IndirectFunctions,
        GlEnums,
        GlSpecsGroupNamesMap
    ),
    % io:format(user, "BitfieldTypes:~n~p~n", [BitfieldTypes]),
    % io:format(user, "EnumToConstantMap2:~n~p~n", [EnumToConstantMap2]),

    EnumToConstantMap = maps:merge(
        EnumToConstantMap1,
        EnumToConstantMap2
    ),
    % io:format(user, "EnumToConstantMap:~n~p~n", [EnumToConstantMap]),
    % erlang:halt(),

    % % Resolve functions (in-place).
    ResolvedDirectFunctions = direct_function_resolver:resolve(
        DirectFunctions,
        EnumTypes,
        BitfieldTypes,
        EnumToConstantMap
    ),
    % io:format(user, "ResolvedFunctions:~n~p~n", [ResolvedFunctions]),

    ResolvedIndirectFunctions = indirect_function_resolver:resolve(
        IndirectFunctions,
        EnumTypes,
        BitfieldTypes,
        EnumToConstantMap
    ),
    io:format(user, "ResolvedFunctions:~n~p~n", [ResolvedIndirectFunctions]),

    % Gather the extra types from the resolved indirect functions.
    ExtraTypes = gather_extra_types(ResolvedIndirectFunctions),


    #{
        constants => Constants,
        object_types => ObjectTypes1,
        enum_types => EnumTypes,
        bitfield_types => BitfieldTypes,
        extra_types => ExtraTypes,
        functions => maps:merge(ResolvedDirectFunctions, ResolvedIndirectFunctions)
    }.

doc_url_prefix({gl, {4, _}}) ->
    "https://docs.gl/gl4/";
doc_url_prefix({gl, {3, _}}) ->
    "https://docs.gl/gl3/";
doc_url_prefix({gl, {2, _}}) ->
    "https://docs.gl/gl2/";
doc_url_prefix({gles, {3, _}}) ->
    "https://docs.gl/es3/";
doc_url_prefix({gles, {2, _}}) ->
    "https://docs.gl/es2/".

doc_url_suffix("glFooBari") ->
    "glFooBar";
doc_url_suffix(GlCommand) ->
    GlCommand.

resolve_doc_url(Target, GlCommand) ->
    doc_url_prefix(Target) ++ doc_url_suffix(GlCommand).

resolve_constants(GlEnums, GlEnumsValuesMap) ->
    % Given the OpenGL constants to be included in the bindings (e.g: GL_RGB8),
    % and the global map that associates all OpenGL constants to their
    % hexadecimal values (e.g: 0x8235), we build a list of values suitable to
    % generate the Erlang constants (e.g: `-define(GL_RGB8, 16#8235).`).
    lists:foldl(fun(GlEnum, Acc) ->
        Value = case maps:get(GlEnum, GlEnumsValuesMap) of
            [$0, $x|Part] ->
                [$1, $6, $# | Part];
            RawValue ->
                % Warning: some values are expressed in decimal formats
                % (e.g: GL_FALSE = 0, GL_TRUE = 1, GL_NONE = 0, etc).
                RawValue
        end,
        maps:put(GlEnum, Value, Acc)
    end, #{}, GlEnums).

compute_enum_types(DirectFunctions, IndirectFunctions, GlEnums, GlGroupNamesMap) ->
    % Iterate over the return value and the parameters of all functions, and
    % collect all "GLenum" parameters. We're interested retrieving the group
    % name (not the enum name) as it dictates the possible values.
    GlEnumGroups = maps:fold(fun(_FunctionName, FunctionData, Acc1) ->
        Acc3 = lists:foldl(fun
            ({in, _ParamName, {gl_enum, GlEnumGroupName}}, Acc2) ->
                sets:add_element(GlEnumGroupName, Acc2);
            (_, Acc2) ->
                Acc2
        end, Acc1, maps:get(params_specs, FunctionData)),
        case maps:get(return_specs, FunctionData) of
            {gl_enum, GlEnumGroupName} ->
                sets:add_element(GlEnumGroupName, Acc3);
            _ ->
                Acc3
        end
    end, sets:new(), maps:merge(DirectFunctions, IndirectFunctions)),

    % For all needed OpenGL enum groups, we compute the list of possible
    % values (and we also "Erlangify" the names).
    % For instance, the "ShaderType" group has values "GL_GEOMETRY_SHADER",
    % "GL_VERTEX_SHADER", etc. We want to have a type `shader_type()` allowing
    % only atoms `geometry_shader`, `vertex_shader`, etc. We also construct a
    % map that associates the Erlang atom name to the OpenGL constant name
    % (e.g: `geometry_shader` -> `GL_GEOMETRY_SHADER`).
    sets:fold(fun(GlEnumGroupName, {EnumTypes, EnumToConstantMap}) ->
        % Compute the list of values for this group. We must only include the
        % values that are used in the OpenGL API we're targeting.
        Values = maps:get(GlEnumGroupName, GlGroupNamesMap),
        FilteredValues = lists:filter(fun(Value) ->
            lists:member(Value, GlEnums)
        end, Values),

        UpdateEnumTypes = maps:put(
            opengl_gen:erlangify_enum_group_name(GlEnumGroupName),
            lists:map(
                fun opengl_gen:erlangify_enum_name/1,
                FilteredValues
            ),
            EnumTypes
        ),
        UpdateEnumToConstantMap = lists:foldl(fun(GlEnumName, Acc) ->
            ErlangName = opengl_gen:erlangify_enum_name(GlEnumName),
            ConstantName = GlEnumName,
            maps:put(ErlangName, ConstantName, Acc)
        end, EnumToConstantMap, FilteredValues),
        {UpdateEnumTypes, UpdateEnumToConstantMap}
    end, {#{}, #{}}, GlEnumGroups).

compute_bitfield_types(DirectFunctions, IndirectFunctions, GlEnums, GlGroupNamesMap) ->
    % Iterate over the return value and the parameters of all functions, and
    % collect all "GLbitfield" parameters. We're interested retrieving the
    % group name (not the enum name) as it dictates the possible values.
    GlEnumGroups = maps:fold(fun(_FunctionName, FunctionData, Acc1) ->
        Acc3 = lists:foldl(fun
            ({in, _ParamName, {gl_bitfield, GlEnumGroupName}}, Acc2) ->
                sets:add_element(GlEnumGroupName, Acc2);
            (_, Acc2) ->
                Acc2
        end, Acc1, maps:get(params_specs, FunctionData)),
        case maps:get(return_specs, FunctionData) of
            {gl_bitfield, GlEnumGroupName} ->
                sets:add_element(GlEnumGroupName, Acc3);
            _ ->
                Acc3
        end
    end, sets:new(), maps:merge(DirectFunctions, IndirectFunctions)),

    % For all needed OpenGL bitfield groups, we compute the list of possible
    % values (and we also "Erlangify" the names).
    % For instance, the "ClearBufferMask" group has values
    % "GL_DEPTH_BUFFER_BIT", "GL_ACCUM_BUFFER_BIT", etc. We want to have a type
    % `clear_buffer_mask()` allowing only a list of atoms `depth_buffer_bit`,
    % `accum_buffer-bit`, etc. We also construct a map that associates the
    % Erlang atom name to the OpenGL constant name (e.g: `depth_buffer_bit` ->
    % `GL_DEPTH_BUFFER_BIT`).
    sets:fold(fun(GlEnumGroupName, {BitfieldTypes, EnumToConstantMap}) ->
        % Compute the list of values for this group. We must only include the
        % values that are used in the OpenGL API we're targeting.
        Values = maps:get(GlEnumGroupName, GlGroupNamesMap),
        FilteredValues = lists:filter(fun(Value) ->
            lists:member(Value, GlEnums)
        end, Values),

        UpdateBitfieldTypes = maps:put(
            opengl_gen:erlangify_enum_group_name(GlEnumGroupName),
            lists:map(
                fun opengl_gen:erlangify_enum_name/1,
                FilteredValues
            ),
            BitfieldTypes
        ),
        UpdateEnumToConstantMap = lists:foldl(fun(GlEnumName, Acc) ->
            ErlangName = opengl_gen:erlangify_enum_name(GlEnumName),
            ConstantName = GlEnumName,
            maps:put(ErlangName, ConstantName, Acc)
        end, EnumToConstantMap, FilteredValues),
        {UpdateBitfieldTypes, UpdateEnumToConstantMap}
    end, {#{}, #{}}, GlEnumGroups).

gather_extra_types(IndirectFunctions) ->
    % When indirect functions have more than one variant, they define an extra
    % type to make the function specs (the `-specs` attribute) more readable.
    maps:fold(fun(_FunctionName, FunctionData, ExtraTypes) ->
        case maps:get(extra_type, FunctionData) of
            undefined ->
                ExtraTypes;
            ExtraType ->
                [ExtraType|ExtraTypes]
        end
    end, [], IndirectFunctions).
