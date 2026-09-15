%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It resolves the OpenGL items (to be included in the binding), with the help
%% of the binding specs, to a data structure that can be used to generate the
%% binding.
%%
-module(binding_resolver).
-export([resolve/4]).

resolve(Target, {GlEnums, GlCommandsDirty}, GlSpecs, BindingSpecs) ->

    GlCommands = dedupe_preserving_order(GlCommandsDirty),


    % To help with resolving, we first extract infos from the OpenGL specs
    % (irrelevantly to what need to be resolved) and place them in data
    % structure easy to work with.
    GlSpecsEnums = gl_specs:read_enums(GlSpecs),
    GlSpecsEnumsValuesMap = gl_specs:enums_values_map(GlSpecsEnums),
    GlSpecsGroupNamesMap = gl_specs:enums_group_names_map(GlSpecsEnums),

    % Compute the list of BEAM functions. So far we have the list of OpenGL
    % functions, but some will be segregated into one BEAM function. It
    % basically reworks the data structure of the binding specs so for each
    % BEAM function to be included in the binding, is associated all the data
    % needed to resolve them on a one-to-one basis.
    FunctionsSpecs = proplists:get_value(functions, BindingSpecs),
    AggregateFunctionsSpecs = proplists:get_value(aggregate_functions, BindingSpecs),
    Result = lists:foldl(fun(GlCommand, {DirectFunctions0, IndirectFunctions0, MissingCmds0}) ->
        case proplists:get_all_values(GlCommand, FunctionsSpecs) of
            [] ->
                % Khronos commands without a binding spec are recorded and
                % skipped. Intentionally omitted families are listed in
                % docs/omissions.md.
                {DirectFunctions0, IndirectFunctions0, [GlCommand|MissingCmds0]};
            FunctionSpecs ->
                lists:foldl(fun(FunctionSpec, Acc) ->
                    resolve_command_function_spec(
                        Target,
                        GlCommand,
                        FunctionSpec,
                        AggregateFunctionsSpecs,
                        Acc
                    )
                end, {DirectFunctions0, IndirectFunctions0, MissingCmds0}, FunctionSpecs)
        end
    end, {#{}, #{}, []}, GlCommands),
    {DirectFunctions, IndirectFunctions, MissingCmds} = Result,
    % io:format(user, "DirectFunctions:~n~p~n", [DirectFunctions]),
    % io:format(user, "IndirectFunctions:~n~p~n", [IndirectFunctions]),
    maybe_print_missing_cmds(MissingCmds),

    ResolvedGlEnums = resolve_constants(GlEnums, GlSpecsEnumsValuesMap),
    % io:format(user, "ResolvedGlEnums:~n~p~n", [ResolvedGlEnums]),

    % We augment the OpenGL API with an extra type for each "OpenGL object".
    % Note that not all versions support those, so we adjust according to the
    % targeted API.
    ObjectTypes0 = [
        {shader,             "An OpenGL shader object."},
        {program,            "An OpenGL program object."},
        {buffer,             "An OpenGL buffer object."},
        {renderbuffer,       "An OpenGL renderbuffer object."},
        {framebuffer,        "An OpenGL framebuffer object."},
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

    % We compute the list of "OpenGL enum" types. For instance, a shader type
    % is a "GLenum" (an integer). We augment the API again by exposing this as
    % an atom to choose from a list of atoms.
    % As a byproduct of this operation, we also construct a map that correlates
    % the BEAM atom name to its integer value.
    {EnumTypes, GlEnumsMap1} = compute_enum_types(
        DirectFunctions,
        IndirectFunctions,
        GlEnums,
        GlSpecsGroupNamesMap
    ),
    % io:format(user, "EnumTypes:~n~p~n", [EnumTypes]),
    % io:format(user, "EnumToConstantMap1:~n~p~n", [EnumToConstantMap1]),

    % We do the same for "OpenGL bitfield" types.
    {BitfieldTypes, GlEnumsMap2} = compute_bitfield_types(
        DirectFunctions,
        IndirectFunctions,
        GlEnums,
        GlSpecsGroupNamesMap
    ),
    % io:format(user, "BitfieldTypes:~n~p~n", [BitfieldTypes]),
    % io:format(user, "EnumToConstantMap2:~n~p~n", [EnumToConstantMap2]),

    GlEnumsMap = maps:merge(
        GlEnumsMap1,
        GlEnumsMap2
    ),
    % io:format(user, "GlEnumsMap:~n~p~n", [GlEnumsMap]),

    % erlang:halt(),

    % Make the computed data available to the resolver functions (to avoid
    % passing them around).
    erlang:put(gl_enums_map, GlEnumsMap),

    erlang:put(enum_types, EnumTypes),
    % erlang:put(gl_specs_enums_values_map, GlSpecsEnumsValuesMap),
    % erlang:put(gl_specs_group_names_map, GlSpecsGroupNamesMap),

    % Resolve direct functions.
    ResolvedDirectFunctions = direct_function_resolver:resolve(
        DirectFunctions,
        EnumTypes,
        BitfieldTypes,
        GlEnumsMap
    ),
    % io:format(user, "ResolvedFunctions:~n~p~n", [ResolvedDirectFunctions]),

    % Resolve indirect functions.
    ResolvedIndirectFunctions = indirect_function_resolver:resolve(
        IndirectFunctions,
        EnumTypes,
        BitfieldTypes,
        GlEnumsMap
    ),
    % io:format(user, "ResolvedFunctions:~n~p~n", [ResolvedIndirectFunctions]),

    #{
        gl_enums_value_map => ResolvedGlEnums,
        gl_enums_name_map => GlEnumsMap,
        object_types => ObjectTypes1,
        enum_types => EnumTypes,
        bitfield_types => BitfieldTypes,
        functions => maps:merge(ResolvedDirectFunctions, ResolvedIndirectFunctions),
        missing_commands => lists:sort(MissingCmds)
    }.

maybe_print_missing_cmds(MissingCmds) ->
    case os:getenv("OPENGL_GEN_DEBUG_MISSING_CMDS") of
        Value when Value =:= "1"; Value =:= "true" ->
            io:format(user, "MissingCmds:~n~p~n", [MissingCmds]);
        _ ->
            ok
    end.

dedupe_preserving_order(Items) ->
    {Deduped, _Seen} = lists:foldl(fun(Item, {Acc, Seen}) ->
        case maps:is_key(Item, Seen) of
            true ->
                {Acc, Seen};
            false ->
                {[Item | Acc], maps:put(Item, true, Seen)}
        end
    end, {[], #{}}, Items),
    lists:reverse(Deduped).

resolve_command_function_spec(
    Target,
    GlCommand,
    FunctionSpecData,
    AggregateFunctionsSpecs,
    {DirectFunctions0, IndirectFunctions0, MissingCmds0}
) ->
    case FunctionSpecData of
        {direct, FunctionSpec} ->
            case direct_function_spec_matches_target(Target, FunctionSpec) of
                false ->
                    {DirectFunctions0, IndirectFunctions0, MissingCmds0};
                true ->
                    FunctionName = proplists:get_value(name, FunctionSpec),
                    FunctionParams = proplists:get_value(params, FunctionSpec),
                    FunctionReturn = proplists:get_value(return, FunctionSpec),
                    ErrorCheck = proplists:get_value(error_check, FunctionSpec, true),
                    RawFunctionName = proplists:get_value(raw_name, FunctionSpec, GlCommand),
                    FunctionArity = public_params_arity(FunctionParams),
                    FunctionDoc = proplists:get_value(doc, FunctionSpec),
                    FunctionExample = proplists:get_value(example, FunctionSpec),
                    FunctionUrl = resolve_doc_url(Target, GlCommand),

                    FunctionKey = {FunctionName, FunctionArity},
                    false = maps:is_key(FunctionKey, IndirectFunctions0),

                    FunctionData = #{
                        gl_command => GlCommand,
                        raw_function => RawFunctionName,
                        params_specs => FunctionParams,
                        return_specs => FunctionReturn,
                        error_check => ErrorCheck,
                        doc_description => FunctionDoc,
                        doc_example => FunctionExample,
                        doc_url => FunctionUrl
                    },
                    DirectFunctions1 = put_direct_function(
                        FunctionKey,
                        FunctionData,
                        DirectFunctions0
                    ),
                    {DirectFunctions1, IndirectFunctions0, MissingCmds0}
            end;
        {indirect, {GlCommandX, TypeX, FormX}} ->
            case proplists:get_value(GlCommandX, AggregateFunctionsSpecs, undefined) of
                undefined ->
                    {DirectFunctions0, IndirectFunctions0, [GlCommandX|MissingCmds0]};
                AggregateFunctionSpec ->
                    FunctionName = proplists:get_value(name, AggregateFunctionSpec),
                    FunctionParams = proplists:get_value(params, AggregateFunctionSpec),
                    FunctionReturn = proplists:get_value(return, AggregateFunctionSpec),
                    FunctionArity = public_params_arity(FunctionParams) + 1,
                    FunctionDoc = proplists:get_value(doc, AggregateFunctionSpec),
                    FunctionExample = proplists:get_value(example, AggregateFunctionSpec),

                    IndirectFunctions1 = case maps:get({FunctionName, FunctionArity}, IndirectFunctions0, undefined) of
                        undefined ->
                            FunctionUrl = resolve_doc_url(Target, GlCommandX),
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
                    {DirectFunctions0, IndirectFunctions1, MissingCmds0}
            end
    end.

put_direct_function(FunctionKey, FunctionData, DirectFunctions) ->
    case maps:get(FunctionKey, DirectFunctions, undefined) of
        undefined ->
            maps:put(FunctionKey, FunctionData, DirectFunctions);
        ExistingFunctionData ->
            maps:put(
                FunctionKey,
                merge_direct_function_overload(
                    FunctionKey,
                    ExistingFunctionData,
                    FunctionData
                ),
                DirectFunctions
            )
    end.

merge_direct_function_overload(FunctionKey, ExistingFunctionData, NewFunctionData) ->
    ExistingOverloads = maps:get(
        direct_overloads,
        ExistingFunctionData,
        [direct_overload_data(ExistingFunctionData)]
    ),
    NewOverload = direct_overload_data(NewFunctionData),
    Overloads = ExistingOverloads ++ [NewOverload],
    assert_compatible_direct_overloads(FunctionKey, Overloads),
    ExistingFunctionData#{
        direct_overloads => Overloads,
        gl_commands => [maps:get(gl_command, Overload) || Overload <- Overloads]
    }.

direct_overload_data(FunctionData) ->
    maps:with(
        [
            gl_command,
            raw_function,
            params_specs,
            return_specs,
            error_check,
            doc_description,
            doc_example,
            doc_url
        ],
        FunctionData
    ).

assert_compatible_direct_overloads(FunctionKey, Overloads) ->
    [First | Rest] = Overloads,
    FirstShape = direct_overload_public_param_shape(First),
    FirstReturn = maps:get(return_specs, First),
    lists:foreach(fun(Overload) ->
        case {
            direct_overload_public_param_shape(Overload),
            maps:get(return_specs, Overload)
        } of
            {FirstShape, FirstReturn} ->
                ok;
            Other ->
                erlang:error({
                    incompatible_direct_overload,
                    FunctionKey,
                    {FirstShape, FirstReturn},
                    Other
                })
        end
    end, Rest).

direct_overload_public_param_shape(Overload) ->
    lists:filtermap(fun
        ({const, _Name, _Specs}) ->
            false;
        ({out, _Name, {gl_scalar, _StorageType, _ReturnType}}) ->
            false;
        ({out, _Name, {gl_enum, _Group}}) ->
            false;
        ({out, _Name, {gl_enum, _Group, _PublicName}}) ->
            false;
        ({out, _Name, {typed_value_list_from_counted_input, _SourceParam, _Type}}) ->
            false;
        ({out, _Name, {active_reflection_info, _TypeSpec}}) ->
            {true, {in, "MaxLength"}};
        ({out, _Name, {active_reflection_info, _SizeType, _TypeSpec}}) ->
            {true, {in, "MaxLength"}};
        ({Direction, Name, _Specs}) ->
            {true, {Direction, Name}}
    end, maps:get(params_specs, Overload)).

direct_function_spec_matches_target({TargetApi, _} = Target, FunctionSpec) ->
    TargetMatches = case proplists:get_value(targets, FunctionSpec, all) of
        all ->
            true;
        Targets when is_list(Targets) ->
            lists:member(Target, Targets)
    end,
    ApiMatches = case proplists:get_value(apis, FunctionSpec, all) of
        all ->
            true;
        TargetApis when is_list(TargetApis) ->
            lists:member(TargetApi, TargetApis)
    end,
    TargetMatches andalso ApiMatches.

public_params_arity(FunctionParams) ->
    length(lists:filter(fun
        ({const, _Name, _Specs}) ->
            false;
        ({out, _Name, {gl_scalar, _StorageType, _ReturnType}}) ->
            false;
        ({out, _Name, {gl_enum, _Group}}) ->
            false;
        ({out, _Name, {gl_enum, _Group, _PublicName}}) ->
            false;
        ({out, _Name, {typed_value_list_from_counted_input, _SourceParam, _Type}}) ->
            false;
        ({out, _Name, shader_precision_format}) ->
            false;
        (_Param) ->
            true
    end, FunctionParams)).

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

% doc_url_suffix("glFooBari") ->
%     "glFooBar";
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

enum_public_type_name({gl_enum, GlEnumGroupName}) ->
    opengl_gen:erlangify_enum_group_name(GlEnumGroupName);
enum_public_type_name({gl_enum, _GlEnumGroupName, PublicName}) when is_atom(PublicName) ->
    atom_to_list(PublicName);
enum_public_type_name({gl_enum, _GlEnumGroupName, PublicName}) ->
    PublicName.

enum_group_names({gl_enum, GlEnumGroupName}) ->
    [GlEnumGroupName];
enum_group_names({gl_enum, GlEnumGroupNames, _PublicName}) when is_list(GlEnumGroupNames) ->
    case GlEnumGroupNames of
        [First | _] when is_list(First) ->
            GlEnumGroupNames;
        _ ->
            [GlEnumGroupNames]
    end.

enum_type_keys(EnumSpec) ->
    PublicTypeName = enum_public_type_name(EnumSpec),
    BaseKeys = case EnumSpec of
        {gl_enum, {values, GlEnumNames}, _PublicName} ->
            [{values, GlEnumNames, PublicTypeName}];
        _ ->
            [{GlEnumGroupName, PublicTypeName} || GlEnumGroupName <- enum_group_names(EnumSpec)]
    end,
    BaseKeys ++ extra_enum_type_keys(PublicTypeName).

extra_enum_type_keys("state_parameter_name") ->
    [{values, state_parameter_name_extra_values(), "state_parameter_name"}];
extra_enum_type_keys("program_pipeline_parameter_name") ->
    [{values, ["GL_VALIDATE_STATUS"], "program_pipeline_parameter_name"}];
extra_enum_type_keys(_) ->
    [].

state_parameter_name_extra_values() ->
    [
        "GL_BLEND",
        "GL_DEBUG_GROUP_STACK_DEPTH",
        "GL_DEBUG_LOGGED_MESSAGES",
        "GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH",
        "GL_DEPTH_WRITEMASK",
        "GL_FRAGMENT_INTERPOLATION_OFFSET_BITS",
        "GL_LINE_WIDTH",
        "GL_MAJOR_VERSION",
        "GL_MAX_3D_TEXTURE_SIZE",
        "GL_MAX_ARRAY_TEXTURE_LAYERS",
        "GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS",
        "GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE",
        "GL_MAX_CLIP_DISTANCES",
        "GL_MAX_COLOR_ATTACHMENTS",
        "GL_MAX_COLOR_TEXTURE_SAMPLES",
        "GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_COMBINED_ATOMIC_COUNTERS",
        "GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES",
        "GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS",
        "GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS",
        "GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS",
        "GL_MAX_COMBINED_IMAGE_UNIFORMS",
        "GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES",
        "GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS",
        "GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS",
        "GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS",
        "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS",
        "GL_MAX_COMBINED_UNIFORM_BLOCKS",
        "GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS",
        "GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_COMPUTE_ATOMIC_COUNTERS",
        "GL_MAX_COMPUTE_IMAGE_UNIFORMS",
        "GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS",
        "GL_MAX_COMPUTE_SHARED_MEMORY_SIZE",
        "GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS",
        "GL_MAX_COMPUTE_UNIFORM_BLOCKS",
        "GL_MAX_COMPUTE_UNIFORM_COMPONENTS",
        "GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS",
        "GL_MAX_CUBE_MAP_TEXTURE_SIZE",
        "GL_MAX_CULL_DISTANCES",
        "GL_MAX_DEBUG_GROUP_STACK_DEPTH",
        "GL_MAX_DEBUG_LOGGED_MESSAGES",
        "GL_MAX_DEBUG_MESSAGE_LENGTH",
        "GL_MAX_DEPTH_TEXTURE_SAMPLES",
        "GL_MAX_DRAW_BUFFERS",
        "GL_MAX_DUAL_SOURCE_DRAW_BUFFERS",
        "GL_MAX_ELEMENT_INDEX",
        "GL_MAX_ELEMENTS_INDICES",
        "GL_MAX_ELEMENTS_VERTICES",
        "GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_FRAGMENT_ATOMIC_COUNTERS",
        "GL_MAX_FRAGMENT_IMAGE_UNIFORMS",
        "GL_MAX_FRAGMENT_INPUT_COMPONENTS",
        "GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS",
        "GL_MAX_FRAGMENT_UNIFORM_BLOCKS",
        "GL_MAX_FRAGMENT_UNIFORM_COMPONENTS",
        "GL_MAX_FRAGMENT_UNIFORM_VECTORS",
        "GL_MAX_FRAMEBUFFER_HEIGHT",
        "GL_MAX_FRAMEBUFFER_LAYERS",
        "GL_MAX_FRAMEBUFFER_SAMPLES",
        "GL_MAX_FRAMEBUFFER_WIDTH",
        "GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_GEOMETRY_ATOMIC_COUNTERS",
        "GL_MAX_GEOMETRY_IMAGE_UNIFORMS",
        "GL_MAX_GEOMETRY_INPUT_COMPONENTS",
        "GL_MAX_GEOMETRY_OUTPUT_COMPONENTS",
        "GL_MAX_GEOMETRY_OUTPUT_VERTICES",
        "GL_MAX_GEOMETRY_SHADER_INVOCATIONS",
        "GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS",
        "GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS",
        "GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS",
        "GL_MAX_GEOMETRY_UNIFORM_BLOCKS",
        "GL_MAX_GEOMETRY_UNIFORM_COMPONENTS",
        "GL_MAX_IMAGE_UNITS",
        "GL_MAX_INTEGER_SAMPLES",
        "GL_MAX_LABEL_LENGTH",
        "GL_MAX_PATCH_VERTICES",
        "GL_MAX_TESS_PATCH_COMPONENTS",
        "GL_MAX_PROGRAM_TEXEL_OFFSET",
        "GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET",
        "GL_MAX_RECTANGLE_TEXTURE_SIZE",
        "GL_MAX_RENDERBUFFER_SIZE",
        "GL_MAX_SAMPLE_MASK_WORDS",
        "GL_MAX_SAMPLES",
        "GL_MAX_SHADER_STORAGE_BLOCK_SIZE",
        "GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS",
        "GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS",
        "GL_MAX_SUBROUTINES",
        "GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS",
        "GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS",
        "GL_MAX_TESS_CONTROL_INPUT_COMPONENTS",
        "GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS",
        "GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS",
        "GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS",
        "GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS",
        "GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS",
        "GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS",
        "GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS",
        "GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS",
        "GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS",
        "GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS",
        "GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS",
        "GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS",
        "GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS",
        "GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS",
        "GL_MAX_TESS_GEN_LEVEL",
        "GL_MAX_TEXTURE_BUFFER_SIZE",
        "GL_MAX_TEXTURE_IMAGE_UNITS",
        "GL_MAX_TEXTURE_SIZE",
        "GL_MAX_TRANSFORM_FEEDBACK_BUFFERS",
        "GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS",
        "GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS",
        "GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS",
        "GL_MAX_UNIFORM_BLOCK_SIZE",
        "GL_MAX_UNIFORM_BUFFER_BINDINGS",
        "GL_MAX_UNIFORM_LOCATIONS",
        "GL_MAX_VARYING_COMPONENTS",
        "GL_MAX_VARYING_VECTORS",
        "GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS",
        "GL_MAX_VERTEX_ATOMIC_COUNTERS",
        "GL_MAX_VERTEX_ATTRIB_BINDINGS",
        "GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET",
        "GL_MAX_VERTEX_ATTRIB_STRIDE",
        "GL_MAX_VERTEX_ATTRIBS",
        "GL_MAX_VERTEX_IMAGE_UNIFORMS",
        "GL_MAX_VERTEX_OUTPUT_COMPONENTS",
        "GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS",
        "GL_MAX_VERTEX_STREAMS",
        "GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS",
        "GL_MAX_VERTEX_UNIFORM_BLOCKS",
        "GL_MAX_VERTEX_UNIFORM_COMPONENTS",
        "GL_MAX_VERTEX_UNIFORM_VECTORS",
        "GL_MAX_VIEWPORTS",
        "GL_MIN_PROGRAM_TEXEL_OFFSET",
        "GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET",
        "GL_MINOR_VERSION",
        "GL_NUM_COMPRESSED_TEXTURE_FORMATS",
        "GL_NUM_EXTENSIONS",
        "GL_NUM_PROGRAM_BINARY_FORMATS",
        "GL_NUM_SHADER_BINARY_FORMATS",
        "GL_NUM_SHADING_LANGUAGE_VERSIONS",
        "GL_PACK_ALIGNMENT",
        "GL_PACK_IMAGE_HEIGHT",
        "GL_PACK_ROW_LENGTH",
        "GL_PACK_SKIP_IMAGES",
        "GL_PACK_SKIP_PIXELS",
        "GL_PACK_SKIP_ROWS",
        "GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED",
        "GL_SAMPLE_BUFFERS",
        "GL_SAMPLES",
        "GL_SHADER_COMPILER",
        "GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT",
        "GL_SUBPIXEL_BITS",
        "GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT",
        "GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT",
        "GL_UNPACK_ALIGNMENT",
        "GL_UNPACK_IMAGE_HEIGHT",
        "GL_UNPACK_ROW_LENGTH",
        "GL_UNPACK_SKIP_IMAGES",
        "GL_UNPACK_SKIP_PIXELS",
        "GL_UNPACK_SKIP_ROWS",
        "GL_VIEWPORT_SUBPIXEL_BITS"
    ].

add_enum_type_keys(EnumSpec, Acc) ->
    lists:foldl(fun(EnumTypeKey, Acc1) ->
        sets:add_element(EnumTypeKey, Acc1)
    end, Acc, enum_type_keys(EnumSpec)).

compute_enum_types(DirectFunctions, IndirectFunctions, GlEnums, GlGroupNamesMap) ->
    % Iterate over the return value and the parameters of all functions, and
    % collect all "GLenum" parameters. We're interested retrieving the group
    % name (not the enum name) as it dictates the possible values.
    GlEnumGroups = maps:fold(fun(_FunctionName, FunctionData, Acc1) ->
        Acc3 = lists:foldl(fun
            ({in, _Name, {gl_enum, _GlEnumGroupName} = EnumSpec}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({in, _Name, {gl_enum, _GlEnumGroupName, _PublicName} = EnumSpec}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({in, _Name, {counted_list, _CountName, {gl_enum, _GlEnumGroupName} = EnumSpec}}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({in, _Name, {counted_list, _CountName, {gl_enum, _GlEnumGroupName, _PublicName} = EnumSpec}}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({in, _Name, {gl_enum_list_with_count, GlEnumGroupName, PublicName}}, Acc2) ->
                add_enum_type_keys({gl_enum, GlEnumGroupName, PublicName}, Acc2);
            ({out, _Name, {gl_enum, _GlEnumGroupName} = EnumSpec}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({out, _Name, {gl_enum, _GlEnumGroupName, _PublicName} = EnumSpec}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({out, _Name, {active_reflection_info, EnumSpec}}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({out, _Name, {active_reflection_info, _SizeType, EnumSpec}}, Acc2) ->
                add_enum_type_keys(EnumSpec, Acc2);
            ({out, _Name, {debug_message_log, _MaxMessagesName, _MessageLogSizeName}}, Acc2) ->
                Acc3 = add_enum_type_keys({gl_enum, "DebugSource", debug_source}, Acc2),
                Acc4 = add_enum_type_keys({gl_enum, "DebugType", debug_type}, Acc3),
                add_enum_type_keys({gl_enum, "DebugSeverity", debug_severity}, Acc4);
            (_, Acc2) ->
                Acc2
        end, Acc1, maps:get(params_specs, FunctionData)),
        case maps:get(return_specs, FunctionData) of
            {_Name, {gl_enum, _GlEnumGroupName} = EnumSpec} ->
                add_enum_type_keys(EnumSpec, Acc3);
            {_Name, {gl_enum, _GlEnumGroupName, _PublicName} = EnumSpec} ->
                add_enum_type_keys(EnumSpec, Acc3);
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
    sets:fold(fun
        ({values, GlEnumNames, PublicTypeName}, {EnumTypes, EnumToConstantMap}) ->
            FilteredValues = lists:filter(fun(Value) ->
                lists:member(Value, GlEnums)
            end, GlEnumNames),
            ErlangEnumValues = lists:map(
                fun opengl_gen:erlangify_enum_name/1,
                FilteredValues
            ),

            UpdateEnumTypes = maps:update_with(
                PublicTypeName,
                fun(ExistingValues) ->
                    dedupe_preserving_order(ExistingValues ++ ErlangEnumValues)
                end,
                ErlangEnumValues,
                EnumTypes
            ),
            UpdateEnumToConstantMap = lists:foldl(fun(GlEnumName, Acc) ->
                ErlangName = opengl_gen:erlangify_enum_name(GlEnumName),
                ConstantName = GlEnumName,
                maps:put(ErlangName, ConstantName, Acc)
            end, EnumToConstantMap, FilteredValues),
            {UpdateEnumTypes, UpdateEnumToConstantMap};
        ({GlEnumGroupName, PublicTypeName}, {EnumTypes, EnumToConstantMap}) ->
        % Compute the list of values for this group. We must only include the
        % values that are used in the OpenGL API we're targeting.
        Values = maps:get(GlEnumGroupName, GlGroupNamesMap),
        FilteredValues = lists:filter(fun(Value) ->
            lists:member(Value, GlEnums)
        end, Values),
        ErlangEnumValues = lists:map(
            fun opengl_gen:erlangify_enum_name/1,
            FilteredValues
        ),

        UpdateEnumTypes = maps:update_with(
            PublicTypeName,
            fun(ExistingValues) ->
                dedupe_preserving_order(ExistingValues ++ ErlangEnumValues)
            end,
            ErlangEnumValues,
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
            ({in, _Name, {gl_bitfield, GlEnumGroupName}}, Acc2) ->
                sets:add_element(GlEnumGroupName, Acc2);
            (_, Acc2) ->
                Acc2
        end, Acc1, maps:get(params_specs, FunctionData)),
        case maps:get(return_specs, FunctionData) of
            {_Name, {gl_bitfield, GlEnumGroupName}} ->
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
