%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% Common logic related to resolving function specs (used to resolve both
%% direct and indirect functions).
%%
-module(common_specs_resolver).
-export([resolve_function_specs/2, resolve_function_specs/3]).

resolve_function_specs(ParamsSpecs, ReturnSpecs) ->
    % If there is no" gl_x" in the param and return specs, the second parameter
    % is unused and can be null.
    resolve_function_specs(ParamsSpecs, ReturnSpecs, undefined).

resolve_function_specs(ParamsSpecs, ReturnSpecs, TypeSpecsX) ->

    % We iterate over the list of parameters (as specified by the binding
    % specs) and build a list of actual parameters and return values. If there
    % is no return value, the function return 'ok'. If there is one or more
    % return values, the function return a tuple {ok, V1, V2, ...}.
    {SpecsParamsTmp, SpecsReturn0Tmp} = lists:foldl(
        fun resolve_function_specs_param_fold/2,
        {[], []},
        ParamsSpecs
    ),
    SpecsParams = lists:map(fun
        ({Name, gl_x}) -> {Name, TypeSpecsX};
        (Any) -> Any
    end, SpecsParamsTmp),
    SpecsReturn0 = lists:map(fun
        ({Name, gl_x}) -> {Name, TypeSpecsX};
        ({Name, {list, gl_x}}) -> {Name, {list, TypeSpecsX}};
        (Any) -> Any
    end, SpecsReturn0Tmp),

    % If there is no "out" parameters, perhaps a return value is specified
    % instead ("out" parameters and a specified return value are mutually
    % exclusive).
    SpecsReturn1 = case {SpecsReturn0, ReturnSpecs} of
        {[], gl_void} ->
            % No "out" parameters and no return value specified. It means there
            % really is not any return value.
            [];
        {_, gl_void} ->
            SpecsReturn0;
        {_, {_, debug_message_log_count}} ->
            % glGetDebugMessageLog returns the number of drained messages, but
            % public callers receive only the message records.
            SpecsReturn0;
        {[], {Name, Specs}} ->
            % No "out" parameters, but a return value is specified.
            [{Name, resolve_param_to_type_specs(Specs)}]
    end,

    {lists:reverse(SpecsParams), lists:reverse(SpecsReturn1)}.

resolve_param_to_type_specs(gl_bool) ->
    {gl, boolean, []};
resolve_param_to_type_specs(gl_byte) ->
    {gl, byte, []};
resolve_param_to_type_specs(gl_ubyte) ->
    {gl, ubyte, []};
resolve_param_to_type_specs(gl_short) ->
    {gl, short, []};
resolve_param_to_type_specs(gl_ushort) ->
    {gl, ushort, []};
resolve_param_to_type_specs(gl_int) ->
    {gl, int, []};
resolve_param_to_type_specs(gl_int64) ->
    {gl, int64, []};
resolve_param_to_type_specs(gl_uint) ->
    {gl, uint, []};
resolve_param_to_type_specs(gl_uint_positive) ->
    {undefined, pos_integer, []};
resolve_param_to_type_specs(gl_enum_value) ->
    % Runtime-defined GLenum tokens, such as program binary formats, cannot be
    % represented as generated atom enums.
    {gl, uint, []};
resolve_param_to_type_specs(gl_uint64) ->
    {gl, uint64, []};
resolve_param_to_type_specs(gl_float) ->
    {gl, float, []};
resolve_param_to_type_specs(gl_double) ->
    {gl, double, []};
resolve_param_to_type_specs(gl_sizei) ->
    {gl, sizei, []};
resolve_param_to_type_specs(gl_intptr) ->
    {gl, intptr, []};
resolve_param_to_type_specs(gl_sizeiptr) ->
    {gl, sizeiptr, []};
resolve_param_to_type_specs({fixed_integer, Value}) when is_integer(Value) ->
    Value;

resolve_param_to_type_specs(gl_offset) ->
    % Example: glVertexAttribPointer(). Public callers pass a byte offset into
    % the currently bound array buffer, not a raw client pointer.
    {gl, offset, []};
resolve_param_to_type_specs(multi_draw_arrays) ->
    {list, {tuple, [resolve_param_to_type_specs(gl_int), resolve_param_to_type_specs(gl_sizei)]}};
resolve_param_to_type_specs(multi_draw_elements) ->
    {list, {tuple, [resolve_param_to_type_specs(gl_sizei), resolve_param_to_type_specs(gl_offset)]}};
resolve_param_to_type_specs(multi_draw_elements_base_vertex) ->
    {
        list,
        {tuple, [
            resolve_param_to_type_specs(gl_sizei),
            resolve_param_to_type_specs(gl_offset),
            resolve_param_to_type_specs(gl_int)
        ]}
    };
resolve_param_to_type_specs({multi_bind_object_list, ObjectName}) ->
    {list, {set, [resolve_param_to_type_specs({gl_object, ObjectName}), none]}};
resolve_param_to_type_specs({gl_object_list_with_count, ObjectName}) ->
    {list, resolve_param_to_type_specs({gl_object, ObjectName})};
resolve_param_to_type_specs(multi_bind_buffer_ranges) ->
    {
        list,
        {tuple, [
            {set, [resolve_param_to_type_specs({gl_object, buffer}), none]},
            resolve_param_to_type_specs(gl_intptr),
            resolve_param_to_type_specs(gl_sizeiptr)
        ]}
    };
resolve_param_to_type_specs(multi_bind_vertex_buffers) ->
    {
        list,
        {tuple, [
            {set, [resolve_param_to_type_specs({gl_object, buffer}), none]},
            resolve_param_to_type_specs(gl_intptr),
            resolve_param_to_type_specs(gl_sizei)
        ]}
    };
resolve_param_to_type_specs(gl_binary_or_null) ->
    % Example: glBufferData()
    {set, [undefined, {undefined, binary, []}]};
resolve_param_to_type_specs({byte_data_or_size, _SizeName, _DataName}) ->
    % Example: glBufferData(). The wrapper derives the byte size when byte data
    % is provided, or treats a non-negative integer as NULL allocation size.
    {set, [{undefined, iodata, []}, {undefined, non_neg_integer, []}]};
resolve_param_to_type_specs({byte_data, _SizeName}) ->
    % Example: glBufferSubData(). The wrapper derives the byte size from
    % iodata before calling the raw NIF.
    {undefined, iodata, []};
resolve_param_to_type_specs({byte_data, _SizeName, _SizeType}) ->
    % Some byte-data APIs derive a size parameter with a non-GLsizeiptr type,
    % for example compressed texture uploads use GLsizei ImageSize.
    {undefined, iodata, []};
resolve_param_to_type_specs({byte_data_with_trailing_size, _SizeName, _SizeType}) ->
    % Some byte-data APIs place the raw size after the data pointer. Example:
    % glProgramBinary(Program, Format, Binary, Length).
    {undefined, iodata, []};
resolve_param_to_type_specs(byte_data_pointer) ->
    % Example: glTexSubImage2D(). Texture image uploads pass only a raw pixel
    % pointer; no byte size is present in the OpenGL signature.
    {undefined, iodata, []};
resolve_param_to_type_specs(byte_data_pointer_or_none) ->
    % Example: glTexImage2D(). Public callers may pass byte data or request
    % NULL pixel storage allocation with the semantic atom none.
    {set, [{undefined, iodata, []}, none]};

resolve_param_to_type_specs(gl_string) ->
    % NUL-terminated GL string input. Public callers provide byte-oriented
    % iodata and the wrapper normalizes it before the raw NIF call.
    {undefined, iodata, []};


% resolve_param_to_type_specs(gl_string) ->
%     % Example: glGetShaderSource(), glGetProgramInfoLog(), etc.
%     % XXX: Should be either string or binary...
%     {set, [{undefined, string, []}, {undefined, binary, []}]};

resolve_param_to_type_specs({gl_string, char}) ->
    % Length-prefixed GLchar input. Public callers provide byte-oriented
    % iodata; the wrapper derives the raw GLsizei length before the NIF call.
    {undefined, iodata, []};

resolve_param_to_type_specs({gl_string, glubyte}) ->
    % Example: glGetString(). Public GL strings are byte-oriented Erlang
    % binaries, not Erlang character lists.
    {undefined, binary, []};


resolve_param_to_type_specs(gl_binary) ->
    {undefined, binary, []};

resolve_param_to_type_specs({gl_enum, V}) ->
    Name = opengl_gen:erlangify_enum_group_name(V),
    {undefined, list_to_atom(Name), []};
resolve_param_to_type_specs({gl_enum, _V, PublicName}) ->
    {undefined, PublicName, []};
resolve_param_to_type_specs({gl_enum_list_with_count, EnumGroup, PublicName}) ->
    {list, resolve_param_to_type_specs({gl_enum, EnumGroup, PublicName})};
resolve_param_to_type_specs({gl_bitfield, V}) ->
    Name = opengl_gen:erlangify_enum_group_name(V),
    {undefined, list_to_atom(Name), []};

resolve_param_to_type_specs({gl_vector, N, Type}) ->
    Name = lists:flatten(io_lib:format("vector~p", [N])),
    TypeNode = resolve_param_to_type_specs(Type),
    {undefined, list_to_atom(Name), [TypeNode]};
resolve_param_to_type_specs({single_vector, N, Type}) ->
    resolve_param_to_type_specs({gl_vector, N, Type});

resolve_param_to_type_specs({gl_object, Name}) ->
    {undefined, Name, []};
resolve_param_to_type_specs({gl_object, Name, SpecialAtoms}) ->
    {set, [{undefined, Name, []} | SpecialAtoms]};
resolve_param_to_type_specs({gl_object_union, Names}) ->
    {set, [resolve_param_to_type_specs({gl_object, Name}) || Name <- Names]};


resolve_param_to_type_specs(list_gl_strings)->
    % The parameter is a list of byte-oriented source chunks, normalized
    % before reaching the NIF function.
    % Example: glShaderSource().
    {list, {undefined, iodata, []}};

resolve_param_to_type_specs(list_gl_strings_null_terminated)->
    % The parameter is a list of byte-oriented GL string names. The wrapper
    % normalizes each item before the NIF allocates temporary NUL-terminated
    % GLchar* strings.
    % Example: glTransformFeedbackVaryings().
    {list, {undefined, iodata, []}};

resolve_param_to_type_specs(gl_uint_list_with_count)->
    % Public callers provide a list of GLuint-sized values. The raw NIF derives
    % the GLsizei count from the packed binary before passing count+pointer to
    % OpenGL.
    % Example: glGetActiveUniformsiv().
    {list, resolve_param_to_type_specs(gl_uint)};

resolve_param_to_type_specs(specialization_constant_list)->
    % glSpecializeShader takes two parallel GLuint arrays. Public callers use a
    % single list of {Index, Value} pairs so the arrays cannot drift apart.
    {list, {tuple, [resolve_param_to_type_specs(gl_uint), resolve_param_to_type_specs(gl_uint)]}};

% resolve_param_to_type_specs({list, ItemSpecs}) when ItemSpecs =/= gl_x ->
%     {list, resolve_param_to_type_specs(ItemSpecs)};

resolve_param_to_type_specs({list, ItemSpecs}) ->
    {list, resolve_param_to_type_specs(ItemSpecs)};

resolve_param_to_type_specs({counted_list, _CountName, ItemSpecs}) ->
    {list, resolve_param_to_type_specs(ItemSpecs)};
resolve_param_to_type_specs({counted_list_or_all, _CountName, ItemSpecs}) ->
    {set, [all, {list, resolve_param_to_type_specs(ItemSpecs)}]};



resolve_param_to_type_specs(V) ->
    erlang:error({unsupported_param_specs, V}).

resolve_function_specs_param_fold({in, ParamName, gl_x}, {ParamsAcc, ReturnAcc}) ->
    % If this is the X parameter, it gets resolved later.
    {[{ParamName, gl_x}|ParamsAcc], ReturnAcc};

resolve_function_specs_param_fold({const, _ParamName, _ParamSpecs}, {ParamsAcc, ReturnAcc}) ->
    % Raw OpenGL constants are generated into wrapper bodies but are not public
    % Erlang parameters.
    {ParamsAcc, ReturnAcc};

resolve_function_specs_param_fold({in, ParamName, ParamSpecs}, {ParamsAcc, ReturnAcc}) ->
    % Most of the time, an "in" parameter corresponds to a single parameter.
    TypeSpecs = resolve_param_to_type_specs(ParamSpecs),
    {[{ParamName, TypeSpecs}|ParamsAcc], ReturnAcc};

resolve_function_specs_param_fold({out, ParamName, gl_x}, {ParamsAcc, ReturnAcc}) ->
    % When the X parameter is an "out" parameter, it must be an array. It
    % generates a parameter
    % The rest is resolved later.
    SizeType = {undefined, pos_integer, []},
    {
        [{"N", SizeType}|ParamsAcc],
        [{ParamName, {list, gl_x}}|ReturnAcc]
    };

% resolve_function_specs_param_fold({out, Name, gl_string}, {ParamsAcc, ReturnAcc}) ->

%     SizeName = "StringSize",
%     SizeType = {undefined, pos_integer, []},
%     Type = {undefined, binary, []},
%     {
%         [{SizeName, SizeType}|ParamsAcc],
%         [{Name, Type}|ReturnAcc]
%     };

resolve_function_specs_param_fold({out, Name, {gl_binary, {_, SizeName, _SizeType}}}, {ParamsAcc, ReturnAcc}) ->
    % It generates an additional parameter to specify the size of the
    % allocated binary. Example: glGetTextureImage().
    SizeType = {undefined, non_neg_integer, []},
    Type = {undefined, binary, []},
    {
        [{SizeName, SizeType}|ParamsAcc],
        [{Name, Type}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {gl_binary, {_, SizeName}}}, {ParamsAcc, ReturnAcc}) ->
    % It generates an additional parameter to specify the size of the
    % allocated binary. Example: glReadPixels().
    SizeType = {undefined, non_neg_integer, []},
    Type = {undefined, binary, []},
    {
        [{SizeName, SizeType}|ParamsAcc],
        [{Name, Type}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, _Name, {program_binary, SizeName}}, {ParamsAcc, ReturnAcc}) ->
    % glGetProgramBinary exposes caller capacity and returns the driver
    % runtime binary format token plus only the bytes actually written.
    SizeType = {undefined, non_neg_integer, []},
    {
        [{SizeName, SizeType}|ParamsAcc],
        [
            {"Binary", {undefined, binary, []}},
            {"BinaryFormat", {gl, uint, []}}
            | ReturnAcc
        ]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list, CountName, gl_x}}, {ParamsAcc, ReturnAcc}) ->
    % Caller-sized typed value readback exposes an element count and returns
    % exactly that many scalar storage values.
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, gl_x}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list, CountName, Type}}, {ParamsAcc, ReturnAcc}) ->
    % Direct caller-sized typed value readback has a concrete scalar storage
    % type instead of an aggregate suffix selector.
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Type)}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list_with_size, CountName, gl_x}}, {ParamsAcc, ReturnAcc}) ->
    % Caller-sized typed value readback where OpenGL also receives the count.
    % The concrete scalar type is supplied by the aggregate suffix selector.
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, gl_x}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list_with_size, CountName, Type}}, {ParamsAcc, ReturnAcc}) ->
    % Caller-sized typed value readback where OpenGL also receives the count.
    % Example: glGetInternalformativ().
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Type)}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list_with_byte_size, CountName, gl_x}}, {ParamsAcc, ReturnAcc}) ->
    % Caller-sized typed value readback where OpenGL receives byte capacity.
    % The concrete scalar type is supplied by the aggregate suffix selector.
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, gl_x}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list_with_byte_size, CountName, Type}}, {ParamsAcc, ReturnAcc}) ->
    % Caller-sized typed value readback where OpenGL receives byte capacity.
    % Example: glGetnUniformfv().
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Type)}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, {typed_value_list_from_counted_input, _SourceParam, Type}}, {ParamsAcc, ReturnAcc}) ->
    % Count-derived typed value readback exposes no public Count parameter.
    % The raw NIF allocates exactly one output element per source-list item.
    {ParamsAcc, [{Name, {list, resolve_param_to_type_specs(Type)}}|ReturnAcc]};
resolve_function_specs_param_fold(
    {out, Name, {caller_sized_typed_value_list, CountName, _LengthName, Type}},
    {ParamsAcc, ReturnAcc}
) ->
    % Caller-sized property readback exposes a maximum element capacity and
    % returns only the values OpenGL reports through its Length out parameter.
    SizeType = {undefined, pos_integer, []},
    {
        [{CountName, SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Type)}}|ReturnAcc]
    };
resolve_function_specs_param_fold(
    {out, Name, {debug_message_log, _MaxMessagesName, MessageLogSizeName}},
    {ParamsAcc, ReturnAcc}
) ->
    SizeType = {undefined, pos_integer, []},
    MessageType = {
        tuple,
        [
            resolve_param_to_type_specs({gl_enum, "DebugSource", debug_source}),
            resolve_param_to_type_specs({gl_enum, "DebugType", debug_type}),
            resolve_param_to_type_specs(gl_uint),
            resolve_param_to_type_specs({gl_enum, "DebugSeverity", debug_severity}),
            {undefined, binary, []}
        ]
    },
    {
        [{MessageLogSizeName, SizeType}|ParamsAcc],
        [{Name, {list, MessageType}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, _Name, shader_precision_format}, {ParamsAcc, ReturnAcc}) ->
    % glGetShaderPrecisionFormat has fixed output shape:
    % GLint range[2] plus GLint precision. Public callers receive the three
    % scalar values directly, without a count parameter.
    Type = resolve_param_to_type_specs(gl_int),
    {
        ParamsAcc,
        [
            {"Precision", Type},
            {"RangeMax", Type},
            {"RangeMin", Type}
            | ReturnAcc
        ]
    };

resolve_function_specs_param_fold({out, Name, {gl_scalar, _StorageType, boolean}}, {ParamsAcc, ReturnAcc}) ->
    % A fixed OpenGL query can hide its scalar out pointer and expose the
    % result as a semantic Erlang return value.
    {ParamsAcc, [{Name, {gl, boolean, []}}|ReturnAcc]};
resolve_function_specs_param_fold({out, Name, {gl_scalar, StorageType, integer}}, {ParamsAcc, ReturnAcc}) ->
    % Fixed scalar OpenGL queries expose the semantic scalar result instead of
    % leaking a one-element output pointer shape.
    Type = resolve_param_to_type_specs(StorageType),
    {ParamsAcc, [{Name, Type}|ReturnAcc]};
resolve_function_specs_param_fold({out, Name, {active_reflection_info, TypeSpec}}, Acc) ->
    resolve_function_specs_param_fold(
        {out, Name, {active_reflection_info, gl_int, TypeSpec}},
        Acc
    );
resolve_function_specs_param_fold({out, _Name, {active_reflection_info, SizeType, TypeSpec}}, {ParamsAcc, ReturnAcc}) ->
    % Active attribute/uniform reflection has raw shape:
    % MaxLength, Length*, Size*, Type*, Name*. Public callers provide only the
    % maximum name length and receive the semantic metadata tuple.
    Type = resolve_param_to_type_specs(TypeSpec),
    Size = resolve_param_to_type_specs(SizeType),
    {
        [{"MaxLength", {undefined, pos_integer, []}}|ParamsAcc],
        [
            {"Name", {undefined, binary, []}},
            {"Type", Type},
            {"Size", Size}
            | ReturnAcc
        ]
    };
resolve_function_specs_param_fold({out, Name, {gl_enum, _Group} = Enum}, {ParamsAcc, ReturnAcc}) ->
    % Reflection queries can return enum metadata by value while keeping the
    % raw one-element GLenum pointer hidden from the public API.
    {ParamsAcc, [{Name, resolve_param_to_type_specs(Enum)}|ReturnAcc]};
resolve_function_specs_param_fold({out, Name, {gl_enum, _Group, _PublicName} = Enum}, {ParamsAcc, ReturnAcc}) ->
    {ParamsAcc, [{Name, resolve_param_to_type_specs(Enum)}|ReturnAcc]};

resolve_function_specs_param_fold({out, Name, {{list, 1}, Item}}, {ParamsAcc, ReturnAcc}) ->
    % It generates a parameter to specify the number of values to read from the
    % OpenGL output, and a return value which is the list itself.
    % Example: glGet*()
    % XXX: Param name ???
    SizeType = {undefined, pos_integer, []},
    {
        [{"N", SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Item)}}|ReturnAcc]
    };


resolve_function_specs_param_fold({out, Name1, {{list, 2, Name2}, Item}}, {ParamsAcc, ReturnAcc}) ->
    % It generates a parameter to specify the number of values to request
    % OpenGL (to know how much memory to allocate), and a return value which
    % is the list itself.
    % Example: glGenTextures()
    % XXX: Param name ???
    SizeType = {undefined, pos_integer, []},
    {
        [{Name2, SizeType}|ParamsAcc],
        [{Name1, {list, resolve_param_to_type_specs(Item)}}|ReturnAcc]
    };
resolve_function_specs_param_fold(
    {out, Name, {caller_sized_list, MaxCountName, _CountName, Item}},
    {ParamsAcc, ReturnAcc}
) ->
    % Caller-sized list readback exposes a maximum capacity, then returns only
    % the elements actually written by OpenGL.
    % Example: glGetAttachedShaders().
    SizeType = {undefined, non_neg_integer, []},
    {
        [{MaxCountName, SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Item)}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, gl_string}, {ParamsAcc, ReturnAcc}) ->
    % Little exception for out gl string, as it generates an arg.
    {[{"MaxLength", {undefined, pos_integer, []}}|ParamsAcc], [{Name, {undefined, binary, []}}|ReturnAcc]}.

% resolve_function_specs_param_fold({out, Name, Specs}, {ParamsAcc, ReturnAcc}) ->
    % % If this was not for any of the previous cases, an "out" parameter simply
    % % corresponds to a single return value.
    % {ParamsAcc, [{Name, resolve_param_to_type_specs(Specs)}|ReturnAcc]}.
