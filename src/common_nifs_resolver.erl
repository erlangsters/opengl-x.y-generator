%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% Common logic related to resolving NIF functions (used to resolve both
%% direct and indirect functions).
%%
%% Recipe rule (params):
%%
%% - `integer_to_glenum`
%% - `integer_to_glbitfield`
%%
%% - `integer_to_glint`
%%
%% - `double_to_glfloat`
%% - `double_to_gldouble`
%%
%% - `integer_to_glsizei`

%% - `boolean_to_glbool`

%% - `foo`
%% - `foo`
%%
%% Recipe rule (return):
%%
%% - `void`
%% - `const_glubyte_to_string` xx verify
%% - `bar`
%% - `bar`
%% - `bar`
%%
-module(common_nifs_resolver).
-export([resolve_nif_function/2]).

enum_public_type_name({gl_enum, EnumGroup}) ->
    opengl_gen:erlangify_enum_group_name(EnumGroup);
enum_public_type_name({gl_enum, _EnumGroup, PublicName}) when is_atom(PublicName) ->
    atom_to_list(PublicName);
enum_public_type_name({gl_enum, _EnumGroup, PublicName}) ->
    PublicName.

resolve_nif_function(ParamsSpecs, ReturnSpecs) ->
    NifParams = resolve_nif_function_params(ParamsSpecs),
    NifReturn = resolve_nif_function_return(ReturnSpecs),
    {NifParams, NifReturn}.


resolve_gl_type_convert_data(gl_bool) ->
    {"GLboolean", "bool", "custom_enif_get_bool", "custom_enif_make_bool"};
resolve_gl_type_convert_data(gl_byte) ->
    {"GLbyte", "int", "enif_get_int", "enif_make_int"};
resolve_gl_type_convert_data(gl_ubyte) ->
    {"GLubyte", "unsigned int", "enif_get_uint", "enif_make_uint"};
resolve_gl_type_convert_data(gl_short) ->
    {"GLshort", "int", "enif_get_int", "enif_make_int"};
resolve_gl_type_convert_data(gl_ushort) ->
    {"GLushort", "unsigned int", "enif_get_uint", "enif_make_uint"};
resolve_gl_type_convert_data(gl_int) ->
    {"GLint", "int", "enif_get_int", "enif_make_int"};
resolve_gl_type_convert_data(gl_uint) ->
    {"GLuint", "unsigned int", "enif_get_uint", "enif_make_uint"};
resolve_gl_type_convert_data(gl_uint_positive) ->
    resolve_gl_type_convert_data(gl_uint);
resolve_gl_type_convert_data(gl_enum_value) ->
    {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"};
resolve_gl_type_convert_data(gl_int64) ->
    {"GLint64", "ErlNifSInt64", "enif_get_int64", "enif_make_int64"};
resolve_gl_type_convert_data(gl_uint64) ->
    {"GLuint64", "ErlNifUInt64", "enif_get_uint64", "enif_make_uint64"};
resolve_gl_type_convert_data(gl_float) ->
    {"GLfloat", "double", "enif_get_double", "enif_make_double"};
resolve_gl_type_convert_data(gl_double) ->
    {"GLdouble", "double", "enif_get_double", "enif_make_double"};
resolve_gl_type_convert_data(gl_sizei) ->
    {"GLsizei", "int", "enif_get_int", "enif_make_int"};
resolve_gl_type_convert_data(gl_intptr) ->
    {"GLintptr", "int", "enif_get_int", "enif_make_int"};
resolve_gl_type_convert_data(gl_sizeiptr) ->
    {"GLsizeiptr", "int", "enif_get_int", "enif_make_int"};

resolve_gl_type_convert_data({gl_object, _Name}) ->
    resolve_gl_type_convert_data(gl_uint);
resolve_gl_type_convert_data({gl_object, _Name, _SpecialAtoms}) ->
    resolve_gl_type_convert_data(gl_uint);
resolve_gl_type_convert_data({gl_object_union, _Names}) ->
    resolve_gl_type_convert_data(gl_uint);
resolve_gl_type_convert_data({gl_enum, _Group}) ->
    {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"};
resolve_gl_type_convert_data({gl_enum, _Group, _PublicName}) ->
    {"GLenum", "unsigned int", "enif_get_uint", "enif_make_uint"};
resolve_gl_type_convert_data({gl_bitfield, _Group}) ->
    {"GLbitfield", "unsigned int", "enif_get_uint", "enif_make_uint"}.

resolve_nif_function_param(Name, in, gl_bool) ->
    [{Name, boolean_to_glbool}];
% resolve_nif_function_param(Name, in, gl_short) ->
%     [{Name, integer_to_glshort}];


resolve_nif_function_param(Name, in, gl_string) ->
    [{Name, in_gl_string}];

resolve_nif_function_param(Name, in, {gl_vector, 1, Type}) ->
    [{_, Rule}] = resolve_nif_function_param(Name, in, Type),
    [{"V1", Rule}];
resolve_nif_function_param(Name, in, {gl_vector, 2, Type}) ->
    [{_, Rule}] = resolve_nif_function_param(Name, in, Type),
    [{"V1", Rule}, {"V2", Rule}];
resolve_nif_function_param(Name, in, {gl_vector, 3, Type}) ->
    [{_, Rule}] = resolve_nif_function_param(Name, in, Type),
    [{"V1", Rule}, {"V2", Rule}, {"V3", Rule}];
resolve_nif_function_param(Name, in, {gl_vector, 4, Type}) ->
    [{_, Rule}] = resolve_nif_function_param(Name, in, Type),
    [{"V1", Rule}, {"V2", Rule}, {"V3", Rule}, {"V4", Rule}];

resolve_nif_function_param(Name, in, gl_binary) ->
    [{Name, binary_to_glbinary}];

resolve_nif_function_param(_Name, in, {fixed_integer, _Value}) ->
    [];


resolve_nif_function_param(Name, in, {list, {gl_object, _Name}}) ->
    [{Name, binary_to_glbinary}]; %XXX

resolve_nif_function_param(Name, in, {counted_list, CountName, {gl_object, _ObjectName}}) ->
    resolve_nif_function_param(CountName, in, gl_sizei) ++
        [{Name, binary_to_glbinary}];
resolve_nif_function_param(Name, in, {counted_list, CountName, {gl_enum, _EnumGroup}}) ->
    resolve_nif_function_param(CountName, in, gl_sizei) ++
        [{Name, binary_to_glbinary}];
resolve_nif_function_param(Name, in, {counted_list, CountName, {gl_enum, _EnumGroup, _PublicName}}) ->
    resolve_nif_function_param(CountName, in, gl_sizei) ++
        [{Name, binary_to_glbinary}];
resolve_nif_function_param(Name, in, {counted_list_or_all, CountName, gl_uint}) ->
    resolve_nif_function_param(CountName, in, gl_sizei) ++
        resolve_nif_function_param(Name, in, gl_binary_or_null);


resolve_nif_function_param(Name, in, {list, {gl_vector, _, Type}}) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    [{Name, {list_gl_type, ConvertData}}];
resolve_nif_function_param(Name, in, {single_vector, _, Type}) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    [{Name, {list_gl_type, ConvertData}}];
resolve_nif_function_param(Name, in, {counted_list, CountName, {gl_vector, _, Type}}) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    resolve_nif_function_param(CountName, in, gl_sizei) ++
        [{Name, {list_gl_type, ConvertData}}];
resolve_nif_function_param(Name, in, {gl_matrix, _, _, Type}) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    [{Name, {list_gl_type, ConvertData}}];
resolve_nif_function_param(Name, in, {list, {gl_matrix, _, _, Type}}) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    [{Name, {list_gl_type, ConvertData}}];

resolve_nif_function_param(Name, in, {list, Type}) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    [{Name, {list_gl_type, ConvertData}}];

resolve_nif_function_param(Name, const, {gl_enum_constant, _Constant}) ->
    ConvertData = resolve_gl_type_convert_data({gl_enum, constant}),
    [{Name, {gl_type, ConvertData}}];
resolve_nif_function_param(Name, const, {gl_sizei_constant, _Value}) ->
    resolve_nif_function_param(Name, in, gl_sizei);
resolve_nif_function_param(Name, const, {gl_bool_constant, _Value}) ->
    resolve_nif_function_param(Name, in, gl_bool);


resolve_nif_function_param(Name, in, list_gl_strings) ->
    % Only used by glShaderSource().
    [{Name, in_list_gl_strings}];
resolve_nif_function_param(Name, in, list_gl_strings_null_terminated) ->
    % Used by APIs that take count plus const GLchar** without a lengths array.
    % Example: glTransformFeedbackVaryings().
    [{Name, in_list_gl_strings_null_terminated}];
resolve_nif_function_param(Name, in, gl_uint_list_with_count) ->
    % Public [gl:uint()] is normalized to a packed binary in the wrapper. The
    % NIF derives the raw GLsizei count before passing count+GLuint* to OpenGL.
    [{Name, in_gl_uint_list_with_count}];
resolve_nif_function_param(Name, in, {gl_object_list_with_count, _ObjectName}) ->
    [{Name, in_gl_object_list_with_count}];
resolve_nif_function_param(Name, in, specialization_constant_list) ->
    [{Name, in_specialization_constant_list}];
resolve_nif_function_param(Name, in, {gl_enum_list_with_count, _EnumGroup, _PublicName}) ->
    % Public enum atoms are normalized to a packed GLenum binary in the wrapper.
    % The NIF derives the raw GLsizei count before passing count+GLenum*.
    [{Name, in_gl_enum_list_with_count}];
resolve_nif_function_param(Name, in, gl_binary_or_null) ->
    % Example: glBufferData().
    [{Name, in_gl_binary_or_null}];
resolve_nif_function_param(_Name, in, {byte_data_or_size, SizeName, DataName}) ->
    % Public DataOrSize expands to the raw OpenGL size and data pointer.
    % Example: glBufferData().
    resolve_nif_function_param(SizeName, in, gl_sizeiptr) ++
        resolve_nif_function_param(DataName, in, gl_binary_or_null);
resolve_nif_function_param(Name, in, {byte_data, SizeName}) ->
    % Public byte data expands to the raw OpenGL size and data pointer.
    % Example: glBufferSubData().
    resolve_nif_function_param(SizeName, in, gl_sizeiptr) ++
        resolve_nif_function_param(Name, in, gl_binary);
resolve_nif_function_param(Name, in, {byte_data, SizeName, SizeType}) ->
    % Public byte data expands to the raw OpenGL size and data pointer, using
    % the command-specific native size type.
    resolve_nif_function_param(SizeName, in, SizeType) ++
        resolve_nif_function_param(Name, in, gl_binary);
resolve_nif_function_param(Name, in, {byte_data_with_trailing_size, SizeName, SizeType}) ->
    % Public byte data expands to the raw OpenGL data pointer and trailing
    % size, using the command-specific native size type.
    resolve_nif_function_param(Name, in, gl_binary) ++
        resolve_nif_function_param(SizeName, in, SizeType);
resolve_nif_function_param(Name, in, byte_data_pointer) ->
    % Public iodata is normalized in the wrapper; the raw NIF receives only a
    % binary data pointer.
    resolve_nif_function_param(Name, in, gl_binary);
resolve_nif_function_param(Name, in, byte_data_pointer_or_none) ->
    % Public iodata is normalized in the wrapper; public none becomes the
    % internal undefined atom consumed by the existing NULL pointer NIF path.
    resolve_nif_function_param(Name, in, gl_binary_or_null);
resolve_nif_function_param(Name, in, {derived_count, _SourceName}) ->
    % Counted aggregate arrays expose only the list-shaped public value. The
    % wrapper derives this GLsizei count before calling the raw NIF.
    resolve_nif_function_param(Name, in, gl_sizei);

resolve_nif_function_param(Name, in, gl_offset) ->
    % Example: glVertexAttribPointer().
    [{Name, in_gl_offset}];
resolve_nif_function_param(Name, in, multi_draw_arrays) ->
    [{Name, in_multi_draw_arrays}];
resolve_nif_function_param(Name, in, multi_draw_elements) ->
    [{Name, in_multi_draw_elements}];
resolve_nif_function_param(Name, in, multi_draw_elements_base_vertex) ->
    [{Name, in_multi_draw_elements_base_vertex}];
resolve_nif_function_param(Name, in, {multi_bind_object_list, _ObjectName}) ->
    [{Name, in_multi_bind_object_list}];
resolve_nif_function_param(Name, in, multi_bind_buffer_ranges) ->
    [{Name, in_multi_bind_buffer_ranges}];
resolve_nif_function_param(Name, in, multi_bind_vertex_buffers) ->
    [{Name, in_multi_bind_vertex_buffers}];
resolve_nif_function_param(Name, in, {gl_string, char}) ->
    [{Name, binary_to_gl_string_char}];
resolve_nif_function_param(Name, in, Type) ->
    ConvertData = resolve_gl_type_convert_data(Type),
    [{Name, {gl_type, ConvertData}}];


resolve_nif_function_param(_Name, out, gl_string) ->
    % Example: glGetShaderSource(), glGetProgramInfoLog(), etc.
    [{"MaxLength", out_gl_string}];






resolve_nif_function_param(Name, out, {gl_binary, {implicit, _}}) ->
    [{Name, out_binary_implicit}];
resolve_nif_function_param(Name, out, {gl_binary, {explicit, _, SizeType}}) ->
    [{Name, {out_binary_explicit, SizeType}}];
resolve_nif_function_param(Name, out, {gl_binary, {explicit, _}}) ->
    [{Name, out_binary_explicit}];
resolve_nif_function_param(Name, out, {program_binary, _SizeName}) ->
    [{Name, out_program_binary}];
resolve_nif_function_param(Name, out, {typed_value_list, _CountName, Type}) ->
    {GlType, _, _, NifFunction} = resolve_gl_type_convert_data(Type),
    [{Name, {out_typed_value_list, GlType, NifFunction}}];
resolve_nif_function_param(Name, out, {typed_value_list_with_size, _CountName, Type}) ->
    {GlType, _, _, NifFunction} = resolve_gl_type_convert_data(Type),
    [{Name, {out_typed_value_list_with_size, GlType, NifFunction}}];
resolve_nif_function_param(Name, out, {typed_value_list_with_byte_size, _CountName, Type}) ->
    {GlType, _, _, NifFunction} = resolve_gl_type_convert_data(Type),
    [{Name, {out_typed_value_list_with_byte_size, GlType, NifFunction}}];
resolve_nif_function_param(Name, out, {typed_value_list_from_counted_input, SourceParam, Type}) ->
    {GlType, _, _, NifFunction} = resolve_gl_type_convert_data(Type),
    [{Name, {out_typed_value_list_from_counted_input, SourceParam, GlType, NifFunction}}];
resolve_nif_function_param(Name, out, {caller_sized_typed_value_list, CountName, LengthName, Type}) ->
    {GlType, _, _, NifFunction} = resolve_gl_type_convert_data(Type),
    [{Name, {caller_sized_typed_value_list, CountName, LengthName, GlType, NifFunction}}];
resolve_nif_function_param(Name, out, {debug_message_log, MaxMessagesName, _MessageLogSizeName}) ->
    [{Name, {
        out_debug_message_log,
        MaxMessagesName,
        transform_map_for_enum({gl_enum, "DebugSource", debug_source}),
        transform_map_for_enum({gl_enum, "DebugType", debug_type}),
        transform_map_for_enum({gl_enum, "DebugSeverity", debug_severity})
    }}];
resolve_nif_function_param(Name, out, shader_precision_format) ->
    [{Name, out_shader_precision_format}];
resolve_nif_function_param(Name, out, {gl_scalar, StorageType, boolean}) ->
    {GlType, _, _, _} = resolve_gl_type_convert_data(StorageType),
    [{Name, {out_scalar, GlType, glint_to_boolean}}];
resolve_nif_function_param(Name, out, {gl_scalar, gl_int, integer}) ->
    {GlType, _, _, _} = resolve_gl_type_convert_data(gl_int),
    [{Name, {out_scalar, GlType, glint_to_integer}}];
resolve_nif_function_param(Name, out, {gl_scalar, gl_int64, integer}) ->
    {GlType, _, _, _} = resolve_gl_type_convert_data(gl_int64),
    [{Name, {out_scalar, GlType, glint64_to_integer}}];
resolve_nif_function_param(Name, out, {active_reflection_info, TypeSpec}) ->
    resolve_nif_function_param(Name, out, {active_reflection_info, gl_int, TypeSpec});
resolve_nif_function_param(_Name, out, {active_reflection_info, SizeType, TypeSpec}) ->
    {GlType, _, _, _} = resolve_gl_type_convert_data(SizeType),
    [{"MaxLength", {out_active_reflection_info, GlType, transform_map_for_enum(TypeSpec)}}];
resolve_nif_function_param(Name, out, {gl_enum, EnumGroupRaw}) ->
    [{Name, {out_enum, "GLenum", transform_map_for_enum({gl_enum, EnumGroupRaw})}}];
resolve_nif_function_param(Name, out, {gl_enum, EnumGroupRaw, PublicName}) ->
    [{Name, {out_enum, "GLenum", transform_map_for_enum({gl_enum, EnumGroupRaw, PublicName})}}];
% resolve_nif_function_param(Name, in, {list, _}) ->
%     % XXX: Will have to be reworked.
%     [{Name, binary_to_glbinary}];

% resolve_nif_function_param(Name, out, {{list, 1}, gl_bool}) ->
%     % Example: glGetBooleanv().
%     Rule = {out_list_alloc_1, "GLboolean", "gl_bool_to_erl_boolean"},
%     [{Name, Rule}];

resolve_nif_function_param(Name, out, {{list, 1}, Type}) ->
    % Example: glGetIntegerv(), glGetTexParameterX()
    {GlType, _, _, NifFunction} = resolve_gl_type_convert_data(Type),

    Rule = {out_list_alloc_1, GlType, NifFunction},
    [{Name, Rule}];


resolve_nif_function_param(Name1, out, {{list, 2, _Name2}, gl_uint}) ->
    % Example: Indirectly used by glGenTextures().
    Rule = {return_list_terms_alloc, "GLuint", "enif_make_uint"},
    [{Name1, Rule}];
resolve_nif_function_param(Name1, out, {{list, N, Name2}, {gl_object, _Object}}) ->

    % Example: glGenTextures().
    % OpenGL objects are nothing but GLuint. xxx
    resolve_nif_function_param(Name1, out, {{list, N, Name2}, gl_uint});
resolve_nif_function_param(
    _Name,
    out,
    {caller_sized_list, MaxCountName, _CountName, {gl_object, _Object}}
) ->
    % Example: glGetAttachedShaders(). Public callers provide the maximum
    % capacity; OpenGL reports the actual count written.
    [{MaxCountName, {caller_sized_list, "GLuint", "enif_make_uint"}}];


resolve_nif_function_param(Name, Direction, Type) ->
    io:format(user, "debug: resolve_nif_function_param(~p, ~p, ~p)~n", [Name, Direction, Type]),
    erlang:halt(0).

% For each parameter, we dictate how the Erlang value passed to the NIF
% function is used to be eventually passed to the OpenGL function.


resolve_nif_function_params(ParamsSpecs) ->
    % Note that we preserve the names (which are not used in the generated C
    % code) so we can generate more readable code in the generated Erlang
    % module (see the NIF placeholders).
    lists:foldl(fun({Direction, Name, Specs}, Params) ->
        % Note that it can generate more than one param.
        Params ++ resolve_nif_function_param(Name, Direction, Specs)
    end, [], ParamsSpecs).

transform_map_for_enum(EnumSpec) ->
    GlEnumsMap = erlang:get(gl_enums_map),
    EnumGroup = enum_public_type_name(EnumSpec),
    Enums = maps:get(EnumGroup, erlang:get(enum_types)),
    lists:map(fun(Enum) ->
        {maps:get(Enum, GlEnumsMap), Enum}
    end, Enums).

resolve_nif_function_return(gl_void) ->
    % Most functions return no value and therefore no transform rule.
    void;

resolve_nif_function_return({_Name, gl_bool}) ->
    glbool_to_boolean;
resolve_nif_function_return({_Name, gl_int}) ->
    glint_to_integer;
resolve_nif_function_return({_Name, gl_uint}) ->
    gluint_to_uint;
resolve_nif_function_return({_Name, gl_int64}) ->
    glint64_to_integer;
resolve_nif_function_return({_Name, gl_uint64}) ->
    gluint64_to_uint;
resolve_nif_function_return({_Name, debug_message_log_count}) ->
    debug_message_log_count;
resolve_nif_function_return({_Name, {gl_object, _}}) ->
    gluint_to_integer;
resolve_nif_function_return({_Name, {gl_enum, EnumGroupRaw}}) ->
    {glenum_to_atom, transform_map_for_enum({gl_enum, EnumGroupRaw})};
resolve_nif_function_return({_Name, {gl_enum, EnumGroupRaw, PublicName}}) ->
    {glenum_to_atom, transform_map_for_enum({gl_enum, EnumGroupRaw, PublicName})};
resolve_nif_function_return({_Name, {gl_string, glubyte}}) ->
    const_glubyte_to_string;
resolve_nif_function_return(Type) ->
    io:format(user, "debug: resolve_nif_function_return(~p)~n", [Type]),
    erlang:halt(0).
