%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% Common logic related to resolving function clauses (used to resolve both
%% direct and indirect functions).
%%
%% Recipe rules:
%% 
%% - `do_nothing` pass it as is 
%% - `{gl_enum_to_uint, TransformMap}` transform the atom to an integer
%% - `{gl_bitfield_to_uint, TransformMap}` transform the list of atoms to an
%%   integer value
%% - `list_gl_objects_to_binary` transform the list of integers to a
%% - `{gl_enum_constant, Constant}` injects an OpenGL enum constant into the
%%   raw NIF call without exposing it as a public Erlang parameter
%% - `{gl_sizei_constant, Value}` and `{gl_bool_constant, Value}` do the same
%%   for hidden size and boolean parameters
%% - scalar out parameters are hidden from the public wrapper and allocated by
%%   the raw NIF function
%%
%% - `{gl_matrix_to_list, M, N}` does this
%% - `{list_gl_matrix_to_list, M, N}` does this
%% - `{gl_vector_to_list, N}` does this
%% - `{gl_vector_to_pointer_list, N}` does this
%% - `{list_gl_vector_to_list, N}` does this
%%
-module(common_clauses_resolver).
-export([resolve_params/4]).

enum_public_type_name({gl_enum, EnumGroup}) ->
    opengl_gen:erlangify_enum_group_name(EnumGroup);
enum_public_type_name({gl_enum, _EnumGroup, PublicName}) when is_atom(PublicName) ->
    atom_to_list(PublicName);
enum_public_type_name({gl_enum, _EnumGroup, PublicName}) ->
    PublicName.

transform_rule_gl_enum_to_uint(EnumGroup, EnumTypes, EnumToConstantMap) ->
    % We instruct the module generator to transform the atom to an integer
    % before it reaches the NIF level. For this, it needs a map that translates
    % the possible atom values to the OpenGL constant names.
    EnumValues = maps:get(
        enum_public_type_name(EnumGroup),
        EnumTypes
    ),
    TransformMap = lists:map(fun(EnumValue) ->
        ConstantValue = maps:get(EnumValue, EnumToConstantMap),
        {EnumValue, ConstantValue}
    end, EnumValues),
    {gl_enum_to_uint, TransformMap}.

transform_rule_gl_bitfield_to_uint(EnumGroup, EnumTypes, EnumToConstantMap) ->
    EnumValues = maps:get(
        opengl_gen:erlangify_enum_group_name(EnumGroup),
        EnumTypes
    ),
    TransformMap = lists:map(fun(EnumValue) ->
        ConstantValue = maps:get(EnumValue, EnumToConstantMap),
        {EnumValue, ConstantValue}
    end, EnumValues),
    {gl_bitfield_to_uint, TransformMap}.

% resolve_function_clause_param({Direction, ParamName, {gl_x, Type, Form}}, _Data) ->
%     % Special handling for the X parameter.
%     indirect_function_resolver:resolve_x_param(Direction, ParamName, Type, Form);
resolve_function_clause_param({in, ParamName, gl_bool}, _Data) ->
    % We let the NIF handle the conversation from atom to GLboolean
    % and therefore we pass it down to the NIF function as-is.
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_byte}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_ubyte}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_short}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_ushort}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_int}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_uint}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_uint_positive}, _Data) ->
    % Pass it down as-is (the NIF function will convert and validate it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_enum_value}, _Data) ->
    % Runtime-defined GLenum tokens are passed as integers, without generated
    % atom enum conversion.
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_int64}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_uint64}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_float}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_double}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};

resolve_function_clause_param({in, ParamName, {gl_string, char}}, _Data) ->
    % Public callers provide byte-oriented iodata. The NIF receives a binary
    % and derives the raw GLsizei length plus temporary GLchar* pointer.
    {ParamName, normalize_gl_string};




resolve_function_clause_param({in, ParamName, gl_sizei}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};

resolve_function_clause_param({in, ParamName, gl_intptr}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, gl_sizeiptr}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, {fixed_integer, _Value}}, _Data) ->
    % Public selector parameters choose a wrapper clause but are not passed to
    % the raw OpenGL command.
    {ParamName, ignore};

resolve_function_clause_param({in, ParamName, gl_binary_or_null}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    % Example: glBufferData().
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, {byte_data_or_size, SizeName, DataName}}, _Data) ->
    % The public API accepts either byte data or an allocation size. The wrapper
    % turns that into the raw OpenGL Size/Data pair before calling the NIF.
    % Example: glBufferData().
    {ParamName, {byte_data_or_size, SizeName, DataName}};
resolve_function_clause_param({in, ParamName, {byte_data, SizeName}}, _Data) ->
    % The public API accepts byte data and derives the raw OpenGL Size.
    % Example: glBufferSubData().
    {ParamName, {byte_data, SizeName}};
resolve_function_clause_param({in, ParamName, {byte_data, SizeName, SizeType}}, _Data) ->
    % The public API accepts byte data and derives a typed raw OpenGL size.
    % Example: compressed texture uploads derive GLsizei ImageSize.
    {ParamName, {byte_data, SizeName, SizeType}};
resolve_function_clause_param({in, ParamName, {byte_data_with_trailing_size, SizeName, SizeType}}, _Data) ->
    % The public API accepts byte data and derives a typed raw OpenGL size,
    % but the raw command expects the pointer before the size.
    {ParamName, {byte_data_with_trailing_size, SizeName, SizeType}};
resolve_function_clause_param({in, ParamName, byte_data_pointer}, _Data) ->
    % Texture image upload APIs expose an iodata pixel payload but no public
    % byte-size parameter because OpenGL derives the consumed size from pixel
    % format/type and pixel-store state.
    {ParamName, byte_data_pointer};
resolve_function_clause_param({in, ParamName, byte_data_pointer_or_none}, _Data) ->
    % Full texture image allocation accepts either pixel byte data or a
    % semantic none that maps to raw NULL.
    {ParamName, byte_data_pointer_or_none};
resolve_function_clause_param({in, ParamName, {derived_count, SourceName}}, _Data) ->
    % Counted aggregate arrays expose only the Erlang list. The wrapper
    % derives the raw OpenGL count from that list before calling the raw NIF.
    % Example: glUniform*fv().
    {ParamName, {derived_count, SourceName}};
resolve_function_clause_param({in, ParamName, gl_offset}, _Data) ->
    % Pass it down as-is (the NIF function will convert it).
    % Example: glVertexAttribPointer().
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, multi_draw_arrays}, _Data) ->
    {ParamName, multi_draw_arrays};
resolve_function_clause_param({in, ParamName, multi_draw_elements}, _Data) ->
    {ParamName, multi_draw_elements};
resolve_function_clause_param({in, ParamName, multi_draw_elements_base_vertex}, _Data) ->
    {ParamName, multi_draw_elements_base_vertex};
resolve_function_clause_param({in, ParamName, {multi_bind_object_list, _ObjectName}}, _Data) ->
    {ParamName, multi_bind_object_list};
resolve_function_clause_param({in, ParamName, {gl_object_list_with_count, _ObjectName}}, _Data) ->
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, multi_bind_buffer_ranges}, _Data) ->
    {ParamName, multi_bind_buffer_ranges};
resolve_function_clause_param({in, ParamName, multi_bind_vertex_buffers}, _Data) ->
    {ParamName, multi_bind_vertex_buffers};

resolve_function_clause_param({in, ParamName, {gl_matrix, M, N, _GlType}}, _Data) ->
    {ParamName, {gl_matrix_to_list, M, N}};
resolve_function_clause_param({in, ParamName, {list, {gl_matrix, M, N, _GlType}}}, _Data) ->
    {ParamName, {list_gl_matrix_to_list, M, N}};
resolve_function_clause_param({in, ParamName, {gl_vector, N, _GlType}}, _Data) ->
    {ParamName, {gl_vector_to_list, N}};
resolve_function_clause_param({in, ParamName, {single_vector, N, _GlType}}, _Data) ->
    {ParamName, {gl_vector_to_pointer_list, N}};
resolve_function_clause_param({in, ParamName, {list, {gl_vector, N, _GlType}}}, _Data) ->
    {ParamName, {list_gl_vector_to_list, N}};

resolve_function_clause_param({in, ParamName, {gl_enum, EnumGroup}}, Data) ->
    % We pre-process the atom and convert to its integer value.
    {EnumTypes, _, GlEnumsMap} = Data,
    {ParamName, transform_rule_gl_enum_to_uint({gl_enum, EnumGroup}, EnumTypes, GlEnumsMap)};
resolve_function_clause_param({in, ParamName, {gl_enum, EnumGroup, PublicName}}, Data) ->
    % Same as the two-item enum form, but the generated public type can use a
    % cleaned-up Erlang name while retaining the Khronos enum group values.
    {EnumTypes, _, GlEnumsMap} = Data,
    {ParamName, transform_rule_gl_enum_to_uint({gl_enum, EnumGroup, PublicName}, EnumTypes, GlEnumsMap)};
resolve_function_clause_param({in, ParamName, {gl_bitfield, EnumGroup}}, Data) ->
    % We pre-process the list of atoms and convert to its integer value.
    {_, BitfieldTypes, GlEnumsMap} = Data,
    {ParamName, transform_rule_gl_bitfield_to_uint(EnumGroup, BitfieldTypes, GlEnumsMap)};
resolve_function_clause_param({const, ParamName, {gl_enum_constant, Constant}}, _Data) ->
    {ParamName, {gl_enum_constant, Constant}};
resolve_function_clause_param({const, ParamName, {gl_sizei_constant, Value}}, _Data) ->
    {ParamName, {gl_sizei_constant, Value}};
resolve_function_clause_param({const, ParamName, {gl_bool_constant, Value}}, _Data) ->
    {ParamName, {gl_bool_constant, Value}};

resolve_function_clause_param({in, ParamName, {gl_object, _Name}}, Data) ->
    % An "OpenGL object" is nothing but a GLuint.
    resolve_function_clause_param({in, ParamName, gl_uint}, Data);
resolve_function_clause_param({in, ParamName, {gl_object_union, _Names}}, Data) ->
    % OpenGL image-copy object slots accept texture or renderbuffer object
    % names; both are represented as GLuint at the raw boundary.
    resolve_function_clause_param({in, ParamName, gl_uint}, Data);
resolve_function_clause_param({in, ParamName, {gl_object, _Name, SpecialAtoms}}, _Data) ->
    % Some binding APIs give object name 0 a semantic meaning. Public callers
    % use explicit atoms for those cases, while the raw NIF still receives a
    % GLuint.
    SpecialValues = lists:map(fun special_gl_object_atom_to_uint/1, SpecialAtoms),
    {ParamName, {gl_object_to_uint, SpecialValues}};



resolve_function_clause_param({in, ParamName, gl_string}, _Data) ->
    % Public callers provide byte-oriented iodata. Normalize it before the NIF
    % allocates the temporary NUL-terminated GLchar*.
    % Example: glBindAttribLocation().
    {ParamName, normalize_gl_string};

resolve_function_clause_param({in, ParamName, gl_binary}, _Data) ->
    % Pass it down as-is (the Erlang bin will be read in the NIF,
    % read-only way).
    {ParamName, do_nothing};



resolve_function_clause_param({in, ParamName, {list, {gl_object, _Name}}}, _Data) ->
    % We pre-process the list of integers and make a binary out of it
    % before passing it to the NIF function.
    % Example: glDeleteTextures() takes a list of textures.
    {ParamName, list_gl_objects_to_binary};

resolve_function_clause_param({in, ParamName, {counted_list, CountName, {gl_object, _Name}}}, _Data) ->
    % Counted lists expose only the Erlang list in the public API. The wrapper
    % derives the count from that list before calling the raw NIF.
    % Example: glDeleteTextures().
    {ParamName, {counted_list_gl_objects_to_binary, CountName}};
resolve_function_clause_param({in, ParamName, {counted_list, CountName, {gl_enum, EnumGroup}}}, Data) ->
    % Counted enum lists mirror counted object lists, except public atoms are
    % converted to GLenum constants before the wrapper packs the pointer data.
    % Example: glDrawBuffers().
    {EnumTypes, _, GlEnumsMap} = Data,
    {gl_enum_to_uint, TransformMap} =
        transform_rule_gl_enum_to_uint({gl_enum, EnumGroup}, EnumTypes, GlEnumsMap),
    {ParamName, {counted_list_gl_enums_to_binary, CountName, TransformMap}};
resolve_function_clause_param({in, ParamName, {counted_list, CountName, {gl_enum, EnumGroup, PublicName}}}, Data) ->
    {EnumTypes, _, GlEnumsMap} = Data,
    {gl_enum_to_uint, TransformMap} =
        transform_rule_gl_enum_to_uint({gl_enum, EnumGroup, PublicName}, EnumTypes, GlEnumsMap),
    {ParamName, {counted_list_gl_enums_to_binary, CountName, TransformMap}};
resolve_function_clause_param({in, ParamName, {counted_list, CountName, {gl_vector, N, _GlType}}}, _Data) ->
    % Counted vector lists expose a list of fixed-size tuples. The wrapper
    % derives the raw OpenGL count and flattens the tuples for the C pointer.
    % Example: glViewportArrayv().
    {ParamName, {counted_list_gl_vectors_to_list, CountName, N}};
resolve_function_clause_param({in, ParamName, {counted_list_or_all, CountName, gl_uint}}, _Data) ->
    % Some OpenGL commands use count=0 plus NULL as a semantic "all" selector,
    % while non-empty ID lists keep the usual count-plus-pointer shape.
    % Example: glDebugMessageControl().
    {ParamName, {counted_list_or_all_gl_uints_to_binary, CountName}};
resolve_function_clause_param({in, ParamName, gl_uint_list_with_count}, _Data) ->
    % Reflection property-array commands take a GLsizei count plus a GLuint*
    % input list. The public API exposes only the list.
    {ParamName, gl_uint_list_with_count};
resolve_function_clause_param({in, ParamName, specialization_constant_list}, _Data) ->
    {ParamName, do_nothing};
resolve_function_clause_param({in, ParamName, {gl_enum_list_with_count, EnumGroup, PublicName}}, Data) ->
    % Reflection property-array commands take a GLsizei count plus a GLenum*
    % input list. The public API exposes only the enum atom list.
    {EnumTypes, _, GlEnumsMap} = Data,
    {gl_enum_to_uint, TransformMap} =
        transform_rule_gl_enum_to_uint({gl_enum, EnumGroup, PublicName}, EnumTypes, GlEnumsMap),
    {ParamName, {gl_enum_list_with_count, TransformMap}};

resolve_function_clause_param({in, ParamName, {list, _Type}}, _Data) ->
    {ParamName, do_nothing};



resolve_function_clause_param({out, _ParamName, gl_string}, _Data) ->
    % The public wrapper exposes the allocation size, not the hidden GLchar*
    % output pointer. Example: glGetShaderSource(), glGetProgramInfoLog().
    {"MaxLength", do_nothing};
resolve_function_clause_param({out, _ParamName, {active_reflection_info, _TypeSpec}}, _Data) ->
    % Active reflection exposes only caller-provided string capacity. The raw
    % NIF owns Length*, Size*, Type*, and Name*.
    {"MaxLength", do_nothing};
resolve_function_clause_param({out, _ParamName, {active_reflection_info, _SizeType, _TypeSpec}}, _Data) ->
    % Active reflection exposes only caller-provided string capacity. The raw
    % NIF owns Length*, Size*, Type*, and Name*.
    {"MaxLength", do_nothing};
resolve_function_clause_param({out, _ParamName, shader_precision_format}, _Data) ->
    skip;


% % resolve_function_clause_param({out, _ParamName, {list, {gl_object, _Name}}}, _Data) ->

resolve_function_clause_param({out, _ParamName, {gl_binary, {implicit, SizeParamName}}}, _Data) ->
    % It generates a parameter to specify the size of the binary to be
    % allocated. Example: glReadPixels().
    {SizeParamName, do_nothing};
resolve_function_clause_param({out, _ParamName, {gl_binary, {explicit, SizeParamName, _SizeType}}}, _Data) ->
    % Caller-sized readback exposes the requested byte count, while the raw NIF
    % expands it to the OpenGL size plus output pointer pair.
    {SizeParamName, do_nothing};
resolve_function_clause_param({out, _ParamName, {gl_binary, {explicit, SizeParamName}}}, _Data) ->
    % Caller-sized readback exposes the requested byte count, while the raw NIF
    % expands it to the OpenGL size plus output pointer pair.
    {SizeParamName, do_nothing};
resolve_function_clause_param({out, _ParamName, {program_binary, SizeParamName}}, _Data) ->
    % glGetProgramBinary exposes only caller capacity at the wrapper level.
    {SizeParamName, do_nothing};
resolve_function_clause_param({out, _ParamName, {typed_value_list, CountName, gl_x}}, _Data) ->
    % Caller-sized typed value readback exposes only the element count.
    {CountName, do_nothing};
resolve_function_clause_param({out, _ParamName, {typed_value_list, CountName, _Type}}, _Data) ->
    % Indirect resolver replaces gl_x with the concrete scalar storage type.
    {CountName, do_nothing};
resolve_function_clause_param({out, _ParamName, {typed_value_list_with_size, CountName, _Type}}, _Data) ->
    {CountName, do_nothing};
resolve_function_clause_param({out, _ParamName, {typed_value_list_with_byte_size, CountName, _Type}}, _Data) ->
    {CountName, do_nothing};
resolve_function_clause_param({out, _ParamName, {typed_value_list_from_counted_input, _SourceParam, _Type}}, _Data) ->
    skip;
resolve_function_clause_param({out, _ParamName, {caller_sized_typed_value_list, CountName, _LengthName, _Type}}, _Data) ->
    {CountName, do_nothing};
resolve_function_clause_param({out, _ParamName, {debug_message_log, _MaxMessagesName, MessageLogSizeName}}, _Data) ->
    {MessageLogSizeName, do_nothing};


resolve_function_clause_param({out, _Name, {gl_scalar, _StorageType, _ReturnType}}, _Data) ->
    skip;

resolve_function_clause_param({out, _Name, {gl_enum, _Group}}, _Data) ->
    skip;
resolve_function_clause_param({out, _Name, {gl_enum, _Group, _PublicName}}, _Data) ->
    skip;

resolve_function_clause_param({out, _Name, {caller_sized_list, MaxCountName, _CountName, _Item}}, _Data) ->
    % Public callers provide the allocation capacity. The raw NIF allocates up
    % to that capacity and trims the returned list to OpenGL's actual count.
    {MaxCountName, do_nothing};

resolve_function_clause_param({out, _Name1, {{list, _, Name2}, _}}, _Data) ->
    % Any "out" parameter that is a list of simple values creates a blabla
    %  xxx: write
    % Example: glGenTextures() takes a list of textures.
    {Name2, do_nothing};
resolve_function_clause_param({in, ParamName, list_gl_strings}, _Data) ->
    % We normalize the list so it's only a list of binaries.
    % Example: glShaderSource()
    {ParamName, normalize_list_strings_or_binary};
resolve_function_clause_param({in, ParamName, list_gl_strings_null_terminated}, _Data) ->
    % Same public Erlang shape as list_gl_strings, but the raw NIF allocates
    % NUL-terminated strings instead of passing an explicit lengths array.
    % Example: glTransformFeedbackVaryings().
    {ParamName, normalize_list_strings_or_binary};

resolve_function_clause_param({out, Name, Type}, _Data) ->
    % Any other "out" parameter are either simple types, or list of simple 
    % types.
    io:format("[clause debug]: ~p~n", [Type]),
    {Name, do_nothing};

resolve_function_clause_param(V, _) ->
    io:format("[clauses] ~p~n", [V]),
    erlang:halt(0).

special_gl_object_atom_to_uint(none) ->
    {none, 0}.

% resolve_function_clause_param({out, _, _}, _Data) ->
%     % Any simple "out" paramater (GLboolean, GLint, etc.) does not correspond
%     % to any parameter. (The NIF function will create the variable, the OpenGL
%     % function will write to it, and the NIF function will return it).
%     skip.

resolve_params(ParamsSpecs, EnumTypes, BitfieldTypes, GlEnumsMap) ->
    % Prepare the data needed to resolve the parameters of a function clause.
    Data = {EnumTypes, BitfieldTypes, GlEnumsMap},

    % For each parameter of the function clause, we resolve them to a "recipe"
    % rule that tells how to transform the value and pass it to the NIF 
    % function.
    lists:foldl(fun(ParamSpecs, Acc) ->
        case resolve_function_clause_param(ParamSpecs, Data) of
            skip ->
                Acc;
            R ->
                Acc ++ [R]
        end
    end, [], ParamsSpecs).
