%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It generates the `gl.erl` file from the binding data that were previously
%% computed.
%%
-module(gl_module_generator).
-export([generate/2]).
-import(opengl_gen, [open/1, write/2, write/3, close/0]).

generate(TargetApi, BindingData) ->
    open("gl.erl"),

    write(0, "%% This file is generated. Do not edit!\n"),
    write(0, "-module(gl).\n"),
    write_module_doc(BindingData),
    write(0, "\n"),

    write(0, "-compile({nowarn_redefined_builtin_type, [boolean/0, byte/0, float/0]}).\n\n"),

    write(0, "% Low-level OpenGL types.\n"),
    write(0, "-export_type([boolean/0]).\n"),
    write(0, "-export_type([byte/0]).\n"),
    write(0, "-export_type([ubyte/0]).\n"),
    write(0, "-export_type([short/0]).\n"),
    write(0, "-export_type([ushort/0]).\n"),
    write(0, "-export_type([int/0]).\n"),
    write(0, "-export_type([uint/0]).\n"),
    write(0, "-export_type([int64/0]).\n"),
    write(0, "-export_type([uint64/0]).\n"),
    write(0, "-export_type([sizei/0]).\n"),
    write(0, "-export_type([intptr/0]).\n"),
    write(0, "-export_type([sizeiptr/0]).\n"),
    write(0, "-export_type([offset/0]).\n"),
    write(0, "-export_type([float/0]).\n"),
    write(0, "-export_type([double/0]).\n"),
    write(0, "\n"),
    write(0, "-export_type([scalar/1]).\n"),
    write(0, "-export_type([vector1/1, vector2/1, vector3/1, vector4/1]).\n"),
    write(0, "-export_type([matrix2/1, matrix3/1, matrix4/1]).\n"),
    write(0, "-export_type([matrix2x3/1, matrix3x2/1]).\n"),
    write(0, "-export_type([matrix2x4/1, matrix4x2/1]).\n"),
    write(0, "-export_type([matrix3x4/1, matrix4x3/1]).\n"),
    write(0, "\n"),
    write(0, "-export_type([enum/0]).\n"),
    write(0, "-export_type([bitfield/0]).\n"),
    write(0, "\n"),

    write(0, "% High-level OpenGL types.\n"),
    write_export_object_types(BindingData),
    write(0, "\n"),

    write(0, "% The OpenGL enum types.\n"),
    write_export_enum_types(BindingData),
    write(0, "\n"),

    write(0, "% The OpenGL bitfield types.\n"),
    write_export_bitfield_types(BindingData),
    write(0, "\n"),

    write(0, "% Extra functions.\n"),
    write(0, "-export([enum_groups_/0]).\n"),
    write(0, "-export([enums_/1]).\n"),
    write(0, "-export([enum_value_/1]).\n"),
    write(0, "-export([value_enums_/1]).\n"),
    write(0, "-export([value_enum_/2]).\n"),
    write(0, "\n"),
    % write(0, "-export([bitfield_groups_/]).\n"),
    % write(0, "-export([bitfields_/1]).\n"),
    % write(0, "-export([bitfield_value_/1]).\n"),
    % write(0, "-export([value_bitfields_/1]).\n"),
    % write(0, "-export([value_bitfield_/2]).\n"),
    write(0, "\n"),

    write_glad_exports(TargetApi),

    write_export_functions(maps:get(functions, BindingData)),
    write(0, "\n"),

    write(0, "-include(\"../include/gl.hrl\").\n\n"),

    write_call_raw_func_macro(),

    write(0, "-doc \"The OpenGL `boolean` type.\".\n"),
    write(0, "-type boolean() :: erlang:boolean().\n"),
    write(0, "-doc \"The OpenGL `byte` type.\".\n"),
    write(0, "-type byte() :: integer().\n"),
    write(0, "-doc \"The OpenGL `ubyte` type.\".\n"),
    write(0, "-type ubyte() :: non_neg_integer().\n"),
    write(0, "-doc \"The OpenGL `short` type.\".\n"),
    write(0, "-type short() :: integer().\n"),
    write(0, "-doc \"The OpenGL `ushort` type.\".\n"),
    write(0, "-type ushort() :: non_neg_integer().\n"),
    write(0, "-doc \"The OpenGL `int` type.\".\n"),
    write(0, "-type int() :: integer().\n"),
    write(0, "-doc \"The OpenGL `uint` type.\".\n"),
    write(0, "-type uint() :: non_neg_integer().\n"),
    write(0, "-doc \"The OpenGL `int64` type.\".\n"),
    write(0, "-type int64() :: integer().\n"),
    write(0, "-doc \"The OpenGL `uint64` type.\".\n"),
    write(0, "-type uint64() :: non_neg_integer().\n"),
    write(0, "-doc \"The OpenGL `sizei` type.\".\n"),
    write(0, "-type sizei() :: integer().\n"),
    write(0, "-doc \"The OpenGL `intptr` type.\".\n"),
    write(0, "-type intptr() :: integer().\n"),
    write(0, "-doc \"The OpenGL `sizeiptr` type.\".\n"),
    write(0, "-type sizeiptr() :: integer().\n"),
    write(0, "-doc \"A byte offset into the currently bound OpenGL buffer.\".\n"),
    write(0, "-type offset() :: non_neg_integer().\n"),
    write(0, "-doc \"The OpenGL `float` type.\".\n"),
    write(0, "-type float() :: erlang:float().\n"),
    write(0, "-doc \"The OpenGL `double` type.\".\n"),
    write(0, "-type double() :: erlang:float().\n"),
    write(0, "\n"),
    write(0, "-doc \"A scalar value of the given type.\".\n"),
    write(0, "-type scalar(Type) :: Type.\n"),
    write(0, "\n"),
    write(0, "-doc \"A 1D vector.\".\n"),
    write(0, "-type vector1(Type) :: {scalar(Type)}.\n"),
    write(0, "-doc \"A 2D vector.\".\n"),
    write(0, "-type vector2(Type) :: {scalar(Type), scalar(Type)}.\n"),
    write(0, "-doc \"A 3D vector.\".\n"),
    write(0, "-type vector3(Type) :: {scalar(Type), scalar(Type), scalar(Type)}.\n"),
    write(0, "-doc \"A 4D vector.\".\n"),
    write(0, "-type vector4(Type) :: {scalar(Type), scalar(Type), scalar(Type), scalar(Type)}.\n"),
    write(0, "\n"),
    write(0, "-doc \"A 2x2 matrix.\".\n"),
    write(0, "-type matrix2(Type) :: {vector2(Type), vector2(Type)}.\n"),
    write(0, "-doc \"A 3x3 matrix.\".\n"),
    write(0, "-type matrix3(Type) :: {vector3(Type), vector3(Type), vector3(Type)}.\n"),
    write(0, "-doc \"A 4x4 matrix.\".\n"),
    write(0, "-type matrix4(Type) :: {vector4(Type), vector4(Type), vector4(Type), vector4(Type)}.\n"),
    write(0, "\n"),
    write(0, "-doc \"A 2x3 matrix.\".\n"),
    write(0, "-type matrix2x3(Type) :: {vector3(Type), vector3(Type)}.\n"),
    write(0, "-doc \"A 3x2 matrix.\".\n"),
    write(0, "-type matrix3x2(Type) :: {vector2(Type), vector2(Type), vector2(Type)}.\n"),
    write(0, "\n"),
    write(0, "-doc \"A 2x4 matrix.\".\n"),
    write(0, "-type matrix2x4(Type) :: {vector4(Type), vector4(Type)}.\n"),
    write(0, "-doc \"A 4x2 matrix.\".\n"),
    write(0, "-type matrix4x2(Type) :: {vector2(Type), vector2(Type), vector2(Type), vector2(Type)}.\n"),
    write(0, "\n"),
    write(0, "-doc \"A 3x4 matrix.\".\n"),
    write(0, "-type matrix3x4(Type) :: {vector4(Type), vector4(Type), vector4(Type)}.\n"),
    write(0, "-doc \"A 4x3 matrix.\".\n"),
    write(0, "-type matrix4x3(Type) :: {vector3(Type), vector3(Type), vector3(Type), vector3(Type)}.\n"),
    write(0, "\n"),

    write(0, "-doc \"An OpenGL enum.\".\n"),
    write(0, "-type enum() :: atom().\n"),
    write(0, "-doc \"An OpenGL bit field.\".\n"),
    write(0, "-type bitfield() :: [atom()].\n"),
    write(0, "\n"),

    write_object_types(maps:get(object_types, BindingData)),
    write(0, "\n"),
    write_enum_types(maps:get(enum_types, BindingData)),
    write(0, "\n"),
    write_bitfield_types(maps:get(bitfield_types, BindingData)),
    write(0, "\n"),

    write_nif_attributes(maps:get(functions, BindingData)),
    write(0, "\n"),
    write(0, "-on_load(init_nif/0).\n"),
    write(0, "\n"),
    write(0, """
init_nif() ->
    % The OpenGL NIF module depends on the EGL NIF module, so we compute its
    % location first, then pass it to the OpenGL NIF loader.
    EGLPrivDir = case code:priv_dir(egl) of
        {error, bad_name} ->
            code:priv_dir(egl_1_5);
        EGLDir ->
            EGLDir
    end,
    EGLNifLocation = filename:join(EGLPrivDir, "beam-egl") ++
        case os:type() of
            {win32, _} -> ".dll";
            _ -> ".so"
        end,
    LibName = "beam-gl",
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, LibName]);
                _ ->
                    filename:join([priv, LibName])
            end;
        PrivDir ->
            filename:join(PrivDir, LibName)
    end,
    erlang:load_nif(SoName, EGLNifLocation).
""", []), % XXX
    write(0, "\n\n"),

    write_glad_placeholder(TargetApi),
    
    write_extra_functions(BindingData),

    maps:foreach(fun({FunctionName, _}, FunctionData) ->
        write_function(FunctionName, FunctionData)
    end, maps:get(functions, BindingData)),
    write(0, "\n"),

    write_nif_placeholders(maps:get(functions, BindingData)),

    close(),

    ok.

needs_glad({gl, _}) ->
    true;
needs_glad({gles, _}) ->
    false.

write_glad_exports(TargetApi) ->
    case needs_glad(TargetApi) of
        true ->
            write(0, "-export([glad_load_gl/0]).\n"),
            write(0, "-nifs([glad_load_gl/0]).\n"),
            write(0, "\n");
        false ->
            ok
    end.

write_glad_placeholder(TargetApi) ->
    case needs_glad(TargetApi) of
        true ->
            write(0, "-spec glad_load_gl() -> boolean().\n"),
            write(0, "glad_load_gl() ->\n"),
            write(0, "    erlang:nif_error(nif_library_not_loaded).\n"),
            write(0, "\n");
        false ->
            ok
    end.

write_module_doc(BindingData) ->
    write(0, "-moduledoc \"\"\"\n"),
    documentation_writer:write(BindingData),
    write(0, "\n\"\"\".\n").

write_export_object_types(BindingData) ->
    ObjectTypes = maps:get(object_types, BindingData),
    lists:foreach(fun({ObjectType, _}) ->
        write(0, "-export_type([~s/0]).\n", [ObjectType])
    end, ObjectTypes).

write_export_enum_types(BindingData) ->
    BitfieldTypes = maps:get(enum_types, BindingData),
    maps:foreach(fun(BitfieldName, _BitfieldValues) ->
        write(0, "-export_type([~s/0]).\n", [BitfieldName])
    end, BitfieldTypes).

write_export_bitfield_types(BindingData) ->
    EnumTypes = maps:get(bitfield_types, BindingData),
    maps:foreach(fun(EnumName, _EnumValues) ->
        write(0, "-export_type([~s/0]).\n", [EnumName])
    end, EnumTypes).

write_export_functions(Functions) ->
    maps:foreach(fun({FunctionName, _}, FunctionData) ->
        Arity = maps:get(function_arity, FunctionData),
        write(0, "-export([~s/~p]).\n", [FunctionName, Arity])
    end, Functions).

write_call_raw_func_macro() ->
    write(0, """
-define(CALL_RAW_FUNC_UNCHECKED(Func),
    begin
    case Func of
        {} ->
            ok;
        {Arg__} ->
            {ok, Arg__};
        {Arg1__, Arg2__} ->
            {ok, Arg1__, Arg2__};
        {Arg1__, Arg2__, Arg3__} ->
            {ok, Arg1__, Arg2__, Arg3__}
    end
    end
).
-ifndef(DEBUG).
-define(CALL_RAW_FUNC(Func), ?CALL_RAW_FUNC_UNCHECKED(Func)).
-else.
-define(CALL_RAW_FUNC(Func), 
    begin
    Result__ = Func,
    case gl:get_error() of
        {ok, no_error} ->
            ?CALL_RAW_FUNC_UNCHECKED(Result__);
        {ok, Code__} ->
            {error, Code__}
    end
    end
).
-endif.
""", []),
    write(0, "\n", []).

write_object_types(ObjectTypes) ->
    lists:foreach(fun({ObjectType, Documentation}) ->
        write(0, "-doc \"~s\".\n", [Documentation]),
        write(0, "-type ~s() :: pos_integer().\n", [ObjectType])
    end, ObjectTypes).

write_enum_types(EnumTypes) ->
    maps:foreach(fun(EnumName, EnumValues) ->
        write(0, "-doc \"The OpenGL `~s` enum.\".\n", [EnumName]),
        write(0, "-type ~s() ::\n", [EnumName]),
        Parts = lists:foldl(fun(EnumValue, Acc) ->
            [io_lib:format("    ~s", [EnumValue])|Acc]
        end, [], EnumValues),
        write(0, string:join(Parts, " |\n")),
        write(0, "\n.\n")
    end, EnumTypes).

write_bitfield_types(BitfieldTypes) ->
    maps:foreach(fun(BitfieldName, BitfieldValues) ->
        write(0, "-doc \"The OpenGL `~s` bitfield.\".\n", [BitfieldName]),
        write(0, "-type ~s() :: [\n", [BitfieldName]),
        Parts = lists:foldl(fun(BitfieldValue, Acc) ->
            [io_lib:format("    ~s", [BitfieldValue])|Acc]
        end, [], BitfieldValues),
        write(0, string:join(Parts, " |\n")),
        write(0, "\n].\n")
    end, BitfieldTypes).

write_nif_attributes(Functions) ->
    lists:foreach(fun({NifFunctionName, NifFunctionData}) ->
        Arity = maps:get(arity, NifFunctionData),
        write(0, "-nifs([~s_raw/~p]).\n", [NifFunctionName, Arity])
    end, unique_nif_functions(Functions)).

write_function(FunctionName, FunctionData) ->
    % If it has an extra type, write it first. (Note that they're not
    % exported).
    case maps:get(extra_type, FunctionData, undefined) of
        undefined ->
            ok;
        {TypeName, TypeSpecs} ->
            write_function_extra_type(TypeName, TypeSpecs)
    end,

    write_function_doc(FunctionName, FunctionData),
    write_function_specs(FunctionName, FunctionData),

    FunctionClauses = lists:map(fun(FunctionClauseData) ->
        make_function_clause_bit(FunctionName, FunctionClauseData)
    end, maps:get(function_clauses, FunctionData)),
    write(0, "~s.\n\n", [string:join(FunctionClauses, ";\n")]),

    ok.

write_function_extra_type(TypeName, TypeSpecs) ->
    case TypeSpecs of
        {set, TypeSpecsSet} ->
            write(0, "-type ~s() ::\n", [TypeName]),
            Bits = lists:map(fun stringify_type_specs/1, TypeSpecsSet),
            write(0, "    ~s", [string:join(Bits, " |\n    ")]),
            write(0, "\n.\n", []);
        _ ->
            write(0,
                "-type ~s() :: ~s.\n",
                [TypeName, stringify_type_specs(TypeSpecs)]
            )
    end,
    write(0, "\n", []).

write_function_doc(_FunctionName, FunctionData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, "~s\n\n", [maps:get(doc_description, FunctionData)]),
    case maps:get(gl_commands, FunctionData, undefined) of
        undefined ->
            GlFunctionName = maps:get(gl_command, FunctionData),
            write(0, "It implements the `~s` function\n\n", [GlFunctionName]);
        GlCommandNames ->
            write(0, "It implements the following OpenGL commands:\n\n", []),
            lists:foreach(fun
                ({GlCommandName, _, _}) ->
                    write(0, "- `~s`\n", [GlCommandName]);
                (GlCommandName) ->
                    write(0, "- `~s`\n", [GlCommandName])
            end, GlCommandNames),
            write(0, "\n")
    end,

    % If an example is provided, write it.
    case maps:get(doc_example, FunctionData) of
        undefined ->
            ok;
        Example ->
            write(0, "```\n", []),
            write(0, "~s", [Example]),
            write(0, "\n```\n\n", [])
    end,

    write(0,
        "Consult the documentation of the underlying [OpenGL function](~s) for "
        "more information.\n",
        [maps:get(doc_url, FunctionData)]
    ),
    write(0, "\"\"\".\n"),

    ok.

stringify_type_specs({undefined, Name, Params}) ->
    Parts = lists:map(fun stringify_type_specs/1, Params),
    io_lib:format("~s(~s)", [Name, string:join(Parts, ", ")]);
stringify_type_specs({Module, Name, []}) ->
    io_lib:format("~s:~s()", [Module, Name]);
stringify_type_specs({list, TypeSpecs}) ->
    io_lib:format("[~s]", [stringify_type_specs(TypeSpecs)]);
stringify_type_specs({tuple, TypeSpecs}) ->
    Parts = lists:map(fun stringify_type_specs/1, TypeSpecs),
    io_lib:format("{~s}", [string:join(Parts, ", ")]);
stringify_type_specs({set, TypeSpecs}) when is_list(TypeSpecs) ->
    Strings = lists:map(fun stringify_type_specs/1, TypeSpecs),
    string:join(Strings, " | ");
stringify_type_specs(TypeSpecs) when is_integer(TypeSpecs) ->
    integer_to_list(TypeSpecs);
stringify_type_specs(TypeSpecs) when is_atom(TypeSpecs) ->
    atom_to_list(TypeSpecs).

make_specs_params_bit(SpecsParams) ->
    Parts = lists:map(fun({Name, Specs}) ->
        io_lib:format("~s :: ~s", [Name, stringify_type_specs(Specs)])
    end, SpecsParams),

    % Split over multiple lines if there is more than one parameter.
    case Parts of
        [] -> "";
        [Part] -> Part;
        _ -> "\n    " ++ string:join(Parts, ",\n    ") ++ "\n"
    end.

make_specs_return_bit([]) ->
    "ok";
make_specs_return_bit(SpecsReturn) ->
    Parts = lists:map(fun({Name, Specs}) ->
        io_lib:format("~s :: ~s", [Name, stringify_type_specs(Specs)])
    end, SpecsReturn),
    io_lib:format("{ok, ~s}", [string:join(Parts, ", ")]).

write_function_specs(FunctionName, FunctionData) ->
    SpecsParams = make_specs_params_bit(maps:get(specs_params, FunctionData)),
    SpecsReturn = make_specs_return_bit(maps:get(specs_return, FunctionData)),

    write(0, "-spec ~s(", [FunctionName]),
    write(0, SpecsParams),
    write(0, ") -> ~s | {error, atom()}.\n", [SpecsReturn]),

    ok.

make_raw_function_call_bit(RawFunction, Args, true) ->
    io_lib:format(
        "    ?CALL_RAW_FUNC(~s_raw(~s))",
        [RawFunction, string:join(Args, ", ")]
    );
make_raw_function_call_bit(RawFunction, Args, false) ->
    io_lib:format(
        "    ?CALL_RAW_FUNC_UNCHECKED(~s_raw(~s))",
        [RawFunction, string:join(Args, ", ")]
    ).

transform_param_to_arg(ParamName, {gl_enum_to_uint, TransformMap}) ->
    % We transform at the Erlang module level, a "enum atom" (like
    % 'front_and_back') to the corresponding integer value (that we
    % can get by using the constant '?GL_FRONT_AND_BACK').

    ArgName = "New" ++ ParamName,
    Bit1 = io_lib:format("    ~s = case ~s of\n", [ArgName, ParamName]),
    Bits2_ = lists:map(fun({Atom, Constant}) ->
        io_lib:format("        ~s -> ?~s", [Atom, Constant])
    end, TransformMap),
    Bits2 = string:join(Bits2_, ";\n"),
    Bit3 = "\n    end,\n",
    {Bit1 ++ Bits2 ++ Bit3, ArgName};

transform_param_to_arg(ParamName, {gl_object_to_uint, SpecialValues}) ->
    % Generated object types represent real names. Semantic atoms cover
    % validated OpenGL zero-handle meanings at the public API boundary.
    ArgName = "New" ++ ParamName,
    ObjectName = ParamName ++ "0",
    Bit1 = io_lib:format("    ~s = case ~s of\n", [ArgName, ParamName]),
    SpecialBits = lists:map(fun({Atom, Value}) ->
        io_lib:format("        ~s -> ~p", [Atom, Value])
    end, SpecialValues),
    ObjectBit = io_lib:format(
        "        ~s when is_integer(~s), ~s > 0 -> ~s",
        [ObjectName, ObjectName, ObjectName, ObjectName]
    ),
    Bit2 = string:join(SpecialBits ++ [ObjectBit], ";\n"),
    Bit3 = "\n    end,\n",
    {Bit1 ++ Bit2 ++ Bit3, ArgName};

transform_param_to_arg(ParamName, do_nothing) ->
    {"", ParamName};
transform_param_to_arg(ParamName, multi_draw_arrays) ->
    {"", ParamName};
transform_param_to_arg(ParamName, multi_draw_elements) ->
    {"", ParamName};
transform_param_to_arg(ParamName, multi_draw_elements_base_vertex) ->
    {"", ParamName};
transform_param_to_arg(ParamName, multi_bind_object_list) ->
    {"", ParamName};
transform_param_to_arg(ParamName, multi_bind_buffer_ranges) ->
    {"", ParamName};
transform_param_to_arg(ParamName, multi_bind_vertex_buffers) ->
    {"", ParamName};
transform_param_to_arg(_ParamName, {gl_enum_constant, Constant}) ->
    {"", "?" ++ Constant};
transform_param_to_arg(_ParamName, {gl_sizei_constant, Value}) ->
    {"", integer_to_list(Value)};
transform_param_to_arg(_ParamName, {gl_bool_constant, true}) ->
    {"", "true"};
transform_param_to_arg(_ParamName, {gl_bool_constant, false}) ->
    {"", "false"};
% transform_param_to_arg(ParamName, {gl_vector_to_list, 1}) ->
%     % NewFoo = ?GL_UNPACK_VECTOR_M(Foo).
%     ArgName = "New" ++ ParamName,
%     Line = io_lib:format(
%         "    ~s = ?GL_UNPACK_VECTOR_~p(~s),\n",
%         [ArgName, M, ParamName]
%     ),
%     {Line, ArgName};
% transform_param_to_arg(ParamName, {gl_vector_to_list, 2}) ->
%     % NewFoo = ?GL_UNPACK_VECTOR_M(Foo).
%     ArgName = "New" ++ ParamName,
%     Line = io_lib:format(
%         "    ~s = ?GL_UNPACK_VECTOR_~p(~s),\n",
%         [ArgName, M, ParamName]
%     ),
%     {Line, ArgName};
% transform_param_to_arg(ParamName, {gl_vector_to_list, 3}) ->
%     % NewFoo = ?GL_UNPACK_VECTOR_M(Foo).
%     ArgName = "New" ++ ParamName,
%     Line = io_lib:format(
%         "    ~s = ?GL_UNPACK_VECTOR_~p(~s),\n",
%         [ArgName, M, ParamName]
%     ),
%     {Line, ArgName};
transform_param_to_arg(ParamName, {gl_vector_to_list, 1}) ->
    Line = io_lib:format("    [V1] = ?GL_PACK_VECTOR_1(~s),\n", [ParamName]),
    ArgName = "V1",
    {Line, ArgName};
transform_param_to_arg(ParamName, {gl_vector_to_list, 2}) ->
    Line = io_lib:format("    [V1, V2] = ?GL_PACK_VECTOR_2(~s),\n", [ParamName]),
    ArgName = "V1, V2",
    {Line, ArgName};
transform_param_to_arg(ParamName, {gl_vector_to_list, 3}) ->
    Line = io_lib:format("    [V1, V2, V3] = ?GL_PACK_VECTOR_3(~s),\n", [ParamName]),
    ArgName = "V1, V2, V3",
    {Line, ArgName};
transform_param_to_arg(ParamName, {gl_vector_to_list, 4}) ->
    Line = io_lib:format("    [V1, V2, V3, V4] = ?GL_PACK_VECTOR_4(~s),\n", [ParamName]),
    ArgName = "V1, V2, V3, V4",
    {Line, ArgName};

transform_param_to_arg(ParamName, {gl_vector_to_pointer_list, M}) ->
    % Fixed-size vector pointer commands take one vector value, not a counted
    % list. Pack the tuple into a flat list for the raw NIF's temporary C array.
    ArgName = "New" ++ ParamName,
    Line = io_lib:format(
        "    ~s = ?GL_PACK_VECTOR_~p(~s),\n",
        [ArgName, M, ParamName]
    ),
    {Line, ArgName};

transform_param_to_arg(ParamName, {list_gl_vector_to_list, M}) ->
    % NewFoo = lists:map(fun(Matrix, Acc) -> Acc ++ ?GL_PACK_VECTOR_M(Matrix) end, Foo).
    ArgName = "New" ++ ParamName,
    Line = io_lib:format(
        "    ~s = lists:foldl(fun(Matrix, Acc) -> Acc ++ ?GL_PACK_VECTOR_~p(Matrix) end, [], ~s),\n",
        [ArgName, M, ParamName]
    ),
    {Line, ArgName};

transform_param_to_arg(ParamName, {gl_matrix_to_list, M, N}) ->
    % NewFoo = ?GL_PACK_MATRIX_MxN(Foo).
    ArgName = "New" ++ ParamName,
    Line = io_lib:format(
        "    ~s = ?GL_PACK_MATRIX_~px~p(~s),\n",
        [ArgName, M, N, ParamName]
    ),
    {Line, ArgName};

transform_param_to_arg(ParamName, {list_gl_matrix_to_list, M, N}) ->
    % NewFoo = lists:map(fun(Matrix, Acc) -> Acc ++ ?GL_PACK_MATRIX_MxN(Matrix) end, Foo).
    ArgName = "New" ++ ParamName,
    Line = io_lib:format(
        "    ~s = lists:foldl(fun(Matrix, Acc) -> Acc ++ ?GL_PACK_MATRIX_~px~p(Matrix) end, [], ~s),\n",
        [ArgName, M, N, ParamName]
    ),
    {Line, ArgName};

transform_param_to_arg(ParamName, {gl_bitfield_to_uint, TransformMap}) ->
    % Result = lists:foldl(fun(Field, L) ->
    %     R = case Field of
    %         foo -> ?GL_FOO;
    %         bar -> ?GL_BAR
    %     end,
    %     L bor R
    % end, 16x00, Fields),
    ArgName = "New" ++ ParamName,
    Bit1 = io_lib:format("    ~s = lists:foldl(fun(Field, L) ->\n", [ArgName]),
    Bit2 = io_lib:format("        R = case Field of\n", []),
    Bits3_ = lists:map(fun({Atom, Constant}) ->
        io_lib:format("            ~s -> ?~s", [Atom, Constant])
    end, TransformMap),
    Bits3 = string:join(Bits3_, ";\n"),
    Bit4 = io_lib:format("\n        end,\n", []),
    Bit5 = io_lib:format("        L bor R\n", []),
    Bit6 = io_lib:format("    end, 16#00, ~s),", [ParamName]),
    {Bit1 ++ Bit2 ++ Bits3 ++ Bit4 ++ Bit5 ++ Bit6, ArgName};

transform_param_to_arg(ParamName, list_gl_objects_to_binary) ->
    % We pack the list of "OpenGL objects" (aka GLuint) into a binary ready to
    % used in the NIF function.

    % Binary = << <<ID:32/native>> || ID <- IDs >>,
    % XXX: Portability issue here.
    ArgName = "New" ++ ParamName,
    Bits = io_lib:format(
        "    ~s = << <<ID:32/native>> || ID <- ~s >>,",
        [ArgName, ParamName]
    ),
    {Bits, ArgName};

transform_param_to_arg(ParamName, {counted_list_gl_objects_to_binary, CountName}) ->
    % The public API only accepts the list. The raw NIF still follows OpenGL's
    % count-plus-pointer shape, so derive the count from the same list we pack.
    ArgName = "New" ++ ParamName,
    Bits = io_lib:format(
        "    ~s = length(~s),\n"
        "    ~s = << <<ID:32/native>> || ID <- ~s >>,",
        [CountName, ParamName, ArgName, ParamName]
    ),
    {Bits, CountName ++ ", " ++ ArgName};

transform_param_to_arg(ParamName, {counted_list_gl_enums_to_binary, CountName, TransformMap}) ->
    % Counted enum lists expose only a list of public atoms. Convert each atom
    % to its GLenum constant, derive the count, and pack the raw pointer data.
    ArgName = "New" ++ ParamName,
    Bits1 = io_lib:format("    ~s = length(~s),\n", [CountName, ParamName]),
    Bits2 = io_lib:format("    ~s = << <<(case EnumValue of\n", [ArgName]),
    Bits3_ = lists:map(fun({Atom, Constant}) ->
        io_lib:format("        ~s -> ?~s", [Atom, Constant])
    end, TransformMap),
    Bits3 = string:join(Bits3_, ";\n"),
    Bits4 = io_lib:format("\n    end):32/native>> || EnumValue <- ~s >>,", [ParamName]),
    {Bits1 ++ Bits2 ++ Bits3 ++ Bits4, CountName ++ ", " ++ ArgName};

transform_param_to_arg(ParamName, {counted_list_gl_vectors_to_list, CountName, M}) ->
    % Counted vector-list inputs expose only the Erlang list. The raw NIF keeps
    % OpenGL's count-plus-pointer shape.
    ArgName = "New" ++ ParamName,
    Line = io_lib:format(
        "    ~s = length(~s),\n"
        "    ~s = lists:foldl(fun(Vector, Acc) -> Acc ++ ?GL_PACK_VECTOR_~p(Vector) end, [], ~s),\n",
        [CountName, ParamName, ArgName, M, ParamName]
    ),
    {Line, CountName ++ ", " ++ ArgName};

transform_param_to_arg(ParamName, {counted_list_or_all_gl_uints_to_binary, CountName}) ->
    % The atom `all` is the public spelling for OpenGL's count=0 plus NULL
    % selector. Non-empty GLuint lists use the normal count-plus-pointer path.
    ArgName = "New" ++ ParamName,
    Bits = io_lib:format(
        "    {~s, ~s} = case ~s of\n"
        "        all ->\n"
        "            {0, undefined};\n"
        "        [_ | _] ->\n"
        "            {length(~s), << <<ID:32/native>> || ID <- ~s >>};\n"
        "        _ ->\n"
        "            erlang:error(badarg)\n"
        "    end,",
        [CountName, ArgName, ParamName, ParamName, ParamName]
    ),
    {Bits, CountName ++ ", " ++ ArgName};

transform_param_to_arg(ParamName, gl_uint_list_with_count) ->
    % The raw NIF derives GLsizei count from this packed GLuint list.
    ArgName = "New" ++ ParamName,
    Bits = io_lib:format(
        "    ~s = case ~s of\n"
        "        [_ | _] -> << <<ID:32/native>> || ID <- ~s >>;\n"
        "        _ -> erlang:error(badarg)\n"
        "    end,",
        [ArgName, ParamName, ParamName]
    ),
    {Bits, ArgName};

transform_param_to_arg(ParamName, {gl_enum_list_with_count, TransformMap}) ->
    % The raw NIF derives GLsizei count from this packed GLenum list.
    ArgName = "New" ++ ParamName,
    Bits1 = io_lib:format("    ~s = case ~s of\n", [ArgName, ParamName]),
    Bits2 = io_lib:format("        [_ | _] -> << <<(case EnumValue of\n", []),
    Bits3_ = lists:map(fun({Atom, Constant}) ->
        io_lib:format("            ~s -> ?~s", [Atom, Constant])
    end, TransformMap),
    Bits3 = string:join(Bits3_, ";\n"),
    Bits4 = io_lib:format("\n        end):32/native>> || EnumValue <- ~s >>;\n", [ParamName]),
    Bits5 = "        _ -> erlang:error(badarg)\n"
            "    end,",
    {Bits1 ++ Bits2 ++ Bits3 ++ Bits4 ++ Bits5, ArgName};

transform_param_to_arg(ParamName, {byte_data_or_size, SizeName, DataName}) ->
    % The public API accepts byte data for upload, or a non-negative integer
    % size for NULL allocation. The raw NIF keeps the OpenGL Size/Data shape.
    Bits = io_lib:format(
        "    {~s, ~s} = case ~s of\n"
        "        ~s0 when is_integer(~s0), ~s0 >= 0 ->\n"
        "            {~s0, undefined};\n"
        "        _ ->\n"
        "            ~s0 = iolist_to_binary(~s),\n"
        "            {byte_size(~s0), ~s0}\n"
        "    end,",
        [
            SizeName,
            DataName,
            ParamName,
            SizeName,
            SizeName,
            SizeName,
            SizeName,
            DataName,
            ParamName,
            DataName,
            DataName
        ]
    ),
    {Bits, SizeName ++ ", " ++ DataName};

transform_param_to_arg(ParamName, {byte_data, SizeName}) ->
    % The public API accepts iodata and derives the raw OpenGL Size/Data shape.
    ArgName = ParamName ++ "0",
    Bits = io_lib:format(
        "    ~s = iolist_to_binary(~s),\n"
        "    ~s = byte_size(~s),\n",
        [ArgName, ParamName, SizeName, ArgName]
    ),
    {Bits, SizeName ++ ", " ++ ArgName};

transform_param_to_arg(ParamName, {byte_data, SizeName, _SizeType}) ->
    % Same wrapper transform as byte_data/2, with a command-specific raw size
    % type resolved at the NIF layer.
    transform_param_to_arg(ParamName, {byte_data, SizeName});

transform_param_to_arg(ParamName, {byte_data_with_trailing_size, SizeName, _SizeType}) ->
    % Same normalization as byte_data/3, but raw OpenGL expects pointer before
    % size. Example: glProgramBinary().
    ArgName = ParamName ++ "0",
    Bits = io_lib:format(
        "    ~s = iolist_to_binary(~s),\n"
        "    ~s = byte_size(~s),\n",
        [ArgName, ParamName, SizeName, ArgName]
    ),
    {Bits, ArgName ++ ", " ++ SizeName};

transform_param_to_arg(ParamName, byte_data_pointer) ->
    % Texture subimage uploads expose only the pixel pointer. The public API
    % still accepts iodata and normalizes it before calling the raw NIF.
    ArgName = ParamName ++ "0",
    Bits = io_lib:format("    ~s = iolist_to_binary(~s),\n", [ArgName, ParamName]),
    {Bits, ArgName};

transform_param_to_arg(ParamName, byte_data_pointer_or_none) ->
    % Texture image allocation accepts iodata or the public atom none for a raw
    % NULL pixel pointer. The raw NIF already uses undefined internally for
    % NULL-capable binary pointers.
    ArgName = ParamName ++ "0",
    Bits = io_lib:format(
        "    ~s = case ~s of\n"
        "        none -> undefined;\n"
        "        _ -> iolist_to_binary(~s)\n"
        "    end,\n",
        [ArgName, ParamName, ParamName]
    ),
    {Bits, ArgName};

transform_param_to_arg(ParamName, {derived_count, SourceName}) ->
    Bits = io_lib:format("    ~s = length(~s),\n", [ParamName, SourceName]),
    {Bits, ParamName};

transform_param_to_arg(ParamName, normalize_gl_string) ->
    % The public API accepts iodata for NUL-terminated GL string inputs.
    ArgName = ParamName ++ "0",
    Bits = io_lib:format("    ~s = iolist_to_binary(~s),\n", [ArgName, ParamName]),
    {Bits, ArgName};

transform_param_to_arg(ParamName, normalize_list_strings_or_binary) ->
    % The parameter is a list of iodata source chunks. We normalize it by
    % converting it to a list of binaries.
    ArgName = ParamName ++ "New",
    ItemArgName = ParamName ++ "Item",
    Bit1 = io_lib:format("    ~s = lists:map(fun\n", [ArgName]),
    Bit2 = io_lib:format(
        "        (~s) when is_list(~s) -> iolist_to_binary(~s);\n",
        [ItemArgName, ItemArgName, ItemArgName]
    ),
    Bit3 = io_lib:format(
        "        (~s) when is_binary(~s) -> ~s\n",
        [ItemArgName, ItemArgName, ItemArgName]
    ),
    Bit4 = io_lib:format("    end, ~s),", [ParamName]),
    Bits = Bit1 ++ Bit2 ++ Bit3 ++ Bit4,
    {Bits, ArgName};

transform_param_to_arg(ParamName, Rule) ->
    io:format(user, "XXX: Implement this (rule: ~p)~n", [Rule]),
    {"", ParamName}.

make_function_clause_body(Params, RawFunction, ErrorCheck) ->
    {Bits, Args} = lists:foldl(fun({ParamName, ParamRule}, {BitsAcc, ArgsAcc}) ->
        case ParamRule of
            ignore ->
                {BitsAcc, ArgsAcc};
            _ ->
                {MoreBits, Arg} = transform_param_to_arg(ParamName, ParamRule),
                {[MoreBits|BitsAcc], [Arg |ArgsAcc]}
        end
    end, {"", []}, Params),
    Bit = make_raw_function_call_bit(RawFunction, lists:reverse(Args), ErrorCheck),
    Bits ++ "\n" ++ Bit.

stringify_guard(var, VarName) ->
    VarName;
stringify_guard(head_var, VarName) ->
    io_lib:format("hd(~s)", [VarName]);
stringify_guard({element, Index, Guard}, VarName) ->
    io_lib:format("element(~p, ~s)", [Index, stringify_guard(Guard, VarName)]);
stringify_guard({is_list, Guard}, VarName) ->
    io_lib:format("is_list(~s)", [stringify_guard(Guard, VarName)]);
stringify_guard({is_tuple, Guard}, VarName) ->
    io_lib:format("is_tuple(~s)", [stringify_guard(Guard, VarName)]);
stringify_guard({tuple_size, Guard, N}, VarName) ->
    io_lib:format(
        "tuple_size(~s) =:= ~p",
        [stringify_guard(Guard, VarName), N]
    );
stringify_guard({is_integer, Guard}, VarName) ->
    io_lib:format("is_integer(~s)", [stringify_guard(Guard, VarName)]);
stringify_guard({is_float, Guard}, VarName) ->
    io_lib:format("is_float(~s)", [stringify_guard(Guard, VarName)]);
stringify_guard({equals, Guard, Value}, VarName) ->
    io_lib:format("~s =:= ~p", [stringify_guard(Guard, VarName), Value]).

make_function_clause_bit(FunctionName, FunctionClause) ->
    ParamBits = lists:filtermap(fun
        ({_ParamName, {gl_enum_constant, _Constant}}) ->
            false;
        ({_ParamName, {gl_sizei_constant, _Value}}) ->
            false;
        ({_ParamName, {gl_bool_constant, _Value}}) ->
            false;
        ({_ParamName, {derived_count, _SourceName}}) ->
            false;
        ({ParamName, _}) ->
            {true, ParamName}
    end, maps:get(params, FunctionClause)),
    ClauseParams = string:join(ParamBits, ", "),

    GuardVar = maps:get(guard_var, FunctionClause, undefined),
    GuardBits = lists:map(fun(Guard) ->
        stringify_guard(Guard, GuardVar)
    end, maps:get(guards, FunctionClause)),
    ClauseGuards = string:join(GuardBits, " andalso\n    "),

    ClauseBody = make_function_clause_body(
        maps:get(params, FunctionClause),
        maps:get(raw_function, FunctionClause),
        maps:get(error_check, FunctionClause, true)
    ),

    case maps:get(guards, FunctionClause) of
        [] ->
            io_lib:format(
                "~s(~s) ->\n~s",
                [FunctionName, ClauseParams, ClauseBody]
            );
        [_] ->
            io_lib:format(
                "~s(~s) when ~s ->\n~s",
                [FunctionName, ClauseParams, ClauseGuards, ClauseBody]
            );
        _ ->
            io_lib:format(
                "~s(~s) when\n    ~s\n->\n~s",
                [FunctionName, ClauseParams, ClauseGuards, ClauseBody]
            )
    end.

write_nif_placeholders(Functions) ->
    lists:foreach(fun({NifFunctionName, NifFunctionData}) ->
        NifFunctionParams = lists:map(fun({ParamName, _}) ->
            "_" ++ ParamName
        end, lists:filter(fun nif_param_consumes_argv/1, maps:get(params, NifFunctionData))),
        write(0, "~s_raw(", [NifFunctionName]),
        write(0, string:join(NifFunctionParams, ", ")),
        write(0, ") ->\n"),
        write(0, "    erlang:nif_error(nif_library_not_loaded).\n"),
        write(0, "\n")
    end, unique_nif_functions(Functions)),

    ok.

nif_param_consumes_argv({_, {out_scalar, _, _}}) ->
    false;
nif_param_consumes_argv({_, {out_enum, _, _}}) ->
    false;
nif_param_consumes_argv({_, {out_typed_value_list_from_counted_input, _, _, _}}) ->
    false;
nif_param_consumes_argv({_, out_shader_precision_format}) ->
    false;
nif_param_consumes_argv(_) ->
    true.

unique_nif_functions(Functions) ->
    {ReversedFunctions, _Seen} = maps:fold(fun(_FunctionName, FunctionData, {Acc0, Seen0}) ->
        maps:fold(fun(NifFunctionName, NifFunctionData, {Acc, Seen}) ->
            case maps:get(NifFunctionName, Seen, undefined) of
                undefined ->
                    {
                        [{NifFunctionName, NifFunctionData}|Acc],
                        maps:put(NifFunctionName, NifFunctionData, Seen)
                    };
                ExistingNifFunctionData ->
                    case
                        nif_function_data_signature(ExistingNifFunctionData) =:=
                        nif_function_data_signature(NifFunctionData)
                    of
                        true ->
                            {Acc, Seen};
                        false ->
                            erlang:error({
                                duplicate_nif_function,
                                NifFunctionName,
                                ExistingNifFunctionData,
                                NifFunctionData
                            })
                    end
            end
        end, {Acc0, Seen0}, maps:get(nif_functions, FunctionData))
    end, {[], #{}}, Functions),
    lists:reverse(ReversedFunctions).

nif_function_data_signature(NifFunctionData) ->
    maps:put(
        params,
        [ParamData || {_ParamName, ParamData} <- maps:get(params, NifFunctionData)],
        NifFunctionData
    ).

write_extra_functions(BindingData) ->
    write_enum_groups_function(BindingData),
    write_enums_function(BindingData),
    write_enum_value_function(BindingData),
    write_value_enums_function(BindingData),
    write_value_enum_function(BindingData),

    % XXX: Same for bitfields?

    ok.

write_enum_groups_function(_BindingData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, """
Return all OpenGL enum groups.

It returns the name of all OpenGL enum groups. The names can be used as an
input for the `enums_/1` function.

```erlang
[
    front_face_direction,
    % ...
    triangle_face
] = gl:enum_groups_().
```

Also see the `?GL_ENUM_GROUPS_` macro.
"""),
    write(0, "\n\"\"\".\n"),
    write(0, "-spec enum_groups_() -> [atom()].\n"),
    write(0, "enum_groups_() ->\n"),
    write(0, "    ?GL_ENUM_GROUPS_.\n"),
    write(0, "\n").

write_enums_function(_BindingData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, """
Return the OpenGL enums of a given group.

It returns the list of OpenGL enum names of a given group.

```erlang
[
    front_and_back,
    front,
    back
] = gl:enums_(triangle_face).
```

Also see the `?GL_ENUM_GROUPS_` macro.
"""),
    write(0, "\n\"\"\".\n"),
    write(0, "-spec enums_(atom()) -> [atom()].\n"),
    write(0, "enums_(EnumGroup) ->\n"),
    write(0, "    maps:get(EnumGroup, ?GL_ENUMS_).\n"),
    write(0, "\n").

write_enum_value_function(_BindingData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, """
Return the raw value of an OpenGL enum.

It returns the raw integer value of an OpenGL enum.

```erlang
16#0408 = gl:enum_value_(front_and_back).
```

Also see the `?GL_ENUM_GROUPS_` macro.
"""),
    write(0, "\n\"\"\".\n"),
    write(0, "-spec enum_value_(atom()) -> non_neg_integer().\n"),
    write(0, "enum_value_(Enum) ->\n"),
    write(0, "    maps:get(Enum, ?GL_ENUM_VALUE_).\n"),
    write(0, "\n").

write_value_enums_function(_BindingData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, """
Return the possible OpenGL enums of a given raw value.

It returns the possible OpenGL enums of a given raw value.

```erlang
[foo, bar] = gl:value_enums_(16#0409).
```

> It's useful when an OpenGL function returns a raw value instead of an OpenGL
> enum such as with the `gl:get/x` function.

Also see the `?GL_VALUE_ENUMS_` macro.
"""),
    write(0, "\n\"\"\".\n"),
    write(0, "-spec value_enums_(non_neg_integer()) -> [atom()].\n"),
    write(0, "value_enums_(Value) ->\n"),
    write(0, "    maps:get(Value, ?GL_VALUE_ENUMS_).\n"),
    write(0, "\n").

write_value_enum_function(_BindingData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, """
Return the OpenGL enum of a given raw value and the enum group.

It returns the OpenGL enum of a given raw value and the enum group.

```erlang
bar = gl:value_enums_(16#0409, foo).
```

> It's useful when an OpenGL function returns a raw value instead of an OpenGL
> enum such as with the `gl:get/x` function.

Also see the `?GL_VALUE_ENUM_` macro.
"""),
    write(0, "\n\"\"\".\n"),
    write(0, "-spec value_enum_(non_neg_integer(), atom()) -> atom().\n"),
    write(0, "value_enum_(Value, EnumGroup) ->\n"),
    write(0, "    maps:get({Value, EnumGroup}, ?GL_VALUE_ENUM_).\n"),
    write(0, "\n").
