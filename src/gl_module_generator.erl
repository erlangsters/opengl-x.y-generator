%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It does this and that...
%%
-module(gl_module_generator).
-export([generate/1]).
-import(opengl_gen, [open/1, write/2, write/3, close/0]).

generate(BindingData) ->
    % io:format("binding data: ~p~n",[maps:get(functions, BindingData)]),

    open("/home/intjelic/Workspace/erlangsters/opengl-x.y-generator/output/gl.erl"),
    % open("/home/intjelic/Workspace/erlangsters/opengl-4.6/src/gl.erl"),

    write(0, "%% This file is generated. Do not edit!\n"),
    write(0, "-module(gl).\n"),
    write_module_doc(BindingData),
    write(0, "\n"),

    % -- extra
    write(0, "% Low-level OpenGL types.\n"),
    write(0, "-export_type([scalar/1]).\n"),
    write(0, "-export_type([vector1/1, vector2/1, vector3/1, vector4/1]).\n"),
    write(0, "-export_type([matrix2/1, matrix3/1, matrix4/1]).\n"),
    write(0, "-export_type([matrix2x3/1, matrix3x2/1]).\n"),
    write(0, "-export_type([matrix2x4/1, matrix4x2/1]).\n"),
    write(0, "-export_type([matrix3x4/1, matrix4x3/1]).\n"),
    write(0, "\n"),
    % -- extra: end

    write(0, "\n"),
    write(0, "-export_type([enum/0]).\n"),
    write(0, "-export_type([bitfield/0]).\n"),
    write(0, "\n"),

    write(0, "% High-level OpenGL types.\n"),
    write_export_object_types(maps:get(object_types, BindingData)),
    write(0, "\n"),

    write(0, "% The OpenGL enum types.\n"),
    write_export_enum_types(maps:get(enum_types, BindingData)),
    write(0, "\n"),

    write(0, "% The OpenGL bitfield types.\n"),
    write_export_bitfield_types(maps:get(bitfield_types, BindingData)),
    write(0, "\n"),

    % write(0, "% Extra functions.\n"),
    % write(0, "-export_type([enums_map/1]).\n"),
    % write(0, "-export_type([integers_map/1]).\n"),
    % write(0, "-export_type([integer_to_enum/1]).\n"),
    % write(0, "-export_type([enum_to_integer/1]).\n"),

    write_export_functions(maps:get(functions, BindingData)),
    write(0, "\n"),

    % xxx
    % write(0, "-include_lib(\"opengl_pilot/include/gl.hrl\").\n\n"),
    write(0, "-include(\"../include/gl.hrl\").\n\n"),

    % -- extra
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type scalar(Type) :: Type.\n"),
    write(0, "\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type vector1(Type) :: {scalar(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type vector2(Type) :: {scalar(Type), scalar(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type vector3(Type) :: {scalar(Type), scalar(Type), scalar(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type vector4(Type) :: {scalar(Type), scalar(Type), scalar(Type), scalar(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "\n"),
    write(0, "-type matrix2(Type) :: {vector2(Type), vector2(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type matrix3(Type) :: {vector3(Type), vector3(Type), vector3(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type matrix4(Type) :: {vector4(Type), vector4(Type), vector4(Type), vector4(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "\n"),
    write(0, "-type matrix2x3(Type) :: {vector3(Type), vector3(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type matrix3x2(Type) :: {vector2(Type), vector2(Type), vector2(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "\n"),
    write(0, "-type matrix2x4(Type) :: {vector4(Type), vector4(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type matrix4x2(Type) :: {vector2(Type), vector2(Type), vector2(Type), vector2(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "\n"),
    write(0, "-type matrix3x4(Type) :: {vector4(Type), vector4(Type), vector4(Type)}.\n"),
    write(0, "-doc \"to be written.\".\n"),
    write(0, "-type matrix4x3(Type) :: {vector3(Type), vector3(Type), vector3(Type), vector3(Type)}.\n"),
    write(0, "\n"),
    % -- extra: end

    write(0, "\n"),
    write(0, "-doc \"An OpenGL enum..\".\n"),
    write(0, "-type enum() :: atom().\n"),
    write(0, "-doc \"An OpenGL bitfield.\".\n"),
    write(0, "-type bitfield() :: [atom()].\n"),
    write(0, "\n"),

    write_object_types(maps:get(object_types, BindingData)),
    write(0, "\n"),
    write_enum_types(maps:get(enum_types, BindingData)),
    write(0, "\n"),
    write_bitfield_types(maps:get(bitfield_types, BindingData)),
    write(0, "\n"),
    write_extra_types(maps:get(extra_types, BindingData)),
    write(0, "\n"),

    write_nif_attributes(maps:get(functions, BindingData)),
    write(0, "\n"),
    write(0, "-on_load(init_nif/0).\n"),
    write(0, "\n"),
    write(0, """
init_nif() ->
    PrivDir = code:priv_dir(?MODULE),
    NifPath = filename:join(PrivDir, "beam-gl"),
    ok = erlang:load_nif(NifPath, 0).
""", []),
    write(0, "\n\n"),

    maps:foreach(fun({FunctionName, _}, FunctionData) ->
        write_function(FunctionName, FunctionData)
    end, maps:get(functions, BindingData)),
    write(0, "\n"),

    write_nif_placeholders(maps:get(functions, BindingData)),

    close(),

    ok.

write_module_doc(BindingData) ->
    write(0, "-moduledoc \"\"\"\n"),
    write(0, "~s binding.\n\n", [maps:get(api_name, BindingData)]),
    write(0,
        "It implements the API of ~s, adjusted for BEAM (Erlang language).\n\n",
        [maps:get(api_name, BindingData)]
    ),

    write(0, """
blabla...
Knowledge of the EGL API in C is assumed, as this module is a direct binding to
the C API. xXxx

XXX: To be written.

Another source of reference is the OpenGL samples project that provides working
C samples with their counterparts in Erlang (and later Elixir).
Repo: [OpenGL Samples](https://github.com/erlangsters/opengl-samples).

Another source of reference is the
[test suites](https://github.com/erlangsters/egl-1.5/tree/master/test)
""", []),
    write(0, "\n\"\"\".\n").

write_export_object_types(ObjectTypes) ->
    lists:foreach(fun({ObjectType, _}) ->
        write(0, "-export_type([~s/0]).\n", [ObjectType])
    end, ObjectTypes).

write_export_enum_types(BitfieldTypes) ->
    maps:foreach(fun(BitfieldName, _BitfieldValues) ->
        write(0, "-export_type([~s/0]).\n", [BitfieldName])
    end, BitfieldTypes).

write_export_bitfield_types(EnumTypes) ->
    maps:foreach(fun(EnumName, _EnumValues) ->
        write(0, "-export_type([~s/0]).\n", [EnumName])
    end, EnumTypes).

write_export_extra_types(ExtraTypes) ->
    lists:foreach(fun({TypeName, _TypeSpecs}) ->
        write(0, "-export_type([~s/0]).\n", [TypeName])
    end, ExtraTypes).

write_export_functions(Functions) ->
    maps:foreach(fun({FunctionName, _}, FunctionData) ->
        Arity = length(maps:get(params_specs, FunctionData)),
        write(0, "-export([~s/~p]).\n", [FunctionName, Arity])
    end, Functions).

write_object_types(ObjectTypes) ->
    lists:foreach(fun({ObjectType, Documentation}) ->
        write(0, "-doc \"~s\".\n", [Documentation]),
        write(0, "-type ~s() :: pos_integer().\n", [ObjectType])
    end, ObjectTypes).

write_enum_types(EnumTypes) ->
    maps:foreach(fun(EnumName, EnumValues) ->
        write(0, "-doc \"The OpenGL `~s` enum.\".\n", [EnumName]),
        write(0, "-type ~s() :: \n", [EnumName]),
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

write_extra_types(ExtraTypes) ->
    lists:foreach(fun({TypeName, TypeSpecs}) ->
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
        end
    end, ExtraTypes).

write_nif_attributes(Functions) ->
    maps:foreach(fun(_FunctionName, FunctionData) ->
        maps:foreach(fun(NifFunctionName, NifFunctionData) ->
            Arity = maps:get(arity, NifFunctionData),
            write(0, "-nifs([~s_raw/~p]).\n", [NifFunctionName, Arity])
        end, maps:get(nif_functions, FunctionData))
    end, Functions).

write_function(FunctionName, FunctionData) ->
    % io:format(user, "Function Name: ~s~n", [FunctionName]),
    % io:format(user, "Function Data: ~p~n", [_FunctionData]),
    write_function_doc(FunctionName, FunctionData),
    write_function_specs(FunctionName, FunctionData),

    FunctionClauses = lists:map(fun(FunctionClauseData) ->
        make_function_clause_bit(FunctionName, FunctionClauseData)
    end, maps:get(function_clauses, FunctionData)),
    write(0, "~s.\n\n", [string:join(FunctionClauses, ";\n")]),

    ok.

write_function_doc(FunctionName, FunctionData) ->
    write(0, "-doc \"\"\"\n"),
    write(0, "~s\n\n", [maps:get(doc_description, FunctionData)]),
    case maps:get(gl_command, FunctionData) of
        undefined ->
            % It's a "indirect commands"
            _GlCommandNames = maps:get(gl_commands, FunctionData),
            % XXX to be written
            ok;
        GlFunctionName ->
            write(0, "It implements the `~s` function\n\n", [GlFunctionName])

    end,

    % If an example is provided, write it.
    case maps:get(doc_example, FunctionData) of
        undefined ->
            ok;
        Example ->
            write(0, "```\n", []),
            write(0, "~s", [Example]),
            write(0, "\n\n```\n\n", [])
    end,

    write(0,
        "Consult the documentation of the underlying [OpenGL function](~s) for"
        "more information.\n",
        [maps:get(doc_url, FunctionData)]
    ),
    write(0, "\"\"\".\n"),

    ok.

% -type type_leaf() :: {
%     Module :: undefined | atom(),
%     Name :: atom(),
%     Args :: [string()]
% }.
% -type type_specs() ::
%     % A reference to a concrete type. Example: io_lib:chars().
%     type_leaf() |
%     % A list. Example: [io_lib:chars()].
%     {list, type_specs()} |
%     % A set. Example: [foo | bar].
%     {set, type_specs()} |
%     % A constant. Example: foo
%     {atom, atom()}
% .

% -spec stringify_type_specs(type_specs()) -> io_lib:chars().
% stringify_type_specs({undefined, Name, []}) ->
%     io_lib:format("~s()", [Name]);
stringify_type_specs({undefined, Name, Params}) ->
    Parts = lists:map(fun stringify_type_specs/1, Params),
    io_lib:format("~s(~s)", [Name, string:join(Parts, ", ")]);
stringify_type_specs({Module, Name, []}) ->
    io_lib:format("~s:~s()", [Module, Name]);
stringify_type_specs({list, TypeSpecs}) ->
    io_lib:format("[~s]", [stringify_type_specs(TypeSpecs)]);
stringify_type_specs({set, TypeSpecs}) when is_list(TypeSpecs) ->
    Strings = lists:map(fun stringify_type_specs/1, TypeSpecs),
    string:join(Strings, " | ");
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
    % io:format("processing ~p function~n", [FunctionName]),
    % io:format("inspect: ~p~n", [maps:get(specs_params, FunctionData)]),
    SpecsParams = make_specs_params_bit(maps:get(specs_params, FunctionData)),
    SpecsReturn = make_specs_return_bit(maps:get(specs_return, FunctionData)),

    write(0, "-spec ~s(", [FunctionName]),
    write(0, SpecsParams),
    write(0, ") -> ~s | {error, atom()}.\n", [SpecsReturn]),

    ok.

make_function_clause_guard_bit(_Guard) ->
    ok.

make_raw_function_call_bit(RawFunction, Args) ->
    io_lib:format(
        "    ~s_raw(~s)",
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


transform_param_to_arg(ParamName, do_nothing) ->
    {"", ParamName};
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

transform_param_to_arg(ParamName, normalize_list_strings_or_binary) ->
    % The parameter is a list of strings or binaries. We normalize it by
    % converting it to a list of binaries.
    ArgName = ParamName ++ "New",
    ItemArgName = ParamName ++ "Item",
    Bit1 = io_lib:format("    ~s = lists:map(fun\n", [ArgName]),
    Bit2 = io_lib:format(
        "        (~s) when is_list(~s) -> list_to_binary(~s);\n",
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

make_function_clause_body(Params, RawFunction) ->
    {Bits, Args} = lists:foldl(fun({ParamName, ParamRule}, {BitsAcc, ArgsAcc}) ->
        {MoreBits, Arg} = transform_param_to_arg(ParamName, ParamRule),
        {[MoreBits|BitsAcc], [Arg |ArgsAcc]}
    end, {"", []}, Params),
    Bit = make_raw_function_call_bit(RawFunction, lists:reverse(Args)),
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
    io_lib:format("is_float(~s)", [stringify_guard(Guard, VarName)]).

make_function_clause_bit(FunctionName, FunctionClause) ->

    ParamBits = lists:map(fun({ParamName, _}) ->
        ParamName
    end, maps:get(params, FunctionClause)),
    ClauseParams = string:join(ParamBits, ", "),

    GuardBits = lists:map(fun(Guard) ->
        stringify_guard(Guard, "MyVar")
    end, maps:get(guards, FunctionClause)),
    ClauseGuards = string:join(GuardBits, " andalso\n    "),

    ClauseBody = make_function_clause_body(
        maps:get(params, FunctionClause),
        maps:get(raw_function, FunctionClause)
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
                "~s(~s) when \n    ~s \n->\n~s",
                [FunctionName, ClauseParams, ClauseGuards, ClauseBody]
            )
    end.

write_nif_placeholders(Functions) ->
    % XXX: A `-spec` attribute could be added here, for clarity, but it's not
    %      essential as it's private API (it would just help read the generated
    %      code more easily).

    maps:foreach(fun(_FunctionName, FunctionData) ->
        maps:foreach(fun(NifFunctionName, NifFunctionData) ->
            NifFunctionParams = lists:map(fun({ParamName, _}) ->
                "_" ++ ParamName
            end, maps:get(params, NifFunctionData)),
            write(0, "~s_raw(", [NifFunctionName]),
            write(0, string:join(NifFunctionParams, ", ")),
            write(0, ") -> \n"),
            write(0, "    erlang:nif_error(nif_library_not_loaded).\n"),
            write(0, "\n")
        end, maps:get(nif_functions, FunctionData))
    end, Functions),

    ok.
