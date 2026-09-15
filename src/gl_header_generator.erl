%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It generates the `gl.hrl` file from the binding data that were previously
%% computed.
%%
-module(gl_header_generator).
-export([generate/2]).
-import(opengl_gen, [open/1, write/2, write/3, close/0]).

generate(TargetApi, BindingData) ->
    open("gl.hrl"),

    write(0, "%% This file is generated. Do not edit!\n"),
    write(0, "\n"),

    write(0, "-define(GL_BINDING_API, ~p).\n", [erlang:element(1, TargetApi)]),
    write(0, "-define(GL_BINDING_VERSION, ~p).\n", [erlang:element(2, TargetApi)]),
    write(0, "\n"),

    write_gl_enums_macros(BindingData),

    write_enum_groups_macro(BindingData),
    write_enums_macro(BindingData),
    write_enum_value(BindingData),
    write_value_enums(BindingData),
    write_value_enum(BindingData),

    write_bitfield_groups_macro(BindingData),
    write_bitfields_macro(BindingData),

    write_unpack_vector_macros(),
    write_pack_vector_macros(),
    write_unpack_matrix_macros(),
    write_pack_matrix_macros(),

    close(),

    ok.

write_gl_enums_macros(BindingData) ->
    maps:foreach(fun(Name, Value) ->
        write(0, "-define(~s, ~s).\n", [Name, Value])
    end, maps:get(gl_enums_value_map, BindingData)),
    write(0, "\n").

write_enum_groups_macro(BindingData) ->
    Tokens = maps:keys(maps:get(enum_types, BindingData)),
    String = string:join(Tokens, ",\n    "),
    write(0, "-define(GL_ENUM_GROUPS_, [\n    ~s\n]).\n\n", [String]).

write_enums_macro(BindingData) ->
    Tokens1 = maps:fold(fun(Key, Values, Acc) ->
        String2 = string:join(Values, ",\n        "),
        Acc ++ [io_lib:format("    ~s => [\n        ~s\n    ]", [Key, String2])]
    end, [], maps:get(enum_types, BindingData)),
    String1 = string:join(Tokens1, ",\n"),
    write(0, "-define(GL_ENUMS_, #{\n~s\n}).\n\n", [String1]).

write_enum_value(BindingData) ->
    EnumsNameMap = maps:get(gl_enums_name_map, BindingData),
    EnumsValueMap = maps:get(gl_enums_value_map, BindingData),
    Data = maps:fold(fun(_EnumGroup, Enums, Acc1) ->
        lists:foldl(fun(Enum, Acc2) ->
            maps:put(Enum, maps:get(maps:get(Enum, EnumsNameMap), EnumsValueMap), Acc2)
        end, Acc1, Enums)
    end, #{}, maps:get(enum_types, BindingData)),
    Tokens = maps:fold(fun(Key, Value, Acc) ->
        [io_lib:format("~s => ~s", [Key, Value])|Acc]
    end, [], Data),
    String = string:join(Tokens, ",\n    "),
    write(0, "-define(GL_ENUM_VALUE_, #{\n    ~s\n}).\n\n", [String]).

write_value_enums(BindingData) ->
    EnumsNameMapInvert = maps:fold(fun(Key, Value, Acc) ->
        maps:put(Value, Key, Acc)
    end, #{}, maps:get(gl_enums_name_map, BindingData)),
    % Note that there's a more natural way to implement this is in order but it
    % works that way too.
    Data = maps:fold(fun(Key, Value, Acc) ->
        Items = maps:get(Value, Acc, []),
        case maps:get(Key, EnumsNameMapInvert, undefined) of
            undefined ->
                Acc;
            Item ->
                maps:put(Value, [Item|Items], Acc)
        end
    end, #{}, maps:get(gl_enums_value_map, BindingData)),
    Tokens = maps:fold(fun(Key, Value, Acc) ->
        [io_lib:format("~s => [~s]", [Key, string:join(Value, ", ")])|Acc]
    end, [], Data),
    String = string:join(Tokens, ",\n    "),
    write(0, "-define(GL_VALUE_ENUMS_, #{\n    ~s\n}).\n\n", [String]).

write_value_enum(BindingData) ->
    EnumsNameMap = maps:get(gl_enums_name_map, BindingData),
    EnumsValueMap = maps:get(gl_enums_value_map, BindingData),
    Data = maps:fold(fun(EnumGroup, Enums, Acc1) ->
        lists:foldl(fun(Enum, Acc2) ->
            Value = maps:get(maps:get(Enum, EnumsNameMap), EnumsValueMap),
            maps:put({Value, EnumGroup}, Enum, Acc2)
        end, Acc1, Enums)
    end, #{}, maps:get(enum_types, BindingData)),
    Tokens = maps:fold(fun({Value, EnumGroup}, Enum, Acc) ->
        [io_lib:format("{~s, ~s} => ~s", [Value, EnumGroup, Enum])|Acc]
    end, [], Data),
    String = string:join(Tokens, ",\n    "),
    write(0, "-define(GL_VALUE_ENUM_, #{\n    ~s\n}).\n\n", [String]).

write_bitfield_groups_macro(BindingData) ->
    Tokens = maps:keys(maps:get(bitfield_types, BindingData)),
    String = string:join(Tokens, ",\n    "),
    write(0, "-define(GL_BITFIELD_GROUPS_, [\n    ~s\n]).\n\n", [String]).

write_bitfields_macro(BindingData) ->
    Tokens1 = maps:fold(fun(Key, Values, Acc) ->
        String2 = string:join(Values, ",\n        "),
        Acc ++ [io_lib:format("    ~s => [\n        ~s\n    ]", [Key, String2])]
    end, [], maps:get(bitfield_types, BindingData)),
    String1 = string:join(Tokens1, ",\n"),
    write(0, "-define(GL_BITFIELDS_, #{\n~s\n}).\n\n", [String1]).

write_unpack_vector_macros() ->
    write(0, """
-define(GL_UNPACK_VECTOR_1(Values),
    begin
        [V1] = Values,
        {V1}
    end
).

-define(GL_UNPACK_VECTOR_2(Values),
    begin
        [V1, V2] = Values,
        {V1, V2}
    end
).

-define(GL_UNPACK_VECTOR_3(Values),
    begin
        [V1, V2, V3] = Values,
        {V1, V2, V3}
    end
).

-define(GL_UNPACK_VECTOR_4(Values),
    begin
        [V1, V2, V3, V4] = Values,
        {V1, V2, V3, V4}
    end
).

""").

write_pack_vector_macros() ->
    write(0, """
-define(GL_PACK_VECTOR_1(Vector),
    begin
        {V1} = Vector,
        [V1]
    end
).

-define(GL_PACK_VECTOR_2(Vector),
    begin
        {V1, V2} = Vector,
        [V1, V2]
    end
).

-define(GL_PACK_VECTOR_3(Vector),
    begin
        {V1, V2, V3} = Vector,
        [V1, V2, V3]
    end
).

-define(GL_PACK_VECTOR_4(Vector),
    begin
        {V1, V2, V3, V4} = Vector,
        [V1, V2, V3, V4]
    end
).

""").

write_unpack_matrix_macros() ->
    write(0, """
-define(GL_UNPACK_MATRIX_2x2(Values),
    begin
        [V1, V2, V3, V4] = Values,
        {
            {V1, V2},
            {V3, V4}
        }
    end
).

-define(GL_UNPACK_MATRIX_3x3(Values),
    begin
        [V1, V2, V3, V4, V5, V6, V7, V8, V9] = Values,
        {
            {V1, V2, V3},
            {V4, V5, V6},
            {V7, V8, V9}
        }
    end
).

-define(GL_UNPACK_MATRIX_4x4(Values),
    begin
        [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16] = Values,
        {
            {V1, V2, V3, V4},
            {V5, V6, V7, V8},
            {V9, V10, V11, V12},
            {V13, V14, V15, V16}
        }
    end
).

-define(GL_UNPACK_MATRIX_2x3(Values),
    begin
        [V1, V2, V3, V4, V5, V6] = Values,
        {
            {V1, V2, V3},
            {V4, V5, V6}
        }
    end
).

-define(GL_UNPACK_MATRIX_3x2(Values),
    begin
        [V1, V2, V3, V4, V5, V6] = Values,
        {
            {V1, V2},
            {V3, V4},
            {V5, V6}
        }
    end
).

-define(GL_UNPACK_MATRIX_2x4(Values),
    begin
        [V1, V2, V3, V4, V5, V6, V7, V8] = Values,
        {
            {V1, V2, V3, V4},
            {V5, V6, V7, V8}
        }
    end
).


-define(GL_UNPACK_MATRIX_4x2(Values),
    begin
        [V1, V2, V3, V4, V5, V6, V7, V8] = Values,
        {
            {V1, V2},
            {V3, V4},
            {V5, V6},
            {V7, V8}
        }
    end
).

-define(GL_UNPACK_MATRIX_3x4(Values),
    begin
        [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12] = Values,
        {
            {V1, V2, V3, V4},
            {V5, V6, V7, V8},
            {V9, V10, V11, V12}
        }
    end
).

-define(GL_UNPACK_MATRIX_4x3(Values),
    begin
        [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12] = Values,
        {
            {V1, V2, V3},
            {V4, V5, V6},
            {V7, V8, V9},
            {V10, V11, V12}
        }
    end
).

""").

write_pack_matrix_macros() ->
    write(0, """
-define(GL_PACK_MATRIX_2x2(Matrix),
    begin
        {
            {V1, V2},
            {V3, V4}
        } = Matrix,
        [V1, V2, V3, V4]
    end
).

-define(GL_PACK_MATRIX_3x3(Matrix),
    begin
        {
            {V1, V2, V3},
            {V4, V5, V6},
            {V7, V8, V9}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6, V7, V8, V9]
    end
).

-define(GL_PACK_MATRIX_4x4(Matrix),
    begin
        {
            {V1, V2, V3, V4},
            {V5, V6, V7, V8},
            {V9, V10, V11, V12},
            {V13, V14, V15, V16}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16]
    end
).

-define(GL_PACK_MATRIX_2x3(Matrix),
    begin
        {
            {V1, V2, V3},
            {V4, V5, V6}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6]
    end
).

-define(GL_PACK_MATRIX_3x2(Matrix),
    begin
        {
            {V1, V2},
            {V3, V4},
            {V5, V6}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6]
    end
).

-define(GL_PACK_MATRIX_2x4(Matrix),
    begin
        {
            {V1, V2, V3, V4},
            {V5, V6, V7, V8}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6, V7, V8]
    end
).

-define(GL_PACK_MATRIX_4x2(Matrix),
    begin
        {
            {V1, V2},
            {V3, V4},
            {V5, V6},
            {V7, V8}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6, V7, V8]
    end
).

-define(GL_PACK_MATRIX_3x4(Matrix),
    begin
        {
            {V1, V2, V3, V4},
            {V5, V6, V7, V8},
            {V9, V10, V11, V12}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12]
    end
).

-define(GL_PACK_MATRIX_4x3(Matrix),
    begin
        {
            {V1, V2, V3},
            {V4, V5, V6},
            {V7, V8, V9},
            {V10, V11, V12}
        } = Matrix,
        [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12]
    end
).

""").
