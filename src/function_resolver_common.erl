%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% Common functions used by the direct and indirect function resolvers.
%%
-module(function_resolver_common).

-export([resolve_function_specs/1, resolve_function_specs/2]).
-export([resolve_nif_function/2]).

resolve_function_specs(FunctionData) ->
    % If there is no `gl_x` in the param and return specs, the second parameter
    % is unused and can be null.
    resolve_function_specs(FunctionData, undefined).

resolve_function_specs(FunctionData, TypeSpecsX) ->
    % We iterate over the list of parameters (as specified by the binding
    % specs) and build a list of actual parameters and return values. If there
    % is no return value, the function return 'ok'. If there are one or more
    % return values, the function return a tuple {ok, V1, V2, ...}.
    {SpecsParamsTmp, SpecsReturn0Tmp} = lists:foldl(
        fun resolve_function_specs_param_fold/2,
        {[], []},
        maps:get(params_specs, FunctionData)
    ),
    SpecsParams = lists:map(fun
        ({Name, gl_x}) -> {Name, TypeSpecsX};
        (Any) -> Any
    end, SpecsParamsTmp),
    SpecsReturn0 = lists:map(fun
        ({Name, gl_x}) -> {Name, TypeSpecsX};
        (Any) -> Any
    end, SpecsReturn0Tmp),

    % If there is no "out" parameters, perhaps a return value is specified
    % instead ("out" parameters and a specified return value are mutually
    % exclusive).
    SpecsReturn1 = case {SpecsReturn0, maps:get(return_specs, FunctionData)} of
        {[], gl_void} ->
            % No "out" parameters and no return value specified. It means there
            % really is not any return value.
            [];
        {_, gl_void} ->
            SpecsReturn0;
        {[], Specs} ->
            % No "out" parameters, but a return value is specified.
            % XXX: Should binding specs specify a return value name?
            [{"NoName", resolve_param_to_type_specs(Specs)}]
    end,

    {lists:reverse(SpecsParams), lists:reverse(SpecsReturn1)}.


resolve_param_to_type_specs(gl_x) ->
    gl_x;
resolve_param_to_type_specs(gl_bool) ->
    {undefined, boolean, []};
resolve_param_to_type_specs(gl_int) ->
    {undefined, integer, []};
resolve_param_to_type_specs(gl_int64) ->
    {undefined, integer, []};
resolve_param_to_type_specs(gl_uint) ->
    {undefined, pos_integer, []};
resolve_param_to_type_specs(gl_uint64) ->
    {undefined, pos_integer, []};
resolve_param_to_type_specs(gl_float) ->
    {undefined, float, []};
resolve_param_to_type_specs(gl_double) ->
    {undefined, float, []};
resolve_param_to_type_specs(gl_sizei) ->
    {undefined, integer, []};
resolve_param_to_type_specs(gl_intptr) ->
    {undefined, integer, []};
resolve_param_to_type_specs(gl_sizeiptr) ->
    {undefined, integer, []};

resolve_param_to_type_specs(gl_offset) ->
    % Example: glVertexAttribPointer()
    {undefined, integer, []};
resolve_param_to_type_specs(gl_binary_or_null) ->
    % Example: glBufferData()
    {set, [undefined, {undefined, binary, []}]};

resolve_param_to_type_specs(gl_string) ->
    % Example: glGetShaderSource(), glGetProgramInfoLog(), etc.
    {set, [{undefined, string, []}, {undefined, binary, []}]};

resolve_param_to_type_specs({gl_string, char}) ->
    % A pointer to char (aka string) in C is modelled as either a string() or
    % a binary() in Erlang.
    % Example: glXXX().
    {set, [{undefined, string, []}, {undefined, binary, []}]};

resolve_param_to_type_specs({gl_string, glubyte}) ->
    {set, [{undefined, string, []}, {undefined, binary, []}]};


resolve_param_to_type_specs(gl_binary) ->
    {undefined, binary, []};
resolve_param_to_type_specs({gl_enum, V}) ->
    Name = opengl_gen:erlangify_enum_group_name(V),
    {undefined, list_to_atom(Name), []};
resolve_param_to_type_specs({gl_bitfield, V}) ->
    Name = opengl_gen:erlangify_enum_group_name(V),
    {undefined, list_to_atom(Name), []};
resolve_param_to_type_specs({gl_object, Name}) ->
    {undefined, Name, []};
resolve_param_to_type_specs(list_gl_strings)->
    % The parameter is a list of strings or binaries (which is normalized 
    % before reaching the NIF function).
    % Example: glShaderSource().
    {list, {set, [{undefined, string, []}, {undefined, binary, []}]}};
resolve_param_to_type_specs({list, ItemSpecs}) when ItemSpecs =/= gl_x ->
    {list, resolve_param_to_type_specs(ItemSpecs)}.



resolve_function_specs_param_fold({in, ParamName, ParamSpecs}, {ParamsAcc, ReturnAcc}) ->
    % Most of the time, an "in" parameter corresponds to a single parameter.
    TypeSpecs = resolve_param_to_type_specs(ParamSpecs),
    {[{ParamName, TypeSpecs}|ParamsAcc], ReturnAcc};

resolve_function_specs_param_fold({out, Name, gl_string}, {ParamsAcc, ReturnAcc}) ->

    SizeName = "StringSize",
    SizeType = {undefined, pos_integer, []},
    Type = {undefined, binary, []},
    {
        [{SizeName, SizeType}|ParamsAcc],
        [{Name, Type}|ReturnAcc]
    };

resolve_function_specs_param_fold({out, Name, {gl_binary, {_, SizeName}}}, {ParamsAcc, ReturnAcc}) ->
    % It generates an additional parameter to specify the size of the
    % allocated binary. Example: glReadPixels().
    SizeType = {undefined, pos_integer, []},
    Type = {undefined, binary, []},
    {
        [{SizeName, SizeType}|ParamsAcc],
        [{Name, Type}|ReturnAcc]
    };
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
resolve_function_specs_param_fold({out, Name, {{list, 2}, Item}}, {ParamsAcc, ReturnAcc}) ->
    % It generates a parameter to specify the number of values to request
    % OpenGL (and indirectly, to know how much memory to allocate), and a
    % return value which is the list itself.
    % Example: glGenTextures()
    % XXX: Param name ???
    SizeType = {undefined, pos_integer, []},
    {
        [{"N", SizeType}|ParamsAcc],
        [{Name, {list, resolve_param_to_type_specs(Item)}}|ReturnAcc]
    };
resolve_function_specs_param_fold({out, Name, Specs}, {ParamsAcc, ReturnAcc}) ->
    % If this was not for any of the previous cases, an "out" parameter simply
    % corresponds to a single return value.
    {ParamsAcc, [{Name, resolve_param_to_type_specs(Specs)}|ReturnAcc]}.




resolve_nif_function(ParamsSpecs, ReturnSpecs) ->
    NifParams = resolve_nif_function_params(ParamsSpecs),
    NifReturn = resolve_nif_function_return(ReturnSpecs),
    {NifParams, NifReturn}.

% For each parameter, we dictate how the Erlang value passed to the NIF
% function is used to be eventually passed to the OpenGL function.
% XXX: It's going to have to change to support gl_vector which generates several params.
resolve_nif_function_param(in, gl_bool) ->
    boolean_to_glbool;
resolve_nif_function_param(in, gl_int) ->
    integer_to_glint;
% resolve_nif_function_param(in, gl_int64) -> % XXX: is needed ?
%     integer_to_glint64;
resolve_nif_function_param(in, gl_intptr) ->
    integer_to_glintptr;
resolve_nif_function_param(in, gl_uint) ->
    integer_to_gluint;
resolve_nif_function_param(in, gl_uint64) ->
    integer_to_gluint64;
resolve_nif_function_param(in, gl_float) ->
    double_to_glfloat;
resolve_nif_function_param(in, gl_double) ->
    double_to_gldouble;

resolve_nif_function_param(in, {gl_string, char}) ->
    binary_to_gl_string_char;

resolve_nif_function_param(in, list_gl_strings) ->
    % Only used by glShaderSource().
    in_list_gl_strings;
resolve_nif_function_param(out, gl_string) ->
    % Example: glGetShaderSource(), glGetProgramInfoLog(), etc.
    out_gl_string;

resolve_nif_function_param(in, gl_sizei) ->
    integer_to_glsizei;
resolve_nif_function_param(in, gl_sizeiptr) ->
    integer_to_glsizeiptr;



resolve_nif_function_param(in, gl_offset) ->
    % Example: glVertexAttribPointer().
    in_gl_offset;
resolve_nif_function_param(in, gl_binary_or_null) ->
    % Example: glBufferData().
    in_gl_binary_or_null;


resolve_nif_function_param(in, {gl_enum, _Group}) ->
    integer_to_glenum;
resolve_nif_function_param(in, {gl_bitfield, _Group}) ->
    integer_to_glbitfield;
resolve_nif_function_param(in, gl_binary) ->
    binary_to_glbinary;
resolve_nif_function_param(out, {gl_binary, {implicit, _}}) ->
    out_binary_implicit;
resolve_nif_function_param(out, {gl_binary, {explicit, _}}) ->
    out_binary_explicit;
resolve_nif_function_param(in, {gl_object, _Name}) ->
    % XXX: Ensure that all object are actual GLuints.
    % OpenGL objects are nothing but GLuint. xxx
    resolve_nif_function_param(in, gl_uint);


resolve_nif_function_param(in, {list, {gl_object, _Name}}) ->
    binary_to_glbinary;

resolve_nif_function_param(in, {list, _}) ->
    % XXX: Will have to be reworked.
    binary_to_glbinary;

resolve_nif_function_param(out, {{list, 1}, gl_bool}) ->
    % Example: glGetBooleanv().
    {out_list_alloc_1, "GLboolean", "gl_bool_to_erl_boolean"};

resolve_nif_function_param(out, {{list, 1}, gl_int}) ->
    % Example: glGetIntegerv().
    {out_list_alloc_1, "GLint", "enif_make_int"};
resolve_nif_function_param(out, {{list, 1}, gl_int64}) ->
    % Example: glGetInteger64v().
    {out_list_alloc_1, "GLint64", "enif_make_int"};
resolve_nif_function_param(out, {{list, 1}, gl_uint}) ->
    % Example: glGetIntegerv().
    {out_list_alloc_1, "GLuint", "enif_make_uint"};
resolve_nif_function_param(out, {{list, 1}, gl_float}) ->
    % Example: glGetFloatv().
    {out_list_alloc_1, "GLfloat", "enif_make_double"};
resolve_nif_function_param(out, {{list, 1}, gl_double}) ->
    % Example: glGetDoublev().
    {out_list_alloc_1, "GLdouble", "enif_make_double"};

resolve_nif_function_param(out, {{list, 2}, gl_uint}) ->
    % Example: No Example.
    {return_list_terms_alloc, "GLuint", "enif_make_uint"};
resolve_nif_function_param(out, {{list, N}, {gl_object, _Name}}) ->
    % Example: glGenTextures().
    % OpenGL objects are nothing but GLuint. xxx
    resolve_nif_function_param(out, {{list, N}, gl_uint}).



resolve_nif_function_params(ParamsSpecs) ->
    % Note that we preserve the names (which are not used in the generated C
    % code) so we can generate more readable code in the generated Erlang
    % module (see the NIF placeholders).
    ResolvedParams = lists:foldl(fun({Direction, Name, Specs}, Acc) ->
        [{Name, resolve_nif_function_param(Direction, Specs)}|Acc]
    end, [], ParamsSpecs),
    lists:reverse(ResolvedParams).

resolve_nif_function_return(gl_void) ->
    void;
resolve_nif_function_return(gl_bool) ->
    glbool_to_boolean;
resolve_nif_function_return({gl_object, _}) ->
    gluint_to_integer;
resolve_nif_function_return({gl_enum, Group}) ->
    TransformMap = write_this(Group),
    {glenum_to_atom, TransformMap};
resolve_nif_function_return({gl_string, glubyte}) ->
    const_glubyte_to_string.

write_this(_) -> [].
