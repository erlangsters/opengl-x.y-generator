%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It generates the `gl.c` file from the binding data that were previously
%% computed.
%%
-module(gl_nif_module_generator).
-export([generate/2]).
-import(opengl_gen, [open/1, write/2, write/3, close/0]).

generate(TargetApi, BindingData) ->
    open("gl.c"),

    write(0, "// This file is generated. Do not edit!\n"),
    write(0, "\n"),

    write(0, "#include <string.h>\n"),
    write(0, "#include <stdio.h>\n"),
    write(0, "#include <limits.h>\n"),
    write(0, "#include <stdint.h>\n"),
    write(0, "#if defined(_WIN32)\n"),
    write(0, "    #include <windows.h>\n"),
    write(0, "#else\n"),
    write(0, "    #include <dlfcn.h>\n"),
    write(0, "#endif\n"),
    write(0, "#include <erl_nif.h>\n"),
    write(0, "#include <EGL/egl.h>\n"),
    write_api_header(TargetApi),
    write(0, "\n"),

    % Compute list of atoms to be created (upfront).
    Atoms = maps:keys(maps:get(gl_enums_name_map, BindingData)),

    % Declare the atoms.
    lists:foreach(fun(Atom) ->
        write(0, "static ERL_NIF_TERM beam_atom_~s;\n", [Atom])
    end, Atoms),

    % Declare static pointer to the execute OpenGL command function of the EGL
    % binding NIF library.
    write(0, """
    static void* egl_nif_lib_handle = NULL;
    typedef ERL_NIF_TERM (*execute_command_fn)(
        ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
        ErlNifEnv*,
        int,
        const ERL_NIF_TERM argv[]
    );
    execute_command_fn egl_nif_execute_command = NULL;
    """),
    write(0, "\n\n"),

    write_nif_load_function(Atoms),
    write(0, "\n"),
    write_nif_unload_function(),

    % XXX: tmp code
    write(0, """
ERL_NIF_TERM custom_enif_make_bool(ErlNifEnv* env, GLboolean val) {
    return enif_make_atom(env, val == GL_TRUE ? "true" : "false");
}
int custom_enif_get_bool(ErlNifEnv* env, ERL_NIF_TERM term, GLboolean* bp) {
    if (enif_is_identical(term, enif_make_atom(env, "true"))) {
        *bp = GL_TRUE;
        return 1;
    } else if (enif_is_identical(term, enif_make_atom(env, "false"))) {
        *bp = GL_FALSE;
        return 1;
    } else {
        return 0;
    }
}
""", []),
    write(0, "\n\n"),

    write_glad_loader(TargetApi),

    % Generate the NIF functions.
    write_nif_functions(maps:get(functions, BindingData)),
    write(0, "\n"),

    % Generate the NIF functions static array.
    write_nif_array(TargetApi, maps:get(functions, BindingData)),
    write(0, "\n"),

    % Generate the ERL_NIF_INIT declaration.
    write(0, """
ERL_NIF_INIT(
    ~s,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
""", ["gl"]),
    write(0, "\n"),

    close(),

    ok.

needs_glad({gl, _}) ->
    true;
needs_glad({gles, _}) ->
    false.

write_api_header({gl, _}) ->
    write(0, "#include <glad/glad.h>\n");
write_api_header({gles, {2, _}}) ->
    write(0, "#include <GLES2/gl2.h>\n");
write_api_header({gles, {3, _}}) ->
    write(0, "#include <GLES3/gl32.h>\n").

write_glad_loader(TargetApi) ->
    case needs_glad(TargetApi) of
        true ->
            write(0, """
static ERL_NIF_TERM nif_gladLoadGl_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    if (!gladLoadGL()) {
        return enif_make_atom(env, "not_ok");
    } else {
        return enif_make_atom(env, "ok");
    }
}

static ERL_NIF_TERM nif_gladLoadGl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return egl_nif_execute_command(nif_gladLoadGl_command, env, argc, (ERL_NIF_TERM *)argv);
}
""", []),
            write(0, "\n");
        false ->
            ok
    end.

write_nif_load_function(Atoms) ->
    write(0, "static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)\n"),
    write(0, "{\n"),
    write(0, "    (void)priv_data;\n"),

    % Load the EGL NIF library, then get the pointer to the function that
    % executes the OpenGL command.
    write(0, """
char beam_egl_so_path[1024];
if (!enif_get_string(env, arg, beam_egl_so_path, sizeof(beam_egl_so_path), ERL_NIF_LATIN1)) {
    fprintf(stderr, "failed to read EGL binding library path from argument\n");
    return -1;
}

#if defined(_WIN32)
egl_nif_lib_handle = (void*)LoadLibraryA(beam_egl_so_path);
if (!egl_nif_lib_handle) {
    fprintf(stderr, "failed to load beam-egl.dll: %lu\n", GetLastError());
    return -1;
}

egl_nif_execute_command = (execute_command_fn)GetProcAddress(
    (HMODULE)egl_nif_lib_handle, "egl_execute_command");
if (!egl_nif_execute_command) {
    fprintf(stderr, "failed to load symbol egl_execute_command: %lu\n", GetLastError());
    FreeLibrary((HMODULE)egl_nif_lib_handle);
    return -1;
}
#else
egl_nif_lib_handle = dlopen(beam_egl_so_path, RTLD_NOW);
if (!egl_nif_lib_handle) {
    fprintf(stderr, "failed to load beam-egl.so: %s\n", dlerror());
    return -1;
}

egl_nif_execute_command = dlsym(egl_nif_lib_handle, "egl_execute_command");
if (!egl_nif_execute_command) {
    fprintf(stderr, "failed to load symbol egl_execute_command: %s\n", dlerror());
    dlclose(egl_nif_lib_handle);
    return -1;
}
#endif

"""),
    write(0, "\n\n"),

    % Create the atoms.
    lists:foreach(fun(Atom) ->
        write(1, "beam_atom_~s = enif_make_atom(env, \"~s\");\n", [Atom, Atom])
    end, Atoms),

    write(0, "    return 0;\n"),
    write(0, "}\n"),

    ok.

write_nif_unload_function() ->
    write(0, "static void nif_module_unload(ErlNifEnv* caller_env, void* priv_data)\n"),
    write(0, "{\n"),
    write(0, "    (void)caller_env;\n"),
    write(0, "    (void)priv_data;\n"),
    write(0, "}\n"),

    ok.

write_nif_functions(Functions) ->
    lists:foreach(fun({NifFunctionName, NifFunctionData}) ->
        write_nif_function(NifFunctionName, NifFunctionData)
    end, unique_nif_functions(Functions)),

    ok.

write_nif_function(FunctionName, FunctionData) ->
    write(0, "static ERL_NIF_TERM nif_~s_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])\n", [FunctionName]),
    write(0, "{\n"),
    case length(maps:get(params, FunctionData)) of
        0 ->
            write(1, "    (void)argv;\n\n");
        _ ->
            ok
    end,
    write_nif_function_body(FunctionName, FunctionData),
    write(0, "}\n"),
    write(0, "\n"),

    write(0, "static ERL_NIF_TERM nif_~s(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])\n", [FunctionName]),
    write(0, "{\n"),
    write(0, "    return egl_nif_execute_command(nif_~s_command, env, argc, (ERL_NIF_TERM *)argv);\n", [FunctionName]),
    write(0, "}\n"),
    write(0, "\n"),

    ok.

write_multi_draw_list_header(VarName, Index) ->
    write(1, "unsigned int ~s_drawcount_tmp;\n", [VarName]),
    write(1, "if (!enif_get_list_length(env, argv[~p], &~s_drawcount_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_drawcount_tmp == 0 || ~s_drawcount_tmp > (unsigned int)INT_MAX) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_drawcount = (GLsizei)~s_drawcount_tmp;\n", [VarName, VarName]),
    write(1, "ERL_NIF_TERM ~s_tail = argv[~p];\n", [VarName, Index]),
    write(1, "ERL_NIF_TERM ~s_head;\n", [VarName]).

write_multi_draw_alloc_check(VarName, Suffixes) ->
    Condition = string:join([lists:flatten(io_lib:format("!~s_~s", [VarName, Suffix])) || Suffix <- Suffixes], " || "),
    write(1, "if (~s) {\n", [Condition]),
    write_multi_draw_free_lines(VarName, Suffixes),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []).

write_multi_draw_badarg_free(VarName, Suffixes) ->
    write_multi_draw_free_lines(VarName, Suffixes),
    write(1, "        return enif_make_badarg(env);\n", []).

write_multi_draw_free_lines(VarName, Suffixes) ->
    lists:foreach(
        fun(Suffix) ->
            write(1, "    if (~s_~s) enif_free(~s_~s);\n", [VarName, Suffix, VarName, Suffix])
        end,
        Suffixes
    ).

write_multi_bind_list_header(VarName, Index) ->
    write(1, "unsigned int ~s_count_tmp;\n", [VarName]),
    write(1, "if (!enif_get_list_length(env, argv[~p], &~s_count_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_count_tmp == 0 || ~s_count_tmp > (unsigned int)INT_MAX) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),
    write(1, "ERL_NIF_TERM ~s_tail = argv[~p];\n", [VarName, Index]),
    write(1, "ERL_NIF_TERM ~s_head;\n", [VarName]).

write_multi_bind_object_value(VarName, Term, Destination, Suffixes) ->
    write(1, "    if (enif_compare(~s, enif_make_atom(env, \"none\")) == 0) {\n", [Term]),
    write(1, "        ~s = 0;\n", [Destination]),
    write(1, "    } else {\n", []),
    write(1, "        unsigned int ~s_object_tmp;\n", [VarName]),
    write(1, "        if (!enif_get_uint(env, ~s, &~s_object_tmp) || ~s_object_tmp == 0) {\n", [
        Term,
        VarName,
        VarName
    ]),
    write_multi_draw_badarg_free(VarName, Suffixes),
    write(1, "        }\n", []),
    write(1, "        ~s = (GLuint)~s_object_tmp;\n", [Destination, VarName]),
    write(1, "    }\n", []).

write_debug_log_free_lines(VarName) ->
    write(1, "    if (~s_sources) enif_free(~s_sources);\n", [VarName, VarName]),
    write(1, "    if (~s_types) enif_free(~s_types);\n", [VarName, VarName]),
    write(1, "    if (~s_ids) enif_free(~s_ids);\n", [VarName, VarName]),
    write(1, "    if (~s_severities) enif_free(~s_severities);\n", [VarName, VarName]),
    write(1, "    if (~s_lengths) enif_free(~s_lengths);\n", [VarName, VarName]),
    write(1, "    if (~s_message_log) enif_free(~s_message_log);\n", [VarName, VarName]).

write_debug_log_enum_term(RetVarName, ValueExpr, TransformMap) ->
    write(1, "    ERL_NIF_TERM ~s;\n", [RetVarName]),
    write(1, "    switch (~s) {\n", [ValueExpr]),
    lists:foreach(fun({Value, Atom}) ->
        write(1, "        case ~s: ~s = beam_atom_~s; break;\n", [Value, RetVarName, Atom])
    end, TransformMap),
    write(1, "        default: ~s = enif_make_atom(env, \"unknown\"); break;\n", [RetVarName]),
    write(1, "    }\n", []).

process_nif_param(boolean_to_glbool, Index) ->
    % XXX: There must be a more efficient way to handle this. Probably by
    %      creating the atoms ahead of time.

    % red = enif_compare(argv[0], enif_make_atom(env, "true")) == 0;

    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLboolean ~s;\n", [VarName]),
    write(1, "if (enif_compare(argv[~p], enif_make_atom(env, \"true\")) == 0) {\n", [Index]),
    write(1, "    ~s = GL_TRUE;\n", [VarName]),
    write(1, "} else if (enif_compare(argv[~p], enif_make_atom(env, \"false\")) == 0) {\n", [Index]),
    write(1, "    ~s = GL_FALSE;\n", [VarName]),
    write(1, "} else {\n"),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};

process_nif_param({gl_type, {GlType, NativeType, NifFunction, _}}, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "~s ~s;\n", [GlType, VarName]),
    write(1, "~s ~s_tmp;\n", [NativeType, VarName]),
    write(1, "if (!~s(env, argv[~p], &~s_tmp)) {\n", [NifFunction, Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    write(1, "~s = (~s)~s_tmp;\n", [VarName, GlType, VarName]),
    {VarName, [VarName], []};


process_nif_param({list_gl_type, {GlType, NativeType, NifFunction, _}}, Index) ->
    % // Get the list length
    % unsigned list_length;
    % if (!enif_get_list_length(env, argv[0], &list_length)) {
    %     return enif_make_badarg(env);
    % }

    % // Allocate temporary array for the floats
    % GLfloat* params = enif_alloc(sizeof(GLfloat) * list_length);
    % if (!params) {
    %     return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "out_of_memory"));
    % }

    % // Convert the list elements to floats
    % ERL_NIF_TERM head, tail = argv[0];
    % for (unsigned i = 0; i < list_length; i++) {
    %     double val;
    %     if (!enif_get_list_cell(env, tail, &head, &tail) ||
    %         !enif_get_double(env, head, &val)) {
    %         enif_free(params);
    %         return enif_make_badarg(env);
    %     }
    %     params[i] = (GLfloat)val;
    % }

    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "unsigned int ~s_length;\n", [VarName]),
    write(1, "if (!enif_get_list_length(env, argv[~p], &~s_length)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),

    write(1, "~s* ~s_array = enif_alloc(sizeof(~s) * ~s_length);\n", [GlType, VarName, GlType, VarName]),
    write(1, "if (!~s_array) {\n", [VarName]),
    write(1, "    return enif_make_tuple2(env, enif_make_atom(env, \"error\"), enif_make_atom(env, \"out_of_memory\"));\n"),
    write(1, "}\n", []),

    write(1, "ERL_NIF_TERM ~s_head, ~s_tail = argv[~p];\n", [VarName, VarName, Index]),
    write(1, "for (unsigned i = 0; i < ~s_length; i++) {\n", [VarName]),
    write(1, "    ~s ~s_val;\n", [NativeType, VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [VarName, VarName, VarName]),
    write(1, "        !~s(env, ~s_head, &~s_val)) {\n", [NifFunction, VarName, VarName]),
    write(1, "        enif_free(~s_array);\n", [VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n", []),
    write(1, "    ~s_array[i] = (~s)~s_val;\n", [VarName, GlType, VarName]),
    write(1, "}\n", []),

    PassArg = io_lib:format("~s_array", [VarName]),
    {VarName, [PassArg], [{free_array, PassArg}]};

process_nif_param(in_gl_binary_or_null, Index) ->
    % The parameter is either `undefined` atom (meaning NULL) or a binary.
    % XXX: more explanation
    %
    % ```
    % void glBufferData(
    %   GLenum target,
    %   GLsizeiptr size,
    %   const void * data,
    %   GLenum usage
    % );
    % ```
    %
    % Take the glBufferData() function as a reference.

    VarName = io_lib:format("arg_~p", [Index]),

% const void* var = NULL;
% ErlNifBinary var_bin;

% if (enif_is_identical(argv[42], enif_make_atom(env, "undefined"))) {
%     var = NULL;
% }
% else if (enif_inspect_binary(env, argv[42], &var_bin)) {
%     var = var_bin.data;
% }

    % const void* arg_2 = NULL;
    % ErlNifBinary arg_2_bin;
    % if (enif_is_identical(argv[2], enif_make_atom(env, "undefined"))) {
    %     arg_2 = NULL;
    % }
    % else if (enif_inspect_binary(env, argv[2], &arg_2_bin)) {
    %     if (arg_2_bin.size != arg_1) {
    %         return enif_make_tuple2(env,
    %             enif_make_atom(env, "error"),
    %             enif_make_atom(env, "size_mismatch")
    %         );
    %     }
    %     arg_2 = arg_2_bin.data;
    % }
    % else {
    %     return enif_make_badarg(env);
    % }



    write(1, "const void* ~s = NULL;\n", [VarName]),
    write(1, "ErlNifBinary ~s_bin;\n", [VarName]),
    write(1, "if (enif_is_identical(argv[~p], enif_make_atom(env, \"undefined\"))) {\n", [Index]),
    write(1, "    ~s = NULL;\n", [VarName]),
    write(1, "}\n", []),
    write(1, "else if (enif_inspect_binary(env, argv[~p], &~s_bin)) {\n", [Index, VarName]),
    write(1, "    ~s = ~s_bin.data;\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "else {\n", []),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    {VarName, [VarName], []};



process_nif_param(in_gl_offset, Index) ->
    % Public callers pass a byte offset into the currently bound array buffer.
    % OpenGL still receives that offset through the pointer-shaped raw slot.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifUInt64 ~s;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),
    write(1, "if (~s > UINTPTR_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    PassArg = io_lib:format("(GLvoid*)(uintptr_t)~s", [VarName]),
    {VarName, [PassArg], []};

process_nif_param(in_multi_draw_arrays, Index) ->
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_draw_list_header(VarName, Index),
    write(1, "GLint* ~s_first = enif_alloc(sizeof(GLint) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write(1, "GLsizei* ~s_count = enif_alloc(sizeof(GLsizei) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["first", "count"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_drawcount_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    const ERL_NIF_TERM* ~s_tuple;\n", [VarName]),
    write(1, "    int ~s_arity;\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [VarName, VarName, VarName]),
    write(1, "        !enif_get_tuple(env, ~s_head, &~s_arity, &~s_tuple) ||\n", [VarName, VarName, VarName]),
    write(1, "        ~s_arity != 2) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["first", "count"]),
    write(1, "    }\n", []),
    write(1, "    int ~s_first_tmp;\n", [VarName]),
    write(1, "    int ~s_count_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_int(env, ~s_tuple[0], &~s_first_tmp) ||\n", [VarName, VarName]),
    write(1, "        !enif_get_int(env, ~s_tuple[1], &~s_count_tmp)) {\n", [VarName, VarName]),
    write_multi_draw_badarg_free(VarName, ["first", "count"]),
    write(1, "    }\n", []),
    write(1, "    ~s_first[~s_i] = (GLint)~s_first_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_count[~s_i] = (GLsizei)~s_count_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_first", [VarName]),
    PassArg2 = io_lib:format("~s_count", [VarName]),
    PassArg3 = io_lib:format("~s_drawcount", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3], [{in_multi_draw_arrays, VarName}]};

process_nif_param(in_multi_draw_elements, Index) ->
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_draw_list_header(VarName, Index),
    write(1, "GLsizei* ~s_count = enif_alloc(sizeof(GLsizei) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write(1, "const GLvoid** ~s_indices = enif_alloc(sizeof(GLvoid*) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["count", "indices"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_drawcount_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    const ERL_NIF_TERM* ~s_tuple;\n", [VarName]),
    write(1, "    int ~s_arity;\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [VarName, VarName, VarName]),
    write(1, "        !enif_get_tuple(env, ~s_head, &~s_arity, &~s_tuple) ||\n", [VarName, VarName, VarName]),
    write(1, "        ~s_arity != 2) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["count", "indices"]),
    write(1, "    }\n", []),
    write(1, "    int ~s_count_tmp;\n", [VarName]),
    write(1, "    ErlNifUInt64 ~s_offset_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_int(env, ~s_tuple[0], &~s_count_tmp) ||\n", [VarName, VarName]),
    write(1, "        !enif_get_uint64(env, ~s_tuple[1], &~s_offset_tmp) ||\n", [VarName, VarName]),
    write(1, "        ~s_offset_tmp > UINTPTR_MAX) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["count", "indices"]),
    write(1, "    }\n", []),
    write(1, "    ~s_count[~s_i] = (GLsizei)~s_count_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_indices[~s_i] = (const GLvoid*)(uintptr_t)~s_offset_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLvoid* const*)~s_indices", [VarName]),
    PassArg3 = io_lib:format("~s_drawcount", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3], [{in_multi_draw_elements, VarName}]};

process_nif_param(in_multi_draw_elements_base_vertex, Index) ->
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_draw_list_header(VarName, Index),
    write(1, "GLsizei* ~s_count = enif_alloc(sizeof(GLsizei) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write(1, "const GLvoid** ~s_indices = enif_alloc(sizeof(GLvoid*) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write(1, "GLint* ~s_basevertex = enif_alloc(sizeof(GLint) * ~s_drawcount_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["count", "indices", "basevertex"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_drawcount_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    const ERL_NIF_TERM* ~s_tuple;\n", [VarName]),
    write(1, "    int ~s_arity;\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [VarName, VarName, VarName]),
    write(1, "        !enif_get_tuple(env, ~s_head, &~s_arity, &~s_tuple) ||\n", [VarName, VarName, VarName]),
    write(1, "        ~s_arity != 3) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["count", "indices", "basevertex"]),
    write(1, "    }\n", []),
    write(1, "    int ~s_count_tmp;\n", [VarName]),
    write(1, "    ErlNifUInt64 ~s_offset_tmp;\n", [VarName]),
    write(1, "    int ~s_basevertex_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_int(env, ~s_tuple[0], &~s_count_tmp) ||\n", [VarName, VarName]),
    write(1, "        !enif_get_uint64(env, ~s_tuple[1], &~s_offset_tmp) ||\n", [VarName, VarName]),
    write(1, "        !enif_get_int(env, ~s_tuple[2], &~s_basevertex_tmp) ||\n", [VarName, VarName]),
    write(1, "        ~s_offset_tmp > UINTPTR_MAX) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["count", "indices", "basevertex"]),
    write(1, "    }\n", []),
    write(1, "    ~s_count[~s_i] = (GLsizei)~s_count_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_indices[~s_i] = (const GLvoid*)(uintptr_t)~s_offset_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_basevertex[~s_i] = (GLint)~s_basevertex_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLvoid* const*)~s_indices", [VarName]),
    PassArg3 = io_lib:format("~s_drawcount", [VarName]),
    PassArg4 = io_lib:format("~s_basevertex", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3, PassArg4], [{in_multi_draw_elements_base_vertex, VarName}]};

process_nif_param(in_multi_bind_object_list, Index) ->
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_bind_list_header(VarName, Index),
    write(1, "GLuint* ~s_objects = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["objects"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_count_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail)) {\n", [
        VarName,
        VarName,
        VarName
    ]),
    write_multi_draw_badarg_free(VarName, ["objects"]),
    write(1, "    }\n", []),
    write_multi_bind_object_value(
        VarName,
        io_lib:format("~s_head", [VarName]),
        io_lib:format("~s_objects[~s_i]", [VarName, VarName]),
        ["objects"]
    ),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLuint*)~s_objects", [VarName]),
    {VarName, [PassArg1, PassArg2], [{in_multi_bind_object_list, VarName}]};

process_nif_param(in_multi_bind_buffer_ranges, Index) ->
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_bind_list_header(VarName, Index),
    write(1, "GLuint* ~s_buffers = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLintptr* ~s_offsets = enif_alloc(sizeof(GLintptr) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLsizeiptr* ~s_sizes = enif_alloc(sizeof(GLsizeiptr) * ~s_count_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["buffers", "offsets", "sizes"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_count_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    const ERL_NIF_TERM* ~s_tuple;\n", [VarName]),
    write(1, "    int ~s_arity;\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [VarName, VarName, VarName]),
    write(1, "        !enif_get_tuple(env, ~s_head, &~s_arity, &~s_tuple) ||\n", [VarName, VarName, VarName]),
    write(1, "        ~s_arity != 3) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["buffers", "offsets", "sizes"]),
    write(1, "    }\n", []),
    write_multi_bind_object_value(
        VarName,
        io_lib:format("~s_tuple[0]", [VarName]),
        io_lib:format("~s_buffers[~s_i]", [VarName, VarName]),
        ["buffers", "offsets", "sizes"]
    ),
    write(1, "    int ~s_offset_tmp;\n", [VarName]),
    write(1, "    int ~s_size_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_int(env, ~s_tuple[1], &~s_offset_tmp) ||\n", [VarName, VarName]),
    write(1, "        !enif_get_int(env, ~s_tuple[2], &~s_size_tmp)) {\n", [VarName, VarName]),
    write_multi_draw_badarg_free(VarName, ["buffers", "offsets", "sizes"]),
    write(1, "    }\n", []),
    write(1, "    ~s_offsets[~s_i] = (GLintptr)~s_offset_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_sizes[~s_i] = (GLsizeiptr)~s_size_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLuint*)~s_buffers", [VarName]),
    PassArg3 = io_lib:format("(const GLintptr*)~s_offsets", [VarName]),
    PassArg4 = io_lib:format("(const GLsizeiptr*)~s_sizes", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3, PassArg4], [{in_multi_bind_buffer_ranges, VarName}]};

process_nif_param(in_multi_bind_vertex_buffers, Index) ->
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_bind_list_header(VarName, Index),
    write(1, "GLuint* ~s_buffers = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLintptr* ~s_offsets = enif_alloc(sizeof(GLintptr) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLsizei* ~s_strides = enif_alloc(sizeof(GLsizei) * ~s_count_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["buffers", "offsets", "strides"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_count_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    const ERL_NIF_TERM* ~s_tuple;\n", [VarName]),
    write(1, "    int ~s_arity;\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [VarName, VarName, VarName]),
    write(1, "        !enif_get_tuple(env, ~s_head, &~s_arity, &~s_tuple) ||\n", [VarName, VarName, VarName]),
    write(1, "        ~s_arity != 3) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["buffers", "offsets", "strides"]),
    write(1, "    }\n", []),
    write_multi_bind_object_value(
        VarName,
        io_lib:format("~s_tuple[0]", [VarName]),
        io_lib:format("~s_buffers[~s_i]", [VarName, VarName]),
        ["buffers", "offsets", "strides"]
    ),
    write(1, "    int ~s_offset_tmp;\n", [VarName]),
    write(1, "    int ~s_stride_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_int(env, ~s_tuple[1], &~s_offset_tmp) ||\n", [VarName, VarName]),
    write(1, "        !enif_get_int(env, ~s_tuple[2], &~s_stride_tmp)) {\n", [VarName, VarName]),
    write_multi_draw_badarg_free(VarName, ["buffers", "offsets", "strides"]),
    write(1, "    }\n", []),
    write(1, "    ~s_offsets[~s_i] = (GLintptr)~s_offset_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_strides[~s_i] = (GLsizei)~s_stride_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLuint*)~s_buffers", [VarName]),
    PassArg3 = io_lib:format("(const GLintptr*)~s_offsets", [VarName]),
    PassArg4 = io_lib:format("(const GLsizei*)~s_strides", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3, PassArg4], [{in_multi_bind_vertex_buffers, VarName}]};
process_nif_param(binary_to_glbinary, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "ErlNifBinary ~s;\n", [VarName]),
    write(1, "if (!enif_inspect_binary(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    PassArg = io_lib:format("(void*)~s.data", [VarName]),
    {VarName, [PassArg], []};

process_nif_param(in_gl_uint_list_with_count, Index) ->
    % Public [gl:uint()] is packed by the wrapper into a binary. The raw
    % OpenGL command receives the derived GLsizei count plus the GLuint* data.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifBinary ~s;\n", [VarName]),
    write(1, "if (!enif_inspect_binary(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s.size == 0 || ~s.size % sizeof(GLuint) != 0) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "size_t ~s_count_tmp = ~s.size / sizeof(GLuint);\n", [VarName, VarName]),
    write(1, "if (~s_count_tmp > (size_t)INT_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLuint*)~s.data", [VarName]),
    {VarName, [PassArg1, PassArg2], []};

process_nif_param(in_gl_enum_list_with_count, Index) ->
    % Public enum atoms are packed by the wrapper into a binary. The raw
    % OpenGL command receives the derived GLsizei count plus the GLenum* data.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifBinary ~s;\n", [VarName]),
    write(1, "if (!enif_inspect_binary(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s.size == 0 || ~s.size % sizeof(GLenum) != 0) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "size_t ~s_count_tmp = ~s.size / sizeof(GLenum);\n", [VarName, VarName]),
    write(1, "if (~s_count_tmp > (size_t)INT_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLenum*)~s.data", [VarName]),
    {VarName, [PassArg1, PassArg2], []};

process_nif_param(in_gl_object_list_with_count, Index) ->
    % Public [shader()] is parsed into count plus GLuint* for glShaderBinary().
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write_multi_bind_list_header(VarName, Index),
    write(1, "GLuint* ~s_objects = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write_multi_draw_alloc_check(VarName, ["objects"]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_count_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail)) {\n", [
        VarName,
        VarName,
        VarName
    ]),
    write_multi_draw_badarg_free(VarName, ["objects"]),
    write(1, "    }\n", []),
    write(1, "    unsigned int ~s_object_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_uint(env, ~s_head, &~s_object_tmp) || ~s_object_tmp == 0) {\n", [
        VarName,
        VarName,
        VarName
    ]),
    write_multi_draw_badarg_free(VarName, ["objects"]),
    write(1, "    }\n", []),
    write(1, "    ~s_objects[~s_i] = (GLuint)~s_object_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLuint*)~s_objects", [VarName]),
    {VarName, [PassArg1, PassArg2], [{in_gl_object_list_with_count, VarName}]};

process_nif_param(in_specialization_constant_list, Index) ->
    % Public [{Index, Value}] is parsed into parallel GLuint arrays for
    % glSpecializeShader(). An empty list is valid and maps to count=0, NULL.
    VarName = lists:flatten(io_lib:format("arg_~p", [Index])),

    write(1, "unsigned int ~s_count_tmp;\n", [VarName]),
    write(1, "if (!enif_get_list_length(env, argv[~p], &~s_count_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLuint ~s_count = (GLuint)~s_count_tmp;\n", [VarName, VarName]),
    write(1, "GLuint* ~s_indices = NULL;\n", [VarName]),
    write(1, "GLuint* ~s_values = NULL;\n", [VarName]),
    write(1, "if (~s_count_tmp > 0) {\n", [VarName]),
    write(1, "    ~s_indices = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "    ~s_values = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "    if (!~s_indices || !~s_values) {\n", [VarName, VarName]),
    write(1, "        if (~s_indices) enif_free(~s_indices);\n", [VarName, VarName]),
    write(1, "        if (~s_values) enif_free(~s_values);\n", [VarName, VarName]),
    write(1, "        return enif_make_tuple2(env,\n", []),
    write(1, "            enif_make_atom(env, \"error\"),\n", []),
    write(1, "            enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "        );\n", []),
    write(1, "    }\n", []),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s_tail = argv[~p];\n", [VarName, Index]),
    write(1, "ERL_NIF_TERM ~s_head;\n", [VarName]),
    write(1, "for (unsigned int ~s_i = 0; ~s_i < ~s_count_tmp; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    const ERL_NIF_TERM* ~s_tuple;\n", [VarName]),
    write(1, "    int ~s_arity;\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tail, &~s_head, &~s_tail) ||\n", [
        VarName,
        VarName,
        VarName
    ]),
    write(1, "        !enif_get_tuple(env, ~s_head, &~s_arity, &~s_tuple) ||\n", [
        VarName,
        VarName,
        VarName
    ]),
    write(1, "        ~s_arity != 2) {\n", [VarName]),
    write_multi_draw_badarg_free(VarName, ["indices", "values"]),
    write(1, "    }\n", []),
    write(1, "    unsigned int ~s_index_tmp;\n", [VarName]),
    write(1, "    unsigned int ~s_value_tmp;\n", [VarName]),
    write(1, "    if (!enif_get_uint(env, ~s_tuple[0], &~s_index_tmp) ||\n", [
        VarName,
        VarName
    ]),
    write(1, "        !enif_get_uint(env, ~s_tuple[1], &~s_value_tmp)) {\n", [
        VarName,
        VarName
    ]),
    write_multi_draw_badarg_free(VarName, ["indices", "values"]),
    write(1, "    }\n", []),
    write(1, "    ~s_indices[~s_i] = (GLuint)~s_index_tmp;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_values[~s_i] = (GLuint)~s_value_tmp;\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLuint*)~s_indices", [VarName]),
    PassArg3 = io_lib:format("(const GLuint*)~s_values", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3], [{in_specialization_constant_list, VarName}]};

process_nif_param(in_gl_string, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    StringVarName = io_lib:format("arg_~p_string", [Index]),
    write(1, "ErlNifBinary ~s;\n", [VarName]),
    write(1, "if (!enif_inspect_binary(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLchar* ~s = (GLchar*)enif_alloc(~s.size + 1);\n", [StringVarName, VarName]),
    write(1, "if (!~s) {\n", [StringVarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s.size > 0) {\n", [VarName]),
    write(1, "    memcpy(~s, ~s.data, ~s.size);\n", [StringVarName, VarName, VarName]),
    write(1, "}\n", []),
    write(1, "~s[~s.size] = '\\0';\n", [StringVarName, VarName]),
    PassArg = io_lib:format("(const GLchar*)~s", [StringVarName]),
    {VarName, [PassArg], [{in_gl_string, StringVarName}]};




process_nif_param(in_list_gl_strings, Index) ->
    % The parameter is a list of binaries. We must allocate an array of
    % GLchar* and make each item point to the data of the binaries. We must
    % also allocate an array of GLint to store the length of each binary. The
    % OpenGL function is then called with GLsizei to indicate the number of
    % binaries, GLchar** for the array of strings, and GLint* for the lengths.
    %
    % ```
    % void glShaderSource(
    %       GLuint shader,
    %   	GLsizei count,
    %   	const GLchar **string,
    %   	const GLint *length
    % );
    % ```
    %
    % Take the glShaderSource() function as a reference.

    % Compute the variable name which will also be used as a prefix for the
    % temporary variables.
    VarName = io_lib:format("arg_~p", [Index]),

    % Verify that the argument is a list.
    write(1, "if (!enif_is_list(env, argv[~p])) {\n", [Index]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    write(1, "ERL_NIF_TERM ~s_list = argv[~p];\n", [VarName, Index]),
    write(1, "ERL_NIF_TERM ~s_head;\n\n", [VarName]),

    % Count the number of binaries in the list, and verify that each item
    % is a binary.
    write(1, "unsigned ~s_count = 0;\n", [VarName]),
    write(1, "ERL_NIF_TERM ~s_tmp_list = ~s_list;\n", [VarName, VarName]),
    write(1, "while (!enif_is_empty_list(env, ~s_tmp_list)) {\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tmp_list, &~s_head, &~s_tmp_list)) {\n", [VarName, VarName, VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n", []),
    write(1, "    if (!enif_is_binary(env, ~s_head)) {\n", [VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n", []),
    write(1, "    ~s_count++;\n", [VarName]),
    write(1, "}\n\n", []),

    write(1, "if (~s_count == 0) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    % Allocate the arrays for the strings, lengths, and binaries.
    write(1, "const GLchar** ~s_strings = enif_alloc(sizeof(GLchar*) * ~s_count);\n", [VarName, VarName]),
    write(1, "GLint* ~s_lengths = enif_alloc(sizeof(GLint) * ~s_count);\n", [VarName, VarName]),
    write(1, "ErlNifBinary* ~s_binaries = enif_alloc(sizeof(ErlNifBinary) * ~s_count);\n", [VarName, VarName]),
    write(1, "if (!~s_strings || !~s_lengths || !~s_binaries) {\n", [VarName, VarName, VarName]),
    write(1, "    if (~s_strings) enif_free(~s_strings);\n", [VarName, VarName]),
    write(1, "    if (~s_lengths) enif_free(~s_lengths);\n", [VarName, VarName]),
    write(1, "    if (~s_binaries) enif_free(~s_binaries);\n", [VarName, VarName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n\n", []),

    % Iterate over the list again, this time to fill the arrays with the
    % data from the binaries.
    write(1, "~s_tmp_list = ~s_list;\n", [VarName, VarName]),
    write(1, "unsigned ~s_i = 0;\n", [VarName]),
    write(1, "while (!enif_is_empty_list(env, ~s_tmp_list)) {\n", [VarName]),
    write(1, "    enif_get_list_cell(env, ~s_tmp_list, &~s_head, &~s_tmp_list);\n", [VarName, VarName, VarName]),
    write(1, "    if (!enif_inspect_binary(env, ~s_head, &~s_binaries[~s_i])) {\n", [VarName, VarName, VarName]),
    write(1, "        for (unsigned ~s_j = 0; ~s_j < ~s_i; ~s_j++) {\n", [VarName, VarName, VarName, VarName]),
    write(1, "            enif_release_binary(&~s_binaries[~s_j]);\n", [VarName, VarName]),
    write(1, "        }\n", []),
    write(1, "        enif_free(~s_strings);\n", [VarName]),
    write(1, "        enif_free(~s_lengths);\n", [VarName]),
    write(1, "        enif_free(~s_binaries);\n", [VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n\n", []),
    write(1, "    ~s_strings[~s_i] = (const GLchar*)~s_binaries[~s_i].data;\n", [VarName, VarName, VarName, VarName]),
    write(1, "    ~s_lengths[~s_i] = (GLint)~s_binaries[~s_i].size;\n", [VarName, VarName, VarName, VarName]),
    write(1, "    ~s_i++;\n", [VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("(GLsizei)~s_count", [VarName]),
    PassArg2 = io_lib:format("~s_strings", [VarName]),
    PassArg3 = io_lib:format("~s_lengths", [VarName]),

    {VarName, [PassArg1, PassArg2, PassArg3], [{in_list_gl_strings, VarName}]};

process_nif_param(in_list_gl_strings_null_terminated, Index) ->
    % The parameter is a list of binaries. Unlike glShaderSource(), these
    % OpenGL APIs do not accept a parallel lengths array, so each binary is
    % copied into a temporary NUL-terminated GLchar* string.
    %
    % ```
    % void glTransformFeedbackVaryings(
    %     GLuint program,
    %     GLsizei count,
    %     const GLchar *const* varyings,
    %     GLenum bufferMode
    % );
    % ```
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "if (!enif_is_list(env, argv[~p])) {\n", [Index]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    write(1, "ERL_NIF_TERM ~s_list = argv[~p];\n", [VarName, Index]),
    write(1, "ERL_NIF_TERM ~s_head;\n\n", [VarName]),

    write(1, "unsigned ~s_count = 0;\n", [VarName]),
    write(1, "ERL_NIF_TERM ~s_tmp_list = ~s_list;\n", [VarName, VarName]),
    write(1, "while (!enif_is_empty_list(env, ~s_tmp_list)) {\n", [VarName]),
    write(1, "    if (!enif_get_list_cell(env, ~s_tmp_list, &~s_head, &~s_tmp_list)) {\n", [VarName, VarName, VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n", []),
    write(1, "    if (!enif_is_binary(env, ~s_head)) {\n", [VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n", []),
    write(1, "    ~s_count++;\n", [VarName]),
    write(1, "}\n\n", []),

    write(1, "if (~s_count == 0) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    write(1, "GLchar** ~s_strings = enif_alloc(sizeof(GLchar*) * ~s_count);\n", [VarName, VarName]),
    write(1, "if (!~s_strings) {\n", [VarName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),
    write(1, "for (unsigned ~s_j = 0; ~s_j < ~s_count; ~s_j++) {\n", [VarName, VarName, VarName, VarName]),
    write(1, "    ~s_strings[~s_j] = NULL;\n", [VarName, VarName]),
    write(1, "}\n\n", []),

    write(1, "~s_tmp_list = ~s_list;\n", [VarName, VarName]),
    write(1, "unsigned ~s_i = 0;\n", [VarName]),
    write(1, "while (!enif_is_empty_list(env, ~s_tmp_list)) {\n", [VarName]),
    write(1, "    enif_get_list_cell(env, ~s_tmp_list, &~s_head, &~s_tmp_list);\n", [VarName, VarName, VarName]),
    write(1, "    ErlNifBinary ~s_binary;\n", [VarName]),
    write(1, "    if (!enif_inspect_binary(env, ~s_head, &~s_binary)) {\n", [VarName, VarName]),
    write(1, "        for (unsigned ~s_j = 0; ~s_j < ~s_i; ~s_j++) {\n", [VarName, VarName, VarName, VarName]),
    write(1, "            enif_free(~s_strings[~s_j]);\n", [VarName, VarName]),
    write(1, "        }\n", []),
    write(1, "        enif_free(~s_strings);\n", [VarName]),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n\n", []),
    write(1, "    GLchar* ~s_string = (GLchar*)enif_alloc(~s_binary.size + 1);\n", [VarName, VarName]),
    write(1, "    if (!~s_string) {\n", [VarName]),
    write(1, "        for (unsigned ~s_j = 0; ~s_j < ~s_i; ~s_j++) {\n", [VarName, VarName, VarName, VarName]),
    write(1, "            enif_free(~s_strings[~s_j]);\n", [VarName, VarName]),
    write(1, "        }\n", []),
    write(1, "        enif_free(~s_strings);\n", [VarName]),
    write(1, "        return enif_make_tuple2(env,\n", []),
    write(1, "            enif_make_atom(env, \"error\"),\n", []),
    write(1, "            enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "        );\n", []),
    write(1, "    }\n", []),
    write(1, "    if (~s_binary.size > 0) {\n", [VarName]),
    write(1, "        memcpy(~s_string, ~s_binary.data, ~s_binary.size);\n", [VarName, VarName, VarName]),
    write(1, "    }\n", []),
    write(1, "    ~s_string[~s_binary.size] = '\\0';\n", [VarName, VarName]),
    write(1, "    ~s_strings[~s_i] = ~s_string;\n", [VarName, VarName, VarName]),
    write(1, "    ~s_i++;\n", [VarName]),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("(GLsizei)~s_count", [VarName]),
    PassArg2 = io_lib:format("(const GLchar* const*)~s_strings", [VarName]),

    {VarName, [PassArg1, PassArg2], [{in_list_gl_strings_null_terminated, VarName}]};

process_nif_param(out_gl_string, Index) ->
    % The parameter is blabla.
    %
    % ```
    % void glGetShaderSource(
    %       GLuint shader,
    %   	GLsizei bufSize,
    %   	GLsizei *length,
    %   	GLchar *source);
    % ```
    %
    % Take the glGetShaderSource() function as a reference.

    % Compute the variable name which will also be used as a prefix for the
    % temporary variables.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "unsigned int ~s_max_length_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint(env, argv[~p], &~s_max_length_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),
    write(1, "if (~s_max_length_tmp == 0) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),
    write(1, "GLsizei ~s_max_length = (GLsizei)~s_max_length_tmp;\n", [VarName, VarName]),

    write(1, "GLchar* ~s_info_log = (GLchar*)enif_alloc(~s_max_length_tmp);\n", [VarName, VarName]),
    write(1, "if (!~s_info_log) {\n", [VarName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_length = 0;\n", [VarName]),

    PassArg1 = io_lib:format("~s_max_length", [VarName]),
    PassArg2 = io_lib:format("&~s_length", [VarName]),
    PassArg3 = io_lib:format("~s_info_log", [VarName]),

    {VarName, [PassArg1, PassArg2, PassArg3], [{out_gl_string, VarName}]};

process_nif_param({out_active_reflection_info, SizeVarType, TransformMap}, Index) ->
    % Active attribute/uniform reflection uses:
    %   bufSize, length*, size*, type*, name*
    % in the raw OpenGL call. Public callers provide only bufSize.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "unsigned int ~s_max_length_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint(env, argv[~p], &~s_max_length_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),
    write(1, "if (~s_max_length_tmp == 0) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),
    write(1, "GLsizei ~s_max_length = (GLsizei)~s_max_length_tmp;\n", [VarName, VarName]),

    write(1, "GLchar* ~s_name = (GLchar*)enif_alloc(~s_max_length_tmp);\n", [VarName, VarName]),
    write(1, "if (!~s_name) {\n", [VarName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_length = 0;\n", [VarName]),
    write(1, "~s ~s_size;\n", [SizeVarType, VarName]),
    write(1, "GLenum ~s_type;\n", [VarName]),

    PassArg1 = io_lib:format("~s_max_length", [VarName]),
    PassArg2 = io_lib:format("&~s_length", [VarName]),
    PassArg3 = io_lib:format("&~s_size", [VarName]),
    PassArg4 = io_lib:format("&~s_type", [VarName]),
    PassArg5 = io_lib:format("~s_name", [VarName]),

    {
        VarName,
        [PassArg1, PassArg2, PassArg3, PassArg4, PassArg5],
        [{out_active_reflection_info, VarName, TransformMap}]
    };

process_nif_param(binary_to_gl_string_char, Index) ->
    %% ```
    %% [...]
    %% ErlNifBinary message_bin;
    %% enif_inspect_binary(env, argv[2], &message_bin);
    %%
    %% // 2. Ensure null-terminated string
    %% char* message = enif_alloc(message_bin.size + 1);
    %% if (!message) {
    %%     return enif_make_tuple2(env,
    %%                             enif_make_atom(env, "error"),
    %%                             enif_make_atom(env, "alloc_failed"));
    %% }
    %% memcpy(message, message_bin.data, message_bin.size);
    %% message[message_bin.size] = '\0';
    %%
    % // 3. Call OpenGL function
    % glPushDebugGroup(source, id, message_bin.size, message);
    %%
    %% // 4. Clean up
    %% enif_free(message);
    %% [...]
    %% ```
    VarName = io_lib:format("arg_~p", [Index]),
    MsgVarName = io_lib:format("arg_~p_msg", [Index]),
    write(1, "ErlNifBinary ~s;\n", [VarName]),
    write(1, "if (!enif_inspect_binary(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),

    write(1, "char* ~s = enif_alloc(~s.size + 1);\n", [MsgVarName, VarName]),
    write(1, "if (!~s) {\n", [MsgVarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n"),
    write(1, "if (~s.size > 0) {\n", [VarName]),
    write(1, "    memcpy(~s, ~s.data, ~s.size);\n", [MsgVarName, VarName, VarName]),
    write(1, "}\n", []),
    write(1, "~s[~s.size] = '\\0';\n", [MsgVarName, VarName]),

    PassArg1 = io_lib:format("(GLsizei)~s.size", [VarName]),
    PassArg2 = MsgVarName,
    {VarName, [PassArg1, PassArg2], [{binary_to_gl_string_char, MsgVarName}]};

process_nif_param(out_binary_implicit, Index) ->
    % must return an instruction so an additional return value is produced
    % must produce only one "pass args"

    % unsigned int arg_4_size;
    % enif_get_uint(env, argv[4], &arg_4_size);

    % ERL_NIF_TERM arg_4;
    % unsigned char* arg_4_data = enif_make_new_binary(env, arg_4_size, &arg_4);

    % glGetTexImage(arg_0, arg_1, arg_2, arg_3, arg_4_data);


    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "ErlNifUInt64 ~s_size;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_size)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_size > (ErlNifUInt64)PTRDIFF_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s;\n", [VarName]),
    write(1, "unsigned char* ~s_data = enif_make_new_binary(env, ~s_size, &~s);\n", [VarName, VarName, VarName]),
    write(1, "if (~s_data == NULL && ~s_size > 0) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),

    PassArg = io_lib:format("~s_data", [VarName]),
    {"arg_x", [PassArg], [{foo, VarName}]};

process_nif_param({out_binary_explicit, gl_sizei}, Index) ->
    % must return an instruction so an additional return value is produced
    % must produce two "pass args"
    % Example: glGetTextureImage()

    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifUInt64 ~s_size;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_size)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_size > (ErlNifUInt64)INT_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s_term;\n", [VarName]),
    write(1, "unsigned char* ~s_bin = enif_make_new_binary(env, ~s_size, &~s_term);\n", [VarName, VarName, VarName]),
    write(1, "if (~s_bin == NULL && ~s_size > 0) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("(GLsizei)~s_size", [VarName]),
    PassArg2 = io_lib:format("~s_bin", [VarName]),
    RetArg = io_lib:format("~s_term", [VarName]),
    {"arg_x", [PassArg1, PassArg2], [{foo, RetArg}]};

process_nif_param(out_program_binary, Index) ->
    % glGetProgramBinary uses caller capacity plus Length*, BinaryFormat*, and
    % a byte buffer. The returned binary is trimmed to the actual Length.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifUInt64 ~s_size;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_size)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_size > (ErlNifUInt64)INT_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "unsigned char* ~s_data = NULL;\n", [VarName]),
    write(1, "if (~s_size > 0) {\n", [VarName]),
    write(1, "    ~s_data = enif_alloc((size_t)~s_size);\n", [VarName, VarName]),
    write(1, "    if (!~s_data) {\n", [VarName]),
    write(1, "        return enif_make_tuple2(env,\n", []),
    write(1, "            enif_make_atom(env, \"error\"),\n", []),
    write(1, "            enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "        );\n", []),
    write(1, "    }\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_length = 0;\n", [VarName]),
    write(1, "GLenum ~s_format = 0;\n", [VarName]),

    PassArg1 = io_lib:format("(GLsizei)~s_size", [VarName]),
    PassArg2 = io_lib:format("&~s_length", [VarName]),
    PassArg3 = io_lib:format("&~s_format", [VarName]),
    PassArg4 = io_lib:format("~s_data", [VarName]),
    {"arg_x", [PassArg1, PassArg2, PassArg3, PassArg4], [{out_program_binary, VarName}]};

process_nif_param(out_binary_explicit, Index) ->
    % must return an instruction so an additional return value is produced
    % must produce two "pass args"
    % Example: glGetBufferSubData()

    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifUInt64 ~s_size;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_size)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_size > (ErlNifUInt64)PTRDIFF_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s_term;\n", [VarName]),
    write(1, "unsigned char* ~s_bin = enif_make_new_binary(env, ~s_size, &~s_term);\n", [VarName, VarName, VarName]),
    write(1, "if (~s_bin == NULL && ~s_size > 0) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("(GLsizeiptr)~s_size", [VarName]),
    PassArg2 = io_lib:format("~s_bin", [VarName]),
    RetArg = io_lib:format("~s_term", [VarName]),
    {"arg_x", [PassArg1, PassArg2], [{foo, RetArg}]};



process_nif_param({out_list_alloc_1, VarType, TermFunction}, Index) ->
    % Example: glGetBooleanv().
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "unsigned int ~s_n_tmp;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_n_tmp);\n", [Index, VarName]),
    write(1, "GLsizei ~s_n = (GLsizei)~s_n_tmp;\n", [VarName, VarName]),

    write(1, "~s* ~s = enif_alloc(sizeof(~s) * ~s_n);\n", [VarType, VarName, VarType, VarName]),
    PassArg = io_lib:format("~s", [VarName]),
    {VarName, [PassArg], [{out_list_alloc_1, VarName, TermFunction}]};

process_nif_param({out_typed_value_list, VarType, TermFunction}, Index) ->
    % Caller-sized scalar value readback. Example: glGetUniformfv().
    VarName = io_lib:format("arg_~p", [Index]),
    ValuesName = io_lib:format("~s_values", [VarName]),

    write(1, "ErlNifUInt64 ~s_count_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_count_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_count_tmp == 0 || ~s_count_tmp > (ErlNifUInt64)INT_MAX) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),
    write(1, "~s* ~s = enif_alloc(sizeof(~s) * (size_t)~s_count);\n", [
        VarType,
        ValuesName,
        VarType,
        VarName
    ]),
    write(1, "if (!~s) {\n", [ValuesName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    {VarName, [ValuesName], [{out_typed_value_list, VarName, ValuesName, TermFunction}]};

process_nif_param({out_typed_value_list_with_size, VarType, TermFunction}, Index) ->
    % Caller-sized scalar value readback where OpenGL receives the capacity.
    % Example: glGetInternalformativ().
    VarName = io_lib:format("arg_~p", [Index]),
    ValuesName = io_lib:format("~s_values", [VarName]),

    write(1, "ErlNifUInt64 ~s_count_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_count_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_count_tmp == 0 || ~s_count_tmp > (ErlNifUInt64)INT_MAX) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),
    write(1, "~s* ~s = enif_alloc(sizeof(~s) * (size_t)~s_count);\n", [
        VarType,
        ValuesName,
        VarType,
        VarName
    ]),
    write(1, "if (!~s) {\n", [ValuesName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("~s", [ValuesName]),
    {VarName, [PassArg1, PassArg2], [{out_typed_value_list, VarName, ValuesName, TermFunction}]};

process_nif_param({out_typed_value_list_with_byte_size, VarType, TermFunction}, Index) ->
    % Caller-sized scalar value readback where OpenGL receives byte capacity.
    % Example: glGetnUniformfv().
    VarName = io_lib:format("arg_~p", [Index]),
    ValuesName = io_lib:format("~s_values", [VarName]),

    write(1, "ErlNifUInt64 ~s_count_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_count_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_count_tmp == 0 || ~s_count_tmp > (ErlNifUInt64)(INT_MAX / sizeof(~s))) {\n", [VarName, VarName, VarType]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),
    write(1, "GLsizei ~s_size = (GLsizei)(~s_count_tmp * (ErlNifUInt64)sizeof(~s));\n", [
        VarName,
        VarName,
        VarType
    ]),
    write(1, "~s* ~s = enif_alloc(sizeof(~s) * (size_t)~s_count);\n", [
        VarType,
        ValuesName,
        VarType,
        VarName
    ]),
    write(1, "if (!~s) {\n", [ValuesName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_size", [VarName]),
    PassArg2 = io_lib:format("~s", [ValuesName]),
    {VarName, [PassArg1, PassArg2], [{out_typed_value_list, VarName, ValuesName, TermFunction}]};

process_nif_param({out_typed_value_list_from_counted_input, SourceVarName, VarType, TermFunction}, Index) ->
    % Count-derived scalar value readback. Example: glGetActiveUniformsiv().
    VarName = io_lib:format("out_~p", [Index]),
    ValuesName = io_lib:format("~s_values", [VarName]),

    write(1, "if ((size_t)~s_count == 0 || (size_t)~s_count > (size_t)INT_MAX) {\n", [SourceVarName, SourceVarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count;\n", [VarName, SourceVarName]),
    write(1, "~s* ~s = enif_alloc(sizeof(~s) * (size_t)~s_count);\n", [
        VarType,
        ValuesName,
        VarType,
        VarName
    ]),
    write(1, "if (!~s) {\n", [ValuesName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    {VarName, [ValuesName], [{out_typed_value_list, VarName, ValuesName, TermFunction}]};

process_nif_param({caller_sized_typed_value_list, _CountName, _LengthName, VarType, TermFunction}, Index) ->
    % Caller-sized property-array readback with actual Length out parameter.
    % Example: glGetProgramResourceiv().
    VarName = io_lib:format("arg_~p", [Index]),
    ValuesName = io_lib:format("~s_values", [VarName]),

    write(1, "ErlNifUInt64 ~s_count_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_count_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_count_tmp == 0 || ~s_count_tmp > (ErlNifUInt64)INT_MAX) {\n", [VarName, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_count = (GLsizei)~s_count_tmp;\n", [VarName, VarName]),
    write(1, "GLsizei ~s_length = 0;\n", [VarName]),
    write(1, "~s* ~s = enif_alloc(sizeof(~s) * (size_t)~s_count);\n", [
        VarType,
        ValuesName,
        VarType,
        VarName
    ]),
    write(1, "if (!~s) {\n", [ValuesName]),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_count", [VarName]),
    PassArg2 = io_lib:format("&~s_length", [VarName]),
    PassArg3 = io_lib:format("~s", [ValuesName]),
    {VarName, [PassArg1, PassArg2, PassArg3], [
        {caller_sized_typed_value_list, VarName, ValuesName, TermFunction}
    ]};

process_nif_param({out_debug_message_log, MaxMessagesVarName, SourceMap, TypeMap, SeverityMap}, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "ErlNifUInt64 ~s_size_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint64(env, argv[~p], &~s_size_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s == 0 || ~s > (GLuint)INT_MAX || ~s_size_tmp == 0 || ~s_size_tmp > (ErlNifUInt64)INT_MAX) {\n", [
        MaxMessagesVarName,
        MaxMessagesVarName,
        VarName,
        VarName
    ]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "size_t ~s_count_tmp = (size_t)~s;\n", [VarName, MaxMessagesVarName]),
    write(1, "GLsizei ~s_size = (GLsizei)~s_size_tmp;\n", [VarName, VarName]),
    write(1, "GLenum* ~s_sources = enif_alloc(sizeof(GLenum) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLenum* ~s_types = enif_alloc(sizeof(GLenum) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLuint* ~s_ids = enif_alloc(sizeof(GLuint) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLenum* ~s_severities = enif_alloc(sizeof(GLenum) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLsizei* ~s_lengths = enif_alloc(sizeof(GLsizei) * ~s_count_tmp);\n", [VarName, VarName]),
    write(1, "GLchar* ~s_message_log = enif_alloc((size_t)~s_size_tmp);\n", [VarName, VarName]),
    write(1, "if (!~s_sources || !~s_types || !~s_ids || !~s_severities || !~s_lengths || !~s_message_log) {\n", [
        VarName,
        VarName,
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write_debug_log_free_lines(VarName),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_size", [VarName]),
    PassArg2 = io_lib:format("~s_sources", [VarName]),
    PassArg3 = io_lib:format("~s_types", [VarName]),
    PassArg4 = io_lib:format("~s_ids", [VarName]),
    PassArg5 = io_lib:format("~s_severities", [VarName]),
    PassArg6 = io_lib:format("~s_lengths", [VarName]),
    PassArg7 = io_lib:format("~s_message_log", [VarName]),
    {
        VarName,
        [PassArg1, PassArg2, PassArg3, PassArg4, PassArg5, PassArg6, PassArg7],
        [{out_debug_message_log, VarName, MaxMessagesVarName, SourceMap, TypeMap, SeverityMap}]
    };

process_nif_param(out_shader_precision_format, Index) ->
    % glGetShaderPrecisionFormat writes GLint range[2] plus GLint precision.
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLint ~s_range[2] = {0, 0};\n", [VarName]),
    write(1, "GLint ~s_precision = 0;\n", [VarName]),

    PassArg1 = io_lib:format("~s_range", [VarName]),
    PassArg2 = io_lib:format("&~s_precision", [VarName]),
    {VarName, [PassArg1, PassArg2], [{out_shader_precision_format, VarName}]};

process_nif_param({out_list_alloc_2, VarType, TermFunction}, Index) ->
    % Example: genBuffers?().
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLsizei ~s_n;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_n);\n", [Index, VarName]),

    write(1, "~s* ~s = enif_alloc(sizeof(~s) * ~s_n);\n", [VarType, VarName, VarType, VarName]),
    PassArg1 = io_lib:format("~s_n", [VarName]),
    PassArg2 = io_lib:format("~s", [VarName]),
    {VarName, [PassArg1, PassArg2], [{out_list_alloc_2, VarName, TermFunction}]};

process_nif_param({return_list_terms_noalloc, VarType, TermFunction}, Index) ->
    % Example: glGet*()

    % int n = enif_make_int(env, argv[0], &n);
    % GLint* output;
    % glFooBar(a, b, &output);
    % ERL_NIF_TERM result = enif_make_list(env, 0);
    % for (int i = n-1; i >= 0; i--) {
    %     result = enif_make_list_cell(env, enif_make_int(env, output[i]), result);
    % }
    % return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);

    VarName = io_lib:format("arg_~p", [Index]),

    % tmp code:
    write(1, "GLsizei ~s_n = 1;\n", [VarName]),

    N = io_lib:format("arg_~p_n", [Index]),
    write(1, "~s* ~s;\n", [VarType, VarName]),

    PassArg = io_lib:format("&~s", [VarName]),
    {VarName, [PassArg], [{return_list_terms_noalloc, VarName, N, TermFunction}]};

process_nif_param({return_list_terms_alloc, VarType, TermFunction}, Index) ->
    % Example: glGenTextures()
    VarName = io_lib:format("arg_~p", [Index]),

    % XXX: Should never be negative because we're allocating an array.
    write(1, "unsigned int ~s_n_tmp;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_n_tmp);\n", [Index, VarName]),
    write(1, "GLsizei ~s_n = (GLsizei)~s_n_tmp;\n", [VarName, VarName]),
    write(1,
        "~s* ~s = enif_alloc(sizeof(~s) * ~s_n);\n",
        [VarType, VarName, VarType, VarName]
    ),
    PassArg1 = io_lib:format("~s_n", [VarName]),
    PassArg2 = io_lib:format("~s", [VarName]),
    {VarName, [PassArg1, PassArg2], [{return_list_terms_alloc, VarName, TermFunction}]};

process_nif_param({caller_sized_list, VarType, TermFunction}, Index) ->
    % Example: glGetAttachedShaders().
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "unsigned int ~s_max_tmp;\n", [VarName]),
    write(1, "if (!enif_get_uint(env, argv[~p], &~s_max_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_max_tmp > (unsigned int)INT32_MAX) {\n", [VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "GLsizei ~s_max = (GLsizei)~s_max_tmp;\n", [VarName, VarName]),
    write(1, "GLsizei ~s_count = 0;\n", [VarName]),
    write(1, "~s* ~s = NULL;\n", [VarType, VarName]),
    write(1, "if (~s_max > 0) {\n", [VarName]),
    write(1, "    ~s = enif_alloc(sizeof(~s) * ~s_max);\n", [VarName, VarType, VarName]),
    write(1, "    if (!~s) {\n", [VarName]),
    write(1, "        return enif_make_tuple2(env,\n", []),
    write(1, "            enif_make_atom(env, \"error\"),\n", []),
    write(1, "            enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "        );\n", []),
    write(1, "    }\n", []),
    write(1, "}\n", []),

    PassArg1 = io_lib:format("~s_max", [VarName]),
    PassArg2 = io_lib:format("&~s_count", [VarName]),
    PassArg3 = io_lib:format("~s", [VarName]),
    {VarName, [PassArg1, PassArg2, PassArg3], [{caller_sized_list, VarName, TermFunction}]};

process_nif_param({out_scalar, VarType, TermFunction}, Index) ->
    % Hidden scalar out parameters are allocated on the stack and returned
    % after the OpenGL call.
    VarName = io_lib:format("out_~p", [Index]),
    write(1, "~s ~s;\n", [VarType, VarName]),
    PassArg = io_lib:format("&~s", [VarName]),
    {VarName, [PassArg], [{out_scalar, VarName, TermFunction}]};

process_nif_param({out_enum, VarType, TransformMap}, Index) ->
    % Hidden enum out parameters are returned as atoms after the OpenGL call.
    VarName = io_lib:format("out_~p", [Index]),
    write(1, "~s ~s;\n", [VarType, VarName]),
    PassArg = io_lib:format("&~s", [VarName]),
    {VarName, [PassArg], [{out_enum, VarName, TransformMap}]}.

process_nif_param_fold({ParamName, ParamData0}, Accumulator) ->
    {Index, VarNames, PassArgs0, Instructions0, VarNamesByParam} = Accumulator,
    ParamData = case ParamData0 of
        {out_typed_value_list_from_counted_input, SourceParam, VarType, TermFunction} ->
            SourceVarName = maps:get(SourceParam, VarNamesByParam),
            {out_typed_value_list_from_counted_input, SourceVarName, VarType, TermFunction};
        {out_debug_message_log, MaxMessagesParam, SourceMap, TypeMap, SeverityMap} ->
            MaxMessagesVarName = maps:get(MaxMessagesParam, VarNamesByParam),
            {out_debug_message_log, MaxMessagesVarName, SourceMap, TypeMap, SeverityMap};
        _ ->
            ParamData0
    end,
    IsHiddenOut = case ParamData of
        {out_scalar, _, _} -> true;
        {out_enum, _, _} -> true;
        {out_typed_value_list_from_counted_input, _, _, _} -> true;
        out_shader_precision_format -> true;
        _ -> false
    end,
    {VarName, PassArgs1, Instructions1} = process_nif_param(ParamData, Index),
    NextIndex = case IsHiddenOut of
        true -> Index;
        false -> Index + 1
    end,
    {
        NextIndex,
        [VarName | VarNames],
        PassArgs0 ++ PassArgs1,
        Instructions0 ++ Instructions1,
        maps:put(ParamName, VarName, VarNamesByParam)
    }.

% If the OpenGL call had a return value, it unconditionally becomes the first
% return value (in the "return tuple") and we do the post-processing here.
process_return_value_if_any(void) ->
    [];
process_return_value_if_any(glbool_to_boolean) ->
    % XXX: Rework this.
    write(1, "ERL_NIF_TERM ret_0 = enif_make_atom(env, ret ? \"true\" : \"false\");\n"),
    ["ret_0"];
% process_return_value_if_any(gl_int) ->
%     [];
% process_return_value_if_any(gl_float) ->
%     [];
% process_return_value_if_any(gl_sizei) ->
%     [];
process_return_value_if_any(glint_to_integer)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_int(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any(gluint_to_uint)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_uint(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any(gluint_to_integer)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_int(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any(glint64_to_integer)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_int64(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any(gluint64_to_uint)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_uint64(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any(gluint64_to_integer)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_uint64(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any(debug_message_log_count) ->
    [];
process_return_value_if_any({glenum_to_atom, TransformMap}) ->

    write(1, "ERL_NIF_TERM ret_0;\n"),
    write(1, "switch (ret) {\n"),
    lists:foreach(fun({Value, Atom}) ->
        write(1, "    case ~s: ret_0 = beam_atom_~s; break;\n", [Value, Atom])
    end, TransformMap),
    write(1, "    default: ret_0 = enif_make_atom(env, \"unknown\"); break;\n"),
    write(1, "}\n"),    

    ["ret_0"];

process_return_value_if_any(const_glubyte_to_string) ->
    write(1, "size_t ret_len = ret ? strlen((const char*)ret) : 0;\n"),
    write(1, "ErlNifBinary ret_tmp;\n"),
    write(1, "if (!enif_alloc_binary(ret_len, &ret_tmp)) {\n"),
    write(1, "    return enif_make_tuple2(env,\n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n"),
    write(1, "if (ret_len > 0) {\n"),
    write(1, "    memcpy(ret_tmp.data, ret, ret_len);\n"),
    write(1, "}\n"),
    write(1, "ERL_NIF_TERM ret_0 = enif_make_binary(env, &ret_tmp);\n"),
    write(0, "\n"),

    ["ret_0"].

process_post_call_instruction({free_array, VarName}) ->
    write(1, "enif_free(~s);\n", [VarName]),
    [];

process_post_call_instruction({in_list_gl_strings, VarName}) ->
    % We have allocated the arrays for the strings, lengths, and binaries.
    % We must now release the binaries and free the arrays.
    write(1, "for (unsigned ~s_j = 0; ~s_j < ~s_count; ~s_j++) {\n", [VarName, VarName, VarName, VarName]),
    write(1, "    enif_release_binary(&~s_binaries[~s_j]);\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "enif_free(~s_strings);\n", [VarName]),
    write(1, "enif_free(~s_lengths);\n", [VarName]),
    write(1, "enif_free(~s_binaries);\n\n", [VarName]),

    % Nothing to return.
    [];

process_post_call_instruction({in_list_gl_strings_null_terminated, VarName}) ->
    % Free the temporary NUL-terminated GLchar* strings and the pointer array.
    write(1, "for (unsigned ~s_j = 0; ~s_j < ~s_count; ~s_j++) {\n", [VarName, VarName, VarName, VarName]),
    write(1, "    enif_free(~s_strings[~s_j]);\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "enif_free(~s_strings);\n\n", [VarName]),
    [];

process_post_call_instruction({in_multi_draw_arrays, VarName}) ->
    write(1, "enif_free(~s_first);\n", [VarName]),
    write(1, "enif_free(~s_count);\n", [VarName]),
    [];

process_post_call_instruction({in_multi_draw_elements, VarName}) ->
    write(1, "enif_free(~s_count);\n", [VarName]),
    write(1, "enif_free(~s_indices);\n", [VarName]),
    [];

process_post_call_instruction({in_multi_draw_elements_base_vertex, VarName}) ->
    write(1, "enif_free(~s_count);\n", [VarName]),
    write(1, "enif_free(~s_indices);\n", [VarName]),
    write(1, "enif_free(~s_basevertex);\n", [VarName]),
    [];

process_post_call_instruction({in_multi_bind_object_list, VarName}) ->
    write(1, "enif_free(~s_objects);\n", [VarName]),
    [];

process_post_call_instruction({in_multi_bind_buffer_ranges, VarName}) ->
    write(1, "enif_free(~s_buffers);\n", [VarName]),
    write(1, "enif_free(~s_offsets);\n", [VarName]),
    write(1, "enif_free(~s_sizes);\n", [VarName]),
    [];

process_post_call_instruction({in_multi_bind_vertex_buffers, VarName}) ->
    write(1, "enif_free(~s_buffers);\n", [VarName]),
    write(1, "enif_free(~s_offsets);\n", [VarName]),
    write(1, "enif_free(~s_strides);\n", [VarName]),
    [];

process_post_call_instruction({in_gl_object_list_with_count, VarName}) ->
    write(1, "enif_free(~s_objects);\n", [VarName]),
    [];

process_post_call_instruction({in_specialization_constant_list, VarName}) ->
    write(1, "if (~s_indices) enif_free(~s_indices);\n", [VarName, VarName]),
    write(1, "if (~s_values) enif_free(~s_values);\n", [VarName, VarName]),
    [];

process_post_call_instruction({out_gl_string, VarName}) ->

    write(1, "ERL_NIF_TERM ~s_result;\n", [VarName]),
    write(1, "unsigned char* ~s_bin = enif_make_new_binary(env, ~s_length, &~s_result);\n", [VarName, VarName, VarName]),
    write(1, "if (~s_bin && ~s_length > 0) {\n", [VarName, VarName]),
    write(1, "    memcpy(~s_bin, ~s_info_log, ~s_length);\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),
    write(1, "enif_free(~s_info_log);\n", [VarName]),

    [io_lib:format("~s_result", [VarName])];

process_post_call_instruction({foo, VarName}) ->
    [VarName];

process_post_call_instruction({out_list_alloc_1, VarName, TermFunction}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_n-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, VarName, RetVarName]
    ),
    write(1, "}\n", []),

    write(1, "enif_free(~s);\n", [VarName]),

    [RetVarName];

process_post_call_instruction({out_typed_value_list, VarName, ValuesName, TermFunction}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_count-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, ValuesName, RetVarName]
    ),
    write(1, "}\n", []),
    write(1, "enif_free(~s);\n", [ValuesName]),

    [RetVarName];

process_post_call_instruction({caller_sized_typed_value_list, VarName, ValuesName, TermFunction}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "if (~s_length < 0) {\n", [VarName]),
    write(1, "    ~s_length = 0;\n", [VarName]),
    write(1, "}\n", []),
    write(1, "if (~s_length > ~s_count) {\n", [VarName, VarName]),
    write(1, "    ~s_length = ~s_count;\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_length-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, ValuesName, RetVarName]
    ),
    write(1, "}\n", []),
    write(1, "enif_free(~s);\n", [ValuesName]),

    [RetVarName];

process_post_call_instruction({out_debug_message_log, VarName, MaxMessagesVarName, SourceMap, TypeMap, SeverityMap}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    TermsVarName = io_lib:format("~s_terms", [VarName]),

    write(1, "GLuint ~s_actual_count = ret;\n", [VarName]),
    write(1, "if (~s_actual_count > ~s) {\n", [VarName, MaxMessagesVarName]),
    write(1, "    ~s_actual_count = ~s;\n", [VarName, MaxMessagesVarName]),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM* ~s = NULL;\n", [TermsVarName]),
    write(1, "if (~s_actual_count > 0) {\n", [VarName]),
    write(1, "    ~s = enif_alloc(sizeof(ERL_NIF_TERM) * (size_t)~s_actual_count);\n", [
        TermsVarName,
        VarName
    ]),
    write(1, "    if (!~s) {\n", [TermsVarName]),
    write_debug_log_free_lines(VarName),
    write(1, "        return enif_make_tuple2(env,\n", []),
    write(1, "            enif_make_atom(env, \"error\"),\n", []),
    write(1, "            enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "        );\n", []),
    write(1, "    }\n", []),
    write(1, "}\n", []),
    write(1, "size_t ~s_offset = 0;\n", [VarName]),
    write(1, "for (GLuint ~s_i = 0; ~s_i < ~s_actual_count; ~s_i++) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    size_t ~s_raw_length = 0;\n", [VarName]),
    write(1, "    if (~s_lengths[~s_i] > 0) {\n", [VarName, VarName]),
    write(1, "        ~s_raw_length = (size_t)~s_lengths[~s_i];\n", [VarName, VarName, VarName]),
    write(1, "    }\n", []),
    write(1, "    size_t ~s_remaining = (~s_offset < (size_t)~s_size_tmp) ? ((size_t)~s_size_tmp - ~s_offset) : 0;\n", [
        VarName,
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    if (~s_raw_length > ~s_remaining) {\n", [VarName, VarName]),
    write(1, "        ~s_raw_length = ~s_remaining;\n", [VarName, VarName]),
    write(1, "    }\n", []),
    write(1, "    size_t ~s_message_length = ~s_raw_length;\n", [VarName, VarName]),
    write(1, "    if (~s_message_length > 0 && ~s_message_log[~s_offset + ~s_message_length - 1] == '\\0') {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "        ~s_message_length--;\n", [VarName]),
    write(1, "    }\n", []),
    write_debug_log_enum_term(
        io_lib:format("~s_source_ret", [VarName]),
        io_lib:format("~s_sources[~s_i]", [VarName, VarName]),
        SourceMap
    ),
    write_debug_log_enum_term(
        io_lib:format("~s_type_ret", [VarName]),
        io_lib:format("~s_types[~s_i]", [VarName, VarName]),
        TypeMap
    ),
    write_debug_log_enum_term(
        io_lib:format("~s_severity_ret", [VarName]),
        io_lib:format("~s_severities[~s_i]", [VarName, VarName]),
        SeverityMap
    ),
    write(1, "    ERL_NIF_TERM ~s_id_ret = enif_make_uint(env, ~s_ids[~s_i]);\n", [
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    ERL_NIF_TERM ~s_message_ret;\n", [VarName]),
    write(1, "    unsigned char* ~s_message_bin = enif_make_new_binary(env, ~s_message_length, &~s_message_ret);\n", [
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    if (~s_message_bin == NULL && ~s_message_length > 0) {\n", [VarName, VarName]),
    write(1, "        if (~s) enif_free(~s);\n", [TermsVarName, TermsVarName]),
    write_debug_log_free_lines(VarName),
    write(1, "        return enif_make_badarg(env);\n", []),
    write(1, "    }\n", []),
    write(1, "    if (~s_message_length > 0) {\n", [VarName]),
    write(1, "        memcpy(~s_message_bin, ~s_message_log + ~s_offset, ~s_message_length);\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    }\n", []),
    write(1, "    ~s[~s_i] = enif_make_tuple5(env,\n", [TermsVarName, VarName]),
    write(1, "        ~s_source_ret,\n", [VarName]),
    write(1, "        ~s_type_ret,\n", [VarName]),
    write(1, "        ~s_id_ret,\n", [VarName]),
    write(1, "        ~s_severity_ret,\n", [VarName]),
    write(1, "        ~s_message_ret\n", [VarName]),
    write(1, "    );\n", []),
    write(1, "    ~s_offset += ~s_raw_length;\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int ~s_i = (int)~s_actual_count - 1; ~s_i >= 0; ~s_i--) {\n", [
        VarName,
        VarName,
        VarName,
        VarName
    ]),
    write(1, "    ~s = enif_make_list_cell(env, ~s[~s_i], ~s);\n", [
        RetVarName,
        TermsVarName,
        VarName,
        RetVarName
    ]),
    write(1, "}\n", []),
    write(1, "if (~s) enif_free(~s);\n", [TermsVarName, TermsVarName]),
    write_debug_log_free_lines(VarName),

    [RetVarName];

process_post_call_instruction({out_shader_precision_format, VarName}) ->
    RangeMinRetVarName = io_lib:format("~s_range_min_ret", [VarName]),
    RangeMaxRetVarName = io_lib:format("~s_range_max_ret", [VarName]),
    PrecisionRetVarName = io_lib:format("~s_precision_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_int(env, ~s_range[0]);\n", [RangeMinRetVarName, VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_int(env, ~s_range[1]);\n", [RangeMaxRetVarName, VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_int(env, ~s_precision);\n", [PrecisionRetVarName, VarName]),

    [RangeMinRetVarName, RangeMaxRetVarName, PrecisionRetVarName];

process_post_call_instruction({out_list_alloc_2, VarName, TermFunction}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_n-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, VarName, RetVarName]
    ),
    write(1, "}\n", []),

    write(1, "enif_free(~s);\n", [VarName]),

    [RetVarName];

process_post_call_instruction({return_list_terms_noalloc, VarName, _N, TermFunction}) ->
    % ERL_NIF_TERM result = enif_make_list(env, 0); // Start with empty list
    % for (int i = n-1; i >= 0; i--) {
    %     result = enif_make_list_cell(env, enif_make_uint(env, textures[i]), result);
    % }
    % enif_free(textures);
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_n-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, VarName, RetVarName]
    ),
    write(1, "}\n", []),

    [RetVarName];

process_post_call_instruction({return_list_terms_alloc, VarName, TermFunction}) ->
    % ERL_NIF_TERM result = enif_make_list(env, 0); // Start with empty list
    % for (int i = n-1; i >= 0; i--) {
    %     result = enif_make_list_cell(env, enif_make_uint(env, textures[i]), result);
    % }
    % enif_free(textures);
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_n-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, VarName, RetVarName]
    ),
    write(1, "}\n", []),
    write(1, "enif_free(~s);\n", [VarName]),

    [RetVarName];
process_post_call_instruction({caller_sized_list, VarName, TermFunction}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "if (~s_count < 0) {\n", [VarName]),
    write(1, "    ~s_count = 0;\n", [VarName]),
    write(1, "}\n", []),
    write(1, "if (~s_count > ~s_max) {\n", [VarName, VarName]),
    write(1, "    ~s_count = ~s_max;\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s = enif_make_list(env, 0);\n", [RetVarName]),
    write(1, "for (int i = ~s_count-1; i >= 0; i--) {\n", [VarName]),
    write(1,
        "~s = enif_make_list_cell(env, ~s(env, ~s[i]), ~s);\n",
        [RetVarName, TermFunction, VarName, RetVarName]
    ),
    write(1, "}\n", []),
    write(1, "if (~s) {\n", [VarName]),
    write(1, "    enif_free(~s);\n", [VarName]),
    write(1, "}\n", []),

    [RetVarName];
process_post_call_instruction({out_scalar, VarName, glint_to_boolean}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_atom(env, ~s != 0 ? \"true\" : \"false\");\n", [RetVarName, VarName]),

    [RetVarName];
process_post_call_instruction({out_scalar, VarName, glint_to_integer}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_int(env, ~s);\n", [RetVarName, VarName]),

    [RetVarName];
process_post_call_instruction({out_scalar, VarName, glint64_to_integer}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_int64(env, ~s);\n", [RetVarName, VarName]),

    [RetVarName];
process_post_call_instruction({out_enum, VarName, TransformMap}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s;\n", [RetVarName]),
    write(1, "switch (~s) {\n", [VarName]),
    lists:foreach(fun({Value, Atom}) ->
        write(1, "    case ~s: ~s = beam_atom_~s; break;\n", [Value, RetVarName, Atom])
    end, TransformMap),
    write(1, "    default: ~s = enif_make_atom(env, \"unknown\"); break;\n", [RetVarName]),
    write(1, "}\n"),

    [RetVarName];
process_post_call_instruction({out_program_binary, VarName}) ->
    FormatRetVarName = io_lib:format("~s_format_ret", [VarName]),
    BinaryRetVarName = io_lib:format("~s_binary_ret", [VarName]),
    write(1, "if (~s_length < 0) {\n", [VarName]),
    write(1, "    ~s_length = 0;\n", [VarName]),
    write(1, "}\n", []),
    write(1, "if ((ErlNifUInt64)~s_length > ~s_size) {\n", [VarName, VarName]),
    write(1, "    ~s_length = (GLsizei)~s_size;\n", [VarName, VarName]),
    write(1, "}\n", []),
    write(1, "ERL_NIF_TERM ~s = enif_make_uint(env, ~s_format);\n", [FormatRetVarName, VarName]),
    write(1, "ERL_NIF_TERM ~s;\n", [BinaryRetVarName]),
    write(1, "unsigned char* ~s_bin = enif_make_new_binary(env, (size_t)~s_length, &~s);\n", [
        VarName,
        VarName,
        BinaryRetVarName
    ]),
    write(1, "if (~s_bin == NULL && ~s_length > 0) {\n", [VarName, VarName]),
    write(1, "    if (~s_data) {\n", [VarName]),
    write(1, "        enif_free(~s_data);\n", [VarName]),
    write(1, "    }\n", []),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n", []),
    write(1, "if (~s_data && ~s_length > 0) {\n", [VarName, VarName]),
    write(1, "    memcpy(~s_bin, ~s_data, (size_t)~s_length);\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),
    write(1, "if (~s_data) {\n", [VarName]),
    write(1, "    enif_free(~s_data);\n", [VarName]),
    write(1, "}\n", []),

    [FormatRetVarName, BinaryRetVarName];
process_post_call_instruction({out_active_reflection_info, VarName, TransformMap}) ->
    SizeRetVarName = io_lib:format("~s_size_ret", [VarName]),
    TypeRetVarName = io_lib:format("~s_type_ret", [VarName]),
    NameRetVarName = io_lib:format("~s_name_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_int(env, ~s_size);\n", [SizeRetVarName, VarName]),
    write(1, "ERL_NIF_TERM ~s;\n", [TypeRetVarName]),
    write(1, "switch (~s_type) {\n", [VarName]),
    lists:foreach(fun({Value, Atom}) ->
        write(1, "    case ~s: ~s = beam_atom_~s; break;\n", [Value, TypeRetVarName, Atom])
    end, TransformMap),
    write(1, "    default: ~s = enif_make_atom(env, \"unknown\"); break;\n", [TypeRetVarName]),
    write(1, "}\n"),
    write(1, "ERL_NIF_TERM ~s;\n", [NameRetVarName]),
    write(1, "unsigned char* ~s_bin = enif_make_new_binary(env, ~s_length, &~s);\n", [VarName, VarName, NameRetVarName]),
    write(1, "if (~s_bin && ~s_length > 0) {\n", [VarName, VarName]),
    write(1, "    memcpy(~s_bin, ~s_name, ~s_length);\n", [VarName, VarName, VarName]),
    write(1, "}\n", []),
    write(1, "enif_free(~s_name);\n", [VarName]),

    [SizeRetVarName, TypeRetVarName, NameRetVarName];
process_post_call_instruction({out_scalar, VarName, gluint64_to_uint}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_uint64(env, ~s);\n", [RetVarName, VarName]),

    [RetVarName];
process_post_call_instruction({out_scalar, VarName, gluint64_to_integer}) ->
    RetVarName = io_lib:format("~s_ret", [VarName]),
    write(1, "ERL_NIF_TERM ~s = enif_make_uint64(env, ~s);\n", [RetVarName, VarName]),

    [RetVarName];
process_post_call_instruction({binary_to_gl_string_char, MsgVarName}) ->
    write(1, "enif_free(~s);\n", [MsgVarName]),
    [];
process_post_call_instruction({in_gl_string, StringVarName}) ->
    write(1, "enif_free(~s);\n", [StringVarName]),
    [].

process_out_params(Instructions) ->
    lists:foldl(fun(Instruction, Acc) ->
        Acc ++ process_post_call_instruction(Instruction)
    end, [], Instructions).

call_pass_args("glMultiDrawElements", [Mode, Type, Counts, Indices, DrawCount]) ->
    [Mode, Counts, Type, Indices, DrawCount];
call_pass_args("glMultiDrawElementsBaseVertex", [Mode, Type, Counts, Indices, DrawCount, BaseVertex]) ->
    [Mode, Counts, Type, Indices, DrawCount, BaseVertex];
call_pass_args(_GlCommandName, PassArgs) ->
    PassArgs.

write_nif_function_body(NifFunctionName, NifFunctionData) ->
    % First, we process the parameters. As we process them, we generate the
    % pre-call code and gather infos to re-use later (the var name, how it's
    % used in the actual OpenGL call). Note that one param can generate more
    % than one variable (for instance, a BEAM binary can produce a C array and
    % its length). It also also generates instructions to produce the post-call
    % code.

    % We never use the argc value.
    write(0, "    (void)argc;\n\n"),

    % XXX: Del VarNames?
    {_, _VarNames, PassArgs, Instructions, _VarNamesByParam} = lists:foldl(
        fun process_nif_param_fold/2,
        {0, [], [], [], #{}},
        maps:get(params, NifFunctionData)
    ),
    write(0, "\n"),

    % Next we write the OpenGL call. The line depends on whether there is a
    % return value.
    case maps:get(return, NifFunctionData) of
        void ->
            write(1, "");
        glbool_to_boolean ->
            write(1, "GLboolean ret = ");
        % gl_int ->
        %     write(1, "GLint ret = ");
        % gl_float ->
        %     write(1, "GLfloat ret = ");u
        % gl_sizei ->
        %     write(1, "GLsizei ret = ")
        glint_to_integer ->
            write(1, "GLint ret = ");
        gluint_to_uint ->
            write(1, "GLuint ret = ");
        gluint_to_integer ->
            write(1, "GLuint ret = ");
        glint64_to_integer ->
            write(1, "GLint64 ret = ");
        gluint64_to_uint ->
            write(1, "GLuint64 ret = ");
        gluint64_to_integer ->
            write(1, "GLuint64 ret = ");
        debug_message_log_count ->
            write(1, "GLuint ret = ");
        {glenum_to_atom, _Group} ->
            write(1, "GLenum ret = ");
        const_glubyte_to_string ->
            write(1, "const GLubyte* ret = ")
    end,
    GlCommandName = maps:get(gl_command, NifFunctionData, NifFunctionName),
    write(0, "~s(", [GlCommandName]),
    write(0, string:join(call_pass_args(GlCommandName, PassArgs), ", ")),
    write(0, ");\n\n"),

    % We write the post-call code. We start with processing the return value
    % (if any), then the "out" parameters.
    RetArgs0 = process_return_value_if_any(maps:get(return, NifFunctionData)),
    RetArgs1 = process_out_params(Instructions),

    % And last, we write the return statement. By now we have all the return
    % "ERL_NIF_TERM" values (if any) prepared. If there is none, we simply
    % return the "ok" atom, otherwise we return a "ok" tuple.
    RetArgs = RetArgs0 ++ RetArgs1,


    % case RetArgs of
    %     [] ->
    %         write(1, "return enif_make_atom(env, \"ok\");\n");
    %     _ ->
    %         TupleArity = length(RetArgs) + 1,
    %         write(1, "return enif_make_tuple~p(env,\n", [TupleArity]),
    %         write(2, "enif_make_atom(env, \"ok\"),\n        "),
    %         write(0, string:join(RetArgs, ",\n        ")),
    %         write(0, "\n    );\n")
    % end,
    case RetArgs of
        [] ->
            write(1, "return enif_make_tuple(env, 0);\n", []);
        _ ->
            TupleArity = length(RetArgs),
            write(1, "return enif_make_tuple(env, ~p,\n        ", [TupleArity]),
            write(0, string:join(RetArgs, ",\n        ")),
            write(0, "\n    );\n")
    end,

    ok.

write_nif_array(TargetApi, Functions) ->
    Data = lists:foldl(fun({NifFunctionName, NifFunctionData}, Acc) ->
        Name = io_lib:format("~s_raw", [NifFunctionName]),
        Arity = maps:get(arity, NifFunctionData),
        Function = io_lib:format("nif_~s", [NifFunctionName]),
        [{Name, Arity, Function}|Acc]
    end, [], unique_nif_functions(Functions)),
    Parts = lists:foldl(fun({Name, Arity, Function}, Acc) ->
        Strings = io_lib:format(
            "    {\"~s\", ~p, ~s, 0}",
            [Name, Arity, Function]
        ),
        [lists:flatten(Strings)|Acc]
    end, [], Data),

    WithGladParts = case needs_glad(TargetApi) of
        true ->
            ["{\"glad_load_gl\", 0, nif_gladLoadGl, 0}" | Parts];
        false ->
            Parts
    end,
    write(0, "static ErlNifFunc nif_functions[] = {\n"),
    write(0, string:join(WithGladParts, ",\n")),
    write(0, "\n"),
    write(0, "};\n"),

    ok.

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
