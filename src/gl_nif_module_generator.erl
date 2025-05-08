%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It does this and that...
%%
%% *out_list_gluint*
%%
%% ```
%% // 1. Parse the count argument
%% GLsizei n;
%% if (!enif_get_int(env, argv[0], &n) || n <= 0) {
%%     return enif_make_badarg(env);
%% }
%%
%% // 2. Allocate temporary array for texture IDs
%% GLuint* textures = enif_alloc(sizeof(GLuint) * n);
%% if (!textures) {
%%     return enif_make_tuple2(env,
%%                             enif_make_atom(env, "error"),
%%                             enif_make_atom(env, "alloc_failed"));
%% }
%%
%% // 3. Generate textures
%% glGenTextures(n, textures);
%%
%% // 4. Build Erlang list of results
%% ERL_NIF_TERM result = enif_make_list(env, 0); // Start with empty list
%% for (int i = n-1; i >= 0; i--) {
%%     result = enif_make_list_cell(env,
%%                                 enif_make_uint(env, textures[i]),
%%                                 result);
%% }
%%
%% // 5. Clean up
%% enif_free(textures);
%%
%% return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
%% ```
%%
-module(gl_nif_module_generator).
-export([generate/1]).
-import(opengl_gen, [open/1, write/2, write/3, close/0]).

generate(BindingData) ->
    % xxx: should be 'gl_nif.c'

    open("/home/intjelic/Workspace/erlangsters/opengl-x.y-generator/output/gl.c"),
    % open("/home/intjelic/Workspace/erlangsters/opengl-4.6/c_src/gl.c"),

    write(0, "// This file is generated. Do not edit!\n"),
    write(0, "\n"),

    % XXX: review what's actually needed.
    write(0, "#include <string.h>\n"),
    write(0, "#include <stdio.h>\n"),
    write(0, "#include <pthread.h>\n"),
    write(0, "#include <dlfcn.h>\n"),
    write(0, "#include <erl_nif.h>\n"),
    write(0, "#include <EGL/egl.h>\n"),
    write(0, "#include <GL/gl.h>\n"),
    write(0, "\n"),

    % Declare static pointer to the execute OpenGL command function of the EGL
    % binding NIF library.
    write(0, """
    static void* egl_nif_lib_handle = NULL;
    typedef ERL_NIF_TERM (*execute_command_fn)(
        ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
        ErlNifEnv*,
        int,
        ERL_NIF_TERM* []
    );
    execute_command_fn egl_nif_execute_command = NULL;
    """),
    write(0, "\n\n"),

    write_nif_load_function(),
    write(0, "\n"),
    write_nif_unload_function(),

    % tmp code
    write(0, """
ERL_NIF_TERM gl_bool_to_erl_boolean(ErlNifEnv* env, GLboolean val) {
    // return enif_make_atom(env, "tmp");
    return enif_make_atom(env, val == GL_TRUE ? "true" : "false");
}
""", []),
    write(0, "\n\n"),

    % Generate the NIF functions.
    write_nif_functions(maps:get(functions, BindingData)),
    write(0, "\n"),

    % Generate the NIF functions static array.
    write_nif_array(maps:get(functions, BindingData)),
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

write_nif_load_function() ->
    write(0, "static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)\n"),
    write(0, "{\n"),

    % Load the EGL NIF library, then get the pointer to the function that
    % executes the OpenGL command.
    write(0, """
        egl_nif_lib_handle = dlopen("/home/intjelic/Workspace/erlangsters/opengl-pilot/_checkouts/egl/priv/beam-egl.so", RTLD_LAZY);
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
    """),
    write(0, "\n\n"),

    write(0, "    return 0;\n"),
    write(0, "}\n"),

    ok.

write_nif_unload_function() ->
    write(0, "static int nif_module_unload(ErlNifEnv* caller_env, void** priv_data)\n"),
    write(0, "{\n"),

    % Nothing to do.

    write(0, "    return 0;\n"),
    write(0, "}\n"),

    ok.

write_nif_functions(Functions) ->
    maps:foreach(fun(_FunctionName, FunctionData) ->
        maps:foreach(fun(NifFunctionName, NifFunctionData) ->
            write_nif_function(NifFunctionName, NifFunctionData)
        end, maps:get(nif_functions, FunctionData))
    end, Functions),

    ok.

write_nif_function(FunctionName, FunctionData) ->
    write(0, "static ERL_NIF_TERM nif_~s_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])\n", [FunctionName]),
    write(0, "{\n"),
    write_nif_function_body(FunctionName, FunctionData),
    write(0, "}\n"),
    write(0, "\n"),

    write(0, "static ERL_NIF_TERM nif_~s(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])\n", [FunctionName]),
    write(0, "{\n"),
    write(0, "    return egl_nif_execute_command(nif_~s_command, env, argc, argv);\n", [FunctionName]),
    write(0, "}\n"),
    write(0, "\n"),

    ok.

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
    write(1, "}\n"),
    {VarName, [VarName], []};
process_nif_param(integer_to_glint, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLint ~s;\n", [VarName]),
    write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};
process_nif_param(integer_to_glintptr, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLintptr ~s;\n", [VarName]),
    write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};
% process_nif_param(integer_to_glint64, Index) -> %XXX Is needed ?
%     VarName = io_lib:format("arg_~p", [Index]),
%     write(1, "GLint64 ~s;\n", [VarName]),
%     write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
%     write(1, "    return enif_make_badarg(env);\n"),
%     write(1, "}\n"),
%     {VarName, [VarName], []};
process_nif_param(integer_to_gluint, Index) ->
    % There is no "unsigned int" at the BEAM level, only "integer". We retrieve
    % the "integer" value and cast it to "GLuint" at the C level.
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLuint ~s;\n", [VarName]),
    write(1, "int ~s_tmp;\n", [VarName]),
    % XXX: use get_uint ???
    write(1, "if (!enif_get_int(env, argv[~p], &~s_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    write(1, "~s = (GLuint)~s_tmp;\n", [VarName, VarName]),
    {VarName, [VarName], []};
process_nif_param(integer_to_gluint64, Index) ->
    % There is no "unsigned int" at the BEAM level, only "integer". We retrieve
    % the "integer" value and cast it to "GLuint" at the C level.
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLuint ~s;\n", [VarName]),
    write(1, "int ~s_tmp;\n", [VarName]),
    % XXX: use get_uint ???
    write(1, "if (!enif_get_int(env, argv[~p], &~s_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    write(1, "~s = (GLuint)~s_tmp;\n", [VarName, VarName]),
    {VarName, [VarName], []};
process_nif_param(double_to_glfloat, Index) ->
    % There is no "float" at the BEAM level, only "double". We retrieve the
    % "double" value and cast it to "GLfloat" at the C level.
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLfloat ~s;\n", [VarName]),
    write(1, "double ~s_tmp;\n", [VarName]),
    write(1, "if (!enif_get_double(env, argv[~p], &~s_tmp)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    write(1, "~s = (GLfloat)~s_tmp;\n", [VarName, VarName]),
    {VarName, [VarName], []};
process_nif_param(double_to_gldouble, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLdouble ~s;\n", [VarName]),
    write(1, "if (!enif_get_double(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};
process_nif_param(integer_to_glsizei, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLsizei ~s;\n", [VarName]),
    write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};
process_nif_param(integer_to_glsizeiptr, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLsizeiptr ~s;\n", [VarName]),
    write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};
process_nif_param(integer_to_glenum, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLenum ~s;\n", [VarName]),
    write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};
process_nif_param(integer_to_glbitfield, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLbitfield ~s;\n", [VarName]),
    write(1, "if (!enif_get_int(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    {VarName, [VarName], []};


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

    write(1, "const void* ~s = NULL;\n", [VarName]),
    write(1, "ErlNifBinary ~s_bin;\n", [VarName]),
    write(1, "if (enif_is_identical(argv[~p], enif_make_atom(env, \"undefined\"))) {\n", [Index]),
    write(1, "    ~s = NULL;\n", [VarName]),
    write(1, "}\n", []),
    write(1, "else if (enif_inspect_binary(env, argv[~p], &~s_bin)) {\n", [Index, VarName]),
    write(1, "    ~s = ~s_bin.data;\n", [VarName, VarName]),
    write(1, "}\n", []),
    {VarName, [VarName], []};

process_nif_param(in_gl_offset, Index) ->
    % The parameter is an integer that we cast to void*
    % XXX: more explanation
    % XXX: Btw, it should be an non_neg_integer() ???
    %
    % ```
    % void glVertexAttribPointer(
    %   GLuint index,
    %   GLint size,
    %   GLenum type,
    %   GLboolean normalized,
    %   GLsizei stride,
    %   const void * pointer
    % );
    % ```
    %
    % Take the glVertexAttribPointer() function as a reference.

    % Compute the variable name which will also be used as a prefix for the
    % temporary variables.
    VarName = io_lib:format("arg_~p", [Index]),

    write(1, "unsigned int ~s;\n", [VarName]),
    write(1, "if (!enif_get_uint(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    PassArg = io_lib:format("(GLvoid*)~s", [VarName]),
    {VarName, [PassArg], []};

process_nif_param(binary_to_glbinary, Index) ->
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "ErlNifBinary ~s;\n", [VarName]),
    write(1, "if (!enif_inspect_binary(env, argv[~p], &~s)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n"),
    write(1, "}\n"),
    PassArg = io_lib:format("~s.data", [VarName]),
    {VarName, [PassArg], []};


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
    write(1, "    return enif_make_tuple2(env, \n", []),
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

    write(1, "unsigned int ~s_size;\n", [VarName]),
    write(1, "if (!enif_get_uint(env, argv[~p], &~s_size)) {\n", [Index, VarName]),
    write(1, "    return enif_make_badarg(env);\n", []),
    write(1, "}\n\n", []),

    write(1, "ErlNifBinary ~s_bin;\n", [VarName]),
    write(1, "if (!enif_alloc_binary(~s_size, &~s_bin)) {\n", [VarName, VarName]),
    write(1, "    return enif_make_tuple2(env, \n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"out_of_memory\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),

    write(1, "GLsizei ~s_length = 0;\n\n", [VarName]),

    PassArg1 = io_lib:format("(GLsizei)~s_size", [VarName]),
    PassArg2 = io_lib:format("~s_length", [VarName]),
    PassArg3 = io_lib:format("(GLchar*)~s_bin.data", [VarName]),

    {VarName, [PassArg1, PassArg2, PassArg3], [{out_gl_string, VarName}]};

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
    write(1, "memcpy(~s, ~s.data, ~s.size);\n", [MsgVarName, VarName, VarName]),
    write(1, "~s[~s.size] = '\\0';\n", [MsgVarName, VarName]),

    PassArg1 = io_lib:format("~s.size", [VarName]),
    PassArg2 = MsgVarName,
    {VarName, [PassArg1, PassArg2], [{binary_to_gl_string_char, MsgVarName}]};

process_nif_param(out_binary_implicit, Index) ->
    % must return an instruction so an additional return value is produced
    % must produce only one "pass args"

% enif_get_uint(env, argv[6], &data_size))

% ErlNifBinary bin;
% if (!enif_alloc_binary(data_size, &bin)) {
%     return enif_make_badarg(env);  // or proper error handling
% }

% glReadPixels(x, y, width, height, format, type, bin.data);

% return enif_make_binary(env, &bin);

    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "unsigned int ~s_size;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_size);\n", [Index, VarName]),
    write(1, "ErlNifBinary ~s;\n", [VarName]),

    PassArg = io_lib:format("~s.data", [VarName]),
    {"arg_x", [PassArg], [{foo, VarName}]};

process_nif_param(out_binary_explicit, Index) ->
    % must return an instruction so an additional return value is produced
    % must produce two "pass args"

    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "unsigned int ~s_size;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_size);\n", [Index, VarName]),
    write(1, "ErlNifBinary ~s;\n", [VarName]),

    PassArg1 = io_lib:format("~s_size", [VarName]),
    PassArg2 = io_lib:format("~s.data", [VarName]),
    {"arg_x", [PassArg1, PassArg2], [{foo, VarName}]};

process_nif_param({out_list_alloc_1, VarType, TermFunction}, Index) ->
    % Example: glGetBooleanv().
    VarName = io_lib:format("arg_~p", [Index]),
    write(1, "GLsizei ~s_n;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_n);\n", [Index, VarName]),

    write(1, "~s* ~s = enif_alloc(sizeof(~s) * ~s_n);\n", [VarType, VarName, VarType, VarName]),
    PassArg = io_lib:format("~s", [VarName]),
    {VarName, [PassArg], [{out_list_alloc_1, VarName, TermFunction}]};

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
    write(1, "GLsizei ~s_n;\n", [VarName]),
    write(1, "enif_get_uint(env, argv[~p], &~s_n);\n", [Index, VarName]),
    write(1,
        "~s* ~s = enif_alloc(sizeof(~s) * ~s_n);\n",
        [VarType, VarName, VarType, VarName]
    ),
    PassArg1 = io_lib:format("~s_n", [VarName]),
    PassArg2 = io_lib:format("~s", [VarName]),
    {VarName, [PassArg1, PassArg2], [{return_list_terms_alloc, VarName, TermFunction}]}.

process_nif_param_fold({_, ParamData}, Accumulator) ->
    {Index, VarNames, PassArgs0, Instructions0} = Accumulator,
    {VarName, PassArgs1, Instructions1} = process_nif_param(ParamData, Index),
    {
        Index + 1,
        [VarName | VarNames],
        PassArgs0 ++ PassArgs1,
        Instructions0 ++ Instructions1
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
process_return_value_if_any(gluint_to_integer)->
    write(1, "ERL_NIF_TERM ret_0 = enif_make_int(env, ret);\n"),
    ["ret_0"];
process_return_value_if_any({glenum_to_atom, _Group0}) ->
    % XXX" write(1, "ERL_NIF_TERM ret_0 = enum_to_atom(here);\n"),

    % write(1, "ERL_NIF_TERM ret_0 = enif_make_atom(env, \"yo\");\n"),

    write(1, "ERL_NIF_TERM ret_0 = enif_make_uint(env, ret);\n"),

    ["ret_0"];
process_return_value_if_any(const_glubyte_to_string) ->
    % XXX" write(1, "ERL_NIF_TERM ret_0 = const_glubyte_to_string(here);\n"),

    % size_t len = strlen((const char*)str);
    % ErlNifBinary bin;
    % if (!enif_alloc_binary(len, &bin)) {
    %     return enif_make_tuple2(env,
    %         enif_make_atom(env, "error"),
    %         enif_make_atom(env, "alloc_failed"));
    % }
    % memcpy(bin.data, str, len);

    write(1, "size_t ret_len = strlen((const char*)ret);\n"),
    write(1, "ErlNifBinary ret_tmp;\n"),
    write(1, "enif_alloc_binary(ret_len, &ret_tmp);\n"),
    write(1, "memcpy(ret_tmp.data, ret, ret_len);\n"),
    write(1, "ERL_NIF_TERM ret_0 = enif_make_binary(env, &ret_tmp);\n"),
    write(0, "\n"),

    ["ret_0"].

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

process_post_call_instruction({out_gl_string, VarName}) ->

    % XXX: what to do in this case ?
    write(1, "if (~s_length <= 0) {\n", [VarName]),
    write(1, "    enif_release_binary(&~s_bin);\n", [VarName]),
    write(1, "    return enif_make_tuple2(env, \n", []),
    write(1, "        enif_make_atom(env, \"error\"),\n", []),
    write(1, "        enif_make_atom(env, \"no_source_available\")\n", []),
    write(1, "    );\n", []),
    write(1, "}\n", []),



    write(1, "if ((GLsizei)~s_bin.size > ~s_length) {\n", [VarName, VarName]),
    write(1, "    if (!enif_realloc_binary(&~s_bin, ~s_length)) {\n", [VarName, VarName]),
    write(1, "        enif_release_binary(&~s_bin);\n", [VarName]),
    write(1, "        return enif_make_tuple2(env, \n", []),
    write(1, "            enif_make_atom(env, \"error\"),\n", []),
    write(1, "            enif_make_atom(env, \"realloc_failed\")\n", []),
    write(1, "        );\n", []),
    write(1, "    }\n", []),
    write(1, "}\n", []),

    write(1, "ERL_NIF_TERM ~s_result = enif_make_binary(env, &~s_bin);\n", [VarName, VarName]),

    [io_lib:format("~s_result", [VarName])];

process_post_call_instruction({foo, Name}) ->
    % enif_make_binary(env, &bin);
    RetVarName = io_lib:format("~s_ret", [Name]),
    write(1, "ERL_NIF_TERM ~s = enif_make_binary(env, &~s);\n", [RetVarName, Name]),
    [RetVarName];

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

process_post_call_instruction({return_list_terms_noalloc, VarName, N, TermFunction}) ->
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
process_post_call_instruction({binary_to_gl_string_char, MsgVarName}) ->
    write(1, "enif_free(~s);\n", [MsgVarName]),
    [].

process_out_params(Instructions) ->
    lists:foldl(fun(Instruction, Acc) ->
        Acc ++ process_post_call_instruction(Instruction)
    end, [], Instructions).

write_nif_function_body(NifFunctionName, NifFunctionData) ->
    % First, we process the parameters. As we process them, we generate the
    % pre-call code and gather infos to re-use later (the var name, how it's
    % used in the actual OpenGL call). Note that one param can generate more
    % than one variable (for instance, a BEAM binary can produce a C array and
    % its length). It also also generates instructions to produce the post-call
    % code.

    % io:format(user, "debug: ~p~n", [maps:get(params, NifFunctionData)]),

    % XXX: Del VarNames?
    {_, _VarNames, PassArgs, Instructions} = lists:foldl(
        fun process_nif_param_fold/2,
        {0, [], [], []},
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
        gluint_to_integer ->
            write(1, "GLuint ret = ");
        {glenum_to_atom, _Group} ->
            write(1, "GLenum ret = ");
        const_glubyte_to_string ->
            write(1, "const GLubyte* ret = ")
    end,
    write(0, "~s(", [NifFunctionName]),
    write(0, string:join(PassArgs, ", ")),
    write(0, ");\n\n"),

    % We write the post-call code. We start with processing the return value
    % (if any), then the "out" parameters.
    RetArgs0 = process_return_value_if_any(maps:get(return, NifFunctionData)),
    RetArgs1 = process_out_params(Instructions),

    % And last, we write the return statement. By now we have all the return
    % "ERL_NIF_TERM" values (if any) prepared. If there is none, we simply
    % return the "ok" atom, otherwise we return a "ok" tuple.
    RetArgs = RetArgs0 ++ RetArgs1,
    case RetArgs of
        [] ->
            write(1, "return enif_make_atom(env, \"ok\");\n");
        _ ->
            TupleArity = length(RetArgs) + 1,
            write(1, "return enif_make_tuple~p(env,\n", [TupleArity]),
            write(2, "enif_make_atom(env, \"ok\"),\n        "),
            write(0, string:join(RetArgs, ",\n        ")),
            write(0, "\n    );\n")
    end,

    ok.

write_nif_array(Functions) ->
    Data = maps:fold(fun(_FunctionName, FunctionData, Acc1) ->
        maps:fold(fun(NifFunctionName, NifFunctionData, Acc2) ->
            Name = io_lib:format("~s_raw", [NifFunctionName]),
            Arity = maps:get(arity, NifFunctionData),
            Function = io_lib:format("nif_~s", [NifFunctionName]),
            [{Name, Arity, Function}|Acc2]
        end, Acc1, maps:get(nif_functions, FunctionData))
    end, [], Functions),
    Parts = lists:foldl(fun({Name, Arity, Function}, Acc) ->
        Strings = io_lib:format(
            "    {\"~s\", ~p, ~s}",
            [Name, Arity, Function]
        ),
        [lists:flatten(Strings)|Acc]
    end, [], Data),
    write(0, "static ErlNifFunc nif_functions[] = {\n"),
    write(0, string:join(Parts, ",\n")),
    write(0, "\n"),
    write(0, "};\n"),

    ok.
