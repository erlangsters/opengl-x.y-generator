%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% It does this and that...
%%
-module(gl_header_generator).
-export([generate/1]).
-import(opengl_gen, [open/1, write/2, write/3, close/0]).

generate(BindingData) ->
    % open("/home/intjelic/Workspace/erlangsters/opengl-4.6/include/gl.hrl"),
    open("gl.hrl"),

    write(0, "%% This file is generated. Do not edit!\n"),
    write(0, "\n"),
    maps:foreach(fun(Name, Value) ->
        write(0, "-define(~s, ~s).\n", [Name, Value])
    end, maps:get(constants, BindingData)),
    write(0, "\n"),

    % % GL enums map.
    % write(0, "-define(GL_ENUMS_MAP, #{\n", []),
    % lists:foreach(fun(Yo) ->
    %     write(0, "  ~s => ~s,\n", [Foo, Bar])
    % write(0, "}.\n", []),

    % % GL enums map.
    % write(0, "-define(GL_INTEGERS_MAP, #{\n", []),
    % lists:foreach(fun(Yo) ->
    %     write(0, "  ~s => ~s,\n", [Foo, Bar])
    % write(0, "}.\n", []),

    close(),

    ok.
