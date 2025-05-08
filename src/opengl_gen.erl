%%
%% OpenGL binding generator for the BEAM.
%%
%% It reads the OpenGL specs (gl-specs.xml) and the binding specs
%% (binding-specs.conf) and generates an OpenGL binding for the targeted API
%% and version.
%%
%% The API can be either OpenGL or OpenGL ES. However, it only supports OpenGL
%% 3.3, 4.1 and 4.6, and OpenGL ES 2.0, 3.0, 3.1 and 3.2. The generated files
%% will inconditionally be `gl.erl`, `gl.c` and `gl.hrl` and be placed in the
%% current working directly.
%%
%% How to use:
%%   ./opengl_gen <api> <version>
%%
%% The binding generation happens in essentially 3 steps:
%%
%% 1. Given the API and version to generate the binding for ("OpenGL ES 3.2"
%%    for example), it determines the list of OpenGL enums and functions to be
%%    included in the binding.
%%
%% 2. Then, it reads the binding specs file in order to "resolve" (understand
%%    "how to transform the C API") and end up with all the information needed
%%    to start the generation of the binding.
%%
%% 3. Finally, it generates the binding module (gl.erl), an Erlang header
%%    (gl.hrl) and the NIF module (gl.c).
%%
%% Note that the binding specs was written manually, with the help of scripts
%% to generate boilerplate code.
%%
-module(opengl_gen).
-export([main/1]).

-export_type([target/0]).
-export([target_api_string/1]).
-export([target_version_string/1]).

-export([open/1]).
-export([write/2]).
-export([write/3]).
-export([close/0]).

-export([erlangify_enum_name/1]).
-export([erlangify_enum_group_name/1]).

-define(GL_SPECS, "gl-specs.xml").
-define(BINDING_SPECS, "binding-specs.conf").

-type target_api() :: gl | gles.
-type target_version() :: {integer(), integer()}.
-type target() :: {target_api(), target_version()}.

target_api_string({gl, _}) -> "gl";
target_api_string({gles, _}) -> "gles".
target_version_string({_, {Major, Minor}}) ->
    integer_to_list(Major) ++ "." ++ integer_to_list(Minor).

namify_target_api({gl, _} = Target) ->
    "OpenGL " ++ target_version_string(Target);
namify_target_api({gles, _} = Target) ->
    "OpenGL ES " ++ target_version_string(Target).

open(File) ->
    {ok, Fd} = file:open(File, [write]),
    erlang:put('$generated_file', {Fd, File}),
    ok.

write(Indentation, Format) ->
    write(Indentation, Format, []).
write(Indentation, Format, Data) ->
    {Fd, _} = erlang:get('$generated_file'),
    Space = case Indentation of
        0 -> "";
        1 -> "    ";
        2 -> "        ";
        3 -> "            "
    end,
    io:format(Fd, Space ++ Format, Data).

close() ->
    {Fd, _File} = erlang:get('$generated_file'),
    file:close(Fd),
    erlang:erase('$generated_file'),
    ok.

-spec erlangify_enum_name(string()) -> string().
erlangify_enum_name(Name) ->
    % Consider name "GL_TEXTURE_MAX_LEVEL". It should be converted to
    % "texture_max_level" in Erlang (to make an atom out of it).

    % Remove "GL_" prefix if present
    Name1 = case string:prefix(Name, "GL_") of
        nomatch -> Name;
        Rest -> Rest
    end,
    % Convert to lowercase and replace underscores with underscores (keeping them)
    string:lowercase(Name1).

-spec erlangify_enum_group_name(string()) -> string().
erlangify_enum_group_name(Name) ->
    % Consider group name "TextureParameterName". It should be converted to
    % "texture_parameter_name" in Erlang (to make an atom out of it).

    % Insert underscore before uppercase letters a (except first char).
    Name1 = lists:reverse(lists:foldl(fun
        (C, []) -> [C];
        (C, [H|T]) when C >= $0, C =< $9 -> [C, $_ | [H|T]];
        (C, [H|T]) when C >= $A, C =< $Z -> [C, $_ | [H|T]];
        (C, Acc) -> [C | Acc]
    end, [], Name)),

    % Convert to lowercase.
    string:lowercase(Name1).

generate(Target, GlSpecs, BindingSpecs) ->
    % First, we compute all the items that must be included in the binding
    % (OpenGL enums and functions).
    GlItems = gl_specs:determine_api_items(Target, GlSpecs),

    % Next, we compute the "binding data", which is all the information needed
    % to generate the binding.
    BindingData0 = binding_resolver:resolve(Target, GlItems, GlSpecs, BindingSpecs),

    % We also include the name of the API (for instance, "OpenGL 4.6") which is
    % in the generation of the in-source documentation.
    BindingData1 = maps:put(api_name, namify_target_api(Target), BindingData0),

    % Finally, we generate the binding itself, which consists of three files.
    gl_header_generator:generate(BindingData1),
    gl_module_generator:generate(BindingData1),
    gl_nif_module_generator:generate(BindingData1),

    ok.

read_args([Arg1, Arg2]) ->
    try
        case Arg1 of
            "gl" ->
                % Only OpenGL 3.3, 4.1, 4.6 are supported.
                TargetNumber = case Arg2 of
                    "3.3" -> {3, 3};
                    "4.1" -> {4, 1};
                    "4.6" -> {4, 6}
                end,
                {gl, TargetNumber};
            "gles" ->
                % All OpenGL ES versions (2.0, 3.0, 3.1, 3.2) are supported.
                TargetNumber = case Arg2 of
                    "2.0" -> {2, 0};
                    "3.0" -> {3, 0};
                    "3.1" -> {3, 1};
                    "3.2" -> {3, 2}
                end,
                {gles, TargetNumber}
        end
    catch
        _:_ ->
            throw(badarg)
    end;
read_args(_) ->
    throw(badarg).

main(Args) ->
    try read_args(Args) of
        {TargetApi, _TargetVersion} = Target ->
            io:format("About to generate "),
            case TargetApi of
                gl ->
                    io:format("OpenGL binding for ");
                gles ->
                    io:format("OpenGL ES binding for ")
            end,
            io:format("version ~s...~n", [target_version_string(Target)]),

            io:format("Reading OpenGL specs (~s)~n", [?GL_SPECS]),
            {GlSpecs, _} = xmerl_scan:file(?GL_SPECS),

            io:format("Reading binding specs (~s)~n", [?BINDING_SPECS]),
            {ok, [BindingSpecs]} = file:consult(?BINDING_SPECS),

            % Just to make sure we're dealing with the right OpenGL specs, we
            % are matching against the known versions this binding generator
            % was written for.
            [
                "1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "2.0", "2.1", "3.0",
                "3.1", "3.2", "3.3", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",
                "4.6"
            ] = gl_specs:read_versions(GlSpecs, "gl"),
            [
                "2.0", "3.0", "3.1", "3.2"
            ] = gl_specs:read_versions(GlSpecs, "gles2"),

            % Generate the OpenGL binding.
            generate(Target, GlSpecs, BindingSpecs),

            ok
    catch
        throw:badarg ->
            io:format("Usage: opengl_gen <api> <version> (only OpenGL 3.3, 4.1, 4.6 and all OpenGL ES versions are supported)~n"),
            erlang:halt(1)
    end,
    erlang:halt(0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

erlangify_enum_name_test() ->
    "texture_max_level" = erlangify_enum_name("GL_TEXTURE_MAX_LEVEL"),
    "src_alpha_saturate" = erlangify_enum_name("GL_SRC_ALPHA_SATURATE"),
    "unsigned_short_4_4_4_4" = erlangify_enum_name("GL_UNSIGNED_SHORT_4_4_4_4"),
    "none" = erlangify_enum_name("GL_NONE"),
    "float_32_unsigned_int_24_8_rev" = erlangify_enum_name("GL_FLOAT_32_UNSIGNED_INT_24_8_REV"),
    "compressed_srgb8_alpha8_astc_12x12" = erlangify_enum_name("GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12"),
    "depth_clamp_near_amd" = erlangify_enum_name("GL_DEPTH_CLAMP_NEAR_AMD"),

    ok.

erlangify_enum_group_name_test() ->
    % XXX: Fixes needed (not urgent because group name like
    %      'ConvolutionTargetEXT' are not used by the binding specs (for now..)
    "texture_parameter_name" = erlangify_enum_group_name("TextureParameterName"),
    % "convolution_target_ext" = erlangify_enum_group_name("ConvolutionTargetEXT"),
    "copy_image_sub_data_target" = erlangify_enum_group_name("CopyImageSubDataTarget"),
    % "fragment_light_model_parameter_sgix" = erlangify_enum_group_name("FragmentLightModelParameterSGIX"),
    "color_table_parameter_p_name" = erlangify_enum_group_name("ColorTableParameterPName"),

    ok.

-endif.
