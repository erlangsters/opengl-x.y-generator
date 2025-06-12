%%
%% This file is part of the OpenGL binding generator for the BEAM.
%%
%% The functions in this module are about extracting data from the OpenGL
%% specs XML file and re-organize them in a way that is easier to work with. In
%% other words, they're just helper functions.
%%
%% There is also the `determine_api_items/2` function that is used to determine
%% the API items (enums + functions) to be included in the generated binding
%% for a given OpenGL version.
%%
-module(gl_specs).
-export([
    read_versions/2,
    read_enums/1,
    read_commands/1,
    enums_values_map/1, enums_group_names_map/1, enum_no_group_list/1,
    determine_api_items/2
]).

%% Example: #{"GL_INT_SAMPLER_2D_MULTISAMPLE" =>
%%             {"0x9109", ["AttributeType", "UniformType"}}.
-type gl_enums() :: #{
    Name :: string() => {Value :: string(), Groups :: [string()]}
}.

%% Example: #{"glAccum" => <command>...</command>}.
-type gl_commands() :: #{
    Name :: string() => xmerl:element()
}.

%% Analyze the OpenGL specs and return the list of versions for a given API.
%%
%% The API argument is either "gl" or "gles2".
%%
-spec read_versions(xmerl:element(), string()) -> [string()].
read_versions(Specs, Api) ->
    lists:filtermap(fun(Feature) ->
        Attributes = xml_utility:xml_attributes(Feature),
        case maps:get(api, Attributes) of
            Api ->
                Version = maps:get(number, Attributes),
                {true, Version};
            _ ->
                false
        end
    end, xmerl_xpath:string("//feature", Specs)).

%% Return the OpenGL enums defined in the OpenGL specs.
%%
%% It reads all defined enums in the OpenGL specs, and return them in a data
%% structure that is easy to work with. For each enum (a name) is associated
%% its value and a list of groups it belongs to.
%%
%% Note that it includes vendor extension enums as well, but that does not
%% mean we use them.
%%
%% How it looks like in the XML file.
%%
%% ```
%% ...
%%     <enum value="0x884E" name="GL_COMPARE_REF_TO_TEXTURE_EXT"/>
%%     <enum value="0x884F" name="GL_TEXTURE_CUBE_MAP_SEAMLESS" group="EnableCap"/>
%% </enums>
%%
%% <enums namespace="GL" start="0x8850" end="0x891F" vendor="NV">
%%     <enum value="0x8850" name="GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV"/>
%%     <enum value="0x8851" name="GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV"/>
%% ...
%% ```
%%
-spec read_enums(xmerl:element()) -> gl_enums().
read_enums(Specs) ->
    lists:foldl(fun(EnumsElement, Acc1) ->
        lists:foldl(fun(EnumElement, Acc2) ->
            Attributes = xml_utility:xml_attributes(EnumElement),
            #{
                name := Name,
                value := Value
            } = Attributes,
            case maps:is_key(Name, Acc2) of
                true ->
                    % Known enum duplicate is 'GL_ACTIVE_PROGRAM_EXT'.
                    io:format(
                        user,
                        "Declaration of enum '~s' in the OpenGL specs is duplicated (ignoring it)...~n",
                        [Name]
                    ),
                    Acc2;
                false ->
                    Group = case maps:get(group, Attributes, undefined) of
                        undefined ->
                            [];
                        Value_ ->
                            string:split(Value_, ",", all)
                    end,
                    maps:put(Name, {Value, Group}, Acc2)
            end
        end, Acc1, xmerl_xpath:string(".//enum", EnumsElement))
    end, #{}, xmerl_xpath:string("//enums", Specs)).

%% Return the OpenGL "commands" defined in the OpenGL specs.
%%
%% It reads all defined commands (another name for "functions") in the OpenGL
%% specs, and return them in a data structure that is easy to work with.
%%
%% Note that it includes vendor extension commands as well, but that does not
%% mean we use them.
%%
%% How it looks like in the XML file. (There is a single 'commands' element
%% that defines all the commands.)
%%
%% ```
%% <commands namespace="GL">
%%     <command>
%%         <proto>void <name>glAccum</name></proto>
%%         <param group="AccumOp"><ptype>GLenum</ptype> <name>op</name></param>
%%         <param kind="Coord"><ptype>GLfloat</ptype> <name>value</name></param>
%%         <glx type="render" opcode="137"/>
%%     </command>
%%     <command>
%%         <proto>void <name>glAccumxOES</name></proto>
%%         ...
%% </commands>
%% ```
%%
-spec read_commands(xmerl:element()) -> gl_commands().
read_commands(Specs) ->
    [CommandsElement] = xmerl_xpath:string("//commands", Specs),

    lists:foldl(fun(CommandElement, Acc) ->
        [NameElement] = xmerl_xpath:string(".//proto/name/text()", CommandElement),
        Name = xmerl_scan:value_of(NameElement),
        case maps:is_key(Name, Acc) of
            true ->
                io:format(
                    user,
                    "Declaration of command '~s' in the OpenGL specs is duplicated (ignoring it)...~n",
                    [Name]
                ),
                Acc;
            false ->
                maps:put(Name, CommandElement, Acc)
        end
    end, #{}, xmerl_xpath:string(".//command", CommandsElement)).

%% Maps enum name to its value (e.g. GL_TEXTURE_2D -> 0x0DE1)
-spec enums_values_map(gl_enums()) -> #{string() => string()}.
enums_values_map(GlEnums) ->
    maps:map(fun(_Name, {Value, _Groups}) -> Value end, GlEnums).

-spec enums_group_names_map(gl_enums()) -> ok.
enums_group_names_map(_Enums) ->
    maps:fold(fun(Name, {_Value, Groups}, Map) ->
        case Groups of
            [] ->
                Map;
            _ ->
                lists:foldl(fun(Group, Acc) ->
                    case maps:get(Group, Acc, undefined) of
                        undefined ->
                            maps:put(Group, [Name], Acc);
                        Names ->
                            maps:put(Group, [Name | Names], Acc)
                    end
                end, Map, Groups)
        end
    end, #{}, _Enums).

-spec enum_no_group_list(gl_enums()) -> ok.
enum_no_group_list(_Enums) ->
    % XXX
    ok.

add_api_items(RequireElements, {Functions0, Enums0}) ->
    lists:foldl(fun(RequireElement, {Functions1, Enums1}) ->
        % XXX: Process "types" ?
        _TypesElements = xmerl_xpath:string(".//type", RequireElement),
        % Process enums.
        Enums2 = lists:foldl(fun(EnumElement, EnumNames) ->
            EnumName = maps:get(name, xml_utility:xml_attributes(EnumElement)),
            [EnumName | EnumNames]
        end, Enums1, xmerl_xpath:string(".//enum", RequireElement)),
        % Process functions.
        Functions2 = lists:foldl(fun(FunctionElement, FunctionNames) ->
            FunctionName = maps:get(name, xml_utility:xml_attributes(FunctionElement)),
            [FunctionName | FunctionNames]
        end, Functions1, xmerl_xpath:string(".//command", RequireElement)),

        {Functions2, Enums2}
    end, {Functions0, Enums0}, RequireElements).

remove_api_items(RemoveElements, {Functions0, Enums0}) ->
    lists:foldl(fun(RemoveElement, {Functions1, Enums1}) ->
        % XXX: Process "types" ?
        _TypesElements = xmerl_xpath:string(".//type", RemoveElement),
        % Process enums.
        Enums2 = lists:foldl(fun(EnumElement, EnumNames) ->
            EnumName = maps:get(name, xml_utility:xml_attributes(EnumElement)),
            lists:delete(EnumName, EnumNames)
        end, Enums1, xmerl_xpath:string(".//enum", RemoveElement)),
        % Process functions.
        Functions2 = lists:foldl(fun(FunctionElement, FunctionNames) ->
            FunctionName = maps:get(name, xml_utility:xml_attributes(FunctionElement)),
            lists:delete(FunctionName, FunctionNames)
        end, Functions1, xmerl_xpath:string(".//command", RemoveElement)),

        {Functions2, Enums2}
    end, {Functions0, Enums0}, RemoveElements).

target_api_string_test({gl, _}) -> "gl";
target_api_string_test({gles, _}) -> "gles2".

-spec determine_api_items(term(), xmerl:element()) -> [{string(), string()}].
determine_api_items(Target, Specs) ->
    % The OpenGL specs specifies all the enums and functions that are added to
    % the API, version after version. Starting from OpenGL 3.2, it also
    % specifies all the enums and functions that are removed. Determining the
    % API items (enums + functions) to be included in the generated binding
    % consists of iterating over each version, one by one, and build the list
    % by adding/removing enums/functions.
    %
    % Here is what it's looking like in the XML specs:
    %
    % ```
    % <feature api="gl" name="GL_VERSION_3_2" number="3.2">
    %     <require comment="Reuse ARB_depth_clamp">
    %         <enum name="GL_DEPTH_CLAMP"/>
    %         ...
    %     </require>
    %     <require comment="Reuse ARB_draw_elements_base_vertex">
    %         <command name="glDrawElementsBaseVertex"/>
    %         ...
    %     </require>
    %     <remove profile="core" comment="Compatibility-only GL 1.0 features removed from GL 3.2">
    %         <enum name="GL_CURRENT_BIT"/>
    %         <command name="glNewList"/>
    %         ...
    %     </remove>
    % </feature>
    % ```
    %
    % For each OpenGL version (a `feature` element), added enums and commands
    % (read "functions") are specified (in `require` elements) followed by the
    % optional removed enums and commands are specified (in `remove` elements).
    TargetApi = target_api_string_test(Target),
    TargetNumber = opengl_gen:target_version_string(Target),
    {Functions, Enums, _} = lists:foldl(fun
        (_FeatureElement, {_Functions, _Enums, true} = Accumulator) ->
            % We have reached the target OpenGL version, we skip the rest.
            Accumulator;
        (FeatureElement, {Functions0, Enums0, false}) ->
            Attributes = xml_utility:xml_attributes(FeatureElement),
            {Functions2, Enums2} = case maps:get(api, Attributes) of
                TargetApi ->
                    % We add the API items this version of OpenGL has added.
                    % (Apparently, we must also filter out elements that has
                    % "profile="compatibility" attribute.)
                    RequireElements = lists:filter(fun(RequireElement) ->
                        case maps:get(profile, xml_utility:xml_attributes(RequireElement), undefined) of
                            "compatibility" ->
                                false;
                            _ ->
                                true
                        end
                    end, xmerl_xpath:string(".//require", FeatureElement)),
                    {Functions1, Enums1} = add_api_items(
                        RequireElements,
                        {Functions0, Enums0}
                    ),
                    % We remove the API items this version of OpenGL has
                    % removed.
                    remove_api_items(
                        xmerl_xpath:string(".//remove", FeatureElement),
                        {Functions1, Enums1}
                    );
                _ ->
                    % Ignore if this is not the target OpenGL API.
                    {Functions0, Enums0}
            end,

            % Check if we are at the target OpenGL version.
            Skip = case Attributes of
                #{api := TargetApi, number := TargetNumber} ->
                    true;
                _ ->
                    false
            end,

            {Functions2, Enums2, Skip}
    end, {[], [], false}, xmerl_xpath:string("//feature", Specs)),
    {lists:reverse(Enums), lists:reverse(Functions)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_enums_test() ->
    {GlSpecs, _} = xmerl_scan:file("gl-specs.xml"),
    GlEnums = read_enums(GlSpecs),
    {"0x8CE3", ["InvalidateFramebufferAttachment"]} =
        maps:get("GL_COLOR_ATTACHMENT3_EXT", GlEnums),
    {"0x0D95", ["MapTarget", "EnableCap","GetPName"]} =
        maps:get("GL_MAP1_TEXTURE_COORD_3", GlEnums),
    {"0x8195", ["MapTarget", "FfdTargetSGIX"]} =
        maps:get("GL_TEXTURE_DEFORMATION_SGIX", GlEnums),
    {"0x9004", []} =
        maps:get("GL_TESSELLATION_MODE_AMD", GlEnums),

    GlEnumsValuesMap = enums_values_map(GlEnums),
    "0x8CE3" = maps:get("GL_COLOR_ATTACHMENT3_EXT", GlEnumsValuesMap),
    "0x0D95" = maps:get("GL_MAP1_TEXTURE_COORD_3", GlEnumsValuesMap),
    "0x8195" = maps:get("GL_TEXTURE_DEFORMATION_SGIX", GlEnumsValuesMap),
    "0x9004" = maps:get("GL_TESSELLATION_MODE_AMD", GlEnumsValuesMap),

    EnumsGroupNamesMap = enums_group_names_map(GlEnums),
    ["GL_INT", "GL_FLOAT", "GL_DOUBLE", "GL_SHORT"] =
        maps:get("TexCoordPointerType", EnumsGroupNamesMap),
    ["GL_REPLACE_MIDDLE_SUN","GL_REPLACE_OLDEST_SUN","GL_RESTART_SUN"] =
        maps:get("TriangleListSUN", EnumsGroupNamesMap),

    % XXX
    % EnumNoGroupList = enum_no_group_list(GlEnums),

    % io:format(user, "~p~n", [GlEnums]),
    % io:format(user, "~p~n", [GlEnumsValuesMap]),
    % io:format(user, "~p~n", [EnumsGroupNamesMap]),
    % io:format(user, "~p~n", [EnumNoGroupList]),

    ok.

-endif.
