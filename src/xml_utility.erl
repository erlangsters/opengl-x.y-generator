-module(xml_utility).
-export([xml_attributes/1]).

-include_lib("xmerl/include/xmerl.hrl").

-spec xml_attributes(xmerl:element()) -> map().
xml_attributes(Element) ->
    #xmlElement{attributes = Attributes} = Element,
    maps:from_list(lists:map(fun(#xmlAttribute{name = Name, value = Value}) ->
        {Name, Value}
    end, Attributes)).
