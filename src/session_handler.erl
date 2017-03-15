-module(session_handler).

-export([init/2, is_authorized/2, allowed_methods/2]).
-export([content_types_accepted/2, from_json/2]).

init(Req, _State) -> {cowboy_rest, Req, #{}}.

is_authorized(Req0, State) ->
    {Body, Req1} = read_all_body(Req0),
    JSON = jiffy:decode(Body, [return_maps]),
    case rpos_login:login(maps:get(<<"email">>, JSON),
                          maps:get(<<"password">>, JSON)) of
        {ok, Session} -> {true, Req1, State#{session => Session}};
        {error, invalid_login} -> {false, Req1, State}
    end.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req, State) ->
    {{true, "/session/" ++ maps:get(session, State)}, Req, State}.

read_all_body(Req) -> read_all_body(cowboy_req:read_body(Req), <<>>).

read_all_body({ok, Data, Req}, Acc) -> {<<Acc/binary, Data/binary>>, Req};
read_all_body({more, Data, Req}, Acc) ->
    read_all_body(cowboy_req:read_body(Req), <<Acc/binary, Data/binary>>).
