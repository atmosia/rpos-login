-module(session_handler).

-export([init/2, is_authorized/2, allowed_methods/2]).
-export([content_types_accepted/2, from_json/2, resource_exists/2]).
-export([options/2]).

init(Req, _State) -> {cowboy_rest, Req, #{}}.

resource_exists(Req, State) -> {false, Req, State}.

is_authorized(Req0, State) ->
    Headers = [{<<"Access-Control-Allow-Origin">>, <<"*">>},
               {<<"Access-Control-Allow-Methods">>, <<"POST, OPTIONS">>},
               {<<"Access-Control-Max-Age">>, <<"1000">>},
               {<<"Access-Control-Allow-Headers">>,
                <<"origin, x-csrftoken, content-type, accept">>},
               {<<"Access-Control-Expose-Headers">>, <<"Location">>}],
    Fn = fun({H, V}, R) ->
        cowboy_req:set_resp_header(H, V, R)
    end,
    Req1 = lists:foldl(Fn, Req0, Headers),
    case cowboy_req:method(Req1) of
        <<"OPTIONS">> -> {true, Req1, State};
        <<"POST">> ->
            {Body, Req2} = read_all_body(Req1),
            JSON = jiffy:decode(Body, [return_maps]),
            case rpos_login:login(maps:get(<<"email">>, JSON),
                                  maps:get(<<"password">>, JSON)) of
                {ok, Session} -> {true, Req2, State#{session => Session}};
                {error, invalid_login} ->
                    Req3 = cowboy_req:set_resp_header(<<"Content-Type">>,
                                                      <<"application/json">>,
                                                      Req2),
                    {{false, "body"}, Req3, State}
            end
    end.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req, State) ->
    {{true, "/session/" ++ maps:get(session, State)}, Req, State}.

options(Req0, State) ->
    Req1 = cowboy_req:set_resp_body(<<"[]">>, Req0),
    {ok, Req1, State}.

read_all_body(Req) -> read_all_body(cowboy_req:read_body(Req), <<>>).

read_all_body({ok, Data, Req}, Acc) -> {<<Acc/binary, Data/binary>>, Req};
read_all_body({more, Data, Req}, Acc) ->
    read_all_body(cowboy_req:read_body(Req), <<Acc/binary, Data/binary>>).
