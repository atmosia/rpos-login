-module(user_handler).

-export([init/2, is_authorized/2, allowed_methods/2, is_conflict/2]).
-export([content_types_accepted/2, from_json/2, resource_exists/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

is_authorized(Req0, State) ->
    case check_auth(Req0, State) of
        {true, User, Req1} ->
            {true, Req1, State#{author => User}};
        {false, _User, Req1} ->
            % TODO: log failed action
            {{false, "rPOS Token"}, Req1, State}
    end.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, State) ->
    Email = cowboy_req:binding(email, Req),
    case rpos_login:register_user(Email) of
        ok                   -> {false, Req, State};
        {error, user_exists} -> {true, Req, State#{conflict => true}}
    end.

is_conflict(Req, State) ->
    {maps:is_key(conflict, State), Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req, State) -> {true, Req, State}.

check_auth(Req0, State) ->
    #{auth := AuthConfig} = State,
    case cowboy_req:header(<<"authorization">>, Req0) of
        undefined        -> {false, none, Req0};
        <<"Token ", Auth/binary>> ->
            {Ret, User} = query_auth(AuthConfig, Auth),
            {Ret, User, Req0};
        _Token           ->
            Body = jiffy:encode(#{error => <<"invalid login method">>}),
            Req1 = cowboy_req:set_resp_body(Body, Req0),
            {false, none, Req1}
    end.

query_auth(#{host := Host, port := Port}, Token) ->
    URL = lists:flatten(
            io_lib:format("~s:~w/session/~s", [Host, Port, Token])),
    {ok, {{_Version, Code, _Reason}, _Headers, Body}} =
        httpc:request(get, {URL, []}, [{autoredirect, true}], [{port, Port}]),
    if  Code =:= 200 ->
           JSON = jiffy:decode(Body, [return_maps]),
           check_permissions(JSON);
        Code =:= 404 -> {false, invalid_session}
    end.

check_permissions(#{<<"username">> := User, <<"permissions">> := Perms}) ->
    Fn = fun(X) -> X =:= [<<"rpos_login">>, <<"create_user">>] end,
    {lists:any(Fn, Perms), User}.
