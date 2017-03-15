-module(session_id_handler).

-export([init/2, is_authorized/2, allowed_methods/2, resource_exists/2]).
-export([content_types_accepted/2, content_types_provided/2]).
-export([to_json/2, from_json/2, delete_resource/2, is_conflict/2]).

init(Req, _State) -> {cowboy_rest, Req, #{}}.

is_authorized(Req, State) ->
    case cowboy_req:method(Req) =:= <<"DELETE">> of
        true ->
            % TODO: if delete check session owner
            {true, Req, State};
        false -> {true, Req, State}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],
     Req, State}.

% TODO: login here
resource_exists(Req, State) ->
    Session = cowboy_req:binding(session_id, Req),
    case rpos_login:fetch_session(Session) of
        {ok, Details} -> {true, Req, State#{session => Details}};
        {error, no_session} ->
            case cowboy_req:method(Req) of
                <<"PUT">> -> new_session(Req, State);
                _Method   -> {false, Req, State}
            end
    end.

is_conflict(Req, State) ->
    {maps:is_key(session, State), Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    % TODO: generate new key and redirect on expiry
    #{session := Session} = State,
    {jiffy:encode(Session), Req, State}.

% TODO: implement
from_json(Req0, State) ->
    case State of
        #{error := invalid_login} ->
            Resp = jiffy:encode(#{error => <<"invalid login">>}),
            Req1 = cowboy_req:set_resp_body(Resp, Req0),
            {false, Req1, State};
        #{new_session := Session} ->
            Resp = jiffy:encode(Session),
            Req1 = cowboy_req:set_resp_body(Resp, Req0),
            {true, Req1, State}
    end.

delete_resource(Req, State) ->
    #{session := Session} = State,
    rpos_login:logout(Session),
    {true, Req, State}.

read_all_body(Req) -> read_all_body(cowboy_req:read_body(Req), <<>>).

read_all_body({ok, Data, Req}, Acc) -> {<<Acc/binary, Data/binary>>, Req};
read_all_body({more, Data, Req}, Acc) ->
    read_all_body(cowboy_req:read_body(Req), <<Acc/binary, Data/binary>>).

new_session(Req0, State) ->
    %% TODO; check for valid user
    Session = cowboy_req:binding(session_id, Req0),
    {Body, Req1} = read_all_body(Req0),
    JSON = jiffy:decode(Body, [return_maps]),
    case rpos_login:login(Session,
                          maps:get(<<"email">>, JSON),
                          maps:get(<<"password">>, JSON)) of
        {ok, _Session} -> {false, Req1, State#{new_session => Session}};
        {error, invalid_login} ->
            {false, Req1, State#{error => invalid_login}};
        {error, session_exists} -> {true, Req1, State}
    end.
