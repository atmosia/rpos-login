-module(session_id_handler).

-export([init/2, allowed_methods/2, resource_exists/2]).
-export([content_types_provided/2]).
-export([to_json/2, delete_resource/2]).

init(Req, _State) -> {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"DELETE">>, <<"OPTIONS">>],
     Req, State}.

resource_exists(Req, State) ->
    Session = cowboy_req:binding(session_id, Req),
    case rpos_login:fetch_session(Session) of
        {ok, Details} -> {true, Req, State#{session => Details}};
        {error, no_session} -> {false, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    % TODO: generate new key and redirect on expiry
    #{session := Session} = State,
    {jiffy:encode(Session), Req, State}.

delete_resource(Req, State) ->
    Session = cowboy_req:binding(session_id, Req),
    ok = rpos_login:logout(Session),
    {true, Req, State}.
