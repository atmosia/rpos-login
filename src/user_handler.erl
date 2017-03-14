-module(user_handler).

-export([init/2, is_authorized/2, allowed_methods/2, is_conflict/2]).
-export([content_types_accepted/2, from_json/2]).

init(Req, _State) -> {cowboy_rest, Req, #{}}.

is_authorized(Req, State) ->
    %% TODO: check auth
    {true, Req, State#{author => "mikeyhc"}}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"OPTIONS">>], Req, State}.

is_conflict(Req, State) ->
    Email = cowboy_req:binding(email, Req),
    case rpos_login:register_user(Email) of
        ok                   -> {false, Req, State};
        {error, user_exists} -> {true, Req, State}
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req, State) -> {true, Req, State}.

