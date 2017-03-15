%%%-------------------------------------------------------------------
%% @doc rpos_login public API
%% @end
%%%-------------------------------------------------------------------

-module(rpos_login).

-behaviour(application).

%% Public API
-export([register_user/1, login/2, login/3, logout/1, valid_token/1]).
-export([set_password/3, fetch_session/1]).

%% Application callbacks
-export([start/2, stop/1]).

-define(LISTENER, rpos_login_listener).

%%====================================================================
%% Public API
%%====================================================================

register_user(Email) ->
    rpos_login_server:register_user(server_pid(), Email).

login(Email, Password) ->
    rpos_login_server:login(server_pid(), Email, Password).

login(Session, Email, Password) ->
    rpos_login_server:login(server_pid(), Session, Email, Password).

logout(Session) ->
    rpos_login_server:logout(server_pid(), Session).

valid_token(Session) ->
    rpos_login_server:valid_token(server_pid(), Session).

set_password(User, Password, Token) ->
    rpos_login_server:set_password(server_pid(), User, Password, Token).

fetch_session(Session) ->
    rpos_login_server:fetch_session(server_pid(), Session).

%%====================================================================
%% application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", status_handler, []},
               {"/user/:email", user_handler, []},
               {"/session", session_handler, []},
               {"/session/:session_id", session_id_handler, []}]}
    ]),
    Port = application:get_env(rpos_login, port, 8080),
    {ok, _} = cowboy:start_clear(?LISTENER, 100,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    rpos_login_sup:start_link().

stop(_State) -> cowboy:stop_listener(?LISTENER).

%%====================================================================
%% Internal functions
%%====================================================================

server_pid() ->
    [{_, Pid, _, _}] = supervisor:which_children(rpos_login_sup),
    Pid.
