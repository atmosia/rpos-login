%%%-------------------------------------------------------------------
%% @doc rpos_login public API
%% @end
%%%-------------------------------------------------------------------

-module(rpos_login).

-behaviour(application).

%% Public API
-export([register_user/1, login/2, logout/1]).
-export([set_password/3, fetch_session/1]).

%% Application callbacks
-export([start/2, stop/1]).

-define(LISTENER, rpos_login_listener).
-define(CONFIG_PATH, "config.json").

%%====================================================================
%% Public API
%%====================================================================

register_user(Email) ->
    rpos_login_server:register_user(server_pid(), Email).

login(Email, Password) ->
    rpos_login_server:login(server_pid(), Email, Password).

logout(Session) ->
    rpos_login_server:logout(server_pid(), Session).

set_password(User, Password, Token) ->
    rpos_login_server:set_password(server_pid(), User, Password, Token).

fetch_session(Session) ->
    rpos_login_server:fetch_session(server_pid(), Session).

%%====================================================================
%% application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    Config = read_config(?CONFIG_PATH),
    #{auth := AuthConfig} = Config,
    Dispatch = cowboy_router:compile([
        {'_', [{"/", status_handler, []},
               {"/user/:email", user_handler, #{auth => AuthConfig}},
               {"/session", session_handler, []},
               {"/session/:session_id", session_id_handler, []}]}
    ]),
    Port = application:get_env(rpos_login, port, 8080),
    {ok, _} = cowboy:start_clear(?LISTENER, 100,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    rpos_login_sup:start_link(Config).

stop(_State) -> cowboy:stop_listener(?LISTENER).

%%====================================================================
%% Internal functions
%%====================================================================

server_pid() ->
    [{_, Pid, _, _}] = supervisor:which_children(rpos_login_sup),
    Pid.


read_config(Path) ->
    case file:read_file(Path) of
        {ok, Data}      ->
            {Config} = jiffy:decode(Data, []),
            {DBConfig} = proplists:get_value(<<"database">>, Config, []),
            {AuthConfig} = proplists:get_value(<<"auth">>, Config, []),
            #{db => lists:foldl(fun parse_db_config/2, [], DBConfig),
              auth => lists:foldl(fun parse_auth_config/2, #{}, AuthConfig)};
        {error, enoent} -> throw({error, no_config_file})
    end.

parse_db_config({<<"host">>, Host}, Acc) ->
    [{host, binary:bin_to_list(Host)}|Acc];
parse_db_config({<<"port">>, Port}, Acc) -> [{port, Port}|Acc];
parse_db_config({<<"user">>, User}, Acc) ->
    [{username, binary:bin_to_list(User)}|Acc];
parse_db_config({<<"pass">>, Pass}, Acc) ->
    [{password, binary:bin_to_list(Pass)}|Acc];
parse_db_config({<<"name">>, Name}, Acc) ->
    [{database, binary:bin_to_list(Name)}|Acc].

parse_auth_config({<<"host">>, Host}, Acc) -> Acc#{host => Host};
parse_auth_config({<<"port">>, Port}, Acc) -> Acc#{port => Port}.
