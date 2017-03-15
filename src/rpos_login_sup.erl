%%%-------------------------------------------------------------------
%% @doc rpos_login top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rpos_login_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Config]) ->
    Flags = #{strategy  => one_for_all,
              intensity => 3,
              period    => 60},
    Children = [#{id       => rpos_login_server,
                  start    => {rpos_login_server, start_link, [Config]},
                  restart  => transient,
                  shutdown => 5,
                  type     => worker}
               ],
    {ok, {Flags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
