-module(rpos_login_server).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([register_user/2, login/3, logout/2]).
-export([set_password/4, fetch_session/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(TIMEFMT, "~4w~2..0w~2..0wT~2..0w:~2..0w:~9..0f").

-record(state, {connection}).

start_link() -> gen_server:start_link(?MODULE, [], []).

start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

register_user(Pid, Email) ->
    gen_server:call(Pid, {register, Email}).

login(Pid, User, Password) ->
    gen_server:call(Pid, {login, User, Password}).

logout(Pid, Token) ->
    gen_server:call(Pid, {logout, Token}).

set_password(Pid, User, Password, Token) ->
    gen_server:call(Pid, {set_password, User, Password, Token}).

fetch_session(Pid, Token) ->
    gen_server:call(Pid, {fetch, Token}).

init([Config]) ->
    gen_server:cast(self(), {connect, Config}),
    {ok, #state{}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extras) -> {ok, State}.

handle_call({register, Email}, _From, State) ->
    {reply, create_user(State#state.connection, Email), State};
handle_call({login, Email, Pass}, _From, State) ->
    {reply, login_user(State#state.connection, Email, Pass), State};
handle_call({logout, Token}, _From, State) ->
    {reply, logout_user(State#state.connection, Token), State};
handle_call({set_password, User, Password, Token}, _From, State) ->
    {reply,
     set_user_password(State#state.connection, User, Password, Token),
     State};
handle_call({fetch, Session}, _From, State) ->
    {reply, fetch_user_session(State#state.connection, Session), State}.

handle_cast({connect, Config}, State) ->
    #{db := DBConfig} = Config,
    {ok, Connection} = epgsql:connect(DBConfig),
    {noreply, State#state{connection=Connection}}.

handle_info(_Message, State) -> {noreply, State}.

create_user(Conn, Email) ->
    Token = generate_token(Email),
    Result = epgsql:equery(Conn, "INSERT INTO rpos_user(email, reset_key)
                                  VALUES($1, $2)",
                           [Email, Token]),
    case Result of
        {ok, _Affected} ->
            % TODO: send email
            ok;
        {error, _Error} ->
            {error, user_exists}
    end.

login_user(Conn, Email, Password) ->
    case valid_login(Conn, Email, Password) of
        true -> add_session(Conn, Email);
        false -> {error, invalid_login}
    end.

logout_user(Conn, Session) ->
    Query = "DELETE FROM rpos_session WHERE session_key=$1",
    {ok, _N} = epgsql:equery(Conn, Query, [Session]),
    ok.

generate_token(Seed) ->
    IOList = io_lib:format("~s~w~n", [Seed, erlang:monotonic_time()]),
    Sha256 = crypto:hash(sha256, IOList),
    Base64 = base64:encode(Sha256),
    binary:replace(Base64, <<"/">>, <<"_">>).

valid_login(Conn, User, Password) ->
    Query = "SELECT password FROM rpos_user WHERE email = $1",
    case epgsql:equery(Conn, Query, [User]) of
        {ok, _Col, [{Pass}]} -> erlpass:match(Password, Pass);
        {ok, _Col, []}       -> false
    end.

add_session(Conn, Email) ->
    Token = generate_token(Email),
    case add_session(Conn, Token, Email) of
        {error, session_exists} -> add_session(Conn, Email);
        Ret  -> Ret
    end.

add_session(Conn, Session, Email) ->
    Query = "INSERT INTO rpos_session(session_key, email)
             VALUES ($1, $2)",
    case epgsql:equery(Conn, Query, [Session, Email]) of
        {ok, _Count} -> {ok, Session};
        {error, {error, error, _Val, unique_violation, _Msg}, _L} ->
            {error, session_exists}
    end.

set_user_password(Conn, User, Password, Token) ->
    Query = "UPDATE rpos_user SET email=$1, password=$2, reset_key=NULL
             WHERE reset_key = $3",
    Hash = erlpass:hash(Password),
    case epgsql:equery(Conn, Query, [User, Hash, Token]) of
        {ok, 1} -> true;
        {ok, 0} -> false
    end.

fetch_user_session(Conn, Session) ->
    Query = "SELECT email, expires FROM rpos_session WHERE session_key=$1",
    case epgsql:equery(Conn, Query, [Session]) of
        {ok, _Cols, [{Email, {{Yr, Mn, Da}, {Hr, Mi, Se}}}]} ->
            IOList = io_lib:format(?TIMEFMT, [Yr, Mn, Da, Hr, Mi, Se]),
            ExpiryStr = lists:flatten(IOList),
            {ok, #{session => Session,
                   email => Email,
                   expires => binary:list_to_bin(ExpiryStr)}};
        _ -> {error, no_session}
    end.
