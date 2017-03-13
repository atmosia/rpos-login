-module(status_handler).

-export([init/2, content_types_provided/2, to_json/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    Provided = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {Provided, Req, State}.

to_json(Req, State) ->
    {_Name, _Desc, Version} = lists:keyfind(rpos_login, 1,
                                            application:which_applications()),
    JSON = jiffy:encode(#{version => binary:list_to_bin(Version),
                          health  => <<"OK">>}),
    {JSON, Req, State}.
