%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

-module(game_server_app).

-behaviour(application).
-include_lib("kernel/include/logger.hrl").
-include_lib("game_server_int.hrl").

%% Application callbacks
-export([start/2, stop/1]).
%%====================================================================
%% API
%%====================================================================
-spec start(any(), any()) -> {ok, pid()}.
start(_Type, _Args) ->
    io:format("asdasdasdasd"),
%%    start_pools(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", game_server_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    game_server_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%-spec start_pools() -> {ok, pid()} | {error, term()} | ok.
%%start_pools() ->
%%    case application:get_env(episcina, pools) of
%%        {ok, Pools} ->
%%            episcina:start_pools(Pools);
%%        _ ->
%%            ok
%%    end.



%%====================================================================
%% Internal functions
%%====================================================================
