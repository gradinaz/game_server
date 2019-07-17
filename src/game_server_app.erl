%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

-module(game_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%%====================================================================
%% API
%%====================================================================
-spec start(any(), any()) -> {ok, pid()}.
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", game_server_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    game_server_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================
