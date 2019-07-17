%%%-------------------------------------------------------------------
%% @doc
%% @end
%%%-------------------------------------------------------------------

-module(game_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_redis/4]).

%% Supervisor callbacks
-export([init/1]).

-include("game_server_int.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
-spec init(Args::term()) -> {ok, {SupFlags::supervisor:sup_flags(), [ChildSpec::supervisor:child_spec()]}} | ignore.
init([]) ->
    RedisParams = application:get_env(game_server, redis_client, []),
    RedisHost = proplists:get_value(host, RedisParams, "localhost"),
    RedisPort = proplists:get_value(port, RedisParams, 6379),
    RedisDatabase = proplists:get_value(db, RedisParams, 0),
    Childs = [
        { game_server_redis, {?MODULE, start_redis, [?REDIS_CLIENT, RedisHost, RedisPort, RedisDatabase]}, permanent, 5000, worker, dynamic}
    ],
    {ok, { {one_for_all, 10, 1}, Childs} }.


%%====================================================================
%% Internal functions
%%====================================================================
-spec start_redis(RegName :: atom(), Host :: string(), Port :: integer(), Database :: integer()) -> {ok, pid()}.
start_redis(RegName, Host, Port, Database) ->
    {ok, Pid} = eredis:start_link(Host, Port, Database),
    erlang:register(RegName, Pid),
    {ok, Pid}.

%%====================================================================
%% Internal functions
%%====================================================================
