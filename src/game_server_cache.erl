%%% @doc
%%%     The API for work with common cache
%%% @end
%%%-------------------------------------------------------------------
-module(game_server_cache).

%% Library API
-export([
         put_token/2,
         get_token/1,
         del_token/1,
         del_all/0
        ]).

-define(SERVER, ?MODULE).

-include("game_server_int.hrl").
-include_lib("kernel/include/logger.hrl").

-define(KEY_PREFIX, "game_server:").
-define(KEY_ID,     "game_server:token:").
-define(TOKEN_CACHE_TTL, 900). %% 15 min
%%====================================================================
%% API
%%====================================================================

-spec put_token(Token::binary(), Params::map()) -> ok.
put_token(Token, Params = #{}) when is_binary(Token) ->
    Val = term_to_binary(Params),
    {ok, <<"OK">>} = eredis:q(?REDIS_CLIENT, ["SETEX", [?KEY_ID, Token], ?TOKEN_CACHE_TTL, Val]),
    ok.

-spec get_token(binary()) ->  map() | undefined | error.
get_token(Token) when is_binary(Token) ->
    case internal_get([?KEY_ID, Token], ?TOKEN_CACHE_TTL) of
        Bin when is_binary(Bin) -> binary_to_term(Bin);
        error -> error;
        undefined -> undefined
    end.

%%TODO: Clean token
-spec del_token(binary()) -> ok.
del_token(Token) when is_binary(Token) ->
    internal_del([?KEY_ID, Token]),
    ok.

%% Clean up all cache
-spec del_all() -> Num::integer().
del_all() ->
    case eredis:q(?REDIS_CLIENT, ["KEYS", ?KEY_PREFIX ++ "*"]) of
        {ok, []} ->
            0;
        {ok, Keys} ->
            {ok, NumBin} = eredis:q(?REDIS_CLIENT, ["DEL" | Keys]),
            binary_to_integer(NumBin)
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
-spec internal_get(Key :: iolist(), TTL::integer()) -> undefined | binary() | error.
internal_get(Key, TTL) ->
    Request = [
        ["GET", Key],
        ["EXPIRE", Key, TTL]
    ],
    case eredis:qp(?REDIS_CLIENT, Request) of
        [{ok, undefined}, _] -> undefined;
        [{ok, V}, {ok, <<"1">>}] -> V;
        Error ->
            ?LOG_ERROR("eredis GET of key:~p Error:~p~n", [Key, Error]),
            error
    end.

-spec internal_del(Key :: iolist()) -> ok.
internal_del(Key) ->
    {ok, _} = eredis:q(?REDIS_CLIENT, ["DEL", Key]),
    ok.
