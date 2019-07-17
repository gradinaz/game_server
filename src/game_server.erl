%% @author Sergiy Vasylchuk

-module(game_server).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

-export([
    authorize/1,
    buy_stars/2,
    get_profile/1,
    gdpr_erase_profile/1,
    register/1,
    win_level/1
]).

-include("game_server_int.hrl").
-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_COINS, 100).
-define(DEFAULT_STARS, 0).
-define(DEFAULT_LEVEL, 0).
-define(PRICE_FOR_1, 10).
%%-----------------------------------------------------------------------------
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> {ok, list(atom())} | {error, Reason :: term()}
%% @doc Start the server.
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, Started::list(atom())} | {error, Reason :: term()}.
start() ->
    application:ensure_all_started(game_server, permanent).

%%====================================================================
-spec register(NickName :: binary()) ->  map() | {error, Reason} when Reason :: internal | already_exist.
register(NickName) ->
    case db_get_user_by_nickname(NickName) of
        {ok, _} ->
            {error, already_exist};
        {error, Reason} ->
            ?LOG_ERROR("Cannot find user with reason~p", [Reason]),
            {error, internal};
        {error, notfound} ->
            case db_save_new_user_info(NickName, ?DEFAULT_COINS, ?DEFAULT_STARS, ?DEFAULT_LEVEL) of
                {error, Reason} ->
                    ?LOG_ERROR("Cannot register new user with reason:~p", [Reason]),
                    {error, internal};
                Id ->
                    #{<<"uid">> => integer_to_binary(Id)}
            end
    end.

-spec authorize(Uid :: binary()) ->  map() | {error, Reason} when Reason :: internal.
authorize(Uid) ->
    Id = binary_to_integer(Uid),
    case db_get_user_info(Id) of
        {ok, {NickName, Coins, Stars, Level}} ->
            TokenValue = #{
                            <<"id">> => Id,
                            <<"nickname">> => NickName,
                            <<"coins">> => Coins,
                            <<"stars">> => Stars,
                            <<"level">> => Level
                        },
            SessionId = get_session_id(),
            ok = game_server_cache:put_token(SessionId, TokenValue),
            #{<<"auth_token">> => SessionId};
        {error, Reason} ->
            ?LOG_ERROR("Cannot get user info with reason:~p", [Reason]),
            {error, internal}
    end.

-spec get_profile(AuthToken::binary()) -> map() | {error, notfound}.
get_profile(AuthToken) ->
    case game_server_cache:get_token(AuthToken) of
        undefined ->
            ?LOG_ERROR("Cannot get profile for chosen user", []),
            {error, notfound};
        Profile ->
            Profile
    end.

-spec win_level(AuthToken::binary()) -> ok |  {error, Reason} when Reason::term().
win_level(AuthToken) ->
    case game_server_cache:get_token(AuthToken) of
        undefined ->
            ?LOG_ERROR("Cannot find profile", []),
            {error, notfound};
        #{<<"id">> := Id} ->
            case db_update_level(Id) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Cannot update level for chosen user with reason~p", [Reason]),
                    {error, internal}
            end
    end.

-spec buy_stars(AuthToken::binary(), StarsCount::integer()) -> map() |  {error, Reason} when Reason::term().
buy_stars(AuthToken, StarsCount) ->
    case game_server_cache:get_token(AuthToken) of
        undefined ->
            ?LOG_ERROR("Cannot find profile", []),
            {error, notfound};
        #{<<"id">> := Id} ->
            NeededCoins = StarsCount * ?PRICE_FOR_1,
            case db_update_stars(Id, NeededCoins) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Cannot update stars for chosen user with reason~p", [Reason]),
                    {error, internal}
            end
    end.

-spec gdpr_erase_profile(AuthToken::binary()) -> map() |  {error, Reason} when Reason::term().
gdpr_erase_profile(AuthToken) ->
    case game_server_cache:get_token(AuthToken) of
        undefined ->
            #{<<"status">> => <<"ok">>};
        #{<<"id">> := Id} ->
            case db_delete_profile(Id) of
                ok ->
                    #{<<"status">> => <<"ok">>};
                {error, Reason} ->
                    ?LOG_ERROR("Cannot delete profile for chosen user with reason~p", [Reason]),
                    {error, internal}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Database functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec db_save_new_user_info(NickName::binary(), Coins::integer(), Stars::integer(), Level::integer()) ->
    Id::integer() | {error, Reason} when Reason::term().
db_save_new_user_info(NickName, Coins, Stars, Level)->
    case dbproxy:query(?DB, "INSERT INTO game_server.user_info(nickname, coins, stars, level)
                                      VALUES ($1, $2, $3, $4)
                                      RETURNING id",
        [NickName, Coins, Stars, Level]) of
        {ok, _, _, [{Id}]} ->
            Id;
        {error, _Reason} = Res ->
            Res
    end.

-spec get_session_id() -> binary().
get_session_id() ->
    {ok, _, [{V}]} = dbproxy:query(?DB,"SELECT nextval('game_server.session_id')"),
    integer_to_binary(V).

-spec db_get_user_by_nickname(NickName::binary()) -> {ok,  integer()} | {error, Reason} when
        Reason :: notfound | term().
db_get_user_by_nickname(NickName) ->
    case dbproxy:query(?DB, "SELECT id
                         FROM game_server.user_info
                         WHERE nickname = $1",
        [NickName]) of
        {ok,_, [{Id}] } ->
            {ok, Id};
        {ok, _, [ ]} ->
            {error, notfound};
        {error, _Reason} = Res ->
            Res
    end.

-spec db_get_user_info(Id::integer()) -> {ok, {NickName::binary(), Coins::integer(),
        Stars::integer(), Level::integer()}} | {error, Reason} when Reason::term().
db_get_user_info(Id) ->
    case dbproxy:query(?DB, "SELECT nickname, coins, stars, level
                         FROM game_server.user_info
                         WHERE id = $1",
        [Id]) of
        {ok,_, [Res]} ->
            {ok, Res};
        {error, _Reason} = Res ->
            Res
    end.

-spec db_update_level(Id::integer()) -> ok | {error, Reason} when Reason::term().
db_update_level(Id) ->
    case dbproxy:query(?DB, "UPDATE game_server.user_info
                         SET level = level + 1
                         WHERE id = $1",
        [Id]) of
        {ok, _} ->
            ok;
        {error, _Reason} = Res ->
            Res
    end.

-spec db_update_stars(Id::integer(), NeededCoins::integer()) -> ok | {error, Reason} when Reason::term().
db_update_stars(Id, NeededCoins) ->
    case dbproxy:query(?DB, "UPDATE game_server.user_info
                         SET level = level + 1, coins = coins - $2
                         WHERE id = $1 AND coins >= $2",
        [Id, NeededCoins]) of
        {ok, _} ->
            ok;
        {error, _Reason} = Res ->
            Res
    end.

-spec db_delete_profile(Id::integer()) -> ok | {error, Reason} when Reason::term().
db_delete_profile(Id) ->
    case dbproxy:query(?DB, "DELETE FROM game_server.user_info
                         WHERE id = $1",
        [Id]) of
        {ok, _} ->
            ok;
        {error, _Reason} = Res ->
            Res
    end.

