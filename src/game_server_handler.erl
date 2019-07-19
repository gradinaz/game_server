%% @author Sergiy Vasylchuk
-module(game_server_handler).

-export([init/2, handle/2, terminate/3]).
-export([
    allowed_methods/2,
content_types_provided/2,
    to_json/2,
    from_json/2,
    content_types_accepted/2
]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    ?LOG_INFO("init :~p", [Req]),
    io:format("zzz"),
    {cowboy_rest, Req, State}.

handle(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    io:format("zzasdasz"),
    {ok, Req3} = parse_req(Method, HasBody, Req),
    {ok, Req3, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>}, from_json], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application/json">>}, to_json}], Req, State}.

parse_req(<<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),
    [{BinJson, true}] = PostVals,
    Json = jsx:decode(BinJson, [return_maps]),
    Action = maps:get(<<"action">>, Json),
    Res = parse_command(Action, Json),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], jsx:encode(Res), Req2);

parse_req(<<"POST">>, false, Req) ->
    io:format("parse_req POST Missing body ~n",  []),
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);

parse_req(_, _, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
        jsx:encode(#{<<"status">> => <<"error">>, <<"msg">> => <<"Internal error">>}), Req).

parse_command(<<"/register">>, Json) ->
    Nickname = maps:get(<<"nickname">>, Json),
    game_server:register(Nickname);

parse_command(<<"authorize">>, Json) ->
    Uid = maps:get(<<"uid">>, Json),
    game_server:authorize(Uid);

parse_command(<<"gdpr_erase_profile">>, Json) ->
    AuthToken = maps:get(<<"auth_token">>, Json),
    game_server:gdpr_erase_profile(AuthToken);

parse_command(<<"win_level">>, Json) ->
    AuthToken = maps:get(<<"auth_token">>, Json),
    game_server:win_level(AuthToken);

parse_command(<<"buy_stars">>, Json) ->
    AuthToken = maps:get(<<"auth_token">>, Json),
    StarsCount = maps:get(<<"start_count">>, Json),
    game_server:buy_stars(AuthToken, StarsCount);

parse_command(<<"get_profile">>, Json) ->
    AuthToken = maps:get(<<"auth_token">>, Json),
    game_server:get_profile(AuthToken).

to_json(Req, State) ->
    Body = jsx:encode(#{<<"status">> => <<"Success">>})
    ,
    {Body, Req, State}.

from_json(Req, State) ->
    Body = jsx:decode(<<"{\"status\" :\"Success\"}">>, [return_maps]),
    {Body, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
