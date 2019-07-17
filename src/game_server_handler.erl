%% @author Sergiy Vasylchuk
-module(game_server_handler).

-behaviour(cowboy_http_handler).

-export([init/2, handle/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    ?LOG_INFO("init :~p", [Req]),
    {ok, Req, State}.

handle(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    {ok, Req3} = parse_req(Method, HasBody, Req),
    {ok, Req3, State}.

%%parse_req(<<"OPTIONS">>, _HasBody, Req) ->
%%    Req1 = cowboy_req:set_resp_header(
%%        <<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
%%    Req2 = cowboy_req:set_resp_header(
%%        <<"access-control-allow-origin">>, <<"*">>, Req1),
%%    cowboy_req:reply(200, Req2);

parse_req(<<"POST">>, true, Req) ->
    ?LOG_INFO("adasd :~p", [Req]),
    {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),
    [{BinJson, true}] = PostVals,
    Json = jsx:decode(BinJson, [return_maps]),
    Action = maps:get(<<"action">>, Json),
    Res = parse_command(Action, Json),
    ?LOG_INFO("adasd :adsasdasdas", []),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json; charset=utf-8">>}], jsx:encode(Res), Req2);

parse_req(<<"POST">>, false, Req) ->
    io:format("parse_req POST Missing body ~n",  []),
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);

parse_req(_, _, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json; charset=utf-8">>}],  jsx:encode(#{<<"z">> => <<"zzzzazz">>}), Req).

parse_command(<<"register">>, Json) ->
    Nickname = maps:get(<<"nickname">>, Json),
    game_server:register(Nickname);

parse_command(<<"authorize">>, Json) ->
    Uid = maps:get(<<"uid">>, Json),
    game_server:authorize(Uid).


