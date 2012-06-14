#!/usr/bin/env escript
%%! -input
-module(test2).
-mode(compile).

-export([main/1]).

main([WsUrl]) ->
    WS = rfc6455_client:new(WsUrl, self()),
    io:format("[*] Connecting ~p~n", [WsUrl]),

    {ok, Opts} = rfc6455_client:open(WS),
    io:format("[.] http ~p~n", [Opts]),

    ok = rfc6455_client:send(WS, <<"a[\"aaaaaaaa\"]">>),

    {ok, D} = rfc6455_client:recv(WS),
    io:format("[.] got ~p~n", [D]),

    {close, R} = rfc6455_client:close(WS, {1002, "yeah baby!"}),
    io:format("[*] close ~p~n", [R]).
