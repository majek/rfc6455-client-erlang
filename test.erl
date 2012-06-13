#!/usr/bin/env escript
%%! -pa ebin deps/cowboy/ebin -input
-module(test).
-mode(compile).

-export([main/1]).

main([WsUrl]) ->
    WS = rfc6455_client:new(WsUrl, self()),
    io:format("[*] Connecting ~p~n", [WsUrl]),
    WS ! {send, <<"a[\"aaaaaaaa\"]">>},
    loop(WS).

loop(WS) ->
    receive
        {rfc6455, recv, WS, Payload} ->
            io:format("[.] Recv ~p~n", [Payload]),
            %% WS ! {send, <<"a[\"aaaaaaaa\"]">>},
            loop(WS);
        {rfc6455, close, WS, R} ->
            io:format("[*] Closed ~p~n", [R]),
            ok
    end.
