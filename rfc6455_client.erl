-module(rfc6455_client).

-export([new/2]).

-record(state, {host, port, addr, path, ppid, socket, data, phase}).

%% --------------------------------------------------------------------------

new(WsUrl, PPid) ->
    "ws://" ++ Rest = WsUrl,
    [Addr, Path] = split("/", Rest, 1),
    [Host, MaybePort] = split(":", Addr, 1, empty),
    Port = case MaybePort of
               empty -> 80;
               V     -> {I, ""} = string:to_integer(V), I
           end,
    State = #state{host = Host,
                   port = Port,
                   addr = Addr,
                   path = "/" ++ Path,
                   ppid = PPid},
    spawn(fun () ->
                  start_conn(State)
          end).

%% --------------------------------------------------------------------------

start_conn(State) ->
    random:seed(now()),
    {ok, Socket} = gen_tcp:connect(State#state.host, State#state.port,
                                   [binary,
                                    {packet, 0}]),
    Key = base64:encode_to_string([random:uniform(256)-1 ||
                                      _ <- lists:seq(0, 15)]),
    gen_tcp:send(Socket,
        "GET " ++ State#state.path ++ " HTTP/1.1\r\n" ++
        "Host: " ++ State#state.addr ++ "\r\n" ++
        "Upgrade: websocket\r\n" ++
        "Connection: Upgrade\r\n" ++
        "Sec-WebSocket-Key: " ++ Key ++ "\r\n" ++
        "Origin: null\r\n" ++
        "Sec-WebSocket-Version: 13\r\n\r\n"),

    loop(State#state{socket = Socket,
                     data   = <<>>,
                     phase  = opening}).

recv(State = #state{phase = opening, data = Data}) ->
    case split("\r\n\r\n", binary_to_list(Data), 1, empty) of
        [Http, empty] -> State;
        [Http, Data1]   ->
            %% TODO: don't ignore http response
            io:format("Http response ~p~n", [Http]),
            State#state{phase = open,
                        data = Data1}
    end;
recv(State = #state{phase = open, data = Data, ppid = PPid}) ->
    R = case Data of
            <<F:1, _:3, O:4, M:1, L:7, Payload:L/binary, Rest/binary>>
              when L < 126 andalso M == 0 ->
                {F, O, undefined, Payload, Rest};
            <<F:1, _:3, O:4, M:1, L:7, Mask:32, Payload:L/binary, Rest/binary>>
              when L < 126 andalso M == 1 ->
                {F, O, Mask, Payload, Rest};

            <<F:1, _:3, O:4, M:1, L:7, L2:16, Payload:L2/binary, Rest/binary>>
              when L == 126 andalso M == 0 ->
                {F, O, undefined, Payload, Rest};
            <<F:1, _:3, O:4, M:1, L:7, L2:16, Mask:32, Payload:L2/binary, Rest/binary>>
              when L == 126 andalso M == 1 ->
                {F, O, Mask, Payload, Rest};

            <<F:1, _:3, O:4, M:1, L:7, L2:64, Payload:L2/binary, Rest/binary>>
              when L == 127 andalso M == 0 ->
                {F, O, undefined, Payload, Rest};
            <<F:1, _:3, O:4, M:1, L:7, L2:64, Mask:32, Payload:L2/binary, Rest/binary>>
              when L == 127 andalso M == 1 ->
                {F, O, Mask, Payload, Rest};
            _ ->
                notmatched
        end,
    case R of
        {1, 1, undefined, Payload2, Rest2} ->
            PPid ! {rfc6455, recv, self(), Payload2},
            State#state{data = Rest2};
        notmatched ->
            State;
        {_, _, _, _, Rest2} ->
            io:format("Unknown frame type~n"),
            State#state{data = Rest2}
    end.

loop(State = #state{socket = Socket, ppid = PPid, data = Data, phase = Phase}) ->
    receive
        {tcp, Socket, Bin} ->
            State1 = State#state{data = iolist_to_binary([Data, Bin])},
            loop(recv(State1));
        {send, Payload} when Phase == open ->
            Payload1 = iolist_to_binary(Payload),
            L = byte_size(Payload1),
            IoData = case L of
                         _ when L < 126 ->
                             <<1:1, 0:3, 1:4, 0:1, L:7, Payload1/binary>>
                     end,
            gen_tcp:send(Socket, iolist_to_binary([IoData])),
            loop(State);
        {tcp_closed, Socket} ->
            PPid ! {rfc6455, close, self(),
                    {1006, "Connection closed abnormally"}},
            ok
    end.




%% --------------------------------------------------------------------------
split(SubStr, Str, Limit) ->
    split(SubStr, Str, Limit, "").

split(SubStr, Str, Limit, Default) ->
    Acc = split(SubStr, Str, Limit, [], Default),
    lists:reverse(Acc).
split(_SubStr, Str, 0, Acc, _Default) -> [Str | Acc];
split(SubStr, Str, Limit, Acc, Default) ->
    {L, R} = case string:str(Str, SubStr) of
                 0 -> {Str, Default};
                 I -> {string:substr(Str, 1, I-1),
                       string:substr(Str, I+length(SubStr))}
             end,
    split(SubStr, R, Limit-1, [L | Acc], Default).
