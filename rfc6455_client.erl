-module(rfc6455_client).

-export([new/2]).

-record(state, {host, port, addr, path, ppid, socket, data, phase}).

%% --------------------------------------------------------------------------

new(WsUrl, PPid) ->
    crypto:start(),
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
    {ok, Socket} = gen_tcp:connect(State#state.host, State#state.port,
                                   [binary,
                                    {packet, 0}]),
    Key = base64:encode_to_string(crypto:rand_bytes(16)),
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
        [_Http, empty] -> State;
        [Http, Data1]   ->
            %% TODO: don't ignore http response
            io:format("Http response ~p~n", [Http]),
            State#state{phase = open,
                        data = Data1}
    end;
recv(State = #state{phase = open, data = Data, socket = Socket, ppid = PPid}) ->
    R = case Data of
            <<F:1, _:3, O:4, 0:1, L:7, Payload:L/binary, Rest/binary>>
              when L < 126 ->
                {F, O, Payload, Rest};

            <<F:1, _:3, O:4, 0:1, 126:7, L2:16, Payload:L2/binary, Rest/binary>> ->
                {F, O, Payload, Rest};

            <<F:1, _:3, O:4, 0:1, 127:7, L2:64, Payload:L2/binary, Rest/binary>> ->
                {F, O, Payload, Rest};

            <<_:1, _:3, _:4, 1:1, _/binary>> ->
                %% According o rfc6455 5.1 the server must not mask any frames.
                die(Socket, PPid, {1006, "Protocol error"}, normal);
            _ ->
                notmatched
        end,
    case R of
        {1, 1, Payload2, Rest2} ->
            PPid ! {rfc6455, recv, self(), Payload2},
            State#state{data = Rest2};
        notmatched ->
            State;
        {_, _, _, _, Rest2} ->
            io:format("Unknown frame type~n"),
            State#state{data = Rest2}
    end.

send(State = #state{socket = Socket}, Payload) ->
    Mask = crypto:rand_bytes(4),
    MaskedPayload = apply_mask(Mask, Payload),

    L = byte_size(Payload),
    IoData = case L of
               _ when L < 126 ->
                     [<<1:1, 0:3, 1:4, 1:1, L:7>>, Mask, MaskedPayload];
               _ when L < 65536 ->
                     [<<1:1, 0:3, 1:4, 1:1, 126:7, L:16>>, Mask, MaskedPayload];
               _ ->
                     [<<1:1, 0:3, 1:4, 1:1, 127:7, L:64>>, Mask, MaskedPayload]
           end,
    gen_tcp:send(Socket, iolist_to_binary(IoData)),
    State.

loop(State = #state{socket = Socket, ppid = PPid, data = Data, phase = Phase}) ->
    receive
        {tcp, Socket, Bin} ->
            State1 = State#state{data = iolist_to_binary([Data, Bin])},
            loop(recv(State1));
        {send, Payload} when Phase == open ->
            State1 = send(State, iolist_to_binary(Payload)),
            loop(State1);
        {tcp_closed, Socket} ->
            die(Socket, PPid, {1006, "Connection closed abnormally"}, normal)
    end.


die(Socket, PPid, WsReason, Reason) ->
    gen_tcp:shutdown(Socket, read_write),
    PPid ! {rfc6455, close, self(), WsReason},
    exit(Reason).


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


apply_mask(Mask, Data) when is_number(Mask) ->
    apply_mask(<<Mask:32>>, Data);

apply_mask(<<0:32>>, Data) ->
    Data;
apply_mask(Mask, Data) ->
    iolist_to_binary(lists:reverse(apply_mask2(Mask, Data, []))).

apply_mask2(M = <<Mask:32>>, <<Data:32, Rest/binary>>, Acc) ->
    T = Data bxor Mask,
    apply_mask2(M, Rest, [<<T:32>> | Acc]);
apply_mask2(<<Mask:24, _:8>>, <<Data:24>>, Acc) ->
    T = Data bxor Mask,
    [<<T:24>> | Acc];
apply_mask2(<<Mask:16, _:16>>, <<Data:16>>, Acc) ->
    T = Data bxor Mask,
    [<<T:16>> | Acc];
apply_mask2(<<Mask:8, _:24>>, <<Data:8>>, Acc) ->
    T = Data bxor Mask,
    [<<T:8>> | Acc].
