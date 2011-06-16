-module(stupid_udp).

-compile([export_all]).

server(Port) ->
    {ok,Socket} = gen_udp:open(Port,[binary]),
    listen(Socket).

listen(Socket) ->
    receive
        {udp, Socket, _Host, _Port, Bin} = Message ->
        io:format("server received:~p~n", [Message]),
        handle_message(Bin),
        listen(Socket)
    end.

handle_message(Bin) ->
    [Key, Value, Type] = binary:split(Bin, [<<":">>, <<"|">>], [global]),
    % FIXME: faster/more direct way?
    IntValue = list_to_integer(binary_to_list(Value)),
    case Type of
        <<"d">> -> estatsd:decrement(Key, IntValue);
        <<"c">> -> estatsd:increment(Key, IntValue);
        <<"ms">> -> estatsd:timing(Key, IntValue)
    end.


