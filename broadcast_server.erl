-module(broadcast_server).
-export([listen/0, send/1]).

% -------------------------
% public interface
% -------------------------

% listen for new messages
listen() ->
    {ok, _} = gen_udp:open(6000),
    loop().

% send 
send(Messages) ->
    case inet:ifget("eth0", [broadaddr]) of
        {ok, [{broadaddr, Ip}]} ->
            {ok, Socket} = gen_udp:open(5010, [{broadcast, true}]),
            gen_udp:send(Socket, Ip, 6000, Messages),
            gen_udp:close(Socket);

        % error case
        _ ->
            io:format("Error while broadcasting")
    end.

% -------------------------
% module internal
% -------------------------

loop() ->
    receive
        Messages -> 
            io:format("new messages:~p~n", [Messages]),
            loop()
    end.

