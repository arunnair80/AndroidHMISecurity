-module(nuitestclient).

-export([connect/2, disconnect/1, send_cmd/3, send_cmd_inf/2]).

connect(Ip, Port) ->
	{ok, S} = gen_tcp:connect(Ip, Port, [binary]),
	inet:setopts(S, [{active, true}]),
	S.

safe_binary_to_term(S, Bin) ->
	safe_binary_to_term(S, Bin, 0).

safe_binary_to_term(_S, _, 10) ->
	{noresponse};
safe_binary_to_term(S, <<>>, N) ->
	safe_binary_to_term(S, read_data(S, infinity), N + 1);
safe_binary_to_term(_, Bin, _) ->
	binary_to_term(Bin).

disconnect(S) ->
	gen_server:close(S).

send_cmd(S, Cmd, SleepTime) ->
	gen_tcp:send(S, term_to_binary(Cmd)),
	%timer:sleep(SleepTime),
	safe_binary_to_term(S, read_more_data(S, <<>>, SleepTime, 100)).

send_cmd_inf(S, Cmd) ->
	gen_tcp:send(S, term_to_binary(Cmd)),
	safe_binary_to_term(S, read_data(S, infinity)).

read_more_data(S, Acc, Timeout) ->
	read_more_data(S, Acc, Timeout, Timeout).

read_more_data(S, Acc, Sleep, Timeout) ->
	case read_data(S, Sleep) of
		<<>> -> 
			Acc;
		MoreData ->
			read_more_data(S, <<Acc/binary, MoreData/binary>>, Timeout, Timeout)
	end.

read_data(S, Timeout) ->
	receive
    {tcp, S, Data} ->
    	Data;
    {tcp_closed, S}->
        {error, io_lib:format("Socket ~p closed~n", [S])};
    {tcp_error, S, Reason} ->
    	{error, io_lib:format("Error on socket ~p reason: ~p~n", [S, Reason])}       
    after
    	Timeout -> 
    		<<>>
    end.

