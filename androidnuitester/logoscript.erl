-module(logoscript).

-export([prepare/2, fuzz/1]).

show_status_window(S) ->
	{ok} = nuitestclient:send_cmd(S, {click, 200, 600}, 3000),
	{ok} = nuitestclient:send_cmd(S, {click, 200, 350}, 3000),
	{ok} = nuitestclient:send_cmd(S, {click, 200, 200}, 3000),
	{ok} = nuitestclient:send_cmd(S, {setcropscreenheight, 692}, 3000).

run_logoapp(S) ->
	{ok} = nuitestclient:send_cmd(S, {ping}, 3000),
	{ok} = nuitestclient:send_cmd(S, {home}, 3000),
	{ok} = nuitestclient:send_cmd(S, {run, "com.siemens.snc.ilogo/qAndroidDevice.QAndroidDevice", "android.intent.action.MAIN"}, 3000),
	timer:sleep(20000),
	show_status_window(S).


prepare(IP, Port) -> 
	S = nuitestclient:connect(IP, Port),
	run_logoapp(S),
	timer:sleep(5000), 
	{ok} = nuitestclient:send_cmd(S, {putscreenoncache, "working"}, 5000),
	S.

fuzz(S) ->
	fuzz(S, true, 0).

fuzz(_, _, 30) ->
	"Too many fails. Crashed?";
fuzz(S, true, _FailCnt) ->
	{ok, Res} = nuitestclient:send_cmd(S, {comparescreenoncache, "working"}, 5000),
	io:format("Got ~p from android.~n", [Res]),
	fuzz(S, Res, 0);
fuzz(S, false, FailCnt) ->
	io:format("Relauncing...~n"),
	{ok} = nuitestclient:send_cmd(S, {click, 358, 463}, 3000),
	timer:sleep(1000),
	show_status_window(S),	
	timer:sleep(1000),
	{ok, Res} = nuitestclient:send_cmd(S, {comparescreenoncache, "working"}, 5000),
	io:format("Got ~p from android.~n", [Res]),
	fuzz(S, Res, FailCnt + 1).
	
	