-module(scrabble_ws_mon).
-export([
    start_link/0,
    monitor_ws_pid/1
]).

start_link() ->
    {ok, proc_lib:spawn_link(
        fun() -> init() end
    )}.

monitor_ws_pid(Pid) ->
    ?MODULE ! {monitor, Pid}.

init() ->
    true = erlang:register(?MODULE, self()),
    process_flag(trap_exit, true),
    loop().

loop() ->
    receive
        {monitor, Pid} ->
            % io:format("[~p] Going to monitor ~p~n", [?MODULE, Pid]),
            _Ref = erlang:monitor(process, Pid),
            loop();
        {'DOWN', _Ref, process, _Pid, normal} ->
            loop();
        {'DOWN', _Ref, process, _Pid, noproc} ->
            loop();
        X ->
            io:format("[~p] received ~p ~n", [?MODULE, X]),
            loop()
    end.
