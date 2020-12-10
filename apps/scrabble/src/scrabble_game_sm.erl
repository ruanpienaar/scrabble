-module(scrabble_game_sm).

-export([
    start_link/0
]).

-behaviour(gen_statem).

-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4,
    handle_event/4
]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

init({}) ->
    {ok, initial_state, undefined}.

handle_event({call, From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply,From,ok}]};
handle_event(cast, _Msg, State, Data) ->
    {next_state, State, Data, []};
handle_event(info, _Msg, State, Data) ->
    {next_state, State, Data, []}.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
