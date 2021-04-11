-module(scrabble_lobby_statem).

%% @doc a lobby for games
%% A lobby is started when a game is created.
%% the lobby and the game processes are seperate gen_statem'
%% @end

-export([
    start_link/0
]).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4
]).
-export([
    lobby_state/3
]).

-spec start_link() ->
            {ok, Pid :: pid()} |
            ignore |
            {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, {}, []).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

-spec init(Args :: term()) ->
          gen_statem:init_result(term()).
init({}) ->
    {ok, lobby_state, _Data=#{}}.

lobby_state(T=internal, Msg, State) ->
    ?LOG_WARNING("Unhandled ~p ~p", [T, Msg]),
    {next_state, lobby_state, State};
lobby_state(T=timeout, Msg, State) ->
    ?LOG_WARNING("Unhandled ~p ~p", [T, Msg]),
    {next_state, lobby_state, State};
lobby_state({call, From}, _Msg, State) ->
    {next_state, lobby_state, State, [{reply, From, ok}]};
lobby_state(cast, _Msg, State) ->
    {next_state, lobby_state, State};
lobby_state(info, _Msg, State) ->
    {next_state, lobby_state, State}.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
