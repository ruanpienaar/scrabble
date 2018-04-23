-module(scrabble_lobby_tests).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------
%% testing a instantiated gen_server instance

instance_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [
     ]
    }.

%% -------------------------------------------
%% Testing Gen Server callbacks

gs_callbacks_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [{"scrabble_lobby_gs_callbacks_test -> init/1",
        fun init/0}
      % handle_cast
      % handle_info
      % terminate
      % code_change
     ]
    }.

init() ->
    ?assertMatch(
        ok,
        scrabble_lobby:init(2)
    ).

%% -------------------------------------------
%% Simple API unit tests

unit_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ?assertEqual(
            [],
            lists:sort(meck:unload())
        )
     end,
     [  {"scrabble_lobby_unit_test -> game_struct/1", fun game_struct/0}
     ]
    }.

game_struct() ->
    ok.