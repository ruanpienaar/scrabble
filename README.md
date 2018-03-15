# scrabble
scrabble

### Cmd line example

```
./rebar3 shell
{ok, Pid} = scrabble_game:start_link(2).
scrabble_game:player_start(Pid, 1).
scrabble_game:player_start(Pid, 2).
sys:get_state(Pid).
```