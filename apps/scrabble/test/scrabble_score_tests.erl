-module(scrabble_score_tests).

-include_lib("eunit/include/eunit.hrl").

letter_score_test() ->
    ok.

word_score_test() ->
    ?assertEqual(
        18,
        scrabble_score:word_score(
            #{},
            [
                #{ x => 8, y => 8, value => $h },
                #{ x => 9, y => 8, value => $e },
                #{ x => 10, y => 8, value => $l },
                #{ x => 11, y => 8, value => $l },
                #{ x => 12, y => 8, value => $o }
            ]
        )
    ).

% h = 4
% e = 1
% l = 1
% l = 1
% o = 2 ( dbl )

% double letter score ( o )
% double word score!

% word = 9

% double world score ( x 8 y 8)

% final word score = 18