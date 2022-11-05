-module(scrabble_score_tests).

-include_lib("eunit/include/eunit.hrl").

letter_score_test() ->
    ok.

word_score_test() ->
    %% Hello (5 letters)
% h = 4
% e = 1
% l = 1
% l = 1
% o = 2 ( dbl )

% double letter score ( o )
% double word score!

% letters = 9

% double world score ( x 8 y 8)

% final word score = 18
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
    ),

    %% jukebox ( 7 characters )
% j = 8
% u = 1
% k = 5
% e = 1
% b = 3 x 2 = 6( dbl )
% o = 1
% x = 8

% letters = 30

% double letter score ( b )
% double word score!

% word 60

% all letters played! +50

% 110

% final_word_score =
    ?assertEqual(
        110,
        scrabble_score:word_score(
            #{},
            [
                #{ x => 8, y => 8, value => $j },
                #{ x => 9, y => 8, value => $u },
                #{ x => 10, y => 8, value => $k },
                #{ x => 11, y => 8, value => $e },
                #{ x => 12, y => 8, value => $b },
                #{ x => 13, y => 8, value => $o },
                #{ x => 14, y => 8, value => $x }
            ]
        )
    ).