-module(scrabble_rules).

%% Playing more than 1 word!

% Can you make two words in Scrabble?
% There are no rules to prevent a player from forming multiple words in a single turn
% as long as all the rules of placing a word on a scrabble game are followed including:

% The second word formed must be parallel to the primary word. That means the two words formed cannot be disconnected.

% All the letters of the second word formed must be part of the existing word and follow the same direction on the board.

% The tiles used to create the second word must come from the same rack used to form the first word.
% A player cannot draw tiles to replace the used tiles while creating the first word to form the second word.

% According to scrabble rules, all tiles played in a single turn must be placed in a single row or column.
% For this reason, a player cannot create the second word by placing two letters in a column to begin a word and
% end the word by placing another letter in a different column or row.

% All the words formed must be legally accepted in the scrabble game and found in the scrabble dictionary used to moderate the game.