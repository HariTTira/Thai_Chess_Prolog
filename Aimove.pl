find_random_move(ValidMoves, RandomMove) :-
    length(ValidMoves, Length),
    Length > 0,
    random_between(1, Length, Index), % Generate a random index between 1 and Length
    nth1(Index, ValidMoves, RandomMove). % Retrieve the move at the random index