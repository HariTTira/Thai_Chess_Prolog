:- discontiguous piece_value/2.
:- discontiguous score_square/2.
:- discontiguous score_row/2.
:- discontiguous score_board/2.
:- [piece_rule]. 

find_random_move(ValidMoves, RandomMove) :-
    length(ValidMoves, Length),
    Length > 0,
    random_between(1, Length, Index), % Generate a random index between 1 and Length
    nth1(Index, ValidMoves, RandomMove). % Retrieve the move at the random index

piece_value('wP', 1).
piece_value('wN', 4).
piece_value('wB', 5).
piece_value('wR', 6).
piece_value('wQ', 3).
piece_value('wL', 3).
piece_value('wK', 0).

piece_value('bP', 1).
piece_value('bN', 4).
piece_value('bB', 5).
piece_value('bR', 6).
piece_value('bQ', 3).
piece_value('bL', 3).
piece_value('bK', 0).

score_square('--', 0).   % empty square
score_square(Piece, Score) :-
    piece_value(Piece, Score), !.
score_square(_, 0).

score_row([], 0).
score_row([Square|Rest], Score) :-
    score_row(Rest, RestScore),
    score_square(Square, SquareScore),
    Score is RestScore + SquareScore.

score_board([], 0).
score_board([Row|Rest], Score) :-
    score_board(Rest, RestScore),
    score_row(Row, RowScore),
    Score is RestScore + RowScore.
