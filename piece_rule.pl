:- dynamic piece/3.
:- discontiguous valid_move/5.

in_boundaries(Col,Row):-
    between(0,7,Col),
    between(0,7,Row).

piece(wp, 5, 0).
piece(wp, 5, 1).
piece(wp, 5, 2).
piece(wp, 5, 3).
piece(wp, 5, 4).
piece(wp, 5, 5).
piece(wp, 5, 6).
piece(wp, 5, 7).

piece(bp, 2, 0).
piece(bp, 2, 1).
piece(bp, 2, 2).
piece(bp, 2, 3).
piece(bp, 2, 4).
piece(bp, 2, 5).
piece(bp, 2, 6).
piece(bp, 2, 7).

piece(wR, 7, 0). 
piece(wR, 7, 7).

piece(bR, 0, 0).
piece(bR, 0, 7).

piece(wN, 7, 1).
piece(wN, 7, 6).

piece(bN, 0, 1).
piece(bN, 0, 6).

piece(wB, 7, 2).
piece(wB, 7, 5).

piece(bB, 0, 2).
piece(bB, 0, 5).

piece(wQ, 7, 4).
piece(bQ, 0, 3).

piece(wK, 7, 3).
piece(bK, 0, 4).

% Pawn move
% White Pawn
valid_move(wp, X, Y, X2, Y2) :- 
    (X2 is X - 1, Y2 = Y),
    piece(wp, X, Y),
    \+ piece(_, X2, Y2).


valid_move(wp, X, Y, X2, Y2) :-
    (   X2 is X - 1, Y2 is Y + 1
    ;   X2 is X - 1, Y2 is Y - 1
    ),
    (   piece(bp, X2, Y2)  % Capturing black pawn
    ;   piece(bR, X2, Y2)  % Capturing black rook
    ;   piece(bN, X2, Y2)  % Capturing black knight
    ;   piece(bB, X2, Y2)  % Capturing black bishop
    ;   piece(bQ, X2, Y2)  % Capturing black queen
    ;   piece(bK, X2, Y2)  % Capturing black king
    ;   piece(bL, X2, Y2)  % Capturing black king
    ),
    piece(wp, X, Y),  % White pawn is at (X, Y)
    retract(piece(_, X2, Y2)).  % Remove captured piece

% Black Pawn
valid_move(bp, X, Y, X2, Y2) :- 
    (X2 is X + 1, Y2 = Y),
    piece(bp, X, Y),
    \+ piece(_, X2, Y2).  

valid_move(bp, X, Y, X2, Y2) :-
    (   X2 is X + 1, Y2 is Y + 1  % Diagonal move down-left
    ;   X2 is X + 1, Y2 is Y - 1  % Diagonal move down-right
    ),
    % Check if the destination contains a white piece
    (   piece(wp, X2, Y2)  % Capturing white pawn
    ;   piece(wR, X2, Y2)  % Capturing white rook
    ;   piece(wN, X2, Y2)  % Capturing white knight
    ;   piece(wB, X2, Y2)  % Capturing white bishop
    ;   piece(wQ, X2, Y2)  % Capturing white queen
    ;   piece(wL, X2, Y2)  % Capturing white queen
    ;   piece(wK, X2, Y2)  % Capturing white king (if allowed)
    ),
    piece(bp, X, Y),  % Black pawn is at (X, Y)
    retract(piece(_, X2, Y2)).  % Remove the captured white piece

opponent_piece(wR, Opponent) :- member(Opponent, [bp, bR, bN, bB, bQ, bK, bL]).
opponent_piece(bR, Opponent) :- member(Opponent, [wp, wR, wN, wB, wQ, wK, wL]).

% Rook move
% White Rook movement
valid_move(wR, X, Y, X2, Y2) :-
    (X2 = X; Y2 = Y),
    clear_path(wR, X, Y, X2, Y2),
    piece(wR, X, Y),
    (   \+ piece(_, X2, Y2)
    ;   piece(bp, X2, Y2),
        retract(piece(bp, X2, Y2))
    ;   piece(bR, X2, Y2),
        retract(piece(bR, X2, Y2))
    ;   piece(bN, X2, Y2),
        retract(piece(bN, X2, Y2))
    ;   piece(bB, X2, Y2),
        retract(piece(bB, X2, Y2))
    ;   piece(bQ, X2, Y2),
        retract(piece(bQ, X2, Y2))
    ;   piece(bL, X2, Y2),
        retract(piece(bL, X2, Y2))
    ;   piece(bK, X2, Y2),
        retract(piece(bK, X2, Y2))
    ).

% Black Rook movement
valid_move(bR, X, Y, X2, Y2) :-
    (X2 = X; Y2 = Y),
    clear_path(bR, X, Y, X2, Y2),
    piece(bR, X, Y),
    (   \+ piece(_, X2, Y2)
    ;   piece(wp, X2, Y2),
        retract(piece(wp, X2, Y2))
    ;   piece(wR, X2, Y2),
        retract(piece(wR, X2, Y2))
    ;   piece(wN, X2, Y2),
        retract(piece(wN, X2, Y2))
    ;   piece(wB, X2, Y2),
        retract(piece(wB, X2, Y2))
    ;   piece(wQ, X2, Y2),
        retract(piece(wQ, X2, Y2))
    ;   piece(wL, X2, Y2),
        retract(piece(wQ, X2, Y2))
    ;   piece(wK, X2, Y2),
        retract(piece(wK, X2, Y2))
    ).

% General clear path check
clear_path(Piece, X, Y, X2, Y2) :-
    (   X2 > X -> move_vertical_down(Piece, X, Y, X2)
    ;   X2 < X -> move_vertical_up(Piece, X, Y, X2)
    ;   Y2 > Y -> move_horizontal_right(Piece, X, Y, Y2)
    ;   Y2 < Y -> move_horizontal_left(Piece, X, Y, Y2)
    ).

% Move vertically upwards with capture
move_vertical_up(Piece, X, Y, X2) :-
    X > X2,
    NextX is X - 1,
    in_boundaries(NextX, Y),
    (   \+ piece(_, NextX, Y),
        (   NextX = X2
        ;   move_vertical_up(Piece, NextX, Y, X2)
        )
    ;   piece(Opponent, NextX, Y),
        opponent_piece(Piece, Opponent),
        NextX = X2,
        retract(piece(Opponent, NextX, Y))
    ).

% Move vertically downwards with capture
move_vertical_down(Piece, X, Y, X2) :-
    X < X2,
    NextX is X + 1,
    in_boundaries(NextX, Y),
    (   \+ piece(_, NextX, Y),
        (   NextX = X2
        ;   move_vertical_down(Piece, NextX, Y, X2)
        )
    ;   piece(Opponent, NextX, Y),
        opponent_piece(Piece, Opponent),
        NextX = X2,
        retract(piece(Opponent, NextX, Y))
    ).

% Move horizontally to the left with capture
move_horizontal_left(Piece, X, Y, Y2) :-
    Y > Y2,
    NextY is Y - 1,
    in_boundaries(X, NextY),
    (   \+ piece(_, X, NextY),
        (   NextY = Y2
        ;   move_horizontal_left(Piece, X, NextY, Y2)
        )
    ;   piece(Opponent, X, NextY),
        opponent_piece(Piece, Opponent),
        NextY = Y2,
        retract(piece(Opponent, X, NextY))
    ).

% Move horizontally to the right with capture
move_horizontal_right(Piece, X, Y, Y2) :-
    Y < Y2,
    NextY is Y + 1,
    in_boundaries(X, NextY),
    (   \+ piece(_, X, NextY),
        (   NextY = Y2
        ;   move_horizontal_right(Piece, X, NextY, Y2)
        )
    ;   piece(Opponent, X, NextY),
        opponent_piece(Piece, Opponent),
        NextY = Y2,
        retract(piece(Opponent, X, NextY))
    ).

% Khon move for white Khon
valid_move(wB, X, Y, X2, Y2) :-
    piece(wB, X, Y),
    (   abs(X2 - X) =:= 1, abs(Y2 - Y) =:= 1           % Diagonal move
    ;   X2 is X - 1, Y2 =:= Y                          % Move one square forward
    ),
    (   \+ piece(_, X2, Y2)                            % Destination is empty
    ;   piece(bp, X2, Y2),                             % Capture black pawn
        retract(piece(bp, X2, Y2))
    ;   piece(bR, X2, Y2),                             % Capture black rook
        retract(piece(bR, X2, Y2))
    ;   piece(bN, X2, Y2),                             % Capture black knight
        retract(piece(bN, X2, Y2))
    ;   piece(bQ, X2, Y2),                             % Capture black queen
        retract(piece(bQ, X2, Y2))
    ;   piece(bK, X2, Y2),                             % Capture black king
        retract(piece(bK, X2, Y2))
    ;   piece(bB, X2, Y2),                             % Capture black king
        retract(piece(bB, X2, Y2))
    ;   piece(bL, X2, Y2),                             % Capture black king
        retract(piece(bL, X2, Y2))
    ).

% Khon move for black Khon
valid_move(bB, X, Y, X2, Y2) :-
    piece(bB, X, Y),
    (   abs(X2 - X) =:= 1, abs(Y2 - Y) =:= 1           % Diagonal move
    ;   X2 is X + 1, Y2 =:= Y                          % Move one square forward
    ),
    (   \+ piece(_, X2, Y2)                            % Destination is empty
    ;   piece(wp, X2, Y2),                             % Capture white pawn
        retract(piece(wp, X2, Y2))
    ;   piece(wR, X2, Y2),                             % Capture white rook
        retract(piece(wR, X2, Y2))
    ;   piece(wN, X2, Y2),                             % Capture white knight
        retract(piece(wN, X2, Y2))
    ;   piece(wQ, X2, Y2),                             % Capture white queen
        retract(piece(wQ, X2, Y2))
    ;   piece(wK, X2, Y2),                             % Capture white king
        retract(piece(wK, X2, Y2))
    ;   piece(wB, X2, Y2),                             % Capture white king
        retract(piece(wB, X2, Y2))
    ;   piece(wL, X2, Y2),                             % Capture white king
        retract(piece(wL, X2, Y2))
    ).

% Met move for white Met
valid_move(wQ, X, Y, X2, Y2) :-
    piece(wQ, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(bp, X2, Y2),           % Destination contains a black pawn
        retract(piece(bp, X2, Y2))   % Capture the black pawn
    ;   piece(bR, X2, Y2),           % Destination contains a black rook
        retract(piece(bR, X2, Y2))   % Capture the black rook
    ;   piece(bN, X2, Y2),           % Destination contains a black knight
        retract(piece(bN, X2, Y2))   % Capture the black knight
    ;   piece(bQ, X2, Y2),           % Destination contains a black queen
        retract(piece(bQ, X2, Y2))   % Capture the black queen
    ;   piece(bK, X2, Y2),           % Destination contains a black king
        retract(piece(bK, X2, Y2))   % Capture the black king
    ;   piece(bB, X2, Y2),           % Destination contains a black king
        retract(piece(bB, X2, Y2))   % Capture the black king
    ;   piece(bL, X2, Y2),           % Destination contains a black king
        retract(piece(bL, X2, Y2))   % Capture the black king
    ).

% Met move for black Met
valid_move(bQ, X, Y, X2, Y2) :-
    piece(bQ, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(wp, X2, Y2),           % Destination contains a white pawn
        retract(piece(wp, X2, Y2))   % Capture the white pawn
    ;   piece(wR, X2, Y2),           % Destination contains a white rook
        retract(piece(wR, X2, Y2))   % Capture the white rook
    ;   piece(wN, X2, Y2),           % Destination contains a white knight
        retract(piece(wN, X2, Y2))   % Capture the white knight
    ;   piece(wQ, X2, Y2),           % Destination contains a white queen
        retract(piece(wQ, X2, Y2))   % Capture the white queen
    ;   piece(wK, X2, Y2),           % Destination contains a white king
        retract(piece(wK, X2, Y2))   % Capture the white king
    ;   piece(wB, X2, Y2),           % Destination contains a white king
        retract(piece(wB, X2, Y2))   % Capture the white king
    ;   piece(wL, X2, Y2),           % Destination contains a white king
        retract(piece(wL, X2, Y2))   % Capture the white king
    ).


% Knight move
valid_move(wN, X, Y, X2, Y2) :- 
    piece(wN, X, Y),
    (   (X2 is X - 2, Y2 is Y - 1)
    ;   (X2 is X - 2, Y2 is Y + 1)
    ;   (X2 is X - 1, Y2 is Y - 2)
    ;   (X2 is X - 1, Y2 is Y + 2)
    ;   (X2 is X + 1, Y2 is Y - 2)
    ;   (X2 is X + 1, Y2 is Y + 2)
    ;   (X2 is X + 2, Y2 is Y - 1)
    ;   (X2 is X + 2, Y2 is Y + 1)
    ),
    (   \+ piece(_, X2, Y2)  % destination is empty
    ;   piece(bp, X2, Y2),    % destination contains a black pawn
        retract(piece(bp, X2, Y2))  % capture the black pawn
    ;   piece(bR, X2, Y2),    % destination contains a black rook
        retract(piece(bR, X2, Y2))  % capture the black rook
    ;   piece(bN, X2, Y2),    % destination contains a black knight
        retract(piece(bN, X2, Y2))  % capture the black knight
    ;   piece(bQ, X2, Y2),    % destination contains a black queen
        retract(piece(bQ, X2, Y2))  % capture the black queen
    ;   piece(bB, X2, Y2),    % destination contains a black bishop
        retract(piece(bB, X2, Y2))  % capture the black bishop
    ;   piece(bK, X2, Y2),    % destination contains a black king
        retract(piece(bK, X2, Y2))  % capture the black king
    ;   piece(bL, X2, Y2),    % destination contains a black king
        retract(piece(bL, X2, Y2))  % capture the black king
    ).


valid_move(bN, X, Y, X2, Y2) :- 
    piece(bN, X, Y),
    (   (X2 is X - 2, Y2 is Y - 1)
    ;   (X2 is X - 2, Y2 is Y + 1)
    ;   (X2 is X - 1, Y2 is Y - 2)
    ;   (X2 is X - 1, Y2 is Y + 2)
    ;   (X2 is X + 1, Y2 is Y - 2)
    ;   (X2 is X + 1, Y2 is Y + 2)
    ;   (X2 is X + 2, Y2 is Y - 1)
    ;   (X2 is X + 2, Y2 is Y + 1)
    ),
    (   \+ piece(_, X2, Y2)  % destination is empty
    ;   piece(wp, X2, Y2),    % destination contains a white pawn
        retract(piece(wp, X2, Y2))  % capture the white pawn
    ;   piece(wR, X2, Y2),    % destination contains a white rook
        retract(piece(wR, X2, Y2))  % capture the white rook
    ;   piece(wN, X2, Y2),    % destination contains a white knight
        retract(piece(wN, X2, Y2))  % capture the white knight
    ;   piece(wQ, X2, Y2),    % destination contains a white queen
        retract(piece(wQ, X2, Y2))  % capture the white queen
    ;   piece(wB, X2, Y2),    % destination contains a white bishop
        retract(piece(wB, X2, Y2))  % capture the white bishop
    ;   piece(wK, X2, Y2),    % destination contains a white king
        retract(piece(wK, X2, Y2))  % capture the white king
    ;   piece(wL, X2, Y2),    % destination contains a white king
        retract(piece(wL, X2, Y2))  % capture the white king
    ).

% White King movement (with capture ability)
valid_move(wK, X, Y, X2, Y2) :-
    piece(wK, X, Y),
    (   (X2 is X + 1, Y2 is Y)
    ;   (X2 is X - 1, Y2 is Y)
    ;   (X2 is X, Y2 is Y + 1)
    ;   (X2 is X, Y2 is Y - 1)
    ;   (X2 is X + 1, Y2 is Y + 1)
    ;   (X2 is X + 1, Y2 is Y - 1)
    ;   (X2 is X - 1, Y2 is Y + 1)
    ;   (X2 is X - 1, Y2 is Y - 1)
    ),
    (   \+ piece(_, X2, Y2)  % destination is empty
    ;   piece(bp, X2, Y2),    % destination contains a black pawn
        retract(piece(bp, X2, Y2))  % capture the black pawn
    ;   piece(bR, X2, Y2),    % destination contains a black rook
        retract(piece(bR, X2, Y2))  % capture the black rook
    ;   piece(bN, X2, Y2),    % destination contains a black knight
        retract(piece(bN, X2, Y2))  % capture the black knight
    ;   piece(bB, X2, Y2),    % destination contains a black bishop
        retract(piece(bB, X2, Y2))  % capture the black bishop
    ;   piece(bQ, X2, Y2),    % destination contains a black queen
        retract(piece(bQ, X2, Y2))  % capture the black queen
    ;   piece(bK, X2, Y2),    % destination contains a black king
        retract(piece(bK, X2, Y2))  % capture the black king
    ;   piece(bL, X2, Y2),    % destination contains a white king
        retract(piece(bL, X2, Y2))  % capture the white king
    ).


% Black King movement (with capture ability)
valid_move(bK, X, Y, X2, Y2) :-
    piece(bK, X, Y),
    (   (X2 is X + 1, Y2 is Y)
    ;   (X2 is X - 1, Y2 is Y)
    ;   (X2 is X, Y2 is Y + 1)
    ;   (X2 is X, Y2 is Y - 1)
    ;   (X2 is X + 1, Y2 is Y + 1)
    ;   (X2 is X + 1, Y2 is Y - 1)
    ;   (X2 is X - 1, Y2 is Y + 1)
    ;   (X2 is X - 1, Y2 is Y - 1)
    ),
    (   \+ piece(_, X2, Y2)  % destination is empty
    ;   piece(wp, X2, Y2),    % destination contains a white pawn
        retract(piece(wp, X2, Y2))  % capture the white pawn
    ;   piece(wR, X2, Y2),    % destination contains a white rook
        retract(piece(wR, X2, Y2))  % capture the white rook
    ;   piece(wN, X2, Y2),    % destination contains a white knight
        retract(piece(wN, X2, Y2))  % capture the white knight
    ;   piece(wB, X2, Y2),    % destination contains a white bishop
        retract(piece(wB, X2, Y2))  % capture the white bishop
    ;   piece(wQ, X2, Y2),    % destination contains a white queen
        retract(piece(wQ, X2, Y2))  % capture the white queen
    ;   piece(wK, X2, Y2),    % destination contains a white king
        retract(piece(wK, X2, Y2))  % capture the white king
    ;   piece(wL, X2, Y2),    % destination contains a white king
        retract(piece(wL, X2, Y2))  % capture the white king
    ).

% Promoted Bia move for white Bia
valid_move(wL, X, Y, X2, Y2) :-
    piece(wL, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(bp, X2, Y2),           % Destination contains a black pawn
        retract(piece(bp, X2, Y2))   % Capture the black pawn
    ;   piece(bR, X2, Y2),           % Destination contains a black rook
        retract(piece(bR, X2, Y2))   % Capture the black rook
    ;   piece(bN, X2, Y2),           % Destination contains a black knight
        retract(piece(bN, X2, Y2))   % Capture the black knight
    ;   piece(bQ, X2, Y2),           % Destination contains a black queen
        retract(piece(bQ, X2, Y2))   % Capture the black queen
    ;   piece(bK, X2, Y2),           % Destination contains a black king
        retract(piece(bK, X2, Y2))   % Capture the black king
    ;   piece(bB, X2, Y2),           % Destination contains a black king
        retract(piece(bB, X2, Y2))   % Capture the black king
    ;   piece(bL, X2, Y2),    % destination contains a white king
        retract(piece(bL, X2, Y2))  % capture the white king
    ).

% Promoted Bia move for black Bia
valid_move(bL, X, Y, X2, Y2) :-
    piece(bL, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(wp, X2, Y2),           % Destination contains a white pawn
        retract(piece(wp, X2, Y2))   % Capture the white pawn
    ;   piece(wR, X2, Y2),           % Destination contains a white rook
        retract(piece(wR, X2, Y2))   % Capture the white rook
    ;   piece(wN, X2, Y2),           % Destination contains a white knight
        retract(piece(wN, X2, Y2))   % Capture the white knight
    ;   piece(wQ, X2, Y2),           % Destination contains a white queen
        retract(piece(wQ, X2, Y2))   % Capture the white queen
    ;   piece(wK, X2, Y2),           % Destination contains a white king
        retract(piece(wK, X2, Y2))   % Capture the white king
    ;   piece(wB, X2, Y2),           % Destination contains a white king
        retract(piece(wB, X2, Y2))   % Capture the white king
    ;   piece(wL, X2, Y2),    % destination contains a white king
        retract(piece(wL, X2, Y2))  % capture the white king
    ).

move_valid(Piece, X1, Y1, X2, Y2) :- valid_move(Piece, X1, Y1, X2, Y2).

promotion_row(white, 0).
promotion_row(black, 7).

can_promote(Piece, Color, Row, _) :-
    Piece = p,
    promotion_row(Color, Row).


