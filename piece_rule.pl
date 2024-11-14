:- dynamic piece/3.
:- discontiguous valid_move/5.
:- discontiguous king_in_check/1.
:- discontiguous move_exposes_check/5.
:- discontiguous legal_move/5.

% Ensure X and Y are within the 8x8 board boundaries (0-7)
in_boundaries(X, Y) :-
    X >= 0, X =< 7,
    Y >= 0, Y =< 7.

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
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    (X2 is X - 1, Y2 = Y),
    piece(wp, X, Y),
    \+ piece(_, X2, Y2).

valid_move(wp, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
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
    piece(wp, X, Y).  % White pawn is at (X, Y)

% Black Pawn
valid_move(bp, X, Y, X2, Y2) :- 
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    (X2 is X + 1, Y2 = Y),
    piece(bp, X, Y),
    \+ piece(_, X2, Y2).  

valid_move(bp, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
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
    piece(bp, X, Y).  % Black pawn is at (X, Y)

opponent_piece(wR, Opponent) :- member(Opponent, [bp, bR, bN, bB, bQ, bK, bL]).
opponent_piece(bR, Opponent) :- member(Opponent, [wp, wR, wN, wB, wQ, wK, wL]).

% Rook move
% White Rook movement
valid_move(wR, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    (X2 = X; Y2 = Y),
    clear_path(wR, X, Y, X2, Y2),
    piece(wR, X, Y),
    (   \+ piece(_, X2, Y2)
    ;   piece(bp, X2, Y2)
    ;   piece(bR, X2, Y2)
    ;   piece(bN, X2, Y2)
    ;   piece(bB, X2, Y2)
    ;   piece(bQ, X2, Y2)
    ;   piece(bL, X2, Y2)
    ;   piece(bK, X2, Y2)
    ).

% Black Rook movement
valid_move(bR, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    (X2 = X; Y2 = Y),
    clear_path(bR, X, Y, X2, Y2),
    piece(bR, X, Y),
    (   \+ piece(_, X2, Y2)
    ;   piece(wp, X2, Y2)
    ;   piece(wR, X2, Y2)
    ;   piece(wN, X2, Y2)
    ;   piece(wB, X2, Y2)
    ;   piece(wQ, X2, Y2)
    ;   piece(wL, X2, Y2)
    ;   piece(wK, X2, Y2)
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
        NextX = X2
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
        NextX = X2
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
        NextY = Y2
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
        NextY = Y2
    ).

% Khon move for white Khon
valid_move(wB, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(wB, X, Y),
    (   abs(X2 - X) =:= 1, abs(Y2 - Y) =:= 1           % Diagonal move
    ;   X2 is X - 1, Y2 =:= Y                          % Move one square forward
    ),
    (   \+ piece(_, X2, Y2)                            % Destination is empty
    ;   piece(bp, X2, Y2)                              % Can capture black pawn
    ;   piece(bR, X2, Y2)                              % Can capture black rook
    ;   piece(bN, X2, Y2)                              % Can capture black knight
    ;   piece(bQ, X2, Y2)                              % Can capture black queen
    ;   piece(bK, X2, Y2)                              % Can capture black king
    ;   piece(bB, X2, Y2)                              % Can capture black bishop
    ;   piece(bL, X2, Y2)                              % Can capture black promoted pawn
    ).

% Khon move for black Khon
valid_move(bB, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(bB, X, Y),
    (   abs(X2 - X) =:= 1, abs(Y2 - Y) =:= 1           % Diagonal move
    ;   X2 is X + 1, Y2 =:= Y                          % Move one square forward
    ),
    (   \+ piece(_, X2, Y2)                            % Destination is empty
    ;   piece(wp, X2, Y2)                              % Can capture white pawn
    ;   piece(wR, X2, Y2)                              % Can capture white rook
    ;   piece(wN, X2, Y2)                              % Can capture white knight
    ;   piece(wQ, X2, Y2)                              % Can capture white queen
    ;   piece(wK, X2, Y2)                              % Can capture white king
    ;   piece(wB, X2, Y2)                              % Can capture white bishop
    ;   piece(wL, X2, Y2)                              % Can capture white promoted pawn
    ).

% Met move for white Met
valid_move(wQ, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(wQ, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(bp, X2, Y2)            % Can capture black pawn
    ;   piece(bR, X2, Y2)            % Can capture black rook
    ;   piece(bN, X2, Y2)            % Can capture black knight
    ;   piece(bQ, X2, Y2)            % Can capture black queen
    ;   piece(bK, X2, Y2)            % Can capture black king
    ;   piece(bB, X2, Y2)            % Can capture black bishop
    ;   piece(bL, X2, Y2)            % Can capture black promoted pawn
    ).

% Met move for black Met
valid_move(bQ, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(bQ, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(wp, X2, Y2)            % Can capture white pawn
    ;   piece(wR, X2, Y2)            % Can capture white rook
    ;   piece(wN, X2, Y2)            % Can capture white knight
    ;   piece(wQ, X2, Y2)            % Can capture white queen
    ;   piece(wK, X2, Y2)            % Can capture white king
    ;   piece(wB, X2, Y2)            % Can capture white bishop
    ;   piece(wL, X2, Y2)            % Can capture white promoted pawn
    ).

% Knight move for white knight
valid_move(wN, X, Y, X2, Y2) :- 
    in_boundaries(X, Y), in_boundaries(X2, Y2),
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
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(bp, X2, Y2)            % Can capture black pawn
    ;   piece(bR, X2, Y2)            % Can capture black rook
    ;   piece(bN, X2, Y2)            % Can capture black knight
    ;   piece(bQ, X2, Y2)            % Can capture black queen
    ;   piece(bB, X2, Y2)            % Can capture black bishop
    ;   piece(bK, X2, Y2)            % Can capture black king
    ;   piece(bL, X2, Y2)            % Can capture black promoted pawn
    ).

% Knight move for black knight
valid_move(bN, X, Y, X2, Y2) :- 
    in_boundaries(X, Y), in_boundaries(X2, Y2),
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
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(wp, X2, Y2)            % Can capture white pawn
    ;   piece(wR, X2, Y2)            % Can capture white rook
    ;   piece(wN, X2, Y2)            % Can capture white knight
    ;   piece(wQ, X2, Y2)            % Can capture white queen
    ;   piece(wB, X2, Y2)            % Can capture white bishop
    ;   piece(wK, X2, Y2)            % Can capture white king
    ;   piece(wL, X2, Y2)            % Can capture white promoted pawn
    ).

% King move for white king
valid_move(wK, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(wK, X, Y),
    abs(X2 - X) =< 1,
    abs(Y2 - Y) =< 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(bp, X2, Y2)            % Can capture black pawn
    ;   piece(bR, X2, Y2)            % Can capture black rook
    ;   piece(bN, X2, Y2)            % Can capture black knight
    ;   piece(bB, X2, Y2)            % Can capture black bishop
    ;   piece(bQ, X2, Y2)            % Can capture black queen
    ;   piece(bK, X2, Y2)            % Can capture black king
    ;   piece(bL, X2, Y2)            % Can capture black promoted pawn
    ).

% King move for black king
valid_move(bK, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(bK, X, Y),
    abs(X2 - X) =< 1,
    abs(Y2 - Y) =< 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(wp, X2, Y2)            % Can capture white pawn
    ;   piece(wR, X2, Y2)            % Can capture white rook
    ;   piece(wN, X2, Y2)            % Can capture white knight
    ;   piece(wB, X2, Y2)            % Can capture white bishop
    ;   piece(wQ, X2, Y2)            % Can capture white queen
    ;   piece(wK, X2, Y2)            % Can capture white king
    ;   piece(wL, X2, Y2)            % Can capture white promoted pawn
    ).

% Promoted Bia move for white Bia
valid_move(wL, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(wL, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(bp, X2, Y2)            % Can capture black pawn
    ;   piece(bR, X2, Y2)            % Can capture black rook
    ;   piece(bN, X2, Y2)            % Can capture black knight
    ;   piece(bB, X2, Y2)            % Can capture black bishop
    ;   piece(bQ, X2, Y2)            % Can capture black queen
    ;   piece(bK, X2, Y2)            % Can capture black king
    ;   piece(bL, X2, Y2)            % Can capture black promoted pawn
    ).

% Promoted Bia move for black Bia
valid_move(bL, X, Y, X2, Y2) :-
    in_boundaries(X, Y), in_boundaries(X2, Y2),
    piece(bL, X, Y),
    abs(X2 - X) =:= 1,               % Ensure the move is exactly one square diagonally
    abs(Y2 - Y) =:= 1,
    (   \+ piece(_, X2, Y2)          % Destination is empty
    ;   piece(wp, X2, Y2)            % Can capture black pawn
    ;   piece(wR, X2, Y2)            % Can capture black rook
    ;   piece(wN, X2, Y2)            % Can capture black knight
    ;   piece(wB, X2, Y2)            % Can capture black bishop
    ;   piece(wQ, X2, Y2)            % Can capture black queen
    ;   piece(wK, X2, Y2)            % Can capture black king
    ;   piece(wL, X2, Y2)            % Can capture black promoted pawn
    ).

all_possible_moves(Pieces, Moves) :-
    findall([(StartRow, StartCol), (EndRow, EndCol)],
        (member(Piece, Pieces),
         between(0, 7, StartRow),
         between(0, 7, StartCol),
         piece(Piece, StartRow, StartCol),
         between(0, 7, EndRow),
         between(0, 7, EndCol),
         valid_move(Piece, StartRow, StartCol, EndRow, EndCol),
         \+ (StartRow = EndRow, StartCol = EndCol)),  % Exclude moves to same position
        Moves).

move_valid(Piece, X1, Y1, X2, Y2) :- valid_move(Piece, X1, Y1, X2, Y2).

promotion_row(white, 0).
promotion_row(black, 7).

can_promote(Piece, Color, Row, _) :-
    Piece = p,
    promotion_row(Color, Row).

king_in_check(Color) :-
    piece(King, KingRow, KingCol),
    King = Color + "K",
    piece(AttackingPiece, AttackRow, AttackCol),
    \+ same_color(AttackingPiece, King),  % Piece must be of opposite color
    valid_move(AttackingPiece, AttackRow, AttackCol, KingRow, KingCol).

% Helper predicate to get piece color
piece_color(Piece, Color) :-
    atom_chars(Piece, [Color|_]).

% Check if a square is under attack
square_under_attack(Row, Col, DefendingColor) :-
    piece(AttackingPiece, AttackRow, AttackCol),
    piece_color(AttackingPiece, AttackColor),
    AttackColor \= DefendingColor,
    valid_move(AttackingPiece, AttackRow, AttackCol, Row, Col).

% Check if a king is in check
king_in_check(Color) :-
    piece(King, KingRow, KingCol),
    atom_chars(King, [Color, 'K']),  % Ensure this matches your king representation
    square_under_attack(KingRow, KingCol, Color).

% Simulate a move and track board state
simulate_move(Piece, FromRow, FromCol, ToRow, ToCol, Result) :-
    % Store current state
    findall(piece(P, R, C), piece(P, R, C), CurrentState),
    
    % Make the move
    retract(piece(Piece, FromRow, FromCol)),
    (piece(CapturedPiece, ToRow, ToCol) -> 
        retract(piece(CapturedPiece, ToRow, ToCol))
    ;   true),
    assertz(piece(Piece, ToRow, ToCol)),
    
    % Check the result
    (call(Result) ->
        ReturnValue = true
    ;   ReturnValue = false),
    
    % Restore original state
    retractall(piece(_, _, _)),
    maplist(assertz, CurrentState),
    
    ReturnValue.

% Check if a move would expose the king to check
move_exposes_check(Piece, FromRow, FromCol, ToRow, ToCol) :-
    piece_color(Piece, Color),
    simulate_move(Piece, FromRow, FromCol, ToRow, ToCol, king_in_check(Color)).

% Find all legal moves for a piece
legal_move(Piece, FromRow, FromCol, ToRow, ToCol) :-
    piece(Piece, FromRow, FromCol),
    piece_color(Piece, _Color),
    valid_move(Piece, FromRow, FromCol, ToRow, ToCol),
    \+ move_exposes_check(Piece, FromRow, FromCol, ToRow, ToCol).

% Get all legal moves for a color
all_legal_moves(Color, Moves) :-
    findall(
        move(Piece, FromRow, FromCol, ToRow, ToCol),
        (
            piece(Piece, FromRow, FromCol),
            piece_color(Piece, Color),
            legal_move(Piece, FromRow, FromCol, ToRow, ToCol)
        ),
        Moves
    ).

% Check for checkmate
is_checkmate(Color) :-
    king_in_check(Color),
    all_legal_moves(Color, []).  % No legal moves available

% Check for stalemate
is_stalemate(Color) :-
    \+ king_in_check(Color),
    all_legal_moves(Color, []).  % No legal moves available

% Find moves that escape check
check_escape_moves(Color, Moves) :-
    findall(
        move(Piece, FromRow, FromCol, ToRow, ToCol),
        (
            piece(Piece, FromRow, FromCol),
            piece_color(Piece, Color),
            legal_move(Piece, FromRow, FromCol, ToRow, ToCol),
            \+ simulate_move(Piece, FromRow, FromCol, ToRow, ToCol, king_in_check(Color))
        ),
        Moves
    ).