:- [piece_rule].

:- dynamic piece/3.

% Detect if the black king is in check
in_check(bK) :-
    piece(bK, X, Y),
    piece(WhitePiece, X2, Y2),
    member(WhitePiece, [wp, wR, wN, wB, wQ, wK]),
    valid_move(WhitePiece, X2, Y2, X, Y).

% Detect if the white king is in check
in_check(wK) :-
    piece(wK, X, Y),
    piece(BlackPiece, X2, Y2),
    member(BlackPiece, [bp, bR, bN, bB, bQ, bK]),
    valid_move(BlackPiece, X2, Y2, X, Y).

% Check if a position is safe for the white king
safe_position_wk(X, Y) :-
    \+ (piece(OpponentPiece, X2, Y2),
        member(OpponentPiece, [bp, bR, bN, bB, bQ, bK]),
        valid_move(OpponentPiece, X2, Y2, X, Y)
    ).

% Check if a position is safe for the black king
safe_position_bk(X, Y) :-
    \+ (piece(OpponentPiece, X2, Y2),
        member(OpponentPiece, [wp, wR, wN, wB, wQ, wK]),
        valid_move(OpponentPiece, X2, Y2, X, Y)
    ).

escape_check(King) :-
    piece(King, X, Y),
    member((X2, Y2), [(X+1, Y), (X-1, Y), (X, Y+1), (X, Y-1), (X+1, Y+1), (X+1, Y-1), (X-1, Y+1), (X-1, Y-1)]),
    valid_move(King, X, Y, X2, Y2),
    \+ in_check(King).


% Check if any friendly piece can block the check or capture the attacking piece
can_block_check(King) :-
    piece(King, KingX, KingY),
    in_check(King),
    % Find any piece that could potentially block the check
    piece(FriendlyPiece, X, Y),
    FriendlyPiece \= King,  % Do not include the king itself
    valid_move(FriendlyPiece, X, Y, KingX, KingY),
    % Ensure moving this piece will no longer leave the king in check
    (   \+ in_check(King)
    ).

% Checkmate if the king is in check, cannot escape, and no other move can block the check
checkmate(bK) :-
    in_check(bK),
    \+ escape_check(bK),
    \+ can_block_check(bK).

checkmate(wK) :-
    in_check(wK),
    \+ escape_check(wK),
    \+ can_block_check(wK).
