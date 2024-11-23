:- discontiguous piece_value/2.
:- discontiguous score_square/2.
:- discontiguous score_row/2.
:- discontiguous score_board/2.
:- dynamic current_move_number/1.
:- [piece_rule]. 

current_move_number(1)

find_random_move(ValidMoves, RandomMove) :-
    length(ValidMoves, Length),
    Length > 0,
    random_between(1, Length, Index), % Generate a random index between 1 and Length
    nth1(Index, ValidMoves, RandomMove). % Retrieve the move at the random index

make_move(X, Y, X2, Y2) :-
    % Get current move number
    write('Attempting to move from '), write(X), write(','), write(Y), nl,
    piece(Piece, X, Y),
    format('Piece: ~w, Current Position: (~d, ~d)~n', [Piece, X, Y]),

    % Get current move number and calculate next move number
    (current_move_number(N) -> 
        NextN is N + 1
    ;
        NextN = 1
    ),

    % Check for captured piece
    (piece(CapturedPiece, X2, Y2) -> 
        true  % A piece exists at X2, Y2, so it can be captured
    ; 
        CapturedPiece = none
    ),

    % Check if the move is valid
    valid_move(Piece, X, Y, X2, Y2),

    % Remove captured piece if any
    (CapturedPiece \= none -> 
        retract(piece(CapturedPiece, X2, Y2))
    ; 
        true
    ),

    % Move the piece (retract old position and assert new one)
    retract(piece(Piece, X, Y)),
    assert(piece(Piece, X2, Y2)),

    % Record the move in history
    assert(move_history(NextN, Piece, X, Y, X2, Y2, CapturedPiece)),

    % Update current move number
    retract(current_move_number(N)),
    assert(current_move_number(NextN)).


undo_move :-
    current_move_number(N),
    N > 0,
    move_history(N, Piece, X, Y, X2, Y2, CapturedPiece),

    % Move piece back
    retract(piece(Piece, X2, Y2)),
    assert(piece(Piece, X, Y)),
    % Restore captured piece if any
    (CapturedPiece \= none ->
        assert(piece(CapturedPiece, X2, Y2))
    ;
        true
    ),
    % Remove move from history
    retract(move_history(N, Piece, X, Y, X2, Y2, CapturedPiece)),
    PrevN is N - 1,
    retract(current_move_number(N)),
    assert(current_move_number(PrevN)).

% Define the mapping of pieces to their types
piece_type('wp', 'p').  % White Pawn
piece_type('bp', 'p').  % Black Pawn
piece_type('wQ', 'Q').  % White Queen
piece_type('bQ', 'Q').  % Black Queen
piece_type('wR', 'R').  % White Rook
piece_type('bR', 'R').  % Black Rook
piece_type('wB', 'B').  % White Bishop
piece_type('bB', 'B').  % Black Bishop
piece_type('wN', 'N').  % White Knight
piece_type('bN', 'N').  % Black Knight
piece_type('wK', 'K').  % White King
piece_type('bK', 'K').  % Black King
piece_type('wL', 'L').  % Promoted White Pawn (Lance)
piece_type('bL', 'L').  % Promoted Black Pawn (Lance)

piece_color('wp', white).
piece_color('bp', black).
piece_color('wQ', white).
piece_color('bQ', black).
piece_color('wR', white).
piece_color('bR', black).
piece_color('wB', white).
piece_color('bB', black).
piece_color('wN', white).
piece_color('bN', black).
piece_color('wK', white).
piece_color('bK', black).
piece_color('wL', white).
piece_color('bL', black).

piece_score('K', 0).
piece_score('Q', 3).
piece_score('R', 5).
piece_score('B', 4).
piece_score('N', 3).
piece_score('p', 1).
piece_score('L', 3).

% Constants
checkmate_score(1000).
stalemate_score(0).
max_depth(1).

knight_position_scores([
    [2, 3, 4, 4, 4, 4, 3, 2],
    [3, 4, 6, 6, 6, 6, 4, 3],
    [4, 6, 8, 8, 8, 8, 6, 4],
    [4, 6, 8, 10, 10, 8, 6, 4],
    [4, 6, 8, 10, 10, 8, 6, 4],
    [4, 6, 8, 8, 8, 8, 6, 4],
    [3, 4, 6, 6, 6, 6, 4, 3],
    [2, 3, 4, 4, 4, 4, 3, 2]
]).


% Position scores for Bishop
bishop_position_scores([
    [4, 3, 2, 1, 1, 2, 3, 4],
    [3, 4, 3, 2, 2, 3, 4, 3],
    [2, 3, 4, 3, 3, 4, 3, 2],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [2, 3, 4, 3, 3, 4, 3, 2],
    [3, 4, 3, 2, 2, 3, 4, 3],
    [4, 3, 2, 1, 1, 2, 3, 4]
]).

% Position scores for Queen
queen_position_scores([
    [1, 1, 1, 3, 3, 1, 1, 1],
    [1, 2, 3, 3, 3, 1, 1, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 1, 2, 3, 3, 1, 1, 1],
    [1, 1, 1, 3, 3, 1, 1, 1]
]).

% Position scores for Promoted pieces (Lance)
promoted_position_scores([
    [1, 1, 1, 3, 1, 1, 1, 1],
    [1, 2, 3, 3, 3, 1, 1, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 1, 2, 3, 3, 1, 1, 1],
    [1, 1, 1, 3, 1, 1, 1, 1]
]).

% Position scores for Rook
rook_position_scores([
    [4, 3, 4, 4, 4, 4, 3, 4],
    [4, 4, 4, 4, 4, 4, 4, 4],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [4, 4, 4, 4, 4, 4, 4, 4],
    [4, 3, 4, 4, 4, 4, 3, 4]
]).

% Position scores for White Pawns
white_pawn_position_scores([
    [8, 8, 8, 8, 8, 8, 8, 8],
    [8, 8, 8, 8, 8, 8, 8, 8],
    [5, 6, 6, 7, 7, 6, 6, 5],
    [2, 3, 3, 5, 5, 3, 3, 2],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [1, 1, 1, 0, 0, 1, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 0]
]).

% Position scores for Black Pawns
black_pawn_position_scores([
    [0, 0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 0, 0, 1, 1, 1],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [2, 3, 3, 5, 5, 3, 3, 2],
    [5, 6, 6, 7, 7, 6, 6, 5],
    [8, 8, 8, 8, 8, 8, 8, 8],
    [8, 8, 8, 8, 8, 8, 8, 8]
]).

% Updated position_score predicate to handle all piece types
position_score('B', Row, Col, Score) :-
    bishop_position_scores(ScoreMatrix),
    nth0(Row, ScoreMatrix, RowScores),
    nth0(Col, RowScores, Score).

position_score('Q', Row, Col, Score) :-
    queen_position_scores(ScoreMatrix),
    nth0(Row, ScoreMatrix, RowScores),
    nth0(Col, RowScores, Score).

position_score('R', Row, Col, Score) :-
    rook_position_scores(ScoreMatrix),
    nth0(Row, ScoreMatrix, RowScores),
    nth0(Col, RowScores, Score).

position_score('L', Row, Col, Score) :-
    promoted_position_scores(ScoreMatrix),
    nth0(Row, ScoreMatrix, RowScores),
    nth0(Col, RowScores, Score).

position_score('p', Row, Col, Score) :-
    piece(Piece, Row, Col),
    piece_color(Piece, Color),
    (Color = white ->
        white_pawn_position_scores(ScoreMatrix)
    ;
        black_pawn_position_scores(ScoreMatrix)
    ),
    nth0(Row, ScoreMatrix, RowScores),
    nth0(Col, RowScores, Score).

% Keep the existing knight position scoring
position_score('N', Row, Col, Score) :-
    knight_position_scores(ScoreMatrix),
    nth0(Row, ScoreMatrix, RowScores),
    nth0(Col, RowScores, Score).

evaluate_board(Score) :-
    calculate_material_score(Score).

calculate_material_score(Score) :-
    findall(
        PieceScore,
        (
            piece(Piece, Row, Col),         % Get piece, row, and column.
            piece_type(Piece, Type),         % Get piece type.
            piece_color(Piece, Color),       % Get piece color (white or black).
            piece_score(Type, BaseScore),    % Get base score for piece type.
            position_score(Type, Row, Col, PosScore),  % Get positional score.
            Factor is 0.1,                   % Scaling factor for positional score.
            TotalScore is BaseScore + (PosScore * Factor),  % Calculate total score for piece.
            format("Piece: ~w, Base: ~w, Positional: ~w, Total: ~w, Color: ~w~n", 
                   [Piece, BaseScore, PosScore, TotalScore, Color]),
            % Accumulate score based on color.
            (Color = white -> 
                PieceScore = TotalScore  % Add score for white.
            ; 
                PieceScore is -TotalScore)  % Subtract score for black.
        ),
        Scores
    ),
    sum_list(Scores, Score).  % Sum all piece scores to get total score.

find_best_move(ValidMoves, BestMove) :-
    max_depth(Depth),
    checkmate_score(Checkmate),
    minimax_alpha_beta(ValidMoves, Depth, -Checkmate, Checkmate, true, Score, BestMove).

minimax_alpha_beta(_, 0, _, _, _, Score, _) :- 
    evaluate_board(Score).

minimax_alpha_beta(ValidMoves, Depth, Alpha, Beta, IsWhite, BestScore, BestMove) :-
    Depth > 0,
    (IsWhite -> 
        find_max_move(ValidMoves, Depth, Alpha, Beta, BestScore, BestMove)
    ;
        find_min_move(ValidMoves, Depth, Alpha, Beta, BestScore, BestMove)
    ).

find_max_move([Move|Moves], Depth, Alpha, Beta, BestScore, BestMove) :-
    make_move(Move),
    NextDepth is Depth - 1,
    all_possible_moves_white(white, Pieces, NextMoves),
    minimax_alpha_beta(NextMoves, NextDepth, Alpha, Beta, false, Score, _),
    undo_move,
    update_alpha(Score, Alpha, Move, Moves, Depth, Beta, BestScore, BestMove).

% Finding minimum score move
find_min_move([Move|Moves], Depth, Alpha, Beta, BestScore, BestMove) :-
    make_move(Move),
    NextDepth is Depth - 1,
    all_possible_moves_black(black, Pieces, NextMoves),
    minimax_alpha_beta(NextMoves, NextDepth, Alpha, Beta, true, Score, _),
    undo_move,
    update_beta(Score, Beta, Move, Moves, Depth, Alpha, BestScore, BestMove).

update_alpha(Score, Alpha, Move, Moves, Depth, Beta, BestScore, BestMove) :-
    AlphaNew is max(Alpha, Value),
    !,
    (AlphaNew >= Beta ->
        BestScore = AlphaNew,
        BestMove = Move
    ;
        find_max_move(Moves, Depth, AlphaNew, Beta, BestScore, BestMove)
    ).

% Update alpha value
update_alpha(Score, Alpha, Move, Moves, Depth, Beta, BestScore, BestMove) :-
    find_max_move(Moves, Depth, Alpha, Beta, BestScore, BestMove).

% Update beta value
update_beta(Score, Beta, Move, Moves, Depth, Alpha, BestScore, BestMove) :-
    BetaNew is min(Beta, Value),
    !,
    (BetaNew <= Alpha ->
        BestScore = BetaNew,
        BestMove = Move
    ;
        find_min_move(Moves, Depth, Alpha, BetaNew, BestScore, BestMove)
    ).

print_all_pieces :-
    forall(piece(Type, X, Y), format("piece(~w, ~d, ~d).~n", [Type, X, Y])).