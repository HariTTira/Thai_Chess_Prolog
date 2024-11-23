from pyswip import Prolog
import ChessEngine
import random
import re
from pyswip.easy import registerForeign

prolog = Prolog()
prolog.consult("aimove.pl")

pieceScore = {"K": 0, "Q": 3, "R": 5, "B": 4, "N": 3, "p": 1, "L": 3}
CHECKMATE = 1000
STATLEMENT = 0
DEPTH = 1
knightScores = [
    [1, 1, 1, 1, 1, 1, 1, 1],
    [1, 2, 2, 2, 2, 2, 2, 1],
    [1, 2, 3, 3, 3, 3, 2, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 2, 3, 3, 3, 3, 2, 1],
    [1, 2, 2, 2, 2, 2, 2, 1],
    [1, 1, 1, 1, 1, 1, 1, 1],
]

bishopScores = [
    [4, 3, 2, 1, 1, 2, 3, 4],
    [3, 4, 3, 2, 2, 3, 4, 3],
    [2, 3, 4, 3, 3, 4, 3, 2],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [2, 3, 4, 3, 3, 4, 3, 2],
    [3, 4, 3, 2, 2, 3, 4, 3],
    [4, 3, 2, 1, 1, 2, 3, 4],
]

queenScores = [
    [1, 1, 1, 3, 1, 1, 1, 1],
    [1, 2, 3, 3, 3, 1, 1, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 1, 2, 3, 3, 1, 1, 1],
    [1, 1, 1, 3, 1, 1, 1, 1],
]

promotedScores = [
    [1, 1, 1, 3, 1, 1, 1, 1],
    [1, 2, 3, 3, 3, 1, 1, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 2, 3, 3, 3, 2, 2, 1],
    [1, 4, 3, 3, 3, 4, 2, 1],
    [1, 1, 2, 3, 3, 1, 1, 1],
    [1, 1, 1, 3, 1, 1, 1, 1],
]

rockScores = [
    [4, 3, 4, 4, 4, 4, 3, 4],
    [4, 4, 4, 4, 4, 4, 4, 4],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [4, 4, 4, 4, 4, 4, 4, 4],
    [4, 3, 4, 4, 4, 4, 3, 4],
]

whitePawnScores = [
    [8, 8, 8, 8, 8, 8, 8, 8],
    [8, 8, 8, 8, 8, 8, 8, 8],
    [5, 6, 6, 7, 7, 6, 6, 5],
    [2, 3, 3, 5, 5, 3, 3, 2],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [1, 1, 1, 0, 0, 1, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 0],
]

blackPawnScores = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 0, 0, 1, 1, 1],
    [1, 1, 2, 3, 3, 2, 1, 1],
    [1, 2, 3, 4, 4, 3, 2, 1],
    [2, 3, 3, 5, 5, 3, 3, 2],
    [5, 6, 6, 7, 7, 6, 6, 5],
    [8, 8, 8, 8, 8, 8, 8, 8],
    [8, 8, 8, 8, 8, 8, 8, 8],
]

piecePositionScores = {
    "N": knightScores,
    "B": bishopScores,
    "Q": queenScores,
    "R": rockScores,
    "bp": blackPawnScores,
    "wp": whitePawnScores,
    "L": promotedScores
}


def findBestMoveMinMax(gs, validMoves):
    global nextMove
    nextMove = None
    # findMoveMinMax(gs, validMoves, DEPTH, gs.whiteToMove)
    # findMoveNegaMax(gs, validMoves, DEPTH, 1 if gs.whiteToMove else -1)
    # findMoveNegaMaxAlphaBeta(gs, validMoves, DEPTH, -CHECKMATE, CHECKMATE, 1 if gs.whiteToMove else -1)
    # findMoveMinimaxAlphaBeta(gs, validMoves, DEPTH, -CHECKMATE, CHECKMATE, gs.whiteToMove)
    findMoveMinimaxAlphaBetaProlog(validMoves, DEPTH, -CHECKMATE, CHECKMATE, gs.whiteToMove)

    return nextMove


def findMoveMinimaxAlphaBeta(gs, validMoves, depth, alpha, beta, whiteToMove):
    global nextMove
    if depth == 0:
        return scoreBoard(gs)

    if whiteToMove:
        maxScore = -1000
        for move in validMoves:
            playermakemove = ChessEngine.Move(move[0], move[1], gs.board)
            gs.makeMove(playermakemove)

            nextMoves = gs.getvalidMoves()
            score = findMoveMinimaxAlphaBeta(gs, nextMoves, depth-1, alpha, beta, False)

            if score > maxScore:
                maxScore = score
                if depth == DEPTH:  # If at the root level, store the best move
                    nextMove = move
            gs.undoMove()

            if maxScore > alpha:
                alpha = maxScore
            if alpha >= beta:
                break
        return maxScore

    else:
        minScore = 1000
        for move in validMoves:
            playermakemove = ChessEngine.Move(move[0], move[1], gs.board)
            gs.makeMove(playermakemove)

            nextMoves = gs.getvalidMoves()
            score = findMoveMinimaxAlphaBeta(gs, nextMoves, depth-1, alpha, beta, True)

            if score < minScore:
                minScore = score
                if depth == DEPTH:
                    nextMove = move
            gs.undoMove()

            if minScore < beta:
                beta = minScore
            if alpha >= beta:
                break
        return minScore

def findMoveMinimaxAlphaBetaProlog(valid_moves, depth, alpha, beta, white_to_move):    
    # Convert game state and valid moves into Prolog-compatible format
    # You need to define how `gs` and `valid_moves` are represented in Prolog terms.
    # Query the Prolog predicate `find_best_move`
    query = f"find_best_move({valid_moves}, BestMove)"
    print(valid_moves)
    results = list(prolog.query(query))
    if results:
        best_move = results[0]['BestMove']  # Retrieve the best move
        return best_move
    else:
        raise Exception("Prolog did not return any result.")


def scoreBoard(gs):
    if gs.checkmate:
        if gs.whiteToMove:
            return -CHECKMATE  # black wins
        else:
            return CHECKMATE  # white wins
    elif gs.stalemate:
        return STATLEMENT
    score = 0
    for row in range(len(gs.board)):
        for col in range(len(gs.board[row])):
            square = gs.board[row][col]
            if square != "--":
                pps = 0
                fac = 0.1
                color = square[0]
                piece = square[1]
                if piece != "K":
                    pps += (
                        piecePositionScores[piece if piece != "p" else square][row][col]
                        * fac
                    )
                if color == "w":
                    score += pieceScore[piece] + pps
                elif color == "b":
                    score -= pieceScore[piece] + pps
    return score

def convert_move_to_prolog_format(move):
    """Convert a Python move tuple to Prolog format."""
    start, end = move
    return f"pos({start[0]},{start[1]},{end[0]},{end[1]})"

def convert_prolog_to_python_format(prolog_move):
    """Convert a Prolog move back to Python tuple format."""
    move_str = str(prolog_move)
    nums = [int(n) for n in move_str.replace('pos(', '').replace(')', '').split(',')]
    return ((nums[0], nums[1]), (nums[2], nums[3]))

def findRandomMove(valid_moves):
    if not valid_moves:
        raise ValueError("No valid moves found.")
        
    prolog = Prolog()
    prolog.consult("Aimove.pl")  # Load your Prolog file

    moves_str = [convert_move_to_prolog_format(move) for move in valid_moves]
    valid_moves_prolog = f"[{','.join(moves_str)}]"

    query = f"find_random_move({valid_moves_prolog}, RandomMove)"
    result = list(prolog.query(query))

    if result:
        # Convert the Prolog result back to Python format
        return convert_prolog_to_python_format(result[0]['RandomMove'])
    else:
        raise ValueError("No valid moves found.")
    
def findMoveMinMax(gs, validMoves, depth, whiteToMove):
    global nextMove
    if depth == 0:
        return score_material(gs.board)

    if whiteToMove:
        maxScore = -CHECKMATE
        for move in validMoves:
            playermakemove = ChessEngine.Move(move[0], move[1], gs.board)
            gs.makeMove(playermakemove)
            nextMoves = gs.getvalidMoves()
            score = findMoveMinMax(gs, nextMoves, depth - 1, False)
            if score > maxScore:
                maxScore = score
                if depth == DEPTH:
                    nextMove = move
            gs.undoMove()
        return maxScore
    else:
        minScore = CHECKMATE
        for move in validMoves:
            playermakemove = ChessEngine.Move(move[0], move[1], gs.board)
            gs.makeMove(playermakemove)
            nextMoves = gs.getvalidMoves()
            score = findMoveMinMax(gs, nextMoves, depth - 1, True)
            if score < minScore:
                minScore = score
                if depth == DEPTH:
                    nextMove = move
            gs.undoMove()
        return minScore

def score_material(board):
    score = 0
    for row in board:
        for square in row:
            if square[0] == 'w':
                score += pieceScore[square[1]]
            elif square[0] == 'b':
                score -= pieceScore[square[1]]
    return score

def findMoveNegaMax(gs, validMoves, depth, turnMultiplier):
    global nextMove
    if depth == 0:
        return turnMultiplier * scoreBoard(gs)
    maxScore = -1000
    for move in validMoves:
        playermakemove = ChessEngine.Move(move[0], move[1], gs.board)
        gs.makeMove(playermakemove)
        nextMoves = gs.getvalidMoves()
        score = -findMoveNegaMax(gs, nextMoves, depth-1, -turnMultiplier)
        if score > maxScore:
            maxScore = score
            if depth == DEPTH:
                nextMove = move
        gs.undoMove()
    return maxScore

def findMoveNegaMaxAlphaBeta(gs, validMoves, depth, alpha, beta, turnMultiplier):
    global nextMove
    if depth == 0:
        return turnMultiplier * scoreBoard(gs)
    
    maxScore = -1000
    for move in validMoves:
        playermakemove = ChessEngine.Move(move[0], move[1], gs.board)
        gs.makeMove(playermakemove)
        nextMoves = gs.getvalidMoves()
        score = -findMoveNegaMaxAlphaBeta(gs, nextMoves, depth-1, -beta, -alpha, -turnMultiplier)
        if score > maxScore:
            maxScore = score
            if depth == DEPTH:
                nextMove = move
        gs.undoMove()
        if maxScore > alpha:
            alpha = maxScore
        if alpha >= beta:
            break
    return maxScore



def scoreBoard(gs):
    if gs.checkmate:
        if gs.whiteToMove:
            return -CHECKMATE  # black wins
        else:
            return CHECKMATE  # white wins
    elif gs.stalemate:
        return STATLEMENT
    score = 0
    for row in range(len(gs.board)):
        for col in range(len(gs.board[row])):
            square = gs.board[row][col]
            if square != "--":
                pps = 0
                fac = 0.1
                color = square[0]
                piece = square[1]
                if piece != "K":
                    pps += (
                        piecePositionScores[piece if piece != "p" else square][row][col]
                        * fac
                    )
                if color == "w":
                    score += pieceScore[piece] + pps
                elif color == "b":
                    score -= pieceScore[piece] + pps
    return score