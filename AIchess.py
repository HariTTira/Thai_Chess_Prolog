from pyswip import Prolog
import ChessEngine
import random

CHECKMATE = 1000
STATLEMENT = 0
DEPTH = 1
pieceScore = {"K": 0, "Q": 3, "R": 5, "B": 4, "N": 3, "p": 1, "L": 3}


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

# def findBestMove(gs, validMoves, opponenentsMoves):
#     turnMultiplier = 1 if gs.whiteToMove else -1
#     opponentMinMaxScore = 1000
#     bestPlayerMove = None
#     random.shuffle(validMoves)
#     for playerMove in validMoves:
#         print("AI ANALYSING")
#         playermakemove = ChessEngine.Move(playerMove[0], playerMove[1], gs.board)
#         gs.makeMove(playermakemove)
#         if gs.checkmate:
#             opponentMaxScore = -CHECKMATE
#         elif gs.stalemate:
#             opponentMaxScore = STATLEMENT
#         else:
#             opponentMaxScore = -CHECKMATE
#         for opponetMove in opponenentsMoves:
#             opponentmakemove = ChessEngine.Move(opponetMove[0], opponetMove[1], gs.board)
#             gs.makeMove(opponentmakemove)
#             if gs.checkmate:
#                 score = CHECKMATE
#             elif gs.stalemate:
#                 score = STATLEMENT
#             else:
#                 score = (turnMultiplier * score_material(gs.board)) * -1
            
#             if score > opponentMaxScore:
#                 opponentMaxScore = score
#             gs.undoMove()

#         if  opponentMaxScore < opponentMinMaxScore:
#             opponentMinMaxScore = opponentMaxScore
#             bestPlayerMove = playerMove
#         gs.undoMove()
#     return bestPlayerMove

def findBestMoveMinMax(gs, validMoves):
    global nextMove
    nextMove = None
    findMoveMinMax(gs, validMoves, DEPTH, gs.whiteToMove)
    return nextMove

def findMoveMinMax(gs, validMoves, depth, whiteToMove):
    print("AI THINKING")
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

def scoreBoard(gs):
    if gs.checkmate:
        if gs.whiteToMove:
            return -CHECKMATE
        else:
            return CHECKMATE
    elif gs.stalemate:
        return STATLEMENT

    score = 0
    for row in gs.board:
        for square in row:
            if square[0] == 'w':
                score += pieceScore[square[1]]
            elif square[0] == 'b':
                score -= pieceScore[square[1]]
    return score

def score_material(board):
    score = 0
    for row in board:
        for square in row:
            if square[0] == 'w':
                score += pieceScore[square[1]]
            elif square[0] == 'b':
                score -= pieceScore[square[1]]
    return score
