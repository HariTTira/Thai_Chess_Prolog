from pyswip import Prolog
from ChessMain import pawn_promotion_ui

class GameState():
    def __init__(self, screen):
        self.screen = screen  # Pass the screen object
        self.inCheck = False
        self.checkmate = False
        self.board = [
            ["bR", "bN", "bB", "bQ", "bK", "bB", "bN", "bR"],
            ["--", "--", "--", "--", "--", "--", "--", "--"],
            ["bp", "bp", "bp", "bp", "bp", "bp", "bp", "bp"],
            ["--", "--", "--", "--", "--", "--", "--", "--"],
            ["--", "--", "--", "--", "--", "--", "--", "--"],
            ["wp", "wp", "wp", "wp", "wp", "wp", "wp", "wp"],
            ["--", "--", "--", "--", "--", "--", "--", "--"],
            ["wR", "wN", "wB", "wK", "wQ", "wB", "wN", "wR"]
        ]
        self.whiteToMove = True
        self.moveLog = []
        
    
        # Initialize Prolog engine
        self.prolog = Prolog()
        self.prolog.consult("piece_rule.pl")  # Load Prolog rules

    def makeMove(self, move):
        # Extract piece and move information
        piece = move.pieceMoved
        startRow, startCol = move.startRow, move.startCol
        endRow, endCol = move.endRow, move.endCol

        first = self.board[startRow][startCol]
        second = self.board[endRow][endCol]

        if first[0] == second[0]:
            return "same_color"

        print("piece: " + piece)
        print("startRow: " + str(startRow) + " startCol: " + str(startCol))
        print("endRow: " + str(endRow) + " endCol: " + str(endCol))
        
        if self.myMove(piece):
            valid_move = list(self.prolog.query(f"move_valid({piece}, {startRow}, {startCol}, {endRow}, {endCol})"))
            print("is it valid: " + str(valid_move))
            if valid_move:
                # If move is valid, update the board
                self.board[startRow][startCol] = "--"
                self.board[endRow][endCol] = piece
                self.prolog.retract(f"piece({piece}, {startRow}, {startCol})")
                self.prolog.assertz(f"piece({piece}, {endRow}, {endCol})")
                self.moveLog.append(move)
                self.whiteToMove = not self.whiteToMove
            # Check for promotion
                if piece[1] == "p":  # Pawn
                    color = "white" if piece[0] == 'w' else "black"
                    promotion_check = list(self.prolog.query(f"can_promote({piece[1]}, {color}, {endRow}, {endCol})"))
                    print("promotion_check: " ,promotion_check)
                    if promotion_check:
                        promoted_piece = pawn_promotion_ui(self.screen, color)
                        self.board[endRow][endCol] = promoted_piece
                        self.prolog.retract(f"piece({piece}, {endRow}, {endCol})")
                        self.prolog.assertz(f"piece({promoted_piece}, {endRow}, {endCol})") 
                        
                self.checkForCheck()
                return True
            else:
                return False
        else:
            return False
        
    def myMove(self, piece):
        piece = piece[0]
        if self.whiteToMove == True and piece == "w":
            return True 
        elif self.whiteToMove == False and piece == "b":
            return True
        else:
            return False

    def checkForCheck(self):
        king = 'bK' if not self.whiteToMove else 'wK'
        
        try:
            # Check if the king is in check
            check_query = f"in_check({king})"
            check_result = list(self.prolog.query(check_query))
            self.inCheck = len(check_result) > 0

            # If in check, determine if checkmate
            if self.inCheck:
                checkmate_query = f"checkmate({king})"
                checkmate_result = list(self.prolog.query(checkmate_query))
                self.checkmate = len(checkmate_result) > 0
        except Exception as e:
            print(f"Prolog query error: {e}")
            self.inCheck = False
            self.checkmate = False


class Move():
    ranksToRows = {"1":7, "2":6, "3":5, "4": 4 , "5":3, "6":2, "7": 1, "8":0}
    rowsToRanks = {v:k for k, v in ranksToRows.items()}

    filesToCols = {"a":0, "b": 1, "c": 2, "d": 3, "e": 4, "f": 5, "g": 6, "h": 7}
    colsToFiles = {v:k for k, v in filesToCols.items()}

    def __init__(self, startSq, endSq, board):
        self.startRow = startSq[0]
        self.startCol = startSq[1]
        self.endRow = endSq[0]
        self.endCol = endSq[1]
        self.pieceMoved = board[self.startRow][self.startCol]
        self.pieceCaptured = board[self.endRow][self.endCol]
    
    def getChessNotation(self):
        return self.getRankFile(self.startRow, self.startCol) + self.getRankFile(self.endRow,self.endCol)
    
    def getRankFile(self,r,c):
        return self.colsToFiles[c] + self.rowsToRanks[r]
