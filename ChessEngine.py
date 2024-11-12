from pyswip import Prolog

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
        self.blackkinglocation = (0,4)
        self.whitekinglocation = (7,3)
        
    
        # Initialize Prolog engine
        self.prolog = Prolog()
        self.prolog.consult("piece_rule.pl")  # Load Prolog rules
        self.prolog.consult("checkdetect.pl")  # Load Prolog rules

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
            self.getvalidMoves()
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
                    print("promotion_check: " + str(promotion_check))
                    if promotion_check:
                        promoted_piece = f"{color[0]}L"  # "wL" for white, "bL" for black
                        self.board[endRow][endCol] = promoted_piece
                        self.prolog.retract(f"piece({piece}, {endRow}, {endCol})")
                        self.prolog.assertz(f"piece({promoted_piece}, {endRow}, {endCol})")
                if piece == "wK":  # Pawn
                    self.whitekinglocation = (endRow, endCol)
                elif piece == "bK":
                    self.blackkinglocation = (endRow, endCol)
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
        
    def get_all_valid_moves(self, pieces):
        # Query Prolog for all possible moves for the specified pieces
        query = f"all_possible_moves({pieces}, Moves)"
        results = list(self.prolog.query(query))
        
        # Process results and remove duplicates
        all_moves = set()  # Use a set to automatically remove duplicates
        
        if results:  # Check if we got any results
            moves = results[0]['Moves']  # Get the moves from the first result
            for move in moves:
                try:
                    # Clean and parse the start position
                    start_str = str(move[0]).replace('(', '').replace(')', '').replace(',', '').strip()
                    start_nums = [int(x) for x in start_str.split() if x.isdigit()]
                    
                    # Clean and parse the end position
                    end_str = str(move[1]).replace('(', '').replace(')', '').replace(',', '').strip()
                    end_nums = [int(x) for x in end_str.split() if x.isdigit()]
                    
                    if len(start_nums) == 2 and len(end_nums) == 2:
                        start_tuple = (start_nums[0], start_nums[1])
                        end_tuple = (end_nums[0], end_nums[1])
                        all_moves.add((start_tuple, end_tuple))
                except (ValueError, IndexError) as e:
                    print(f"Error processing move {move}: {e}")  # For debugging
                    continue
        
        # Convert back to list format and sort for consistency
        return sorted([list(move) for move in all_moves])
        
    def getvalidMoves(self):
        all_moves = self.get_all_valid_moves(['wp', 'bp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL', 'bR', 'bN', 'bB', 'bQ', 'bK', 'bL'])
        print(all_moves)

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
