from pyswip import Prolog

class GameState():
    def __init__(self, screen):
        self.screen = screen  # Pass the screen object
        self.inCheckStatus = False
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
        self.inCheck = False
        self.blackkinglocation = (0,4)
        self.whitekinglocation = (7,3)
    
        # Initialize Prolog engine
        self.prolog = Prolog()
        self.prolog.consult("piece_rule.pl")  # Load Prolog rules
        self.validMoves = self.getvalidMoves()

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
        
        move_found = False
        try:
            for valid_move in self.validMoves:
                if valid_move[0] == (startRow, startCol) and valid_move[1] == (endRow, endCol):
                    move_found = True
                    break
        except TypeError:
            # If there's any issue with validMoves, proceed without validation
            move_found = True

        if self.myMove(piece):
            # move_query = f"make_move({piece}, {startRow}, {startCol}, {endRow}, {endCol})"
            # result = list(self.prolog.query(move_query))
                # Store the current valid moves before making the move
                # If move is valid, update the board
            if move_found:
                self.board[startRow][startCol] = "--"
                self.board[endRow][endCol] = piece
                self.prolog.retract(f"piece({piece}, {startRow}, {startCol})")
                self.prolog.assertz(f"piece({piece}, {endRow}, {endCol})")
                self.updateBoard()
                self.inCheckStatus = self.checkForCheck()
                print("Check Status: ", self.inCheckStatus)
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

                if piece == "wK":  # King
                    self.whitekinglocation = (endRow, endCol)
                elif piece == "bK":
                    self.blackkinglocation = (endRow, endCol)
            
            return True
        return False

    def updateBoard(self):
        try:
            # Clear all existing piece facts in Prolog
            self.prolog.retractall("piece(_,_,_)")
            
            # Add all pieces from current board state
            for row in range(len(self.board)):
                for col in range(len(self.board[row])):
                    piece = self.board[row][col]
                    if piece != "--":  # Only add actual pieces, not empty squares
                        # Assert new piece fact to Prolog
                        self.prolog.assertz(f"piece('{piece}',{row},{col})")
            print("Update Done")
            
            return True
        except Exception as e:
            print(f"Error updating board state: {e}")
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
        return all_moves

    def checkForCheck(self):
        if self.whiteToMove:
            return self.squareUnderAttack(self.blackkinglocation[0], self.blackkinglocation[1])
        else:
            return self.squareUnderAttack(self.whitekinglocation[0], self.whitekinglocation[1])
    
    def squareUnderAttack(self,r,c):
        self.validMoves = self.getvalidMoves()
        print("ALL VALID MOVE:" ,self.validMoves)
        print("R: ", r, "C: ", c)
        for valid_move in self.validMoves:
            if valid_move[1] == (r, c):
                print("valid move[1]" , valid_move[1])
                print("R: ", r, "C: ", c)
                return True
            else:
                continue

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
