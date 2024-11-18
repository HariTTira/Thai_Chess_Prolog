from pyswip import Prolog

class GameState():
    def __init__(self, screen):
        self.screen = screen  # Pass the screen object
        self.inCheckStatus = False
        self.checkmate = False
        self.stalemate = False
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
        self.validMoves = []

    def makeMove(self, move):
        # Extract piece and move information
        piece = move.pieceMoved
        startRow, startCol = move.startRow, move.startCol
        endRow, endCol = move.endRow, move.endCol
        
        first = self.board[startRow][startCol]
        second = self.board[endRow][endCol]

        if first[0] == second[0]:
            return "same_color"

        # print("piece: " + piece)
        # print("startRow: " + str(startRow) + " startCol: " + str(startCol))
        # print("endRow: " + str(endRow) + " endCol: " + str(endCol))
        
        self.validMoves = self.getvalidMoves()
        move_found = False
        for valid_move in self.validMoves:
            if valid_move[0] == (startRow, startCol) and valid_move[1] == (endRow, endCol):
                move_found = True
                break

        if self.myMove(piece):
            if move_found:
                # print("MOVE FOUND")
                self.moveLog.append(move)
                self.board[startRow][startCol] = "--"
                self.board[endRow][endCol] = piece
                self.prolog.retract(f"piece({piece}, {startRow}, {startCol})")
                self.prolog.assertz(f"piece({piece}, {endRow}, {endCol})")
                self.updateBoard()
                # self.inCheckStatus = self.checkForCheck()
                self.whiteToMove = not self.whiteToMove

                if piece[1] == "p":
                    color = "white" if piece[0] == 'w' else "black"
                    promotion_check = list(self.prolog.query(f"can_promote({piece[1]}, {color}, {endRow}, {endCol})"))
                    if promotion_check:
                        promoted_piece = f"{color[0]}L"
                        self.board[endRow][endCol] = promoted_piece
                        self.prolog.retract(f"piece({piece}, {endRow}, {endCol})")
                        self.prolog.assertz(f"piece({promoted_piece}, {endRow}, {endCol})")


                if piece == "wK":
                    self.whitekinglocation = (endRow, endCol)
                elif piece == "bK":
                    self.blackkinglocation = (endRow, endCol)
                    
                self.inCheckStatus = self.checkForCheck()
                if self.isCheckmate():
                    self.checkmate = True
                    winner = "Black" if self.whiteToMove else "White"
                    print(f"Checkmate! {winner} wins!")
                elif self.isStalemate():
                    self.stalemate = True
                    print("Stalemate! The game is a draw.")      

                return True
            else:
                return False
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
            
            return True
        except Exception as e:
            print(f"Error updating board state: {e}")
            return False
                
    def undoMove(self):
        if len(self.moveLog) != 0:
            move = self.moveLog.pop()
            self.board[move.startRow][move.startCol] = move.pieceMoved
            self.board[move.endRow][move.endCol] = move.pieceCaptured
            # print("UNDO UPDATE")
            self.updateBoard()
            self.whiteToMove = not self.whiteToMove  # switch turns
            if move.pieceMoved == "wK":
                self.whiteKingLocation = (move.startRow, move.startCol)
            elif move.pieceMoved == "bK":
                self.blackKingLocation = (move.startRow, move.startCol)
            self.checkmate = False
            self.stalemate = False

    def myMove(self, piece):
        piece = piece[0]
        if self.whiteToMove == True and piece == "w":
            return True 
        elif self.whiteToMove == False and piece == "b":
            return True
        else:
            return False

    def isCheckmate(self):
        # First check if the current player's king is in check
        if not self.inCheckStatus:
            return False
        
        # Get all valid moves using the existing method
        if self.whiteToMove:
            valid_moves = self.get_all_valid_moves(['wp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL'])
        else:
            valid_moves = self.get_all_valid_moves(['bp','bR', 'bN', 'bB', 'bQ', 'bK', 'bL'])
        
        # print("checking Checkmate")
        for start_pos, end_pos in valid_moves:
            move = Move(start_pos, end_pos, self.board)
            if not self.wouldBeInCheck(move):
                # print("startRow: ", startRow , "startCol:" ,startCol)
                # print("endRow: ", endRow , "endCol:" ,endCol)
                return False  # Found at least one legal move
        
        return True

    def isStalemate(self):
        # First check that the king is NOT in check
        if self.inCheckStatus:
            return False
        
        # Get all valid moves using the existing method
        if self.whiteToMove:
            valid_moves = self.get_all_valid_moves(['wp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL'])
        else:
            valid_moves = self.get_all_valid_moves(['bp','bR', 'bN', 'bB', 'bQ', 'bK', 'bL'])

        # Try each move to see if it's legal
        for start_pos, end_pos in valid_moves:
            move = Move(start_pos, end_pos, self.board)
            if not self.wouldBeInCheck(move):
                return False  # Found at least one legal move
        
        # If we get here, no legal moves were found
        return True

    def wouldBeInCheck(self, move):
        """Simulates a move and checks if it would result in the current player's king being in check"""
        # Make a copy of the current board state
        temp_board = [row[:] for row in self.board]
        
        # Save current state
        original_board = self.board
        original_white_king = self.whitekinglocation
        original_black_king = self.blackkinglocation
        
        # Simulate the move
        temp_board[move.startRow][move.startCol] = "--"
        temp_board[move.endRow][move.endCol] = move.pieceMoved
        
        # Update king position if king was moved
        if move.pieceMoved == "wK":
            king_pos = (move.endRow, move.endCol)
        elif move.pieceMoved == "bK":
            king_pos = (move.endRow, move.endCol)
        else:
            king_pos = self.whitekinglocation if self.whiteToMove else self.blackkinglocation
        
        # Set temporary state
        self.board = temp_board
        if self.whiteToMove:
            self.whitekinglocation = king_pos if move.pieceMoved == "wK" else self.whitekinglocation
        else:
            self.blackkinglocation = king_pos if move.pieceMoved == "bK" else self.blackkinglocation
            
        # Update Prolog knowledge base
        self.updateBoard()
        
        # Get opponent's possible moves
        opponent_pieces = ['bp', 'bR', 'bN', 'bB', 'bQ', 'bK', 'bL'] if self.whiteToMove else ['wp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL']
        opponent_moves = self.get_all_valid_moves(opponent_pieces)
        
        # Check if king would be under attack
        king_row, king_col = king_pos
        in_check = False
        for start, end in opponent_moves:
            if end == (king_row, king_col):
                in_check = True
                break
                
        # Restore original state
        self.board = original_board
        self.whitekinglocation = original_white_king
        self.blackkinglocation = original_black_king
        self.updateBoard()
        
        return in_check
        
    def getvalidMoves(self):
        """Get all valid moves considering check rules"""
        if self.whiteToMove:
            pieces = ['wp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL']
        else:
            pieces = ['bp', 'bR', 'bN', 'bB', 'bQ', 'bK', 'bL']

        # Get all possible moves for current player
        moves = self.get_all_valid_moves(pieces)
        valid_moves = []

        # Test each move
        for move_pair in moves:
            start_pos, end_pos = move_pair
            temp_move = Move(start_pos, end_pos, self.board)
            
            if not self.wouldBeInCheck(temp_move):
                valid_moves.append(move_pair)

        return valid_moves

    def checkForCheck(self):
        if self.whiteToMove:
            return self.squareUnderAttack(self.blackkinglocation[0], self.blackkinglocation[1])
        else:
            return self.squareUnderAttack(self.whitekinglocation[0], self.whitekinglocation[1])
    
    def squareUnderAttack(self,r,c):
        # print("ALL VALID MOVE:" ,self.validMoves)
        # print("R: ", r, "C: ", c)
        for valid_move in self.validMoves:
            if valid_move[1] == (r, c):
                # print("valid move[1]" , valid_move[1])
                # print("R: ", r, "C: ", c)
                return True
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
