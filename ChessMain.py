import pygame as p
import ChessEngine
# from pyswip import Prolog
import AIchess


WIDTH = HEIGHT = 640
DIMENSION = 8
SQ_SIZE = HEIGHT // DIMENSION
MAX_FPS = 15
IMAGES = {}
TOP_MARGIN = 50
def loadImages():
    pieces = ['wp', 'bp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL', 'bR', 'bN', 'bB', 'bQ', 'bK', 'bL']
    for piece in pieces:
        IMAGES[piece] = p.transform.scale(p.image.load("Images/"+ piece +".png"),(SQ_SIZE, SQ_SIZE))

def main():
    p.init()
    screen = p.display.set_mode((WIDTH, HEIGHT + TOP_MARGIN))
    clock = p.time.Clock()
    font = p.font.SysFont(None, 36)
    screen.fill(p.Color("white"))
    gs = ChessEngine.GameState(screen)
    loadImages()
    running = True
    sqSelected = ()
    playerClicks = [] #keep track plater clicks(two tuple)
    playerOne = True
    playerTwo = False
    while running:
        humanturn = (gs.whiteToMove and playerOne) or (not gs.whiteToMove and playerTwo)
        for e in p.event.get():
            if e.type == p.QUIT:
                running = False 
            elif e.type == p.MOUSEBUTTONDOWN:
                if humanturn:
                    location = p.mouse.get_pos()

                    if location[1] < TOP_MARGIN:
                        print("Nothing Here")
                        continue

                    col = location[0]//SQ_SIZE
                    row = (location[1] - TOP_MARGIN) // SQ_SIZE
                    piece = gs.board[row][col]

                    if sqSelected == (row,col):
                        sqSelected = ()
                        playerClicks = []
                    elif len(playerClicks) == 0 and gs.board[row][col] == "--":
                        print("Empty square clicked, no piece to select")
                    elif len(playerClicks) == 0 and gs.myMove(piece) == False:
                        print("Not your turn")
                    else:
                        sqSelected = (row, col)
                        playerClicks.append(sqSelected)
                        print(sqSelected)
                    if len(playerClicks) == 2:
                        move = ChessEngine.Move(playerClicks[0], playerClicks[1], gs.board)
                        print(move.getChessNotation())
                        valid = gs.makeMove(move)
                        if valid == "same_color":
                            playerClicks = [playerClicks[1]]
                            sqSelected = playerClicks[0]
                        else:
                            if gs.board[row][col] != "--" and valid == True:
                                animateMove(move, screen, gs.board, clock)
                            sqSelected = ()
                            playerClicks = []

                        for i in range(8):
                            print(gs.board[i])

        screen.fill(p.Color("white"))

        turn = whoseMove(gs)
        turn_text = font.render(f"{turn}'s Turn", True, p.Color("black"))
        text_rect = turn_text.get_rect(center=(WIDTH // 2, TOP_MARGIN // 2))
        screen.blit(turn_text, text_rect)

        if not humanturn and playerOne == True:
            validmoves = gs.get_all_valid_moves(['bp', 'bR', 'bN', 'bB', 'bQ', 'bK', 'bL'])
            AImove = AIchess.findRandomMove(validmoves)
            AImakemove = ChessEngine.Move(AImove[0], AImove[1], gs.board)
            gs.makeMove(AImakemove)
        elif not humanturn and playerTwo == True:
            validmoves = gs.get_all_valid_moves(['wp', 'wR', 'wN', 'wB', 'wQ', 'wK', 'wL'])
            AImove = AIchess.findRandomMove(validmoves)
            AImakemove = ChessEngine.Move(AImove[0], AImove[1], gs.board)
            gs.makeMove(AImove)

        drawGameState(screen, gs, sqSelected)
        clock.tick(MAX_FPS)
        p.display.flip()

def whoseMove(gs):
    if gs.whiteToMove:
        return "White"
    else:
        return "Black"

def highlightsquare(screen, gs, sqSelected):
    if sqSelected != ():
        row, col = sqSelected
        piece = gs.board[row][col]
        
        if piece != "--":
            # Highlight the selected piece square
            s = p.Surface((SQ_SIZE, SQ_SIZE))
            s.set_alpha(100)  # Transparency level for the highlight
            s.fill(p.Color('blue'))  # Change to desired color (blue for selected square)
            screen.blit(s, (col * SQ_SIZE, row * SQ_SIZE + TOP_MARGIN))  # Offset by TOP_MARGIN

            # Define Makruk moves for each piece type
            if piece[1] == 'R':  # Ruea (Rook)
                # Horizontal moves (right and left)
                for direction in [1, -1]:
                    for i in range(col + direction, 8 if direction == 1 else -1, direction):
                        if gs.board[row][i] != "--":
                            if gs.board[row][i][0] != piece[0]:
                                s.fill(p.Color('red'))
                                screen.blit(s, (i * SQ_SIZE, row * SQ_SIZE + TOP_MARGIN))
                            break
                        s.fill(p.Color('yellow'))
                        screen.blit(s, (i * SQ_SIZE, row * SQ_SIZE + TOP_MARGIN))
                
                # Vertical moves (up and down)
                for direction in [1, -1]:
                    for i in range(row + direction, 8 if direction == 1 else -1, direction):
                        if gs.board[i][col] != "--":
                            if gs.board[i][col][0] != piece[0]:
                                s.fill(p.Color('red'))
                                screen.blit(s, (col * SQ_SIZE, i * SQ_SIZE + TOP_MARGIN))
                            break
                        s.fill(p.Color('yellow'))
                        screen.blit(s, (col * SQ_SIZE, i * SQ_SIZE + TOP_MARGIN))

            elif piece[1] == 'p':  # Bia (Pawn)
                direction = 1 if piece[0] == 'b' else -1
                # Forward move
                new_row = row + direction
                if 0 <= new_row < 8 and gs.board[new_row][col] == "--":
                    s.fill(p.Color('yellow'))
                    screen.blit(s, (col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))
                
                # Diagonal captures
                for dcol in [-1, 1]:
                    new_col = col + dcol
                    if 0 <= new_row < 8 and 0 <= new_col < 8:
                        if gs.board[new_row][new_col] != "--" and gs.board[new_row][new_col][0] != piece[0]:
                            s.fill(p.Color('red'))
                            screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))

            elif piece[1] == 'N':  # Ma (Knight)
                knight_moves = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
                for move in knight_moves:
                    new_row = row + move[0]
                    new_col = col + move[1]
                    if 0 <= new_row < 8 and 0 <= new_col < 8:
                        if gs.board[new_row][new_col] != "--":
                            if gs.board[new_row][new_col][0] != piece[0]:
                                s.fill(p.Color('red'))
                                screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))
                        else:
                            s.fill(p.Color('yellow'))
                            screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))

            elif piece[1] == 'B':  # Khon (King)
                # Define moves for the Khon piece: forward, and diagonals (both forward and backward)
                if piece[0] == 'w':  # White Khon moves
                    khon_moves = [(-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)]
                else:  # Black Khon moves
                    khon_moves = [(1, 0), (1, -1), (1, 1), (-1, -1), (-1, 1)]
                
                for drow, dcol in khon_moves:
                    new_row, new_col = row + drow, col + dcol
                    if 0 <= new_row < 8 and 0 <= new_col < 8:
                        if gs.board[new_row][new_col] != "--":
                            if gs.board[new_row][new_col][0] != piece[0]:  # Capture move
                                s.fill(p.Color('red'))  # Color for capture moves
                                screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))
                        else:
                            s.fill(p.Color('yellow'))  # Color for valid moves
                            screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))

            elif piece[1] == 'Q':  # Khon (King)
                # Define moves for the Khon piece: forward, and diagonals (both forward and backward)
                khon_moves = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                
                for drow, dcol in khon_moves:
                    new_row, new_col = row + drow, col + dcol
                    if 0 <= new_row < 8 and 0 <= new_col < 8:
                        if gs.board[new_row][new_col] != "--":
                            if gs.board[new_row][new_col][0] != piece[0]:  # Capture move
                                s.fill(p.Color('red'))  # Color for capture moves
                                screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))
                        else:
                            s.fill(p.Color('yellow'))  # Color for valid moves
                            screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))

            elif piece[1] == 'K':  # Queen (in Makruk, moves one square in any direction)
                queen_moves = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
                for move in queen_moves:
                    new_row = row + move[0]
                    new_col = col + move[1]
                    if 0 <= new_row < 8 and 0 <= new_col < 8:
                        if gs.board[new_row][new_col] != "--":
                            if gs.board[new_row][new_col][0] != piece[0]:
                                s.fill(p.Color('red'))
                                screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))
                        else:
                            s.fill(p.Color('yellow'))
                            screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))

            elif piece[1] == 'L':  # Bian (King)
                # Define moves for the Bian piece: forward, and diagonals (both forward and backward)
                khon_moves = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                
                for drow, dcol in khon_moves:
                    new_row, new_col = row + drow, col + dcol
                    if 0 <= new_row < 8 and 0 <= new_col < 8:
                        if gs.board[new_row][new_col] != "--":
                            if gs.board[new_row][new_col][0] != piece[0]:  # Capture move
                                s.fill(p.Color('red'))  # Color for capture moves
                                screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))
                        else:
                            s.fill(p.Color('yellow'))  # Color for valid moves
                            screen.blit(s, (new_col * SQ_SIZE, new_row * SQ_SIZE + TOP_MARGIN))

def drawCheckStatus(screen, gs):
    if gs.inCheck or gs.checkmate:
        font = p.font.SysFont(None, 36)
        # Draw a background rectangle for better visibility
        status = "Checkmate!" if gs.checkmate else "Check!"
        color = "White" if gs.whiteToMove else "Black"
        text = font.render(f"{color} is in {status}", True, p.Color("red"))
        text_rect = text.get_rect(center=(WIDTH // 2, TOP_MARGIN // 2))
        
        # Add a background for better visibility
        background_rect = text_rect.copy()
        background_rect.inflate_ip(20, 10)  # Make background slightly larger than text
        p.draw.rect(screen, p.Color("white"), background_rect)
        p.draw.rect(screen, p.Color("black"), background_rect, 2)  # Add border
        
        screen.blit(text, text_rect)

def drawGameState(screen, gs, sqSelected):
    drawBoard(screen)
    highlightsquare(screen, gs, sqSelected)
    drawPieces(screen, gs.board)
    drawCheckStatus(screen, gs)

def drawBoard(screen):
    colors = [p.Color("white"), p.Color("gray")]
    for r in range(DIMENSION):
        for c in range(DIMENSION):
            color = colors[((r + c) % 2)]
            p.draw.rect(screen, color, p.Rect(c * SQ_SIZE, TOP_MARGIN + r * SQ_SIZE, SQ_SIZE, SQ_SIZE))

def drawPieces(screen, board):
    for r in range(DIMENSION):
        for c in range (DIMENSION): 
            piece = board[r][c]
            if piece != "--":
                screen.blit(IMAGES[piece], p.Rect(c * SQ_SIZE, TOP_MARGIN + r * SQ_SIZE, SQ_SIZE, SQ_SIZE))            

def animateMove(move, screen, board, clock):
    colors = [p.Color("white"), p.Color("gray")]
    dr = move.endRow - move.startRow
    dc = move.endCol - move.startCol
    framesPerSquare = 10  # Adjust this for animation speed
    frameCount = (abs(dr) + abs(dc)) * framesPerSquare
    for frame in range(frameCount + 1):
        r, c = (move.startRow + dr * frame / frameCount, move.startCol + dc * frame / frameCount)
        drawBoard(screen)
        drawPieces(screen, board)
        
        # Draw an empty square on the starting position to clear the moving piece
        startSquareColor = colors[(move.startRow + move.startCol) % 2]
        startSquare = p.Rect(move.startCol * SQ_SIZE, move.startRow * SQ_SIZE + TOP_MARGIN, SQ_SIZE, SQ_SIZE)
        p.draw.rect(screen, startSquareColor, startSquare)
        
        # Draw the end square and captured piece if any
        endSquareColor = colors[(move.endRow + move.endCol) % 2]
        endSquare = p.Rect(move.endCol * SQ_SIZE, move.endRow * SQ_SIZE + TOP_MARGIN, SQ_SIZE, SQ_SIZE)
        p.draw.rect(screen, endSquareColor, endSquare)
        
        if move.pieceCaptured != "--":
            screen.blit(IMAGES[move.pieceCaptured], endSquare)
        
        # Draw the moving piece
        screen.blit(IMAGES[move.pieceMoved], p.Rect(c * SQ_SIZE, TOP_MARGIN + r * SQ_SIZE, SQ_SIZE, SQ_SIZE))
        p.display.flip()
        clock.tick(60)

    

if __name__ == "__main__":
    main()