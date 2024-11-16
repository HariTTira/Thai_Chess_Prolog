from pyswip import Prolog

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