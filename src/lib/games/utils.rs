use rand::Rng;
use shakmaty::*;
use std::collections::HashSet;

const MAX_ROOKS: usize = 2;
const MAX_BISHOPS: usize = 2;
const MAX_KNIGHTS: usize = 2;
const MAX_QUEENS: usize = 2;

pub fn random_free_square(board: &Board, is_king: bool) -> Square {
    let occupied: HashSet<u32> = board
        .occupied()
        .into_iter()
        .enumerate()
        .map(|(_, s)| s.into())
        .collect::<HashSet<_>>();

    let available: Vec<u32> = (0..63)
        .into_iter()
        .enumerate()
        .filter(|(_, y)| !occupied.contains(y))
        .filter(|(_, y)| {
            !is_king
                || board
                    .attacks_to(Square::new(*y), Color::White, board.occupied())
                    .into_iter()
                    .enumerate()
                    .len()
                    == 0
        })
        .map(|(_, y)| y)
        .collect();

    let square = available[rand::thread_rng().gen_range(0..available.len())];

    Square::new(square)
}

fn random_role(board: &Board, color: Color) -> Piece {
    let pieces = board.by_color(color);

    let rooks = (pieces & board.rooks()).into_iter().enumerate().len();
    let bishops = (pieces & board.bishops()).into_iter().enumerate().len();
    let knights = (pieces & board.knights()).into_iter().enumerate().len();
    let queens = (pieces & board.queens()).into_iter().enumerate().len();

    let mut available_roles: Vec<Role> = Vec::new();

    if rooks < MAX_ROOKS {
        available_roles.push(Role::Rook);
    }

    if bishops < MAX_BISHOPS {
        available_roles.push(Role::Bishop);
    }

    if knights < MAX_KNIGHTS {
        available_roles.push(Role::Knight);
    }

    if queens < MAX_QUEENS {
        available_roles.push(Role::Queen);
    }

    let role = available_roles[rand::thread_rng().gen_range(0..available_roles.len())];

    Piece { color, role }
}

pub fn set_pieces(mut board: Board, nb: u32, color: Color) -> Board {
    for _ in 0..nb {
        let square = random_free_square(&board, false);
        let role = random_role(&board, color);

        board.set_piece_at(square, role, false);
    }

    board
}

pub fn pieces_by_color(board: &Board, color: Color) -> Vec<String> {
    let mut pieces: Vec<String> = Vec::new();

    for (_, sq) in board.by_color(color).into_iter().enumerate() {
        let piece = match board.piece_at(sq) {
            Some(piece) => piece,
            None => panic!("no piece was found using Board::by_color"),
        };

        let piece_letter = match piece.role {
            Role::King => "K",
            Role::Queen => "Q",
            Role::Rook => "R",
            Role::Bishop => "B",
            Role::Knight => "N",
            Role::Pawn => "",
        };

        pieces.push(format!("{}{}", piece_letter, sq));
    }

    pieces
}
