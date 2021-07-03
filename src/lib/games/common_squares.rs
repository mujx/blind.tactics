use super::utils::{pieces_by_color, set_pieces};
use serde::Serialize;
use shakmaty::*;

#[derive(Serialize, Debug, PartialEq)]
pub struct CommonSquares {
    pub fen: String,
    pub squares: Vec<String>,
    pub pieces: Vec<String>,
}

pub fn generate_puzzle(num_pieces: u32) -> CommonSquares {
    let mut board = Board::empty();
    board = set_pieces(board, num_pieces, Color::White);

    let mut common_squares = Bitboard::ALL;

    for (_, (square, piece)) in board.pieces().enumerate() {
        common_squares &= attacks::attacks(square, piece, board.occupied());
    }

    CommonSquares {
        fen: fen::FenOpts::new().board_fen(&board),
        squares: common_squares.into_iter().map(|x| x.to_string()).collect(),
        pieces: pieces_by_color(&board, Color::White),
    }
}
