use super::utils::{pieces_by_color, set_pieces};
use serde::Serialize;
use shakmaty::*;

#[derive(Serialize, Debug, PartialEq)]
pub struct CommonSquares {
    pub fen: String,
    pub squares: Vec<String>,
    pub pieces: Vec<String>,
}

pub fn common_squares(board: &Board) -> Vec<String> {
    let mut squares = Bitboard::ALL;

    for (_, (square, piece)) in board.pieces().enumerate() {
        squares &= attacks::attacks(square, piece, board.occupied());
    }

    squares.into_iter().map(|x| x.to_string()).collect()
}

pub fn generate_puzzle(num_pieces: u32) -> CommonSquares {
    let mut board = Board::empty();
    board = set_pieces(board, num_pieces, Color::White);

    CommonSquares {
        fen: fen::FenOpts::new().board_fen(&board),
        squares: common_squares(&board),
        pieces: pieces_by_color(&board, Color::White),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use shakmaty::fen::Fen;

    #[test]
    fn multiple_pieces_no_match() {
        let fen = "2Q5/1N6/8/8/8/8/4R3/8".to_string();
        let parsed = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());

        let empty_vec: Vec<String> = Vec::new();

        assert_eq!(common_squares(&parsed.board()), empty_vec);
    }

    #[test]
    fn rook_queen_match() {
        let fen = "2Q5/8/8/8/8/8/4R3/8 w - - 0 1".to_string();
        let parsed = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());

        assert_eq!(common_squares(&parsed.board()), vec!["c2", "e6", "e8"]);
    }
}
