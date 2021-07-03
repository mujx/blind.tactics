use super::utils::{pieces_by_color, random_free_square, set_pieces};
use serde::Serialize;
use shakmaty::*;

#[derive(Serialize, Debug, PartialEq)]
pub struct CheckMove {
    pub from: String,
    pub to: String,
    pub role: String,
    pub move_display: String,
}

#[derive(Serialize)]
pub struct ChecksPuzzle {
    pub fen: String,
    pub checks: Vec<CheckMove>,
    pub black: Vec<String>,
    pub white: Vec<String>,
}

// Check if the piece in the given square can be captured by the attacker.
pub fn can_be_captured(sq: Square, attacker: Color, board: &Board) -> bool {
    let defender = match attacker {
        Color::Black => Color::White,
        Color::White => Color::Black,
    };

    let attacker_king = board.by_color(attacker) & board.kings();
    let attacker_pieces = board.attacks_to(sq, attacker, board.occupied());
    let defender_attacks = board.attacks_to(sq, defender, board.occupied());

    if attacker_king & attacker_pieces == attacker_king {
        return defender_attacks == bitboard::Bitboard::EMPTY;
    }

    attacker_pieces != bitboard::Bitboard::EMPTY
}

// Find all the checks without being captured.
pub fn find_solution(board: &Board) -> Vec<CheckMove> {
    let black_king = board.black() & board.kings();

    let mut checks: Vec<CheckMove> = Vec::new();

    for (_, square) in board.white().into_iter().enumerate() {
        let piece = match board.piece_at(square) {
            Some(piece) => piece,
            None => Piece {
                role: Role::King,
                color: Color::White,
            },
        };

        // We can move into squares not attacked by black and squares not already taken by other
        // white pieces.
        let piece_attacks = attacks::attacks(square, piece, board.occupied());

        for (_, new_square) in piece_attacks.into_iter().enumerate() {
            // Create a new board with the piece placed on the new square.
            let mut updated_board = board.clone();
            updated_board.remove_piece_at(square);
            updated_board.set_piece_at(new_square, piece, false);

            if can_be_captured(new_square, Color::Black, &updated_board) {
                continue;
            }

            let mut move_attacks = attacks::attacks(new_square, piece, updated_board.occupied());

            move_attacks &= black_king;

            // We have a check.
            if move_attacks != bitboard::Bitboard::EMPTY {
                if let Some(p) = board.piece_at(new_square) {
                    if p.color == Color::White {
                        continue;
                    }
                }

                let capture = board.piece_at(new_square).map(|p| p.role);

                let move_ = Move::Normal {
                    role: piece.role,
                    from: square,
                    to: new_square,
                    capture,
                    promotion: None,
                };

                checks.push(CheckMove {
                    from: format!("{}", square),
                    to: format!("{}", new_square),
                    role: format!("{:#?}", piece.role),
                    move_display: format!("{}", move_),
                });
            }
        }
    }

    checks
}

// White is the attacker. The black king should not be in check on the initial position.
pub fn generate_puzzle(nb_white: u32, nb_black: u32) -> ChecksPuzzle {
    let mut board = Board::empty();

    board = set_pieces(board, nb_white, Color::White);
    board = set_pieces(board, nb_black, Color::Black);

    // Ensure the king is not in check.
    let square = random_free_square(&board, true);

    board.set_piece_at(
        square,
        Piece {
            role: Role::King,
            color: Color::Black,
        },
        false,
    );

    ChecksPuzzle {
        fen: fen::FenOpts::new().board_fen(&board),
        checks: find_solution(&board),
        black: pieces_by_color(&board, Color::Black),
        white: pieces_by_color(&board, Color::White),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use shakmaty::fen::Fen;

    #[test]
    fn single_check() {
        let fen = "8/8/8/4nb2/8/8/5R2/2k3B1".to_string();
        let parsed_fen = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());
        let checks = find_solution(&parsed_fen.board());

        assert_eq!(checks.len(), 1);
    }

    #[test]
    fn does_not_capture_friendly_pieces() {
        let fen = "8/8/4r3/5R2/8/B3k2b/8/8".to_string();
        let parsed_fen = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());
        let checks = find_solution(&parsed_fen.board());

        assert!(checks.contains(&CheckMove {
            from: "a3".to_string(),
            to: "c1".to_string(),
            role: "Bishop".to_string(),
            move_display: "Ba3-c1".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "a3".to_string(),
            to: "c5".to_string(),
            role: "Bishop".to_string(),
            move_display: "Ba3-c5".to_string()
        }));
        assert_eq!(checks.len(), 2);

        let fen2 = "1q1Q4/8/5R2/8/8/8/2b5/k7".to_string();
        let parsed_fen2 = Fen::from_ascii(&fen2.as_bytes()).unwrap_or(Fen::empty());
        let checks2 = find_solution(&parsed_fen2.board());

        assert!(checks2.contains(&CheckMove {
            from: "f6".to_string(),
            to: "f1".to_string(),
            role: "Rook".to_string(),
            move_display: "Rf6-f1".to_string()
        }));
        assert!(checks2.contains(&CheckMove {
            from: "f6".to_string(),
            to: "a6".to_string(),
            role: "Rook".to_string(),
            move_display: "Rf6-a6".to_string()
        }));
        assert!(checks2.contains(&CheckMove {
            from: "d8".to_string(),
            to: "d4".to_string(),
            role: "Queen".to_string(),
            move_display: "Qd8-d4".to_string()
        }));
        assert!(checks2.contains(&CheckMove {
            from: "d8".to_string(),
            to: "a5".to_string(),
            role: "Queen".to_string(),
            move_display: "Qd8-a5".to_string()
        }));
        assert_eq!(checks2.len(), 4);
    }

    #[test]
    fn can_capture() {
        let fen = "8/1k6/8/8/5n2/1n2R3/8/R7".to_string();
        let parsed_fen = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());
        let checks = find_solution(&parsed_fen.board());

        assert!(checks.contains(&CheckMove {
            from: "e3".to_string(),
            to: "b3".to_string(),
            role: "Rook".to_string(),
            move_display: "Re3xb3".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "e3".to_string(),
            to: "e7".to_string(),
            role: "Rook".to_string(),
            move_display: "Re3-e7".to_string()
        }));
        assert_eq!(checks.len(), 2);
    }

    #[ignore]
    #[test]
    fn supported_attack_with_king_capture() {
        let fen = "8/8/5qk1/8/3N4/2q4Q/8/8".to_string();
        let parsed_fen = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());
        let checks = find_solution(&parsed_fen.board());

        assert_eq!(checks.len(), 2);
        assert!(checks.contains(&CheckMove {
            from: "h3".to_string(),
            to: "g2".to_string(),
            role: "Queen".to_string(),
            move_display: "Qh3-g2".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "h3".to_string(),
            to: "g4".to_string(),
            role: "Queen".to_string(),
            move_display: "Qh3-g4".to_string()
        }));
    }

    #[test]
    fn supported_attack() {
        let fen = "8/Q5r1/5n2/R7/1k6/8/8/8".to_string();
        let parsed_fen = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());
        let checks = find_solution(&parsed_fen.board());

        assert!(checks.contains(&CheckMove {
            from: "a7".to_string(),
            to: "b8".to_string(),
            role: "Queen".to_string(),
            move_display: "Qa7-b8".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "a7".to_string(),
            to: "b6".to_string(),
            role: "Queen".to_string(),
            move_display: "Qa7-b6".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "a7".to_string(),
            to: "d4".to_string(),
            role: "Queen".to_string(),
            move_display: "Qa7-d4".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "a5".to_string(),
            to: "a4".to_string(),
            role: "Rook".to_string(),
            move_display: "Ra5-a4".to_string()
        }));
        assert!(checks.contains(&CheckMove {
            from: "a7".to_string(),
            to: "c5".to_string(),
            role: "Queen".to_string(),
            move_display: "Qa7-c5".to_string()
        }));
        assert_eq!(checks.len(), 5);
    }

    #[test]
    fn king_can_not_capture_protected() {
        let fen = "8/Q5r1/5n2/8/Rk6/8/8/8 w - - 0 1".to_string();
        let parsed = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());

        // The a4 square is protected and the king cannot capture.
        assert!(!can_be_captured(Square::A4, Color::Black, &parsed.board()));
    }

    #[test]
    fn king_can_capture_unprotected() {
        let fen = "1Q6/6r1/5n2/8/Rk6/8/8/8 w - - 0 1".to_string();
        let parsed = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());

        assert!(can_be_captured(Square::A4, Color::Black, &parsed.board()));
    }

    #[test]
    fn pieces_can_capture() {
        let fen = "8/3R2r1/5n2/8/1k2Q3/8/8/8 w - - 0 1".to_string();
        let parsed = Fen::from_ascii(&fen.as_bytes()).unwrap_or(Fen::empty());

        assert!(can_be_captured(Square::D7, Color::Black, &parsed.board()));
        assert!(can_be_captured(Square::E4, Color::Black, &parsed.board()));
        assert!(can_be_captured(Square::G7, Color::White, &parsed.board()));

        assert!(!can_be_captured(Square::C6, Color::Black, &parsed.board()));
    }
}
