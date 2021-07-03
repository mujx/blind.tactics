use super::utils::random_free_square;
use log::debug;
use rand::Rng;
use serde::Serialize;
use shakmaty::*;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;

const N: i32 = 8;

// All eight possible movements for a knight.
static ROWS: &[i32] = &[2, 2, -2, -2, 1, 1, -1, -1];
static COLS: &[i32] = &[-1, 1, 1, -1, 2, -2, 2, -2];

#[derive(Serialize, Debug, PartialEq)]
pub struct KnightPuzzle {
    pub from: String,
    pub to: String,
    pub squares: Vec<Vec<String>>,
    pub fen: String,
}

pub fn random_initial_square() -> Square {
    let available: Vec<u32> = (0..63).into_iter().enumerate().map(|(_, y)| y).collect();
    let square = available[rand::thread_rng().gen_range(0..available.len())];

    Square::new(square)
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Point {
    pub x: i32,
    pub y: i32,
    pub dist: i32,
}

impl Point {
    pub fn new(x: i32, y: i32) -> Point {
        Point { x, y, dist: 0 }
    }
}

fn square_to_point(sq: &Square) -> Point {
    let (file, rank) = sq.coords();

    Point {
        x: match file {
            File::A => 0,
            File::B => 1,
            File::C => 2,
            File::D => 3,
            File::E => 4,
            File::F => 5,
            File::G => 6,
            File::H => 7,
        },
        y: match rank {
            Rank::First => 0,
            Rank::Second => 1,
            Rank::Third => 2,
            Rank::Fourth => 3,
            Rank::Fifth => 4,
            Rank::Sixth => 5,
            Rank::Seventh => 6,
            Rank::Eighth => 7,
        },
        dist: 0,
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.x < other.x || (self.x == other.x && self.y < other.y) {
            return Ordering::Less;
        }

        Ordering::Greater
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn is_valid(x: i32, y: i32) -> bool {
    if x < 0 || y < 0 || x >= N || y >= N {
        return false;
    }

    true
}

pub fn min_moves(src: Point, dest: Point) -> u32 {
    let mut visited: HashSet<Point> = HashSet::new();

    let mut q: VecDeque<Point> = VecDeque::new();
    q.push_back(src);

    while !q.is_empty() {
        if let Some(node) = q.pop_front() {
            let x = node.x;
            let y = node.y;
            let dist = node.dist;

            if x == dest.x && y == dest.y {
                return dist as u32;
            }

            if !visited.contains(&node) {
                visited.insert(node.clone());

                for i in 0..8 {
                    let x1 = x + ROWS[i];
                    let y1 = y + COLS[i];

                    if is_valid(x1, y1) {
                        q.push_back(Point {
                            x: x1,
                            y: y1,
                            dist: dist + 1,
                        });
                    }
                }
            }
        }
    }

    0
}

pub fn find_solution(src: &Square, dest: &Square) -> Vec<Vec<Square>> {
    let mut occupied: HashMap<Square, bool> = HashMap::new();
    let mut paths: Vec<Vec<Square>> = Vec::new();
    let min_distance = min_moves(square_to_point(src), square_to_point(dest)) as usize;

    let path: Vec<Square> = vec![*src];

    let mut queue: VecDeque<Vec<Square>> = VecDeque::new();
    queue.push_back(path);

    while let Some(path) = queue.pop_front() {
        if let Some(last) = path.last() {
            if last == dest && path.len() == min_distance + 1 {
                paths.push(path.clone());
            }

            // Match square as visited
            occupied.insert(*last, true);

            // Find all possible moves.
            let possible_moves: Vec<Square> = attacks::knight_attacks(*last)
                .into_iter()
                .enumerate()
                .map(|(_, sq)| sq)
                .collect();

            // Add them to the queue
            for sq in possible_moves {
                if !occupied.contains_key(&sq) {
                    let mut new_path = path.clone();
                    new_path.push(sq);
                    queue.push_back(new_path);
                }
            }
        }
    }

    paths
}

fn pick_end_square(start: Square, max_steps: u32) -> Square {
    let mut board = Board::empty();
    board.set_piece_at(
        start,
        Piece {
            color: Color::White,
            role: Role::Knight,
        },
        false,
    );

    let mut end = random_free_square(&board, false);
    board.set_piece_at(
        end,
        Piece {
            color: Color::Black,
            role: Role::Pawn,
        },
        false,
    );

    while min_moves(square_to_point(&start), square_to_point(&end)) > max_steps {
        board.remove_piece_at(end);
        end = random_free_square(&board, false);
        board.set_piece_at(
            end,
            Piece {
                color: Color::Black,
                role: Role::Pawn,
            },
            false,
        );
    }

    end
}

pub fn generate_puzzle(max_steps: u32) -> KnightPuzzle {
    let start = random_initial_square();

    let mut board = Board::empty();
    board.set_piece_at(
        start,
        Piece {
            color: Color::White,
            role: Role::Knight,
        },
        false,
    );

    let end = pick_end_square(start, max_steps);
    board.set_piece_at(
        end,
        Piece {
            color: Color::Black,
            role: Role::Pawn,
        },
        false,
    );

    debug!(
        "The minimum step distance between {} and {} is: {}",
        start.to_string(),
        end.to_string(),
        min_moves(square_to_point(&start), square_to_point(&end))
    );

    KnightPuzzle {
        from: start.to_string(),
        to: end.to_string(),
        fen: fen::FenOpts::new().board_fen(&board),
        squares: find_solution(&start, &end)
            .iter()
            .map(|x| x.iter().map(|p| p.to_string()).collect())
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_knight_distance() {
        assert_eq!(min_moves(Point::new(0, 0), Point::new(0, 0)), 0);
        assert_eq!(min_moves(Point::new(0, 0), Point::new(7, 7)), 6);
        assert_eq!(min_moves(Point::new(0, 0), Point::new(2, 2)), 4);
        assert_eq!(min_moves(Point::new(2, 1), Point::new(0, 0)), 1);
        assert_eq!(min_moves(Point::new(1, 1), Point::new(3, 1)), 2);
        assert_eq!(min_moves(Point::new(4, 2), Point::new(4, 4)), 2);
    }
}
