use postgres_types::{FromSql, ToSql};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub enum Orientation {
    White,
    Black,
}

/// Puzzle represantaion as stored in the puzzle CSV available from lichess.org.
/// https://database.lichess.org/#puzzles
#[derive(Debug, ToSql, FromSql, Serialize, Deserialize, Clone)]
pub struct PuzzleRecord {
    pub id: String,
    pub fen: String,
    pub moves: String,
    pub rating: i32,
    pub rating_deviation: i32,
    pub popularity: i32,
    pub nb_plays: i32,
    pub themes: String,
    pub game_url: String,
}

/// Puzzle represantaion as stored in the database.
#[derive(Debug, ToSql, FromSql, Serialize, Deserialize, Clone)]
pub struct PuzzleDbRecord {
    pub id: String,
    pub fen: String,
    pub moves: String,
    pub rating: i32,
    pub rating_deviation: i32,
    pub popularity: i32,
    pub nb_plays: i32,
    pub themes: String,
    pub game_url: String,
    pub pgn: Option<String>,
}

/// Puzzle represantaion for the blind tactics game.
#[derive(Debug, Deserialize, Serialize)]
pub struct GeneratedTacticsPuzzle {
    pub id: String,
    pub fen: String,
    pub moves: Vec<String>,
    pub rating: i32,
    pub rating_deviation: i32,
    pub popularity: i32,
    pub nb_plays: i32,
    pub themes: Vec<String>,
    pub orientation: Orientation,
    pub solution: String,
    pub game_url: String,
}
