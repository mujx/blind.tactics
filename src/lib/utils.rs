use super::db;
use csv::ReaderBuilder;
use deadpool_postgres::Pool;
use log::{error, debug};
use nom::{character::complete::digit1, combinator::map_res, combinator::recognize, IResult};
use pgnparse::parser::parse_pgn_to_rust_struct;
use serde::Deserialize;
use std::error::Error;

// TODO: Have a human readable version of the solution.

use crate::types::{GeneratedTacticsPuzzle, Orientation, PuzzleDbRecord, PuzzleRecord};

named!(game_id_and_orientation<&str, &str>, take_until1!("#"));
named!(parse_game_id<&str, &str>, take!(8));
named!(lichess_url<&str, &str>, tag!("https://lichess.org/"));
named!(remove_slash<&str, &str>, alt!(tag!("/") | take!(0)));
named!(remove_hash<&str, &str>, tag!("#"));

#[derive(Debug, Deserialize)]
struct GameInfo {
    game_id: String,
    nb_move: u32,
    orientation: Orientation,
}

pub async fn fetch_pgn(game_id: &str) -> Result<reqwest::Response, reqwest::Error> {
    let client = reqwest::Client::new();

    client
        .get(&format!("https://lichess.org/game/export/{}", game_id))
        .query(&[("evals", "false"), ("clocks", "false")])
        .send()
        .await
}

fn mk_fen(
    pgn: String,
    puzzle: &PuzzleDbRecord,
    game_info: &GameInfo,
    moves_before: u32,
) -> GeneratedTacticsPuzzle {
    let pgn_info = parse_pgn_to_rust_struct(pgn);

    let new_puzzle_start: usize = std::cmp::max(1, game_info.nb_move - moves_before - 1) as usize;

    let old_moves: Vec<String> = puzzle.moves.split(' ').map(|x| x.to_string()).collect();
    let themes: Vec<String> = puzzle.themes.split(' ').map(|x| x.to_string()).collect();

    let mut moves: Vec<String> = Vec::new();

    for i in new_puzzle_start..(game_info.nb_move as usize) {
        moves.push(pgn_info.moves[i].uci.clone());
    }

    GeneratedTacticsPuzzle {
        id: puzzle.id.clone(),
        fen: pgn_info.moves[new_puzzle_start].fen_before.clone(),
        game_url: puzzle.game_url.clone(),
        moves,
        nb_plays: puzzle.nb_plays,
        orientation: game_info.orientation,
        rating: puzzle.rating,
        rating_deviation: puzzle.rating_deviation,
        solution: old_moves[1].clone(),
        themes,
        popularity: puzzle.popularity,
    }
}

fn parse_u32(input: &str) -> IResult<&str, u32> {
    map_res(recognize(digit1), str::parse)(input)
}

fn extract_game_info(input: &str) -> IResult<&str, GameInfo> {
    let (input, id_with_orientation) = game_id_and_orientation(input)?;
    let (orientation, game_id) = parse_game_id(id_with_orientation)?;

    let orientation = if !orientation.is_empty() {
        let (orientation, _) = remove_slash(orientation)?;
        match orientation {
            "white" => Orientation::Black,
            "black" => Orientation::White,
            _ => panic!("Invalid orientation: {}", orientation),
        }
    } else {
        Orientation::Black
    };

    let (input, _) = remove_hash(input)?;
    let (input, num_move) = parse_u32(input)?;

    Ok((
        input,
        GameInfo {
            game_id: game_id.to_string(),
            nb_move: num_move,
            orientation,
        },
    ))
}

fn get_game_id(puzzle: &PuzzleDbRecord) -> IResult<&str, GameInfo> {
    let (input, _) = lichess_url(&puzzle.game_url)?;
    let (input, gi) = extract_game_info(input)?;

    Ok((
        input,
        GameInfo {
            orientation: gi.orientation,
            game_id: gi.game_id,
            nb_move: gi.nb_move,
        },
    ))
}

pub async fn generate_tactics_puzzle(
    record: &PuzzleDbRecord,
    moves_to_follow: u32,
    pool: &Pool,
) -> Result<GeneratedTacticsPuzzle, reqwest::Error> {
    let game_info = get_game_id(record).expect("failed to retrieve game_id");

    match &record.pgn {
        Some(pgn) => Ok(mk_fen(
            pgn.to_string(),
            record,
            &game_info.1,
            moves_to_follow,
        )),
        None => match fetch_pgn(&game_info.1.game_id.clone()).await {
            Ok(res) => match res.text().await {
                Ok(pgn) => {
                    debug!("saving pgn for puzzle '{}' to db", record.id);

                    match db::set_puzzle_pgn(record, pgn.clone(), pool).await {
                        Ok(_) => {
                            debug!("saved pgn for puzzle '{}'", record.id);
                        }
                        Err(e) => {
                            error!("failed to save pgn for puzzle '{}': {}", record.id, e);
                        }
                    }

                    Ok(mk_fen(pgn, record, &game_info.1, moves_to_follow))
                }
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
    }
}

pub fn load_puzzles(f: &str) -> Result<Vec<PuzzleRecord>, Box<dyn Error>> {
    let mut puzzles: Vec<PuzzleRecord> = Vec::new();

    for result in ReaderBuilder::new().from_path(f)?.records() {
        let puzzle: PuzzleRecord = result?.deserialize(None)?;
        puzzles.push(puzzle);
    }

    Ok(puzzles)
}
