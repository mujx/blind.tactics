use super::types::{PuzzleDbRecord, PuzzleRecord};
use deadpool_postgres::{Config, ManagerConfig, Pool, RecyclingMethod};
use log::info;
use postgres_types::ToSql;
use std::env;
use tokio_postgres::NoTls;

pub async fn mk_pool() -> Result<Pool, std::io::Error> {
    let mut cfg = Config::new();

    cfg.application_name = Some("blind".to_string());
    cfg.user = Some(env::var("BLIND_DB_USER").unwrap_or_else(|_| "blind".to_string()));
    cfg.password = Some(env::var("BLIND_DB_PASSWORD").unwrap_or_else(|_| "blind".to_string()));
    cfg.dbname = Some(env::var("BLIND_DB_NAME").unwrap_or_else(|_| "blind".to_string()));
    cfg.host = Some(env::var("BLIND_DB_HOST").unwrap_or_else(|_| "localhost".to_string()));
    cfg.port = Some(env::var("BLIND_DB_PORT").map_or(5432_u16, |v| v.parse().unwrap()));
    cfg.manager = Some(ManagerConfig {
        recycling_method: RecyclingMethod::Fast,
    });

    info!("db host: {:?}", cfg.host);
    info!("db port: {:?}", cfg.port);
    info!("db user: {:?}", cfg.user);
    info!("db name: {:?}", cfg.dbname);

    let pool = cfg.create_pool(NoTls).unwrap();

    Ok(pool)
}

fn mk_values_template(start: usize, end: usize) -> String {
    let values: String = (start..end)
        .map(|x| format!("${}", x))
        .collect::<Vec<String>>()
        .join(",");

    "(".to_string() + &values + &")".to_string()
}

pub async fn import_tactics_puzzles(
    records: Vec<PuzzleRecord>,
    batch_size: usize,
    pool: &Pool,
) -> Result<(), std::io::Error> {
    let client = pool.get().await.unwrap();

    let total_params = 9;

    for chunk in records.chunks(batch_size) {
        let recs: Vec<PuzzleRecord> = chunk.to_vec().clone();

        let values = (0..recs.len())
            .map(|i| mk_values_template(i * total_params + 1, i * total_params + total_params + 1))
            .collect::<Vec<String>>()
            .join(",");

        let mut params: Vec<&(dyn ToSql + Sync)> = Vec::new();
        for r in recs.iter() {
            params.push(&r.id);
            params.push(&r.fen);
            params.push(&r.moves);
            params.push(&r.rating);
            params.push(&r.rating_deviation);
            params.push(&r.popularity);
            params.push(&r.nb_plays);
            params.push(&r.themes);
            params.push(&r.game_url);
        }

        let stmt_fmt = &format!(
            r#"
                INSERT INTO tactics_puzzles(
                    id,
                    fen,
                    moves,
                    rating,
                    rating_deviation,
                    popularity,
                    nb_plays,
                    themes,
                    game_url
                )
                VALUES {}
                ON CONFLICT (id)
                DO UPDATE SET rating = EXCLUDED.rating,
                              rating_deviation = EXCLUDED.rating_deviation,
                              popularity = EXCLUDED.popularity,
                              nb_plays = EXCLUDED.nb_plays,
                              themes = EXCLUDED.themes
                "#,
            values
        );

        let stmt = client.prepare_cached(stmt_fmt).await.unwrap();

        client.query(&stmt, &params[..]).await.unwrap();
    }

    Ok(())
}

pub async fn pick_random_puzzle(
    min_rating: i32,
    max_rating: i32,
    pool: &Pool,
) -> Result<Option<PuzzleDbRecord>, std::io::Error> {
    let query = r#"
      SELECT id, fen, moves, rating, rating_deviation, popularity, nb_plays, themes, game_url, pgn FROM (
          SELECT *, random() as counting FROM tactics_puzzles 
          WHERE rating >= $1 and rating <= $2
          ORDER BY counting
          LIMIT 1
      ) filtered_table;
    "#;

    let client = pool.get().await.unwrap();

    let rows = client
        .query(query, &[&min_rating, &max_rating])
        .await
        .unwrap();

    match rows.first() {
        Some(row) => Ok(Some(PuzzleDbRecord {
            id: row.get("id"),
            fen: row.get("fen"),
            moves: row.get("moves"),
            rating: row.get("rating"),
            rating_deviation: row.get("rating_deviation"),
            popularity: row.get("popularity"),
            nb_plays: row.get("nb_plays"),
            themes: row.get("themes"),
            game_url: row.get("game_url"),
            pgn: row.get("pgn"),
        })),
        None => Ok(None),
    }
}

pub async fn set_puzzle_pgn(
    puzzle: &PuzzleDbRecord,
    pgn: String,
    pool: &Pool,
) -> Result<(), std::io::Error> {
    let query = "UPDATE tactics_puzzles SET pgn = $1 WHERE id = $2;";
    let client = pool.get().await.unwrap();

    client.query(query, &[&pgn, &puzzle.id]).await.unwrap();

    Ok(())
}
