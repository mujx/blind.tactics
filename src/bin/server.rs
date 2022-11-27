use actix_web::{web, App, HttpResponse, HttpServer};
use blind::db;
use blind::utils::{generate_tactics_puzzle, load_puzzles};
use deadpool_postgres::Pool;
use log::{error, info, debug};
use serde::Deserialize;
use std::env;
use std::sync::Arc;
use structopt::StructOpt;

#[derive(Debug, StructOpt, Clone)]
#[structopt(name = "blind", about = "Blind server & cli puzzle import tool")]
struct Opt {
    /// Number of puzzles to insert per query.
    #[structopt(short, long, default_value = "300")]
    batch_size: usize,

    /// Puzzles file to import.
    #[structopt(short, long)]
    puzzles_file: Option<String>,

    /// Static file directory for UI files.
    #[structopt(short, long, default_value = "./dist")]
    static_files: String,
}

#[derive(Clone)]
struct AppCtx {
    pool: Arc<Pool>,
}

#[derive(Deserialize)]
pub struct PuzzleOpts {
    rating: i32,
    moves_to_follow: u32,
}

#[derive(Deserialize)]
pub struct KnightPuzzleOpts {
    max_steps: u32,
}

#[derive(Deserialize)]
pub struct CommonSquaresOpts {
    max_pieces: u32,
}

#[derive(Deserialize)]
pub struct ChecksOpts {
    nb_black: Option<u32>,
    nb_white: Option<u32>,
}

async fn next_knight_path_puzzle(web::Query(opts): web::Query<KnightPuzzleOpts>) -> HttpResponse {
    let puzzle = blind::games::knight_path::generate_puzzle(opts.max_steps);

    HttpResponse::Ok().json(puzzle)
}

async fn next_common_squares_puzzle(
    web::Query(opts): web::Query<CommonSquaresOpts>,
) -> HttpResponse {
    HttpResponse::Ok().json(blind::games::common_squares::generate_puzzle(
        opts.max_pieces,
    ))
}

async fn next_checks_puzzle(web::Query(opts): web::Query<ChecksOpts>) -> HttpResponse {
    let nb_white = opts.nb_white.unwrap_or(2);
    let nb_black = opts.nb_black.unwrap_or(2);

    let puzzle = blind::games::checks::generate_puzzle(nb_white, nb_black);

    HttpResponse::Ok().json(puzzle)
}

async fn next_tactics_puzzle(
    ctx: web::Data<AppCtx>,
    web::Query(opts): web::Query<PuzzleOpts>,
) -> HttpResponse {
    let min_limit = opts.rating - 200;
    let max_limit = opts.rating + 200;

    let res = db::pick_random_puzzle(min_limit, max_limit, &ctx.pool).await;

    match res {
        Ok(Some(puzzle)) => {
            debug!(
                "problem opts: rating: {}, moves_to_follow: {}",
                puzzle.rating, opts.moves_to_follow
            );
            match generate_tactics_puzzle(&puzzle, opts.moves_to_follow, &ctx.pool).await {
                Ok(p) => HttpResponse::Ok().json(p),
                Err(e) => {
                    error!("failed to generate a puzzle: {:?}", e);
                    HttpResponse::InternalServerError().finish()
                }
            }
        }
        Ok(None) => {
            error!("No puzzles were returned");
            HttpResponse::InternalServerError().finish()
        }
        Err(e) => {
            error!("{}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    std::env::set_var("RUST_LOG", "info");
    env_logger::init();

    let opt = Opt::from_args();

    info!("creating db pool...");

    let pool = db::mk_pool().await?;
    let _ = pool.get().await.unwrap();

    if let Some(file) = opt.puzzles_file.clone() {
        info!("loading puzzles from csv file...");
        let tactics = load_puzzles(&file).expect("error loading puzzles from csv file");

        info!("importing puzzles from csv file to postgres...");

        db::import_tactics_puzzles(tactics.clone(), opt.batch_size, &pool).await?;
    }

    let ctx = AppCtx {
        pool: Arc::new(pool),
    };

    let host = env::var("BLIND_HOST").unwrap_or_else(|_| "127.0.0.1".to_string());
    let port = env::var("BLIND_PORT").map_or(9999_u16, |v| v.parse().unwrap());
    let addr = format!("{}:{}", host, port);

    info!("starting blind tactics server...");
    info!("listening on {}", addr.to_string());

    HttpServer::new(move || {
        let ctx = ctx.clone();

        App::new()
            .app_data(web::Data::new(ctx))
            .route(
                "/api/games/tactics/next",
                web::get().to(next_tactics_puzzle),
            )
            .route("/api/games/checks/next", web::get().to(next_checks_puzzle))
            .route(
                "/api/games/common_squares/next",
                web::get().to(next_common_squares_puzzle),
            )
            .route(
                "/api/games/knight_path/next",
                web::get().to(next_knight_path_puzzle),
            )
            .service(
                actix_files::Files::new("/", opt.static_files.clone())
                    .prefer_utf8(true)
                    .index_file("index.html"),
            )
    })
    .bind(addr)?
    .run()
    .await
}
