begin;

CREATE TABLE IF NOT EXISTS tactics_puzzles (
  id               TEXT    PRIMARY KEY,
  fen              TEXT    NOT NULL,
  moves            TEXT    NOT NULL,
  rating           INTEGER NOT NULL,
  rating_deviation INTEGER NOT NULL,
  popularity       INTEGER NOT NULL,
  nb_plays         INTEGER NOT NULL,
  themes           TEXT    NOT NULL,
  game_url         TEXT    NOT NULL,
  pgn              TEXT
);

commit;
