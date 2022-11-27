# blind.tactics

[![ci](https://github.com/mujx/blind.tactics/actions/workflows/ci.yml/badge.svg)](https://github.com/mujx/blind.tactics/actions/workflows/ci.yml)
![Docker Image Version (latest by date)](https://img.shields.io/docker/v/mujx/blind?label=mujx%2Fblind&style=flat-square)

Web application focused on blindfold chess training. It provides tactics and games to improve your
board visualization.

All tactics puzzles are imported from [lichess](https://database.lichess.org/#puzzles).

The application is available at https://blindtactics.com

## Features

- Games
  - Tactics where you'll have to mentally follow a series of moves before solving the position.
  - Given a position without a board, find all the possible checks.
  - Enumerate all the shortest knight paths between two squares.
  - Given a set of pieces find the squares that are attacked by all.

With more to come!

## Build

The application consists of the single-page frontend app and the backend server.

### Frontend

You'll need nodejs and yarn/npm installed.

```bash
cd ui

yarn install
yarn run bundle
```

The static files will be located at `ui/dist`.

### Server

You'll need rustc and cargo installed. Both can be installed through [rustup](https://rustup.rs/).

```bash
cargo build --release
```

The binary can be found at `./target/release/server`. 

Now we can start the server and make it load the frontend application with the
following command:

```bash
./target/release/server -s ./ui/dist
```

Note that you'll also need to provide the necessary environment variables (found
in `docker-compose.yml`) to establish connectivity with the postgres database.

### Docker

Alternatively you can use the pre-built docker image (`mujx/blind:latest`) with
the docker-compose file to spin up the whole system.

```bash
docker-compose up
```

The app will be available at http://localhost:9999

:warning: At the current stage, the SQL schema and the puzzle import will have to
be done manually.

#### Import lichess puzzles

1. Download the csv file from the puzzles [page](https://database.lichess.org/#puzzles).
2. Use the server cli arguments to load them to the database

```bash
./server --puzzles-file puzzles.csv
```

3. You can re-run the above command to refresh your puzzle database.

## Contributing

Any kind of contribution is greatly appreciated. This could be:

- Bug fixes
- Suggesting/Implementing new features
- UI/UX improvements/suggestions
- Code refactoring
