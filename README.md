# blind.tactics

[![CircleCI](https://circleci.com/gh/mujx/blind.tactics.svg?style=svg)](https://circleci.com/gh/mujx/blind.tactics)
[![BuiltWithNix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://nixos.org/)

Web application focused on blindfold chess training. It provides tactics and games to improve your
board visualization.

All tactics puzzles are imported from [lichess](https://database.lichess.org/#puzzles).

## Features

- Games
  - Tactics where you'll have to mentally follow a series of moves before solving the position.
  - Given a position without a board, find all the possible checks.
  - Enumerate all the shortest knight paths between two squares.
  - Given a set of pieces find the squares that are attacked by all.

With more to come!

## Build

### UI

With [nix](https://nixos.org/download.html) installed

```bash
make -C ui bundle
```

The static files will be located at `ui/dist`.

### Server

With [cargo](https://github.com/rust-lang/cargo) installed

```bash
cargo build --release
```

You can access the binary at `./target/release/server`.

### Docker

Alternatively you can create a docker image that bundles the UI and the server.

```bash
docker build -t blind:latest .
```

## Deploy

:warning: At the current stage, the SQL schema and the puzzle import will have to be done manually.

The following command will create a Postgres database, the UI as a single page application and the
API server.

```bash
docker-compose up
```

The app will be available at http://localhost:9999

## Contributing

Any kind of contribution is greatly appreciated. This could be:

- Bug fixes
- Suggesting/Implementing new features
- UI/UX improvements/suggestions
- Code refactoring
