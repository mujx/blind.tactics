"use strict";

exports.puzzles_ = function () {
  return [
    {
      id: "0009B",
      fen:
        "r2qr1k1/bpp2ppp/p2p2n1/P3p3/1PP1P1n1/B2P1NPb/4BP1P/RN1QR1K1 w - - 3 14",
      moves: ["c4c5", "d6c5", "b4c5", "b7b6", "f3d2", "b6c5"],
      rating: 1228,
      rating_deviation: 77,
      popularity: 100,
      nb_plays: 174,
      themes: ["advantage", "middlegame", "short"],
      orientation: "White",
      solution: "e2g4",
      game_url: "https://lichess.org/4MWQCxQ6/black#32",
    },
    {
      id: "000h7",
      fen: "3q1rk1/1pp3pp/5p1P/4pPP1/rb1pP3/3P1N2/b1PQB3/3K2RR w - - 2 24",
      moves: ["d2c1", "b4a3", "c1d2", "a3b4", "d2c1", "d8a8"],
      rating: 2522,
      rating_deviation: 91,
      popularity: 85,
      nb_plays: 125,
      themes: ["advancedPawn", "crushing", "middlegame", "short"],
      orientation: "White",
      solution: "g5g6",
      game_url: "https://lichess.org/FLmpZbTm/black#52",
    },
    {
      id: "000tp",
      fen: "4r3/3n1pk1/1p4p1/3p3p/2q5/P4N1P/1PR3P1/3Q3K b - - 1 31",
      moves: ["c4b5", "d1d4", "d7f6", "c2f2", "b5c4", "d4b6"],
      rating: 2135,
      rating_deviation: 76,
      popularity: 80,
      nb_plays: 83,
      themes: ["crushing", "endgame", "short", "trappedPiece"],
      orientation: "Black",
      solution: "f6e4",
      game_url: "https://lichess.org/GeXqsW90#67",
    },
    {
      id: "0018S",
      fen: "r3k2r/pp3p2/4p2p/3p2p1/6Q1/1P1P4/q1P2PPP/1N3RK1 w kq - 0 18",
      moves: ["g4d4", "e8c8", "b1c3", "a2c2", "c3b5", "b7b6"],
      rating: 2508,
      rating_deviation: 89,
      popularity: 86,
      nb_plays: 65,
      themes: ["advantage", "endgame", "pin", "short"],
      orientation: "White",
      solution: "d4a1",
      game_url: "https://lichess.org/H1ARO2GL/black#40",
    },
    {
      id: "001cv",
      fen: "4r1k1/5p1p/3p2pP/pp1P2b1/2p5/2P4R/PP6/1K3R2 b - - 3 31",
      moves: ["e8e5", "f1d1", "b5b4", "c3b4", "a5b4", "d1d4"],
      rating: 1820,
      rating_deviation: 76,
      popularity: 86,
      nb_plays: 71,
      themes: ["endgame", "long", "mate", "mateIn3"],
      orientation: "Black",
      solution: "e5e1",
      game_url: "https://lichess.org/Wn5Xtz5X#67",
    },
    {
      id: "001u3",
      fen: "2r3k1/p1q2ppp/Qb2p3/3p4/2nP1P2/B2NP1P1/7P/4N1K1 w - - 8 31",
      moves: ["a3c1", "h7h6", "g1f2", "b6a5", "d3c5", "c7b6"],
      rating: 2274,
      rating_deviation: 83,
      popularity: 52,
      nb_plays: 62,
      themes: ["advantage", "hangingPiece", "middlegame", "short"],
      orientation: "White",
      solution: "a6c8",
      game_url: "https://lichess.org/BBn6ipaK/black#66",
    },
  ];
};