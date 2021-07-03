"use strict";

const Chessground = require("chessground").Chessground;
const boardClass = ".cg-wrap";

function calculateSquareSize(elem) {
  var containerWidth = parseInt(elem.clientWidth, 10);

  // defensive, prevent infinite loop
  if (!containerWidth || containerWidth <= 0) {
    return 0;
  }

  var boardWidth = containerWidth;

  while (boardWidth % 8 !== 0 && boardWidth > 0) {
    boardWidth = boardWidth - 1;
  }

  return boardWidth;
}

function updateBoardSize() {
  const elem = document.querySelector(boardClass);
  const parentElem = elem.parentElement;
  const size = calculateSquareSize(parentElem);

  elem.style.width = `${size}px`;
  elem.style.height = `${size}px`;
}

exports.removeResizeListener = function () {
  window.removeEventListener("resize", updateBoardSize);
};

exports.addResizeListener = function () {
  updateBoardSize();
  window.addEventListener("resize", updateBoardSize);
};

exports.mkChessGround_ = function (element) {
  return function (config) {
    return function () {
      return Chessground(document.getElementById(element), config);
    };
  };
};

exports.setShapes_ = function (board) {
  return function (shapes) {
    return function () {
      board.setShapes(shapes);
    };
  };
};

exports.destroyBoard = function (board) {
  return function () {
    board.destroy();
  };
};

exports.getShapes_ = function (board) {
  return board.state.drawable.shapes;
};

exports.setConfig_ = function (cg) {
  return function (config) {
    return function () {
      cg.set(config);
    };
  };
};

exports.move_ = function (cg) {
  return function (orig) {
    return function (dest) {
      return function () {
        cg.move(orig, dest);
      };
    };
  };
};
