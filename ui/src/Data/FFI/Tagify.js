"use strict";

const Tagify = require("@yaireo/tagify");
const CHESS_MOVE_REGEX = /[BRQNK][a-h][1-8]|[BRQNK][a-h]x[a-h][1-8]|[BRQNK][a-h][1-8]x[a-h][1-8]|[BRQNK][a-h][1-8][a-h][1-8]|[BRQNK][a-h][a-h][1-8]|[BRQNK]x[a-h][1-8]|[a-h]x[a-h][1-8]=(B+R+Q+N)|[a-h]x[a-h][1-8]|[a-h][1-8]x[a-h][1-8]=(B+R+Q+N)|[a-h][1-8]x[a-h][1-8]|[a-h][1-8][a-h][1-8]=(B+R+Q+N)|[a-h][1-8][a-h][1-8]|[a-h][1-8]=(B+R+Q+N)|[a-h][1-8]|[BRQNK][1-8]x[a-h][1-8]|[BRQNK][1-8][a-h][1-8]/;

exports._addTags = function (tagify) {
  return function (tags) {
    return function () {
      tagify.addTags(tags);
    };
  };
};

exports._getTags = function (tagify) {
  return function () {
    return tagify.value.map(x => x.value);
  };
};

exports._removeAllTags = function (tagify) {
  return function () {
    return tagify.removeAllTags();
  };
};

exports._destroy = function (tagify) {
  return function() {
    return tagify.destroy();
  }
}

exports.mkTagify = function (selector) {
  return function () {
    return new Tagify(document.querySelector(selector), {
      pattern: CHESS_MOVE_REGEX,
      callbacks: {
        invalid: onInvalidTag,
      },
    });
  };
};

function onInvalidTag(t) {
  console.log("Inserted invalid tag: ", t);
}
