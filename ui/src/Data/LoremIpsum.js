const LoremIpsum = require("lorem-ipsum").LoremIpsum;

const lorem = new LoremIpsum({
  wordsPerSentence: {
    max: 16,
    min: 4,
  },
});

exports.generateWords = function (amount) {
  return lorem.generateWords(amount);
};

exports.generateSentences = function (amount) {
  return lorem.generateSentences(amount);
};
