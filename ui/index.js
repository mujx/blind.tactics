require('process')
require('typeface-open-sans')
require('@fontsource/roboto')

if (process.env.NODE_ENV == "production") {
  require("./dce-output/Main").main();
} else {
  require("./output/Main").main();
}
