{ name = "ui"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "form-urlencoded"
  , "formatters"
  , "halogen"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "lists"
  , "maybe"
  , "now"
  , "math"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "undefinable"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}