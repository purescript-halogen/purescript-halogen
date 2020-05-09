let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/effects-aff-ajax/**/*.purs" ],
  dependencies = config.dependencies # [ "affjax" ]
}
