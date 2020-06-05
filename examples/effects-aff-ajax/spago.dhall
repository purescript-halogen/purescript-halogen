let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/effects-aff-ajax/src/**/*.purs" ],
  dependencies = config.dependencies # [ "affjax" ]
}
