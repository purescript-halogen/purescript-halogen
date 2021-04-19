let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/interpret/src/**/*.purs" ],
  dependencies = config.dependencies # [ "affjax" ]
}
