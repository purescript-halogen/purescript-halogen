let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/interpret/**/*.purs" ],
  dependencies = config.dependencies # [ "affjax" ]
}
