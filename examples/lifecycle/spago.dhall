let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/lifecycle/**/*.purs" ],
  dependencies = config.dependencies
}
