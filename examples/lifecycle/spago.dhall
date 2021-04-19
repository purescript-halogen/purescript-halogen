let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/lifecycle/src/**/*.purs" ],
  dependencies = config.dependencies
}
