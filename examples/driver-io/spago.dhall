let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/driver-io/src/**/*.purs" ],
  dependencies = config.dependencies
}
