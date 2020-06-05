let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/basic/src/**/*.purs" ],
  dependencies = config.dependencies
}
