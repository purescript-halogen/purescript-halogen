let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/components-inputs/src/**/*.purs" ],
  dependencies = config.dependencies
}
