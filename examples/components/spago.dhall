let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/components/src/**/*.purs" ],
  dependencies = config.dependencies
}
