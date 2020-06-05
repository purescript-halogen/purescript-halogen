let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/components-multitype/src/**/*.purs" ],
  dependencies = config.dependencies
}
