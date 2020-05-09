let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/components-multitype/**/*.purs" ],
  dependencies = config.dependencies 
}
