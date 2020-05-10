let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/components-inputs/**/*.purs" ],
  dependencies = config.dependencies 
}
