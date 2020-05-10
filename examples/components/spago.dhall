let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/components/**/*.purs" ],
  dependencies = config.dependencies 
}
