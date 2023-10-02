let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/raw-html/**/*.purs" ],
  dependencies = config.dependencies 
}
