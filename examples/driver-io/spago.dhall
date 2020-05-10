let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/driver-io/**/*.purs" ],
  dependencies = config.dependencies 
}
