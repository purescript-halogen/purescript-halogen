let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/driver-websockets/**/*.purs" ],
  dependencies = config.dependencies # [ "arrays", "web-socket" ]
}
