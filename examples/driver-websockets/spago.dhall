let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/driver-websockets/src/**/*.purs" ],
  dependencies = config.dependencies # [ "aff-coroutines", "web-socket" ]
}
