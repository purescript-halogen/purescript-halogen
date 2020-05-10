let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/driver-routing/**/*.purs" ],
  dependencies = config.dependencies # [ "aff-coroutines" ]
}
