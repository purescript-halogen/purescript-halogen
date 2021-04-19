let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/driver-routing/src/**/*.purs" ],
  dependencies = config.dependencies # [ "aff-coroutines" ]
}
