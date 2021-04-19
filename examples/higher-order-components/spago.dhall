let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/higher-order-components/src/**/*.purs" ],
  dependencies = config.dependencies
}
