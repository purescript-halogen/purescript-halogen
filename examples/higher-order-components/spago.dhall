let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/higher-order-components/**/*.purs" ],
  dependencies = config.dependencies
}
