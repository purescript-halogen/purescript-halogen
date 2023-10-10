let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/hoc-direct/**/*.purs" ],
  dependencies = config.dependencies  # [ "arrays" ]
}
