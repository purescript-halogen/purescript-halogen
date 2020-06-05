let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/keyboard-input/src/**/*.purs" ],
  dependencies = config.dependencies
}
