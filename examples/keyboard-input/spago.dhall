let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/keyboard-input/**/*.purs" ],
  dependencies = config.dependencies
}
