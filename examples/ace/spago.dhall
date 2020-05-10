let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/ace/**/*.purs" ],
  dependencies = config.dependencies # [ "ace" ]
}
