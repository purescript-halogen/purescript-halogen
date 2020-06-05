let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/ace/src/**/*.purs" ],
  dependencies = config.dependencies # [ "ace" ]
}
