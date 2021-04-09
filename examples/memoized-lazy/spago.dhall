let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/memoized-lazy/**/*.purs" ],
  dependencies = config.dependencies
}
