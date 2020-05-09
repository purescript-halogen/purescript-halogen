let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/effects-effect-random/**/*.purs" ],
  dependencies = config.dependencies # [ "random" ]
}
