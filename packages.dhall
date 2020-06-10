let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200507/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides =
    { web-dom =
        upstream.web-dom // { repo = "https://github.com/srghma/purescript-web-dom.git", version = "patch-1" }
    }

let additions =
  { ace =
      { dependencies =
          [ "arrays"
          , "effect"
          , "foreign"
          , "nullable"
          , "prelude"
          , "web-html"
          , "web-uievents"
          ]
      , repo =
          "https://github.com/purescript-contrib/purescript-ace.git"
      , version =
          "v7.0.0"
      }
  }

in  upstream // overrides // additions
