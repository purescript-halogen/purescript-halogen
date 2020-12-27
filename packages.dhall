let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.14/src/packages.dhall sha256:2c0a5af7ed5158218e0068f2328101fd9f0461e17ea37298e5af6875a96f34ac

in  upstream
  with event.version = "master"
  with event.repo = "https://github.com/thomashoneyman/purescript-event"

  with filterable.version = "master"
  with filterable.repo = "https://github.com/thomashoneyman/purescript-filterable"

  with freeap.version = "master"
  with freeap.repo = "https://github.com/thomashoneyman/purescript-freeap"

  with quickcheck-laws.version = "master"
  with quickcheck-laws.repo = "https://github.com/thomashoneyman/purescript-quickcheck-laws"
