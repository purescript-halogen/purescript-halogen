module Example.TextNodes.Test (mkTest) where

import Halogen.HTML as HH

mkTest :: forall w i. (Array (HH.HTML w i) -> HH.HTML w i) -> Array (HH.HTML w i)
mkTest contaner =
  [ contaner
      -- | should be rendered to <div></div>
      []

  , contaner
      -- | should be rendered to <div></div>
      [ HH.text ""
      ]

  , contaner
      -- | should be rendered to <div></div>
      [ HH.text ""
      , HH.text ""
      ]

  , contaner
      -- | should be rendered to <div>↵↵</div>
      [ HH.text "\n"
      , HH.text "\n"
      ]

  , contaner
      -- | should be rendered to <div>    </div>
      [ HH.text "  "
      , HH.text "  "
      ]

  , contaner
      -- | should be rendered to <div>  foo    bar  </div>
      [ HH.text "  foo  "
      , HH.text "  bar  "
      ]

  , contaner
      -- | should be rendered to <div>foobar</div>
      [ HH.text "foo"
      , HH.text "bar"
      ]

  , contaner
      -- | should be rendered to <div>foobarbaz</div>
      [ HH.text "foo"
      , HH.text "bar"
      , HH.text "baz"
      ]

  , contaner
      -- | should be rendered to <div>foobar</div>
      [ HH.text "foo"
      , HH.text ""
      , HH.text "bar"
      ]

  , contaner
      -- | should be rendered to <div>foo  bar</div>
      [ HH.text "foo"
      , HH.text "  "
      , HH.text "bar"
      ]

  , contaner
      -- | should be rendered to <div>foo↵bar</div>
      [ HH.text "foo"
      , HH.text "\n"
      , HH.text "bar"
      ]

  , contaner
      -- | should be rendered to <div>foobar</div>
      [ HH.text "foo"
      , HH.text ""
      , HH.text ""
      , HH.text "bar"
      ]

  , contaner
      -- | should be rendered to <div>foo    bar</div>
      [ HH.text "foo"
      , HH.text "  "
      , HH.text "  "
      , HH.text "bar"
      ]
  ]
