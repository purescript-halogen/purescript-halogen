module I18n where

import Data.Newtype (class Newtype)

type I18n' s =
  { buttonLabel :: s
  , description :: s
  , congratulations :: s
  }

newtype I18n = I18n (I18n' String)

derive instance newtypeI18n :: Newtype I18n _

english ∷ I18n
english = I18n
  { buttonLabel: "Hello"
  , description: "Click the button"
  , congratulations: "Good work!"
  }

french ∷ I18n
french = I18n
  { buttonLabel: "Bonjour"
  , description: "Cliquez sur le bouton"
  , congratulations: "Bon travail!"
  }
