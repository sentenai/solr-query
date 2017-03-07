import Prelude.Compat
import Test.DocTest

main :: IO ()
main = doctest
  [ "-XConstraintKinds"
  , "-XDataKinds"
  , "-XDeriveFunctor"
  , "-XExistentialQuantification"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XGADTs"
  , "-XGeneralizedNewtypeDeriving"
  , "-XInstanceSigs"
  , "-XLambdaCase"
  , "-XKindSignatures"
  , "-XMultiParamTypeClasses"
  , "-XNoImplicitPrelude"
  , "-XOverloadedStrings"
  , "-XPatternSynonyms"
  , "-XPolyKinds"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "src"
  ]
