import Prelude
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
  , "-XOverloadedStrings"
  , "-XPatternSynonyms"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTypeOperators"
  , "src"
  ]
