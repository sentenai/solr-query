import Test.DocTest

main :: IO ()
main = doctest
  [ "-XConstraintKinds"
  , "-XDataKinds"
  , "-XDeriveFunctor"
  , "-XExistentialQuantification"
  , "-XFlexibleInstances"
  , "-XGADTs"
  , "-XGeneralizedNewtypeDeriving"
  , "-XInstanceSigs"
  , "-XLambdaCase"
  , "-XKindSignatures"
  , "-XMultiParamTypeClasses"
  , "-XOverloadedStrings"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTypeOperators"
  , "src"
  ]
