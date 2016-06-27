import Test.DocTest

main :: IO ()
main = doctest
  [ "-XConstraintKinds"
  , "-XDataKinds"
  , "-XOverloadedStrings"
  , "src"
  ]
