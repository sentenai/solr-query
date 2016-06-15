import Test.DocTest

main :: IO ()
main = doctest
  [ "-XDataKinds"
  , "-XOverloadedStrings"
  , "src"
  ]
