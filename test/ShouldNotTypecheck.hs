module Main where

import Test.DocTest

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Solr.Query
-- >>> let compile' params query = compile params (query :: Query Expr)

main :: IO ()
main = doctest ["test/ShouldNotTypecheck.hs"]

-- | Fuzzy num should not typecheck
--
-- >>> compile' [] ("foo" =: num 5 ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy bool should not typecheck
--
-- >>> compile' [] ("foo" =: true ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy wild should not typecheck
--
-- >>> compile' [] ("foo" =: wild "bar" ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy regex should not typecheck
--
-- >>> compile' [] ("foo" =: regex "bar" ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy datetime should not typecheck
--
-- >>> compile' [] ("foo" =: utctime undefined ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy fuzzed should not typecheck
--
-- >>> compile' [] ("foo" =: (word "bar" ~: 1) ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy boosted should not typecheck
--
-- >>> compile' [] ("foo" =: (word "bar" ^: 1) ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Fuzzy range should not typecheck
--
-- >>> compile' [] ("foo" =: gt (word "bar") ~: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted num should not typecheck
--
-- >>> compile' [] ("foo" =: num 5 ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted bool should not typecheck
--
-- >>> compile' [] ("foo" =: true ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted wild should not typecheck
--
-- >>> compile' [] ("foo" =: wild "bar" ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted regex should not typecheck
--
-- >>> compile' [] ("foo" =: regex "bar" ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted datetime should not typecheck
--
-- >>> compile' [] ("foo" =: utctime undefined ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted fuzzed should not typecheck
--
-- >>> compile' [] ("foo" =: (word "bar" ~: 1) ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted boosted should not typecheck
--
-- >>> compile' [] ("foo" =: (word "bar" ^: 1) ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Boosted range should not typecheck
--
-- >>> compile' [] ("foo" =: gt (word "bar") ^: 1)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Ranged bool should not typecheck
--
-- >>> compile' [] ("foo" =: gt true)
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Ranged wild should not typecheck
--
-- >>> compile' [] ("foo" =: gt (wild "bar"))
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Ranged regex should not typecheck
--
-- >>> compile' [] ("foo" =: gt (regex "bar"))
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Ranged fuzzed should not typecheck
--
-- >>> compile' [] ("foo" =: gt (word "bar" ~: 1))
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Ranged boosted should not typecheck
--
-- >>> compile' [] ("foo" =: gt (word "bar" ^: 1))
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...

-- | Ranged range should not typecheck
--
-- >>> compile' [] ("foo" =: gt (gt (word "bar")))
-- <BLANKLINE>
-- <interactive>...
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...
