------------------------------
-- | Solution for problem 24
--   author Tarek
--
------------------------------

import Data.List

result = (sort $ permutation "0123456789") !! 999999

main = do
  putStrln solve
