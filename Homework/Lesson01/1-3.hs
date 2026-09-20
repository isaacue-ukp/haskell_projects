-- 1-3 How should the definition of the function 
-- qsort be modified so that it produces a reverse 
-- sorted version of a list?

import Prelude hiding (reverse)

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]