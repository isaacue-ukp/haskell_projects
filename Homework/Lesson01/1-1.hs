-- 1-1 Define a function product that produces 
-- the product of a list of numbers, and show using 
-- your definition that product [2,3,4] = 24.

import Prelude hiding (product)
-- hide product, otherwise it would report error.

product [] = 1
product (n:ns) = n * product ns