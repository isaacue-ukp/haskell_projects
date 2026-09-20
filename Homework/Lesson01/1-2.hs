-- 1-2 Define list length function using foldr.

import Prelude hiding (foldr)
-- Why the fuck need I redefine the foldr!?

foldr func orig [] = orig
foldr func orig (n:ns) = func n (foldr func orig ns)

addup x n = n + 1

llt = foldr addup 0