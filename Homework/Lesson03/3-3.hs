import GHC.Internal.Natural (Natural)

-- Guarded Equation
factorial1 :: Natural -> Natural
factorial1 x | x == 0    = 1
             | otherwise = x * factorial1 (x - 1)

--Conditional Expression
factorial2 :: Natural -> Natural
factorial2 x = if x == 0 then 1 else x * factorial2 (x - 1)