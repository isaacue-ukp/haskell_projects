module 19-2 where

-- 19.2. Define a function repeat function that takes a number
-- n and an element a, and constructs a list of length n where
-- all elements are just a.

open import Data.List using (List; []; _∷_)
open import Data.Nat using (ℕ; zero; suc)

𝕃 = List

repeat : ∀ {ℓ} {A : Set ℓ} → ℕ -> A -> 𝕃 A
repeat zero a = []
repeat (suc n) a = a ∷ repeat n a