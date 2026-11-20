module 19-1 where

-- 19.1. Define a polymorphic function takeWhile, which
-- takes in a predicate on type A (i.e., a function of
-- type A → 𝔹), and a list of As, and returns the longest
-- prefix of the list that satisfies the predicate.

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.List using (List; []; _∷_)

𝔹 = Bool
𝕃 = List

takeWhile : ∀ {ℓ} {A : Set ℓ} → (p : A → 𝔹) → (𝕃 A) → (𝕃 A)
takeWhile _ [] = []
takeWhile pred (x ∷ xs) = if pred x then x ∷ (takeWhile pred xs) else []