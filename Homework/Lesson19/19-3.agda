-- 19.3. Prove that if value a satisfies predicate p, then
-- takeWhile p (repeat n a) is equal to repeat n a, where
-- takeWhile is the function you defined in the previous problem.

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.List using (List; []; _∷_)
open import Data.Nat using (ℕ; zero; suc)

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)

open import 19-1 using (takeWhile)
open import 19-2 using (repeat)

𝔹 = Bool
𝕃 = List

keep : ∀ {ℓ} {A : Set ℓ} →
       (a : A) → (n : ℕ) → (p : A → 𝔹) → p a ≡ true →
       takeWhile p (repeat n a) ≡ repeat n a
keep a zero p t = refl
keep a (suc n) p t rewrite t = cong (λ k → a ∷ k) (keep a n p t)