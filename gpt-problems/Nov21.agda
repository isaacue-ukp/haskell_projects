open import Level

open import Data.Bool
  renaming (Bool to 𝔹; true to tt; false to ff)
  using (if_then_else_)

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong)

data 𝕃 {ℓ : Level} (A : Set ℓ) : Set ℓ where
  []  : 𝕃 A
  _::_ : A → 𝕃 A → 𝕃 A

filter : ∀ {ℓ} {A : Set ℓ} →
  (A → 𝔹) → 𝕃 A → 𝕃 A

filter p [] = []
filter p (x :: xs) =
  let xs' = filter p xs in
  if p x then x :: xs' else xs'

filter-idempotent :
  ∀ {ℓ} {A : Set ℓ}
  (p : A → 𝔹) (xs : 𝕃 A) →
  filter p (filter p xs) ≡ filter p xs

filter-idempotent p [] = refl

filter-idempotent p (x :: xs) with p x in eq
... | tt rewrite eq | filter-idempotent p xs = refl
... | ff = filter-idempotent p xs