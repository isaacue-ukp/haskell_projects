open import Data.Nat
  using (ℕ; zero; suc; _+_; _*_; _≤_)

open import Data.Product
  using (Σ; _,_; _×_)

open import Data.Sum
  using (_⊎_; inj₁; inj₂)

open import Data.Nat.Properties
  using (+-suc)

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; sym; trans)

Even : ℕ → Set
Even n = Σ ℕ λ k → n ≡ 2 * k

Odd : ℕ → Set
Odd n = Σ ℕ λ k → n ≡ suc (2 * k)

infix 4 _∣_

_∣_ : ℕ → ℕ → Set
a ∣ b = Σ ℕ λ k → b ≡ a * k

even→div : ∀ {n : ℕ} → Even n → 2 ∣ n
even→div (k , proof) = k , proof

div→even : ∀ {n : ℕ} → 2 ∣ n → Even n
div→even (k , proof) = k , proof

2*suc : (k : ℕ) → 2 * suc k ≡ suc (suc (2 * k))
2*suc k = cong suc (+-suc k (k + 0))

even-or-odd : ∀ {n : ℕ} → Even n ⊎ Odd n
even-or-odd {zero} = inj₁ (0 , refl)
even-or-odd {suc zero} = inj₂ (0 , refl)
even-or-odd {suc (suc n)} with even-or-odd {n}
... | inj₁ (k , proof) =
  inj₁ (suc k ,
    trans 
      (cong suc (cong suc proof))
      (sym (2*suc k))
  )
... | inj₂ (k , proof) =
  inj₂ (suc k ,
    trans
      (cong suc (cong suc proof))
      (sym (cong suc (2*suc k)))
  )

Prime : ℕ → Set
Prime p = (2 ≤ p) × ((d : ℕ) → (d ∣ p) → (d ≡ 1) ⊎ (d ≡ p))

