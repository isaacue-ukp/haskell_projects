data _≡_ {ℓ} {A : Set ℓ} (x : A) : A → Set ℓ where
    refl : x ≡ x
{-# BUILTIN EQUALITY _≡_ #-}

data 𝔹 : Set where
    tt : 𝔹
    ff : 𝔹

data ℕ : Set where
    zero : ℕ
    suc  : ℕ → ℕ

sym : ∀ {x y : ℕ} → x ≡ y → y ≡ x
sym refl = refl

cong : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {x y : A} →
       (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

_*_ : ℕ → ℕ → ℕ
zero * n = zero
suc m * n = n + (m * n)

+0 : (x : ℕ) → (x + zero) ≡ x
+0 zero = refl
+0 (suc x) rewrite +0 x = refl

+suc : (x y : ℕ) → (x + (suc y)) ≡ suc (x + y) 
+suc zero y = refl
+suc (suc x) y rewrite +suc x y = refl

+assoc : (x y z : ℕ) → (x + (y + z)) ≡ ((x + y) + z)
+assoc zero y z = refl
+assoc (suc x) y z rewrite +assoc x y z = refl

+comm : (x y : ℕ) → (x + y) ≡ (y + x) 
+comm zero y rewrite +0 y = refl 
+comm (suc x) y rewrite +suc y x | +comm x y = refl    

*distribr : (x y z : ℕ) → ((x + y) * z) ≡ ((x * z) + (y * z)) 
*distribr zero y z = refl
*distribr (suc x) y z 
    rewrite *distribr x y z 
    = +assoc z (x * z) (y * z)

*0 : ∀ {n : ℕ} → (n * zero) ≡ zero
*0 {zero} = refl
*0 {suc n} = *0 {n}

*comm : (x y : ℕ) → (x * y) ≡ (y * x)
*comm zero y = sym (*0 {y})
*comm (suc x) zero = *0 {x}
*comm (suc x) (suc y) = cong suc (lemma x y) where
    lemma : (x y : ℕ) → (y + (x * suc y)) ≡ (x + (y * suc x))
    lemma x y 
        rewrite *comm (x) (suc y)
              | *comm (y) (suc x)
              | +assoc (y) (x) (y * x)
              | +assoc (x) (y) (x * y)
              | +comm (y) (x)
              | *comm (y) (x)
        = refl

*assoc : (x y z : ℕ) → (x * (y * z)) ≡ ((x * y) * z)
*assoc zero y z = refl
*assoc (suc x) y z 
    rewrite *distribr y (x * y) z 
    = cong (λ k → (y * z) + k) (*assoc x y z)