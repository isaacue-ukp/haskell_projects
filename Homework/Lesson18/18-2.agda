data _≡_ {ℓ} {A : Set ℓ} (x : A) : A → Set ℓ where
    refl : x ≡ x
{-# BUILTIN EQUALITY _≡_ #-}

data 𝔹 : Set where
    tt : 𝔹
    ff : 𝔹

data ℕ : Set where
    zero : ℕ
    suc  : ℕ → ℕ

_||_ : 𝔹 → 𝔹 → 𝔹
tt || _ = tt
ff || b = b

_<_ : ℕ → ℕ → 𝔹
zero < zero = ff
zero < suc _ = tt
suc x < suc y = x < y
suc _ < zero = ff

_=ℕ_ : ℕ → ℕ → 𝔹
zero =ℕ zero = tt
suc x =ℕ suc y = x =ℕ y
_ =ℕ _ = ff

no<self : (n : ℕ) -> (n < n) ≡ ff
no<self zero = refl
no<self (suc n) rewrite no<self n = refl

<-antisym : (x y : ℕ) → (x < y) ≡ tt → (y < x) ≡ ff
<-antisym zero zero ()
<-antisym zero (suc y) _ = refl
<-antisym (suc x) zero ()
<-antisym (suc x) (suc y) p = <-antisym x y p

<-trichotomy : (x y : ℕ) → (((x < y) || (x =ℕ y)) || (y < x)) ≡ tt
<-trichotomy zero zero = refl
<-trichotomy zero (suc y) = refl
<-trichotomy (suc x) zero = refl
<-trichotomy (suc x) (suc y) = <-trichotomy x y