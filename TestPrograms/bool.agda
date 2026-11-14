module bool where

data 𝔹 : Set where
    tt : 𝔹
    ff : 𝔹

-- not
~_ : 𝔹 → 𝔹
~ tt = ff
~ ff = tt

-- and
_&&_ : 𝔹 → 𝔹 → 𝔹
tt && b = b
ff && b = ff

-- or
_||_ : 𝔹 → 𝔹 → 𝔹
tt || b = tt
ff || b = b

if_then_else_ : ∀ {ℓ} {A : Set ℓ} → 𝔹 → A → A → A
if tt then y else z = y
if ff then y else z = z

_xor_ : 𝔹 → 𝔹 → 𝔹
tt xor ff = tt
ff xor tt = tt
tt xor tt = ff
ff xor ff = ff

_nor_ : 𝔹 → 𝔹 → 𝔹
x nor y = ~ (x || y)


data _≡_ {ℓ} {A : Set ℓ} (x : A) : A → Set ℓ where
    refl : x ≡ x
{-# BUILTIN EQUALITY _≡_ #-}

~~tt : (~ ~ tt) ≡ tt
~~tt = refl

~~ff : (~ ~ ff) ≡ ff
~~ff = refl

~~-elim : ∀ (b : 𝔹) → (~ ~ b) ≡ b
~~-elim tt = ~~tt
~~-elim ff = ~~ff

&&-idem : ∀ {b} → (b && b) ≡ b
&&-idem {tt} = refl
&&-idem {ff} = refl

||≡ff₂ : ∀ {b1 b2} → (b1 || b2) ≡ ff → b2 ≡ ff
||≡ff₂ {tt} ()
||≡ff₂ {ff}{tt} ()
||≡ff₂ {ff}{ff} p = refl

||-cong₁ : ∀ {b1 b1' b2} → 
           b1 ≡ b1' → (b1 || b2) ≡ (b1' || b2)
||-cong₁ refl = refl

||-cong₂ : ∀ {b1 b2 b2'} → 
           b2 ≡ b2' → (b1 || b2) ≡ (b1 || b2')
||-cong₂ p rewrite p = refl

ite-same : ∀ {ℓ} {A : Set ℓ} → 
           (b : 𝔹) (x : A) → 
           (if b then x else x) ≡ x
ite-same tt x = refl
ite-same ff x = refl

ite-arg : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} → 
          (f : A → B) (b : 𝔹) (x y : A) → 
          (f (if b then x else y)) ≡ (if b then f x else f y)
ite-arg func tt x y = refl
ite-arg func ff x y = refl