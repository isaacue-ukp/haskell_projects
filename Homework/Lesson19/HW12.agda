module HW12 where

-- How to input the Unicode characters
-- ===================================
-- ℕ    \bN
-- →    \->
-- ∷    \::
-- ≡    \==
-- ⟨    \<
-- ⟩    \>
-- ∨    \or
-- ≟    \?=
-- ≮    \<n

open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Bool using (Bool; true; false; _∨_; if_then_else_)
open import Data.List using (List; []; _∷_)

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong; cong-app)
open Eq.≡-Reasoning using (begin_; step-≡-⟩; step-≡-⟨; step-≡-∣; _∎)

-- Chap. 18

+0 : (x : ℕ) → x + zero ≡ x
+0 zero = refl
+0 (suc x) rewrite +0 x = refl

+suc : (x y : ℕ) → x + suc y ≡ suc (x + y)
+suc zero y = refl
+suc (suc x) y rewrite +suc x y = refl

+assoc : (x y z : ℕ) → x + (y + z) ≡ (x + y) + z
+assoc zero y z = refl
+assoc (suc x) y z rewrite +assoc x y z = refl

+comm : (x y : ℕ) → x + y ≡ y + x
+comm zero y rewrite +0 y = refl
+comm (suc x) y rewrite +suc y x | +comm x y = refl

*distribr : (x y z : ℕ) → (x + y) * z ≡ x * z + y * z
*distribr zero y z = refl
*distribr (suc x) y z
  rewrite *distribr x y z
  = +assoc z (x * z) (y * z)

*0 : (n : ℕ) → n * zero ≡ zero
*0 zero = refl
*0 (suc n) = *0 n

-- Problem 1.1: commutativity of _*_
*-comm : (x y : ℕ) → x * y ≡ y * x
*-comm zero y = sym (*0 y)
*-comm (suc x) zero = *0 x
*-comm (suc x) (suc y) = cong suc (lemma x y) where
  lemma : (x y : ℕ) → y + x * suc y ≡ x + y * suc x
  lemma x y
    rewrite *-comm x (suc y)
          | *-comm y (suc x)
          | +assoc y x (y * x)
          | +assoc x y (x * y)
          | +comm y x
          | *-comm y x
    = refl

-- Problem 1.2: associativity of _*_
*-assoc : (x y z : ℕ) → (x * y) * z ≡ x * (y * z)
*-assoc zero y z = refl
*-assoc (suc x) y z
  rewrite *distribr y (x * y) z
  = cong (λ k → y * z + k) (*-assoc x y z)

-- Problem 2: prove the theorems.
-- Remark: the standard library provides the following comparison based on decidability
--   _<?_ : (x y : ℕ) → Dec (x < y)
-- where `Dec` is the type for decidability;
-- and also the following comparison as inductive relation
--   _<_ : (x y : ℕ) → Set
-- so neither is the one we want.
-- Note: read more on decidability here:
--  * stdlib: https://agda.github.io/agda-stdlib/Relation.Nullary.Decidable.Core.html#1476
--  * PLFA: https://plfa.github.io/Decidable/
-- so we just provide the same definition as given in the slides:
-- (note that stdlib use (Bool; true; false) instead of (𝔹; tt; ff))
infix 9 _≟_
_≟_ : (x y : ℕ) → Bool
zero  ≟ zero  = true
zero  ≟ suc _ = false
suc _ ≟ zero  = false
suc x ≟ suc y = x ≟ y

infix 9 _<_
_<_ : (x y : ℕ) → Bool
zero < zero  = false
zero < suc _ = true
suc _ < zero  = false
suc x < suc y = x < y

-- Problem 2.1
n≮n : (n : ℕ) → n < n ≡ false
n≮n zero = refl
n≮n (suc n) rewrite n≮n n = refl

-- Problem 2.2
<-antisym : (x y : ℕ) → x < y ≡ true → y < x ≡ false
<-antisym zero zero ()
<-antisym zero (suc y) _ = refl
<-antisym (suc x) zero ()
<-antisym (suc x) (suc y) p = <-antisym x y p

-- Problem 2.3
<-trichotomy : (x y : ℕ) → x < y ∨ x ≟ y ∨ y < x ≡ true
<-trichotomy zero zero = refl
<-trichotomy zero (suc y) = refl
<-trichotomy (suc x) zero = refl
<-trichotomy (suc x) (suc y) = <-trichotomy x y


-- Chap. 19

-- This is equivalent to adding a `(A : Set)` to every type with a free variable `A`.
-- For example, `ℕ → A → List A` actually becomes `(A : Set) → ℕ → A → List A`.
variable
  A : Set

takeWhile : (p : A → Bool) → List A → List A
takeWhile _ [] = []
takeWhile p (x ∷ xs) = if p x then x ∷ takeWhile p xs else []

-- This function is usually named `replicate` instead of `repeat`.
replicate : ℕ → A → List A
replicate zero a = []
replicate (suc n) a = a ∷ replicate n a

prop : (a : A) (n : ℕ)
  → (p : A → Bool)
  → p a ≡ true
    -------------------------------------
  → takeWhile p (replicate n a) ≡ replicate n a
prop a zero p t = refl
prop a (suc n) p t rewrite t = cong (λ k → a ∷ k) (prop a n p t)

