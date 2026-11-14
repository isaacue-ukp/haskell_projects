module HW11 where

open import Data.Bool using (Bool; true; false; if_then_else_)

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; step-≡-⟩; step-≡-⟨; step-≡-∣; _∎)

-- problem 17.1
data Day : Set where
  -- fill in your answers here
  monday    : Day
  tuesday   : Day
  wednesday : Day
  thursday  : Day
  friday    : Day
  saturday  : Day
  sunday    : Day

-- problem 17.2
nextday : Day → Day
nextday monday    = tuesday
nextday tuesday   = wednesday
nextday wednesday = thursday
nextday thursday  = friday
nextday friday    = saturday
nextday saturday  = sunday
nextday sunday    = monday

-- problem 17.3
ite-arg : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′}
  → (f : A → B)
  → (b : Bool)
  → (x y : A)
    ----------------------
  → f (if b then x else y)
  ≡ (if b then f x else f y)
ite-arg func tt x y = refl
ite-arg func ff x y = refl