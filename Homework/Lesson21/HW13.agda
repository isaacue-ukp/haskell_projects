module HW13 where

-- How to input the Unicode characters
-- ===================================
-- ℕ    \bN
-- →    \->
-- ∷    \::
-- ≡    \==
-- ⟨    \<
-- ⟩    \>
-- ∙    \.

open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Bool using (Bool; true; false; _∨_; if_then_else_)
open import Data.List using (List; []; _∷_)
open import Data.Vec using (Vec; []; _∷_)

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong; cong-app)
open Eq.≡-Reasoning using (begin_; step-≡-⟩; step-≡-⟨; step-≡-∣; _∎)

-- Chap. 20

-- Problem 20.1
_by_matrix : (n m : ℕ) → Set
n by m matrix = Vec (Vec ℕ m) n

-- Problem 20.2
-- 20.2(a) zero matrix: all zeros
repeat𝕍 : ∀ {ℓ} {A : Set ℓ} → (a : A)(n : ℕ) → Vec A n
repeat𝕍 a 0 = []
repeat𝕍 a (suc n) = a ∷ (repeat𝕍 a n)

zero-matrix : (n m : ℕ) → n by m matrix
zero-matrix n m = repeat𝕍 (repeat𝕍 zero m) n

-- 20.2(b) matrix indexing
module Problem-20-2-b where
  _<_ : (n m : ℕ) → Bool
  zero  < zero  = false
  zero  < suc _ = true
  suc _ < zero  = false
  suc x < suc y = x < y

  nth𝕍 : ∀ {ℓ} {A : Set ℓ}{m : ℕ} →
         (n : ℕ) → n < m ≡ true → Vec A m → A
  nth𝕍 0 _ (x ∷ _) = x
  nth𝕍 (suc n) p (_ ∷ xs) = nth𝕍 n p xs
  nth𝕍 (suc n) () []
  nth𝕍 0 () []

  matrix-elt : {n m : ℕ}
    → n by m matrix
    → (i j : ℕ)
    → i < n ≡ true
    → j < m ≡ true
    → ℕ
  matrix-elt {zero} {m} [] zero j ()
  matrix-elt {zero} {m} [] (suc i) j ()
  matrix-elt {n} {m} (mat ∷ mat₁) zero j _ p2 = nth𝕍 j p2 mat
  matrix-elt {n} {m} (mat ∷ mat₁) (suc i) = matrix-elt mat₁ i

-- 20.2(c): diagonal matrix, with the same element along the main diagonal
map𝕍 : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {n : ℕ} →
       (A → B) → Vec A n → Vec B n
map𝕍 f [] = []
map𝕍 f (x ∷ xs) = f x ∷ map𝕍 f xs

diagonal-matrix : (n : ℕ) → (d : ℕ) → n by n matrix
diagonal-matrix zero d = []
diagonal-matrix (suc n) d = (d ∷ repeat𝕍 zero n) ∷ map𝕍 (λ v → zero ∷ v) (diagonal-matrix n d)

identity-matrix : (n : ℕ) → n by n matrix
identity-matrix n = diagonal-matrix n 1

-- 20.2(d): transpose
zip : {n m : ℕ} → (a : Vec ℕ n) (b : Vec (Vec ℕ m) n) → Vec (Vec ℕ (suc m)) n
zip [] [] = []
zip (a ∷ a₁) (b ∷ b₁) = (a ∷ b) ∷ zip a₁ b₁

transpose : {n m : ℕ} → n by m matrix → m by n matrix
transpose {zero} {m} [] = repeat𝕍 [] m
transpose {suc n} {m} (x ∷ xs) = zip x (transpose xs)

-- 20.2(e): dot product
_∙_ : {n : ℕ} → (x y : Vec ℕ n) → ℕ
[] ∙ [] = 0
(x ∷ x₁) ∙ (y ∷ y₁) = x * y + (x₁ ∙ y₁)

-- Chap. 21
+-identityʳ : (m : ℕ) → m + 0 ≡ m
+-identityʳ 0 = refl
+-identityʳ (suc m) = 
  begin
    suc m + 0
  ≡⟨⟩
    suc (m + 0)
  ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎

+-suc : (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n = refl
+-suc (suc m) n =
  begin
    suc m + suc n
  ≡⟨⟩
    suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m + n))
  ≡⟨⟩
    suc (suc m + n)
  ∎

-- Bonus: rewrite rules
-- 如果 _+_ 是在第二个参数上递归定义的，那么下面这两个性质就是自动得到的了。
-- 因此，也常常说下面两个性质是 _+_ 在第二个参数上的计算规则。
--
-- 有一种办法可以在 Agda 中根据已经证明的等式添加新的计算规则：使用重写规则。
-- 在证明上面两个定理后，试试加上如下规则：
--
--     {-# REWRITE +-identityʳ +-suc #-}
--
-- 然后重新尝试证明加法交换律：
--
--     +-comm : (m n : ℕ) → m + n ≡ n + m
--     +-comm zero    n = ?
--     +-comm (suc m) n = ?
--
-- 注意把光标放进两个 goal 中分别检查目标的类型，并和不用 REWRITE 之前的比较。
-- 使用 REWRITE 能让我们的证明简化，那么它有什么缺点呢？
--
-- 参看官方的文档：
-- https://agda.readthedocs.io/en/v2.8.0/language/rewriting.html;
