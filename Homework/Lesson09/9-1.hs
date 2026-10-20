data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ $ add m n

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Succ m) n = add n $ mul m n