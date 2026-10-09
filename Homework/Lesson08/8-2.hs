map :: (a -> b) -> [a] -> [b]
map f = foldr (\x l -> f x : l) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x l -> if p x then x : l else l) []