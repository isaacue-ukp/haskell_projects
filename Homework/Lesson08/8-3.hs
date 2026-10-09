import Data.Char (ord, chr)
-- Binary String Transmitter

type Bit = Int

bit2int :: [Bit] -> Int
bit2int = foldr (\x y -> x + 2 * y) 0

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit n = mod n 2 : int2bit (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 $ bits ++ repeat 0

int2bit8 :: Int -> [Bit]
int2bit8 = make8 . int2bit

char2bit8 :: Char -> [Bit]
char2bit8 = int2bit8 . ord

checkBit :: [Bit] -> Bit
checkBit l = if odd $ length $ filter (==1) l then 1 else 0

addcheck :: [Bit] -> [Bit]
addcheck l = l ++ [checkBit l] 

encode' :: String -> [Bit]
encode' = concat . map (addcheck . char2bit8)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

dropcheck :: [[Bit]] -> [[Bit]]
dropcheck [] = []
dropcheck (x:xs) | last x == checkBit (init x) && length x == 9 = init x : dropcheck xs
                 | otherwise                                    = error " "

decode' :: [Bit] -> String
decode' = map (chr . bit2int) . dropcheck . chop9