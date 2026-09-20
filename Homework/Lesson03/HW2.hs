module HW2 where

-- Problem #1: 写出 and 函数的三种其他的定义
and1 :: Bool -> Bool -> Bool
and1 x y | x == False = False
         | y == False = False
         | otherwise  = True

and2 :: Bool -> Bool -> Bool
and2 x y = if x == False then False else if y == False then False else True

and3 :: Bool -> Bool -> Bool
and3 x y = if x == True then if y == True then True else False else False
-- End Problem #1

-- Problem #2: 给出函数 div 的一种或多种定义
-- 不用考虑第二个参数为 0 的情况
pdiv :: Integer -> Integer -> Integer   --div for positive integers
pdiv x y | x < y  = 0
         | x >= y = 1 + pdiv (x - y) y

div1 :: Integer -> Integer -> Integer
div1 x y | x > 0 && y > 0 = pdiv x y
         | x > 0 && y < 0 = -(pdiv x (-y))
         | x < 0 && y > 0 = -(pdiv (-x) y)
         | x < 0 && y < 0 = pdiv (-x) (-y)
-- Notion: Easy to learn that it's round toward 0.

-- 如果你认为这个问题无解或很难，请在代码中展示你努力尝试得到的结果，并以注释形
-- 式描述你遇到的困难。如果你认真做到了这些，你的本次作业分数并不会扣减。
-- End Problem #2

-- Problem #3: 写出阶乘函数的其他定义：
-- Part #3.1: 使用条件方程组
factGuard :: Integer -> Integer
factGuard x | x == 0    = 1
            | otherwise = x * factGuard (x - 1)
-- End Part #3.1

-- Part #3.2: 使用分支表达式
factBranch :: Integer -> Integer
factBranch x = if x == 0 then 1 else x * factBranch (x - 1)
-- End Part #3.2
-- End Problem #3
