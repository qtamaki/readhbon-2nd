module Shapes
(Shape, makeCircle) where

data Bool = False | True

data Shape a = Circle {x::a, y::a, r::a} | Rectangle a a a a deriving (Show)

data Person = Man Int | Woman | Child Int|Int

data List a = List (List a) | Nil

-- data Int = A|B|C


makeCircle = Circle 1 1 1

func :: Shape a -> a
func (Circle _ _ x) = x
func (Rectangle _ _ _ x) = x

maybe' :: Maybe Int -> Int
maybe' (Just x) = x
maybe' Nothing = 0

li :: List Int -> Int
li (List x) = 1 + (li x)
li Nil = 0

