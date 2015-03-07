module Shapes
(Shape, makeCircle) where

data Bool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

data Person = Man Int | Woman | Child Int|Int

data List a = List (List a) | Nil

-- data Int = A|B|C


makeCircle = Circle 1 1 1

func :: Shape -> Float
func (Circle _ _ x) = x
func (Rectangle _ _ _ x) = x

maybe' :: Maybe a -> a
maybe' (Just x) = x
maybe' Nothing = undefined

li :: List Int -> Int
li (List x) = 1 + (li x)
li Nil = 0

