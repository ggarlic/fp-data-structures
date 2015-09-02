module Stack where

class Stack stack where
  empty :: stack a
  isEmpty :: stack a -> Bool

  consS :: a -> stack a -> stack a
  headS :: stack a -> a
  tailS :: stack a -> stack a

instance Stack [] where
  empty = []

  isEmpty [] = True
  isEmpty (_:_) = False

  consS x xs = x:xs

  headS [] = error "empty stack"
  headS (x:_) = x

  tailS [] = error "empty stack"
  tailS (_:xs) = xs

{-
 define stack using a custom datatype
-}

data CustomStack a = Nil
                   | Cons a (CustomStack a)
                   deriving (Show)

instance Stack CustomStack where
  empty = Nil

  isEmpty Nil = True
  isEmpty _ = False

  consS = Cons

  headS (Cons a _) = a
  headS Nil = error "empty stack"

  tailS Nil = error "empty stack"
  tailS (Cons _ xs) = xs

{- append function -}
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
         
appendCustom :: CustomStack a -> CustomStack a -> CustomStack a
appendCustom Nil y = y
appendCustom (Cons h t) y = Cons h (appendCustom t y)

{- appendCustom using foldr -}
instance Foldable CustomStack where
  foldr f z Nil = z
  foldr f z (Cons h t) = f h (foldr f z t)
appendCustom' :: CustomStack a -> CustomStack a -> CustomStack a
appendCustom' xs ys = foldr Cons ys xs

{-update function-}

update :: [a] -> Int -> a -> Maybe [a]
update [] _ _ = Nothing
update (x:xs) i y
  | i < 0 = Nothing
  | i == 0 = return (y:xs)
  | otherwise = do
      new <- update xs (pred i) y
      return (x:new)

updateCustom :: CustomStack a -> Int -> a -> Maybe (CustomStack a)
updateCustom Nil _ _ = Nothing
updateCustom (Cons x xs) i y
  | i < 0 = Nothing
  | i == 0 = return (Cons y xs)
  | otherwise = do
      new <- updateCustom xs (pred i) y
      return (Cons x new)

--exec 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : suffixes (tail xs)
