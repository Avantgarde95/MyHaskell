{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P p1) (P p2) = p1 == p2
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = ""
    show (P [u]) = show u
    show (P [0, 1]) = "x"
    show (P [1, 0]) = "1"
    show (P [u, 0]) = show u
    show (P [0, v]) = (show v) ++ "x"
    show (P [u, 1]) = "x + " ++ (show u)
    show (P [1, v]) = (show v) ++ "x" ++ " + 1"
    show (P [u, v]) = (show v) ++ "x + " ++ (show u)
    show (P l) = let (u:us) = (reverse l)
                  in case u of
                       0 -> (show (P (reverse us)))
                       1 -> "x^" ++ (show (length us)) ++
                           " + " ++ (show (P (reverse us)))
                       otherwise -> (show u) ++ "x^" ++ (show (length us)) ++
                           " + " ++ (show (P (reverse us)))

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l1) (P []) = P l1
plus (P []) (P l2) = P l2
plus (P (u:us)) (P (v:vs)) = P ((u+v):ws) where
    P ws = plus (P us) (P vs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P l1) (P [v]) = P (map (* v) l1)
times (P l1) (P (u:us)) = ((P l1) * (P [u])) + (P (0:ws)) where
    P ws = times (P l1) (P us)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = (* P [-1])
    fromInteger u = P [fromInteger u]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P [u]) m = u
applyP (P l) m = (u * (m ^ (length us))) + (applyP (P (reverse us)) m) where
    (u:us) = (reverse l)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 p = p
    nderiv n p = deriv (nderiv (n-1) p)

-- Exercise 9 -----------------------------------------

extractList (P l) = l

instance Num a => Differentiable (Poly a) where
    deriv (P [u]) = P [0]
    deriv (P (x:p)) = P (reverse (fuckHaskell p 1 [])) where
        fuckHaskell [] _ result = result
        fuckHaskell (x:p) n result = fuckHaskell p (n+1) ((x*n):result)
