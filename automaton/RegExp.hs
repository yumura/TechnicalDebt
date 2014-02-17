module RegExp ( RegExp
              , (->-)
              , (-+-)
              , star
              , parser
              ) where


import Data.Set (Set, union, insert)
import DFA (DFA, DFAd)
import NFA (NFA)
import NFAwE (NFAwE, mkNFAwE, brNFAwEd)

import qualified Data.Set as Set
import qualified DFA
import qualified NFAwE


data RegExp a = Empty
              | Epsilon
              | Sym a
              | Cat  [RegExp a]
              | Or   (Set (RegExp a))
              | Star (RegExp a)
                deriving (Eq, Ord)


instance Show a => Show (RegExp a) where
    show Empty    = "∅"
    show Epsilon  = "ε"
    show (Sym x)  = show x

    show (Star (Sym x)) = show x      ++ "*"
    show (Star (Or  x)) = show (Or x) ++ "*"
    show (Star x)       = "(" ++ show x ++ ")*"
    
    show (Cat xs) = showList xs
        where
            showList (x:[]) = show x
            showList (x:xs) = show x ++ showList xs

    show (Or xs)  = "(" ++ showSet (Set.toList xs) ++ ")"
        where
            showSet (x:[]) = show x
            showSet (x:xs) = show x ++ '|' : showSet xs

{--}
infixr 7 ->-
infixr 6 -+-

(->-) :: RegExp a -> RegExp a -> RegExp a
Empty    ->- x        = x
x        ->- Empty    = x
Epsilon  ->- x        = x
x        ->- Epsilon  = x
(Cat xs) ->- (Cat ys) = Cat (xs ++ ys)
(Cat xs) ->- y        = Cat (xs ++ [y])
x        ->- (Cat ys) = Cat (x : ys)
x        ->- y        = Cat [x, y]


(-+-) :: (Ord a) => RegExp a -> RegExp a -> RegExp a
Empty   -+- x       = x
x       -+- Empty   = x
(Or xs) -+- (Or ys) = Or (xs `union` ys)
(Or xs) -+- x       = Or (x `insert` xs)
x       -+- (Or xs) = Or (x `insert` xs)
x       -+- y       = Or (Set.fromList [x, y])


star :: (Ord a) => RegExp a -> RegExp a
star Empty                   = Epsilon
star Epsilon                 = Epsilon
star (Star x)                = Star x
star x                       = Star x


parser :: String -> RegExp Char
parser "" = Empty
parser xs = parser' ('(' : www xs) [(Empty ->-)]
    where
        www      []  = ")"
        www ('(':xs) = "(("  ++ www xs
        www (')':xs) = "))"  ++ www xs
        www ('|':xs) = ")|(" ++ www xs
        www ( x :xs) = x : www xs


parser' :: String -> [RegExp Char -> RegExp Char] -> RegExp Char
parser' (         []) (   f:[]) = f Empty
parser' ('\\': x :xs) (   f:fs) = parser' xs $ (f (Sym x)           ->-) : fs
parser' ( '(':')':xs) (   f:fs) = parser' xs $ (f Epsilon           ->-) : fs
parser' (     '*':xs) (   f:fs) = parser' xs $ (f Epsilon           ->-) : fs
parser' (     '|':xs) (   f:fs) = parser' xs $ (f Epsilon           -+-) : fs
parser' (     '(':xs) (     fs) = parser' xs $ (Empty               ->-) : fs
parser' ( ')':'*':xs) (f:f':fs) = parser' xs $ (f' (star $ f Empty) ->-) : fs
parser' ( ')':'|':xs) (f:f':fs) = parser' xs $ (f' (f Empty)        -+-) : fs
parser' (     ')':xs) (f:f':fs) = parser' xs $ (f' (f Empty)        ->-) : fs
parser' (  x :'*':xs) (   f:fs) = parser' xs $ (f (star $ Sym x)    ->-) : fs
parser' (  x :'|':xs) (   f:fs) = parser' xs $ (f (Sym x)           -+-) : fs
parser' (      x :xs) (   f:fs) = parser' xs $ (f (Sym x)           ->-) : fs


{--}
fromDFA :: (Eq a, Ord b) => DFA a b -> RegExp b
fromDFA (Nothing,  _) = Empty
fromDFA (Just i, dfa) = sumRegExp $ map f js
    where
        (js, _) = DFA.brDFAd dfa
        ks      = Set.toList $ DFA.states dfa
        f j     = r dfa i j ks


r :: (Eq a, Ord b) => DFAd a b -> a -> a -> [a] -> RegExp b
r dfa i j []
    | i == j    = Epsilon -+- x
    | otherwise = x
        where
            x = sumRegExp . map Sym . filter ((== Just j) . DFA.transition dfa i) . Set.toList $ DFA.inSymbols dfa

r dfa i j (k:ks) = r dfa i j ks -+- r dfa i k ks ->- star (r dfa k k ks) ->- r dfa k j ks


sumRegExp :: (Ord a) => [RegExp a] -> RegExp a
sumRegExp = foldl (-+-) Empty


{--}
toNFAwE :: (Ord a) => RegExp a -> NFAwE ([Int], Bool) a 
toNFAwE = toNFAwE' []


toNFAwE' :: (Ord a) => [Int] -> RegExp a -> NFAwE ([Int], Bool) a 
toNFAwE' xs Empty       = mkNFAwE [(xs, False)] [(xs, True)] [] []
toNFAwE' xs Epsilon     = mkNFAwE [(xs, True )] [(xs, True)] [] []
toNFAwE' xs (Sym y)     = mkNFAwE [(xs, False)] [(xs, True)] [] [(((xs, False), y), [(xs, True)])]


toNFAwE' xs (Cat []    ) = toNFAwE' xs Epsilon
toNFAwE' xs (Cat (y:ys)) = mkNFAwE [f] [t] es' (ts1 ++ ts2)
    where
        f = (xs, False)
        t = (xs, True )
        (s1, nfa1)      = toNFAwE' (0:xs) y
        (as1, es1, ts1) = brNFAwEd nfa1
        (s2, nfa2)      = toNFAwE' (2:xs) (Cat ys)
        (as2, es2, ts2) = brNFAwEd nfa2
        es' = (f, [Set.findMin s1]) : es1' ++ es2' ++ es1 ++ es2
            where
                es1' = zip as1 (repeat [Set.findMin s2])
                es2' = zip as2 (repeat [t])


toNFAwE' xs (Or ys)
    | Set.null ys = toNFAwE' xs Epsilon 
    | otherwise   = mkNFAwE [f] [t] es' (ts1 ++ ts2)
        where
            f = (xs, False)
            t = (xs, True )
            (y, ys') = Set.deleteFindMin ys
            (s1, nfa1)      = toNFAwE' (4:xs) y
            (as1, es1, ts1) = brNFAwEd nfa1
            (s2, nfa2)      = toNFAwE' (6:xs) (Or ys)
            (as2, es2, ts2) = brNFAwEd nfa1
            es' = (f, [Set.findMin s1, Set.findMin s2]) : es1 ++ es2


toNFAwE' xs (Star y) = mkNFAwE [t] [t] es ts
    where
        t = (xs, True)
        (s, nfa)     = toNFAwE' (8:xs) y
        (as, es, ts) = brNFAwEd nfa
        es' = (t, [Set.findMin s]) : zip as (repeat [t]) ++ es
