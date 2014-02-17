module RG ( RG
          , nonterminals
          , terminals
          , rules
          , start

          , mkRG
          , brRG

          , language
{-
          , toNFAwE
          , toDFA

          , fromDFA
          -}
          ) where


import Data.Set (Set)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)
import Data.Function (on)

import qualified Data.Set as Set

import DFA (DFA, brDFAd)
import NFAwE (NFAwE, mkNFAwE)
import qualified DFA
import qualified NFAwE


data RG a b = RG { rules      :: Set (a, (b, a))
                 , tRules     :: Set (a, b)
                 , hasEpsilon :: Bool 
                 , start      :: a
                 } deriving (Eq, Show)


nonterminals :: (Ord a) => RG a b -> Set a
nonterminals rg = Set.fromList $ s : map fst ts ++ map fst ns ++ map (snd . snd) ns
    where
        (s, _, ts, ns) = brRG rg


terminals :: (Ord b) => RG a b -> Set b
terminals rg = Set.fromList $ map snd ts ++ map (fst . snd) ns
    where
        (_, _, ts, ns) = brRG rg


{-
-}
mkRG :: (Ord a, Ord b) => a -> Bool -> [(a, b)] -> [(a, (b, a))] -> RG a b
mkRG s h trs nrs = RG
    { rules      = Set.fromList nrs
    , tRules     = Set.fromList trs
    , hasEpsilon = h
    , start      = s
    }


brRG :: RG a b -> (a, Bool, [(a, b)], [(a, (b, a))])
brRG rg = (start rg, hasEpsilon rg, Set.toList $ tRules rg, Set.toList $ rules rg)


{-
-}
language :: (Eq a) => RG a b -> [[b]]
language rg = [[] | hasEpsilon rg] ++ language' rg [([], start rg)]


language' :: (Eq a) => RG a b -> [([b], a)] -> [[b]]
language' rg xss = zs ++ language' rg yss
    where
        (_, _, trs, nrs) = brRG rg
        zs  = [reverse (z:xs) | (xs, x) <- xss, (y, z) <- trs, x == y]
        yss = [(z:xs, w) | (xs, x) <- xss, (y, (z, w)) <- nrs, x == y]


{-
-}
toDFA :: (Ord a, Ord b) => a -> RG a b -> DFA [a] b
toDFA x rg = NFAwE.toDFA $ toNFAwE x rg


toNFAwE :: (Ord a, Ord b) => a -> RG a b -> NFAwE a b
toNFAwE x rg = mkNFAwE [s] [x] [(s, [x]) | h] ns
    where
        (s, h, trs, nrs) = brRG rg
        ns = map f . groupBy ((==) `on` fst) . sortBy (comparing fst) $ trs' ++ nrs'
            where
                trs' = [((y, z), [x]) | (y, z) <- trs]
                nrs' = [((y, z), [w]) | (y, (z, w)) <- nrs]
                f xss = (fst $ head xss, concatMap snd xss)


{-
-}
fromDFA :: (Ord a, Ord b) => DFA a b -> Maybe(RG a b)
fromDFA (Nothing,  _) = Nothing
fromDFA (Just s, dfa) = Just $ mkRG s (f s) ts' ns
    where
        ts' = map fst . filter (f . snd) . snd $ brDFAd dfa
        ns = [(s, (i, t)) | ((s, i), t) <- snd $ brDFAd dfa]
        f = flip Set.member (DFA.accepts dfa)
