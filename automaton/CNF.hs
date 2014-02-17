module CNF ( CNF
           , nonterminals
           , terminals
           , rules
           , tRules
           , hasEpsilon
           , start

           , mkCNF
           , brCNF

           , toCFG
           , fromCFG
           , fromCFG'
           ) where


import Data.Set (Set, member)
import Data.Map (Map)
import Data.Maybe (fromJust)

import qualified Data.Set as Set
import qualified Data.Map as Map

import CFG (CFG, mkCFG, brCFG, delUnitRules)

import qualified CFG


data CNF a b = CNF { rules      :: Set (a, (a, a))
                   , tRules     :: Set (a, b)
                   , hasEpsilon :: Bool 
                   , start      :: a
                   } deriving (Eq, Show)


nonterminals :: (Ord a, Ord b) => CNF a b -> Set a
nonterminals cnf = Set.fromList $ s : map fst trs ++ concat [[x, y, z] | (x, (y, z)) <- nrs]
    where
        (s, _, trs, nrs) = brCNF cnf


terminals :: (Ord b) => CNF a b -> Set b
terminals cnf = Set.fromList $ map snd trs
    where
        (_, _, trs, _) = brCNF cnf


{--}
mkCNF :: (Ord a, Ord b) => a -> Bool -> [(a, b)] -> [(a, (a, a))] -> CNF a b
mkCNF s h trs nrs = CNF
    { rules      = Set.fromList nrs
    , tRules     = Set.fromList trs
    , hasEpsilon = h
    , start      = s
    }


brCNF :: CNF a b -> (a, Bool, [(a, b)], [(a, (a, a))])
brCNF cnf = ( start cnf
            , hasEpsilon cnf
            , Set.toList $ tRules cnf
            , Set.toList $ rules cnf
            )


{--}
toCFG :: (Ord a, Ord b) => CNF a b -> CFG a b
toCFG cnf = mkCFG s $ h' ++ trs' ++ nrs'
    where
        (s, h, trs, nrs) = brCNF cnf
        h'   = [(start cnf, []) | h]
        trs' = map (\(x, y) -> (x, [Right y])) trs
        nrs' = map (\(x, (y, z)) -> (x, [Left y, Left z])) nrs


fromCFG :: (Ord a, Ord b) => a -> CFG a b -> Maybe (CNF [Either a b] b)
fromCFG x = fmap fromCFG'' . delUnitRules x


fromCFG' :: (Ord a, Ord b) => CFG a b -> CNF [Either a b] b
fromCFG' cfg = fromJust $ fromCFG (CFG.start cfg) cfg


fromCFG'' :: (Ord a, Ord b) => CFG a b -> CNF [Either a b] b
fromCFG'' cfg = mkCNF [Left s] h (trs1 ++ trs2) $ concatMap f rs
    where
        (s, rs) = brCFG cfg

        h       = (CFG.start cfg, []) `member` CFG.rules cfg

        trs1 = [([Left x], y) | (x, Right y:[]) <- rs]
        trs2 = map (\x -> ([Right x], x)) (Set.toList $ CFG.terminals cfg) 

        f (x, [])         = []
        f (x, Right y:[]) = []
        f (x, y:z:[])     = [([Left x], ([y], [z]))]
        f (x, y:z:w:xs)   = ([Left x], ([y], z:w:xs)) : g (z:w:xs)
            where
                g (x:y:[])   = [([x, y], ([x], [y]))]
                g (x:y:z:xs) = (x:xs, ([x], y:z:xs)) : g (y:z:xs)

