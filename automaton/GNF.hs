module GNF ( GNF
           , nonterminals
           , terminals
           , rules
           , hasEpsilon
           , start

           , mkGNF
           , brGNF

           , toCFG
           , fromCNF
           , fromCFG
           , fromCFG'
           , GNFRule
           ) where


import Data.Set (Set)
import Data.Map (Map)
import Data.List (partition, sort)
import Data.Maybe (isJust, mapMaybe)

import qualified Data.Set as Set
import qualified Data.Map as Map

import CFG (CFG, mkCFG)
import CNF (CNF, brCNF)
import qualified CFG
import qualified CNF


data GNF a b = GNF { rules      :: Set (a, (b, [a]))
                   , hasEpsilon :: Bool 
                   , start      :: a
                   } deriving (Eq, Show)


nonterminals :: (Ord a) => GNF a b -> Set a
nonterminals gnf = Set.fromList $ s : concat [x:xs | (x, (_, xs)) <- rs]
    where
        (s, _, rs) = brGNF gnf


terminals :: (Ord b) => GNF a b -> Set b
terminals gnf = Set.fromList $ map (fst . snd) rs
    where
        (_, _, rs) = brGNF gnf


{--}
mkGNF :: (Ord a, Ord b) => a -> Bool -> [(a, (b, [a]))] -> GNF a b
mkGNF s h rs = GNF
    { rules      = Set.fromList rs
    , hasEpsilon = h
    , start      = s
    }


brGNF :: GNF a b -> (a, Bool, [(a, (b, [a]))])
brGNF gnf = ( start gnf, hasEpsilon gnf, Set.toList $ rules gnf)


{--}
toCFG :: (Ord a, Ord b) => GNF a b -> CFG a b
toCFG gnf = mkCFG s $ h' ++ rs'
    where
        (s, h, rs) = brGNF gnf
        h'  = [(start gnf, []) | h]
        rs' = map (\(x, (y, zs)) -> (x, Right y : map Left zs)) rs


{--}
data GNFRule a = ARule a | ZRule a deriving (Eq, Ord, Show)


inversionRule :: GNFRule a -> GNFRule a
inversionRule (ARule x) = ZRule x
inversionRule (ZRule x) = ARule x


fromCNF :: (Ord a, Ord b) => CNF a b -> GNF (GNFRule a) b
fromCNF cnf = mkGNF (ARule s) h $ subZRule lls rrs ++ trs' ++ lls
    where
        (s, h, trs, nrs) = brCNF cnf
        trs' = [(ARule x, (y, [])) | (x, y) <- trs]
        nrs' = [(ARule x, [ARule y, ARule z] ) | (x, (y, z)) <- nrs]

        (_, lls, rrs) = gs $ fs (nrs', trs', [])
            where
                ns = map ARule . sort . Set.toList $ CNF.nonterminals cnf

                fs = foldl (.) id $ map (f ns) ns
                    where
                        f (x:xs) y ntr
                          | x >  y    = subRule x y ntr
                          | x == y    = delLeftRecursion x ntr
                          | otherwise = ntr

                gs = foldl (.) id . map (g $ reverse ns) $ reverse ns
                    where
                        g (x:xs) y ntr
                          | x < y     = subRule x y ntr
                          | otherwise = ntr


subRule :: (Eq a) => GNFRule a -> GNFRule a
        -> ([(GNFRule a, [GNFRule a])], [(GNFRule a, (b, [GNFRule a]))], [(GNFRule a, [GNFRule a])])
        -> ([(GNFRule a, [GNFRule a])], [(GNFRule a, (b, [GNFRule a]))], [(GNFRule a, [GNFRule a])])
subRule x y (nrs, trs, rrs) = (nrs', trs', rrs)
    where
        (nrs1, nrs2) =  partition (\(z, w:ws) -> z == x && w == y) nrs
        f = map snd . filter ((== y) . fst)

        nrs' = nrs2 ++ [(x, zs ++ xs) | zs <- f nrs, (_, _:xs) <- nrs1]
        trs' = [(x, (z, zs ++ xs)) | (z, zs) <- f trs, (_, _:xs) <- nrs1] ++ trs


delLeftRecursion :: (Eq a) => GNFRule a
                 -> ([(GNFRule a, [GNFRule a])], [(GNFRule a, (b, [GNFRule a]))], [(GNFRule a, [GNFRule a])])
                 -> ([(GNFRule a, [GNFRule a])], [(GNFRule a, (b, [GNFRule a]))], [(GNFRule a, [GNFRule a])])
delLeftRecursion x (nrs, trs, rrs)
    | null nrs1 = (nrs, trs, rrs)
    | otherwise = (nrs'' ++ nrs', trs' ++ trs, rrs' ++ rrs)
        where
            y = inversionRule x

            (nrs0, nrs'') =  partition ((== x) . fst) nrs
            (nrs1, nrs2 ) =  partition ((== x) . head . snd) nrs0

            nrs' = [(x, zs ++ [y]) | (_, zs) <- nrs2]
            trs' = [(x, (z, zs ++ [y])) | (_, (z, zs)) <- filter ((== x) . fst) trs]
            rrs' = concat [[(y, xs), (y, xs ++ [y])] | (_, _:xs) <- nrs1]


subZRule :: (Ord a, Ord b) => [(GNFRule a, (b, [GNFRule a]))] -> [(GNFRule a, [GNFRule a])] -> [(GNFRule a, (b, [GNFRule a]))]
subZRule trs = concatMap f
    where
        f (x, y:ys) = map (\(_, (z, zs)) -> (x, (z, zs ++ ys))) $ filter ((== y) . fst) trs


{--}
fromCFG :: (Ord a, Ord b) => a -> CFG a b -> Maybe (GNF (GNFRule [Either a b]) b)
fromCFG x = fmap fromCNF . CNF.fromCFG x


fromCFG' :: (Ord a, Ord b) => CFG a b -> GNF (GNFRule [Either a b]) b
fromCFG' = fromCNF . CNF.fromCFG'


{--}
member :: (Eq a, Eq b) => [b] -> GNF a b -> Bool
member [] gnf = hasEpsilon gnf
member xs gnf = member' rs xs [start gnf]
    where
        rs = map (\(y, (z, ws)) -> ((y, z), ws)) . Set.toList $ rules gnf


member' :: (Eq a, Eq b) => [((a, b), [a])] -> [b] -> [a] -> Bool
member' rs []     []     = True
member' rs (x:xs) []     = False
member' rs []     (y:ys) = False
member' rs (x:xs) (y:ys) = any (member' rs xs . (++ ys) . snd) $ filter ((== (y, x)) . fst) rs
