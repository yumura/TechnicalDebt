module CFG ( CFG
           , nonterminals
           , terminals
           , rules
           , start

           , mkCFG
           , brCFG

           , language

           , delUselessSymbols
           , delDeadSymbols
           , delUnreachableSymbols
           , delEpsilonRules
           , delEpsilonRules'
           , delUnitRules
           , delUnitRules'
           , reduce
           , reduce'
           ) where


import Data.Map (Map)
import Data.Set (Set, member, notMember, union, difference)
import Data.List (partition, sort)
import Data.Maybe (isJust, fromJust)
import Data.Either (lefts, rights, partitionEithers)
import Data.Function (on)

import qualified Data.Map as Map
import qualified Data.Set as Set


data CFG a b = CFG { rules        :: Set (a, [Either a b])
                   , start        :: a
                   } deriving (Eq, Show)


nonterminals :: (Ord a) => CFG a b -> Set a
nonterminals cfg = Set.fromList $ s : map fst rs ++ lefts (concatMap snd rs)
    where
        (s, rs) = brCFG cfg


terminals :: (Ord b) => CFG a b -> Set b
terminals cfg = Set.fromList . rights . concatMap snd . Set.toList $ rules cfg


{--}
mkCFG :: (Ord a, Ord b) => a -> [(a, [Either a b])] -> CFG a b
mkCFG s rs = CFG
    { rules        = Set.fromList rs
    , start        = s
    }


brCFG :: CFG a b -> (a, [(a, [Either a b])])
brCFG cfg = (start cfg, Set.toList $ rules cfg) 


{--}
language :: (Eq a) => CFG a b -> [[b]]
language rg = concatMap (fst . derives rg) [1..]


derives :: (Eq a) => CFG a b -> Int -> ([[b]], [[Either a b]])
derives cfg 0 = ([], [[Left $ start cfg]])
derives cfg k = (concatMap fst xs, concatMap snd xs)
    where
        xs = map (derive cfg) . snd $ derives cfg (k - 1)


derive :: (Eq a) => CFG a b -> [Either a b] -> ([[b]], [[Either a b]])
derive cfg xs = partitionDerive (derive' cfg xs [id]) [] []


derive' ::  (Eq a) => CFG a b -> [Either a b] -> [[Either a b] -> [Either a b]] -> [[Either a b]]
derive' _   []           yss = map ($ []) yss
derive' cfg (Right x:xs) yss = derive' cfg xs $ map (. (Right x :)) yss
derive' cfg (Left  x:xs) yss = derive' cfg xs [(ys zs ++) | ys <- yss, zs <- f x]
    where
        f x = map snd . filter ((== x) . fst) . Set.toList $ rules cfg


partitionDerive :: (Eq a) => [[Either a b]] -> [[b]] -> [[Either a b]] -> ([[b]], [[Either a b]])
partitionDerive []       yss zss = (yss, zss)
partitionDerive (xs:xss) yss zss
    | null ws   = partitionDerive xss (ys:yss) zss
    | otherwise = partitionDerive xss yss (xs:zss)
        where
            (ws, ys) = partitionEithers xs


{--}
delUselessSymbols :: (Ord a, Ord b) => CFG a b -> CFG a b
delUselessSymbols = delUnreachableSymbols . delDeadSymbols


filterSym :: (Ord a, Ord b) => (a -> Bool) -> CFG a b -> CFG a b
filterSym f cfg = mkCFG (start cfg) $ filter (\(n, ns) -> f n && all f (lefts ns)) . snd $ brCFG cfg


fInf :: (Eq a) => (Int -> a) -> Int -> a
fInf f n = f . head $ filter (\k -> f k == f (k + 1)) [n..]


{--}
delDeadSymbols :: (Ord a, Ord b) => CFG a b -> CFG a b
delDeadSymbols cfg = filterSym (`member` fst (fInf (lds cfg) 0)) cfg


lds :: (Ord a) => CFG a b -> Int -> (Set a, Set a)
lds cfg 0 = (Set.empty, nonterminals cfg)
lds cfg k = (lives `union` lives', deads `difference` lives')
    where
        (lives, deads) = lds cfg $ k - 1
        lives' = ls' cfg lives deads


ls' :: (Ord a) => CFG a b -> Set a -> Set a -> Set a
ls' cfg lives = Set.filter (\x -> f x || g x)
    where
        f x = not . null . fst $ derive cfg [Left x]
        g x = any (all (`member` lives) . lefts) . snd $ derive cfg [Left x]


{--}
delUnreachableSymbols :: (Ord a, Ord b) => CFG a b -> CFG a b
delUnreachableSymbols cfg = filterSym (`member` fInf (reachableK cfg) 0) cfg


reachableK :: (Ord a) => CFG a b -> Int -> Set a
reachableK cfg 0 = Set.singleton $ start cfg
reachableK cfg k = rs `union` rs'
    where
        rs  = reachableK cfg $ k - 1
        rs' = Set.fromList . concatMap (concatMap lefts . snd . derive cfg . (: []) . Left) $ Set.toList rs


{--}
delEpsilonRules :: (Ord a, Ord b) => a -> CFG a b -> Maybe (CFG a b)
delEpsilonRules x cfg
    | x `member` nonterminals cfg = Nothing
    | otherwise                   = Just $ delEpsilonRules' x cfg


delEpsilonRules' :: (Ord a, Ord b) => a -> CFG a b -> CFG a b
delEpsilonRules' x cfg
    | Set.singleton (start cfg) == ns = mkCFG (start cfg) rs
    | start cfg `notMember` ns        = mkCFG (start cfg) rs
    | otherwise                       = mkCFG x $ (x, [Left $ start cfg]): rs
        where
            ns = fst $ fInf (nullables cfg) 0
            rs = filter (not . null . snd) . concatMap (\(y, ys) -> zip (repeat y) $ f ys) . Set.toList $ rules cfg
                where
                    f [] = [[]]
                    f (Right y : ys) = map (Right y :) $ f ys
                    f (Left  y : ys)
                        | x `member` ns = f ys ++ map (Left y :) (f ys)
                        | otherwise     = map (Left y :) $ f ys


nullables :: (Ord a, Ord b) => CFG a b -> Int -> (Set a, Set (a, [Either a b]))
nullables cfg 0 = (Set.empty, rules cfg)
nullables cfg k = (ns `union` ns', Set.filter ((`notMember` ns') . fst) ms)
    where
        (ns, ms)  = nullables cfg $ k - 1
        ns' = Set.map fst $ Set.filter (all f . snd) ms
            where
                f (Right x) = False
                f (Left  x) = x `member` ns


{--}
delUnitRules :: (Ord a, Ord b) => a -> CFG a b -> Maybe (CFG a b)
delUnitRules x cfg
    | x `member` nonterminals cfg = Nothing
    | otherwise                   = Just $ delUnitRules' x cfg


delUnitRules' :: (Ord a, Ord b) => a -> CFG a b ->  CFG a b
delUnitRules' x cfg = mkCFG (start cfg') . Set.toList . Set.unions . map (units' cfg') . Set.toList $ nonterminals cfg'
        where
            cfg'  = delEpsilonRules' x cfg


units :: (Ord a, Ord b) => CFG a b -> a -> Int -> (Set a, Set (a, [Either a b]))
units cfg x 0 = (Set.singleton x, Set.filter ((`member` Set.map ((: []) . Left) (nonterminals cfg)) . snd) $ rules cfg)
units cfg x k = (us `union` us', rs `difference` rs')
        where
            (us, rs) = units cfg x $ k - 1
            rs' = Set.filter ((`member` us) . fst) rs
            us' = Set.map (\(_, [Left x]) -> x) rs'


units' :: (Ord a, Ord b) => CFG a b -> a -> Set (a, [Either a b])
units' cfg x = Set.map (\(_, ys) -> (x, ys)) . Set.filter f $ rules cfg
    where
        us = fst $ fInf (units cfg x) 0
        vs = Set.map ((: []) . Left) (nonterminals cfg)
        f (y, ys) = (y `member` us) && (ys `notMember` vs)


{--}
reduce :: (Ord a, Ord b) => a -> CFG a b -> Maybe(CFG a b)
reduce x = fmap delUselessSymbols . delUnitRules x


reduce' :: (Ord a, Ord b) => a -> CFG a b -> CFG a b
reduce' x = fromJust . reduce x


{--}
isCNF :: (Eq a) => CFG a b -> Bool
isCNF cfg = all f . Set.toList $ rules cfg
    where
        f (x, [])     = x == start cfg
        f (_, x:[])   = isRight x
        f (_, x:y:[]) = isLeft x && isLeft y


isGNF :: (Eq a) => CFG a b -> Bool
isGNF cfg = all f . Set.toList $ rules cfg
    where
        f (x, [])   = x == start cfg
        f (_, y:ys) = isRight y && all isLeft ys


isSDL :: (Ord a, Ord b) => CFG a b -> Bool
isSDL cfg
  | isGNF cfg = False
  | null xs1  = False
  | otherwise = and . (\xs -> zipWith (/=) xs $ tail xs) . sort $ map (\(x, y:ys) -> (x, y)) xs2
    where
        (xs1, xs2) = partition (null . snd) . Set.toList $ rules cfg


isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft _         = False


isRight :: Either a b -> Bool
isRight = not . isLeft
