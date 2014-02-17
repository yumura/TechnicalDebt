module NPDA ( NPDA
            , states
            , stackSymbols
            , inSymbols
            , transition
            , eTransition
            , accepts

            , mkNPDA
            , brNPDAd

            , configuration
            , height

            , move
            , eMove
            , moves

            , isAccept
            , isStrict
            , isReject
            , language

            , fromStrict
            , toStrict

            , fromGNF
            , fromCNF
            ) where


import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set (Set, member)
import Data.List (sortBy, groupBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Control.Monad (replicateM)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CFG (CFG)
import CNF (CNF)
import GNF (GNF, GNFRule)
import qualified CFG
import qualified CNF
import qualified GNF


data NPDAd a b c = NPDAd { states       :: Set a
                         , stackSymbols :: Set b
                         , inSymbols    :: Set c
                         , transition   :: a -> b -> c -> Set (a, [b])
                         , eTransition  :: a -> b -> Set (a, [b])
                         , accepts      :: Set a
                         }


type NPDA a b c = (Set (a, [b]), NPDAd a b c)


{--}
mkNPDA :: (Ord a, Ord b, Ord c) => [(a, [b])] -> [a] -> [((a, b), [(a, [b])])] -> [((a, b, c), [(a, [b])])] -> NPDA a b c
mkNPDA ss as es ts = (Set.fromList ss, mkNPDAd ss as es ts)


mkNPDAd :: (Ord a, Ord b, Ord c) => [(a, [b])] -> [a] -> [((a, b), [(a, [b])])] -> [((a, b, c), [(a, [b])])] -> NPDAd a b c
mkNPDAd ss as es ts = NPDAd
    { states       = Set.fromList $ map fst ss ++ as ++ concatMap (\((x, _), xs) -> x: map       fst xs) ets
    , stackSymbols = Set.fromList $ concatMap snd ss ++ concatMap (\((_, x), xs) -> x: concatMap snd xs) ets
    , inSymbols    = Set.fromList [x | (_, _, x) <- map fst ts]
    , transition   = \x y z -> Set.fromList . fromMaybe [] . Map.lookup (x, y, z) $ Map.fromList ts
    , eTransition  = \x y   -> Set.fromList . fromMaybe [] . Map.lookup (x, y)    $ Map.fromList es
    , accepts      = Set.fromList as
    } where
        ets       = es ++ map (\((x, y, _), z) -> ((x, y), z)) ts


mkStrictNPDA :: (Ord a, Ord b, Ord c) => [(a, [b])] -> [((a, b), [(a, [b])])] -> [((a, b, c), [(a, [b])])] -> NPDA a b c
mkStrictNPDA ss = mkNPDA ss []


brNPDAd :: (Ord a, Ord b, Ord c) => NPDAd a b c -> ([a], [((a, b), [(a, [b])])], [((a, b, c), [(a, [b])])])
brNPDAd npda = (Set.toList $ accepts npda, f es, f ts)
    where
        f g = [(x, Set.toList y) | (x, y) <- g npda]


es :: (Ord a, Ord b) => NPDAd a b c -> [((a, b), Set (a, [b]))]
es npda = do
    x <- Set.toList $ states npda
    y <- Set.toList $ stackSymbols npda
    return ((x, y), eTransition npda x y)


ts :: (Ord a, Ord b, Ord c) => NPDAd a b c -> [((a, b, c), Set (a, [b]))]
ts npda = do
    x <- Set.toList $ states npda
    y <- Set.toList $ stackSymbols npda
    z <- Set.toList $ inSymbols npda
    return ((x, y, z), transition npda x y z)


{--}
configuration :: (Ord a, Ord b) => NPDA a b c -> Set [b]
configuration = Set.map snd . fst


height :: (Ord a, Ord b) => NPDA a b c -> Set Int
height = Set.map length . configuration


{--}
move :: (Ord a, Ord b) => c -> NPDA a b c -> NPDA a b c
move x (ss, npda)
    | Set.null ss = (ss, npda)
    | otherwise   = (Set.unions . map f . filter (not . null . snd) $ Set.toList ss, npda)
        where
            f (y, z:zs) = Set.map (\(y', zs') -> (y', zs' ++ zs)) $ transition npda y z x


eMove :: (Ord a, Ord b) => NPDA a b c -> NPDA a b c
eMove (ss, npda)
    | Set.null ss   = (ss, npda)
    | otherwise     = (Set.unions . map f . filter (not . null . snd) $ Set.toList ss, npda)
        where
            f (x, y:ys) = Set.map (\(x', ys') -> (x', ys' ++ ys)) $ eTransition npda x y


eMoves :: (Ord a, Ord b) => NPDA a b c -> NPDA a b c
eMoves npda
    | Set.null $ fst next = npda
    | otherwise           = eMoves next
        where
            next = eMove npda


moves :: (Ord a, Ord b) => [c] -> NPDA a b c -> NPDA a b c
moves xs npda = foldl (flip ($)) (eMoves npda) $ map move' xs
    where
        move' x = eMoves . move x


{--}
isAccept :: (Ord a, Ord b) => [c] -> NPDA a b c -> Bool
isAccept xs npda
    | isStrict npda = isAcceptanceByEmptyStack  xs npda
    | otherwise     = isAcceptanceByFinalStates xs npda


isStrict :: (Ord a) => NPDA a b c -> Bool
isStrict (_, npda) = Set.null $ accepts npda


isAcceptanceByEmptyStack :: (Ord a, Ord b) => [c] -> NPDA a b c -> Bool
isAcceptanceByEmptyStack xs npda = any (null . snd) . Set.toList . fst $ moves xs npda


isAcceptanceByFinalStates :: (Ord a, Ord b) => [c] -> NPDA a b c -> Bool
isAcceptanceByFinalStates xs (ss, npda) = any ((`member` accepts npda) . fst) . Set.toList . fst $ moves xs (ss, npda)


isReject :: (Ord a, Ord b) => [c] -> NPDA a b c -> Bool
isReject xs = not . isAccept xs


language :: (Ord a, Ord b) => NPDA a b c -> [[c]]
language (s, npda) = filter (`isAccept` (s, npda)) $ concatMap (\n -> replicateM n . Set.toList $ inSymbols npda) [0..]


{--}
fromStrict :: (Ord a, Ord b, Ord c) => a -> b -> NPDA a b c -> Maybe (NPDA a b c)
fromStrict x y (ss, npda)
    | not $ isStrict (ss, npda)    = Just (ss, npda)
    | x `member` states npda       = Nothing
    | y `member` stackSymbols npda = Nothing
    | otherwise                    = Just $ mkNPDA ss' [x] (es' ++ es) ts
        where
            (as, es, ts) = brNPDAd npda
            ss'          = map (\(x, xs) -> (x, xs ++ [y])) $ Set.toList ss
            es'          = map (\z -> ((z, y), [(x ,[])])) . Set.toList $ states npda


toStrict :: (Ord a, Ord b, Ord c) => a -> b -> c ->  NPDA a b c -> Maybe (NPDA a b c)
toStrict x y z (ss, npda)
    | isStrict (ss, npda)          = Just (ss, npda)
    | x `member` states npda       = Nothing
    | y `member` stackSymbols npda = Nothing
    | z `member` inSymbols npda    = Nothing
    | otherwise                    = Just $ mkStrictNPDA ss' (es' ++ es) (ts' ++ ts)
        where
            (as, es, ts) = brNPDAd npda
            ss'          = map (\(x, xs) -> (x, xs ++ [y])) $ Set.toList ss
            es'          = ((x, y), [(x, [])]) : [((x, w), [(x, [])]) | w <- Set.toList $ stackSymbols npda]
            ts'          = [((a, w, z), [(x, [])]) | a <- as, w <- Set.toList $ stackSymbols npda]


{--}
fromGNF :: (Ord a, Ord b) => GNF a b -> NPDA Bool a b
fromGNF gnf = mkStrictNPDA ss es ts
    where
        ss = [(True, [GNF.start gnf])]
        es = [((True, GNF.start gnf), [(True, [])]) | GNF.hasEpsilon gnf]
        ts = map (\xss -> ((\(x, y) -> (True, x, y)) . fst $ head xss, map ((\xs -> (True, xs)) . snd) xss) ). groupBy ((==) `on` fst) . sortBy (comparing fst) . map (\(x, (y, zs)) -> ((x, y), zs)) . Set.toList $ GNF.rules gnf


fromCNF :: (Ord a, Ord b) => CNF a b -> NPDA Bool (GNFRule a) b
fromCNF = fromGNF . GNF.fromCNF


fromCFG :: (Ord a, Ord b) => a -> CFG a b -> Maybe (NPDA Bool (GNFRule [Either a b]) b)
fromCFG x = fmap fromGNF . GNF.fromCFG x


fromCFG' :: (Ord a, Ord b) => CFG a b -> NPDA Bool (GNFRule [Either a b]) b
fromCFG' = fromGNF . GNF.fromCFG'


toCFG :: (Ord a, Ord b, Ord c) => b -> NPDA a b c -> Maybe (CFG (a, b, a) c)
toCFG s (ss, npda)
    | s `member` stackSymbols npda = Nothing
    | Set.null ss                  = Nothing
    | not $ isStrict (ss, npda)    = Nothing
    | otherwise                    = Just $ toCFG' s (ss, npda)


toCFG' :: (Ord a, Ord b, Ord c) => b -> NPDA a b c -> CFG (a, b, a) c
toCFG' s (ss, npda) = CFG.mkCFG (p0, s, p0) $ rs0 ++ rs1 ++ rs2 ++ rs3 ++ rs4
    where
        (p0, z0:_)   = Set.findMin ss
        (_, es, ts) = brNPDAd npda

        f x (y:[])   = [[(x, y, w)] | w <- Set.toList $ states npda]
        f x (y:z:ys) = concatMap (\w -> map ((x, y, w) :) $ f w (z:ys)) . Set.toList $ states npda

        g ((_, _, x):[]) = x
        g (_:x:xs)       = g $ x:xs 

        rs0 = [((p0, s, p0), [Left (p0, z0, x)]) | x <- Set.toList $ states npda]
        rs1 = [((x, y, z), [])        | ((x, y), zs)    <- es, (z, []) <- zs]
        rs2 = [((x, y, z), [Right w]) | ((x, y, w), zs) <- ts, (z, []) <- zs]
        rs3 = [((x, y, g ws'), map Left ws')           | ((x, y), zs)    <- es, (z, w:ws) <- zs, ws' <- f z (w:ws)]
        rs4 = [((x, y, g ws'), Right u : map Left ws') | ((x, y, u), zs) <- ts, (z, w:ws) <- zs, ws' <- f z (w:ws)]

