module DPDA ( DPDA
            , states
            , stackSymbols
            , inSymbols
            , transition
            , eTransition
            , accepts

            , mkDPDA
            , mkStrictDPDA
            , brDPDAd

            , configuration
            , height

            , isEpsilonMode
            , isReadingMode
            , isRealTimeDPDA

            , move
            , eMove
            , moves

            , isAccept
            , isStrict
            , isReject
            , language

            , fromDFA
            , fromStrict
            , toStrict
            ) where


import Data.Set (Set, member, notMember)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing, maybe)
import Control.Monad (replicateM)

import qualified Data.Set as Set
import qualified Data.Map as Map

import DFA (DFA, brDFAd)


data DPDAd a b c = DPDAd { states       :: Set a
                         , stackSymbols :: Set b
                         , inSymbols    :: Set c
                         , transition   :: a -> b -> c -> Maybe (a, [b])
                         , eTransition  :: a -> b -> Maybe (a, [b])
                         , accepts      :: Set a
                         }


type DPDA a b c = (Maybe (a, [b]), DPDAd a b c)


{--}
mkDPDA :: (Ord a, Ord b, Ord c) => Maybe (a, [b]) -> [a] -> [((a, b), (a, [b]))] -> [((a, b, c), (a, [b]))] -> Maybe(DPDA a b c)
mkDPDA s as es ts
    | flg1 && f es && f ts = Just $ mkDPDA' s as es ts
    | otherwise            = Nothing
        where
            flg1 = all (`notMember` Set.fromList (map fst es)) [(x, y) | ((x, y , _), _)<- ts]
            f xs = null [(x, y, z, w) | (x, y) <- xs, (z, w) <- xs, x == z, y /= w]


mkDPDA' :: (Ord a, Ord b, Ord c) => Maybe (a, [b]) -> [a] -> [((a, b), (a, [b]))] -> [((a, b, c), (a, [b]))] -> DPDA a b c
mkDPDA' s as es ts = (s, mkDPDAd s as es ts)


mkDPDAd :: (Ord a, Ord b, Ord c) => Maybe (a, [b]) -> [a] -> [((a, b), (a, [b]))] -> [((a, b, c), (a, [b]))] -> DPDAd a b c
mkDPDAd s as es ts = DPDAd
    { states       = Set.fromList $ s' ++ as ++ concatMap (\((x, _), (y, __)) -> [x, y]) ets
    , stackSymbols = Set.fromList $ ss ++ concatMap (\((_, x), (_, xs)) -> x : xs) ets
    , inSymbols    = Set.fromList [x | (_, _, x) <- map fst ts]
    , transition   = \x y z -> (x, y, z) `Map.lookup` Map.fromList ts
    , eTransition  = curry (`Map.lookup` Map.fromList es)
    , accepts      = Set.fromList as
    } where
        (s', ss) = maybe ([], []) (\(x, y) -> ([x], y)) s
        ets = es ++ map (\((x, y, _), z) -> ((x, y), z)) ts


mkStrictDPDA :: (Ord a, Ord b, Ord c) => Maybe (a, [b]) -> [((a, b), (a, [b]))] -> [((a, b, c), (a, [b]))] -> Maybe(DPDA a b c)
mkStrictDPDA s = mkDPDA s []


brDPDAd :: (Ord a, Ord b, Ord c) => DPDAd a b c -> ([a], [((a, b), (a, [b]))], [((a, b, c), (a, [b]))])
brDPDAd dpda = (Set.toList $ accepts dpda, f es, f ts)
    where
        f g = [(x, y) | (x, Just y) <- g dpda]


es :: (Ord a, Ord b, Ord c) => DPDAd a b c -> [((a, b), Maybe (a, [b]))]
es dpda = [((x, y), eTransition dpda x y) | x <- Set.toList $ states dpda, y <- Set.toList $ stackSymbols dpda]


ts :: (Ord a, Ord b, Ord c) => DPDAd a b c -> [((a, b, c), Maybe (a, [b]))]
ts dpda = do
    x <- Set.toList $ states dpda
    y <- Set.toList $ stackSymbols dpda
    z <- Set.toList $ inSymbols dpda
    return ((x, y, z), transition dpda x y z)


{--}
configuration :: DPDA a b c -> Maybe [b]
configuration = fmap snd . fst


height :: DPDA a b c -> Maybe Int
height = fmap length . configuration


{--}
isEpsilonMode :: DPDA a b c -> Bool
isEpsilonMode (Nothing, _)           = False
isEpsilonMode (Just (_, []), _)      = False
isEpsilonMode (Just (x, y:ys), dpda) = isJust $ eTransition dpda x y


isReadingMode :: DPDA a b c -> Bool
isReadingMode (Nothing, _)           = False
isReadingMode (Just (_, []), _)      = False
isReadingMode (Just (x, y:ys), dpda) = not $ isEpsilonMode (Just (x, y:ys), dpda)


isRealTimeDPDA :: (Ord a, Ord b, Ord c) => DPDA a b c -> Bool
isRealTimeDPDA dpda = (\(_, es, _) -> null es) . brDPDAd $ snd dpda


{--}
move :: c -> DPDA a b c -> DPDA a b c
move _ (Nothing, dpda)        = (Nothing, dpda)
move _ (Just (_, []), dpda)   = (Nothing, dpda)
move z (Just (x, y:ys), dpda) = (fmap (\(x', ys') -> (x', ys' ++ ys)) $ transition dpda x y z, dpda)


eMove :: DPDA a b c -> DPDA a b c
eMove (Nothing, dpda)        = (Nothing, dpda)
eMove (Just (_, []), dpda)   = (Nothing, dpda)
eMove (Just (x, y:ys), dpda) = (fmap (\(x', ys') -> (x', ys' ++ ys)) $ eTransition dpda x y, dpda)


eMoves :: DPDA a b c -> DPDA a b c
eMoves (Nothing, dpda)   = (Nothing, dpda)
eMoves dpda
    | isEpsilonMode dpda = eMoves $ eMove dpda
    | otherwise          = dpda


moves :: [c] -> DPDA a b c -> DPDA a b c
moves xs dpda = foldl (flip ($)) (eMoves dpda) $ map move' xs
    where
        move' x = eMoves . move x


{--}
isAccept :: (Ord a) => [c] -> DPDA a b c -> Bool
isAccept xs dpda
    | isStrict dpda = isAcceptanceByEmptyStack  xs dpda
    | otherwise     = isAcceptanceByFinalStates xs dpda


isStrict :: DPDA a b c -> Bool
isStrict (_, dpda) = Set.null $ accepts dpda


isAcceptanceByEmptyStack :: [c] -> DPDA a b c -> Bool
isAcceptanceByEmptyStack xs dpda = (== Just True) . fmap (null . snd) . fst $ moves xs dpda


isAcceptanceByFinalStates :: (Ord a) => [c] -> DPDA a b c -> Bool
isAcceptanceByFinalStates xs dpda = (== Just True) . fmap ((`member` accepts (snd dpda)) . fst) . fst $ moves xs dpda


isReject :: (Ord a) => [c] -> DPDA a b c -> Bool
isReject xs = not . isAccept xs


language :: (Ord a) => DPDA a b c -> [[c]]
language (s, dpda) = filter (`isAccept` (s, dpda)) $ concatMap (\n -> replicateM n . Set.toList $ inSymbols dpda) [0..]


{--}
fromDFA :: (Ord a, Ord b) => DFA a b -> DPDA a Bool b
fromDFA (s, dfa) = mkDPDA' s' as [] ts'
    where
        (as, ts) = brDFAd dfa
        s'  = fmap (\x -> (x, [True])) s
        ts' = [((x, True, y), (z, [True])) | ((x, y), z) <- ts]


{--}
fromStrict :: (Ord a, Ord b, Ord c) => a -> b -> DPDA a b c -> Maybe (DPDA a b c)
fromStrict x y (s, dpda)
    | not $ isStrict (s, dpda)     = Just (s, dpda)
    | x `member` states dpda       = Nothing
    | y `member` stackSymbols dpda = Nothing
    | otherwise                    = mkDPDA s' [x] (es' ++ es) ts
        where
            (as, es, ts) = brDPDAd dpda
            s'           = fmap (\(x, xs) -> (x, xs ++ [y])) s
            es'          = map (\z -> ((z, y), (x ,[]))) . Set.toList $ states dpda


toStrict :: (Ord a, Ord b, Ord c) => a -> b -> c ->  DPDA a b c -> Maybe (DPDA a b c)
toStrict x y z (s, dpda)
    | isStrict (s, dpda)           = Just (s, dpda)
    | x `member` states dpda       = Nothing
    | y `member` stackSymbols dpda = Nothing
    | z `member` inSymbols dpda    = Nothing
    | otherwise                    = mkStrictDPDA s' (es' ++ es) (ts' ++ ts)
        where
            (as, es, ts) = brDPDAd dpda
            s'           = fmap (\(x, xs) -> (x, xs ++ [y])) s
            es'          = ((x, y), (x, [])) : [((x, w), (x, [])) | w <- Set.toList $ stackSymbols dpda]
            ts'          = [((a, w, z), (x, [])) | a <- as, w <- Set.toList $ stackSymbols dpda]
