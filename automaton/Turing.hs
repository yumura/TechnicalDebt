module Turing ( Turing
              , states
              , symbols
              , transition
              , accepts
              , spaceSymbol

              , MOVE (..)
              ) where


import Data.Map (Map)
import Data.Set (Set, member, notMember, isSubsetOf)
import Data.Maybe (catMaybes)
import Control.Monad (replicateM)

import qualified Data.Map as Map
import qualified Data.Set as Set


data TuringD a b = TuringD { states      :: Set a
                           , symbols     :: Set b
                           , inSymbols   :: Set b
                           , transition  :: a -> b -> Maybe (a, b, MOVE)
                           , accepts     :: Set a
                           , spaceSymbol :: b
                           }


data MOVE = LMOVE | RMOVE deriving (Eq, Ord, Show)


type Turing a b = (a, TuringD a b)


{--}
mkTuring :: (Ord a, Ord b) => a -> b -> [a] -> [b] -> [((a, b), (a, b, MOVE))] -> Maybe (Turing a b)
mkTuring s s' as is ts
    | (s' `notMember` is') && (is' `isSubsetOf` ss) = Just (s, turing)
    | otherwise                                     = Nothing
        where
            turing = mkTuringD s s' as is ts
            is' = inSymbols turing
            ss  = symbols turing


mkTuringD :: (Ord a, Ord b) => a -> b -> [a] -> [b] -> [((a, b), (a, b, MOVE))] -> TuringD a b
mkTuringD s s' as is ts = TuringD
    { states      = Set.fromList $ s : as ++ concat [[x, y] | ((x, _), (y, _, _)) <- ts]
    , symbols     = Set.fromList $ s' : concat [[x, y] | ((_, x), (_, y, _)) <- ts]
    , transition  = \x y -> Map.lookup (x, y) $ Map.fromList ts
    , inSymbols   = Set.fromList is
    , accepts     = Set.fromList as
    , spaceSymbol = s'
    }


brTuringD :: TuringD a b -> (b, [a], [b], [((a, b), (a, b, MOVE))])
brTuringD turing = (s, as, is, ts)
    where
        s  = spaceSymbol turing
        as = Set.toList $ accepts turing
        is = Set.toList $ inSymbols turing
        ts = catMaybes [fmap (\z -> ((x, y), z)) $ transition turing x y | x <- Set.toList $ states turing, y <- Set.toList $ symbols turing]


{--}
type Tape a = ([a], a, [a])


mkTape :: a -> [a] -> Tape a
mkTape s []     = (repeat s, s, repeat s)
mkTape s (x:xs) = (repeat s, x, xs ++ repeat s)


{--}
type TuringC a b = ((a, Tape b), TuringD a b)


move :: TuringC a b -> Either (TuringC a b) (TuringC a b)
move ((x, (ls, y, rs)), turing) = move' x ls y rs turing $ transition turing x y


move' :: a -> [b] -> b -> [b] -> TuringD a b -> Maybe (a, b, MOVE) -> Either (TuringC a b) (TuringC a b)
move' x ls     y rs     turing Nothing              = Right ((x, (ls, y, rs)),   turing)
move' _ (l:ls) _ rs     turing (Just (x, y, LMOVE)) = Left  ((x, (ls, l, y:rs)), turing)
move' _ ls     _ (r:rs) turing (Just (x, y, RMOVE)) = Left  ((x, (y:ls, r, rs)), turing)


moves :: TuringC a b -> TuringC a b
moves turing = moves' $ Left turing


moves' :: Either (TuringC a b) (TuringC a b) -> TuringC a b
moves' (Right turing) = turing
moves' (Left  turing) = moves' $ move turing


{--}
isAccept :: (Ord a) => [b] -> Turing a b -> Bool
isAccept xs (s, turing) = (`member` accepts turing) . fst . fst $ moves ((s, mkTape (spaceSymbol turing) xs), turing)


isReject :: (Ord a) => [b] -> Turing a b -> Bool
isReject xs = not . isAccept xs


{--}
language :: (Ord a) => Turing a b -> [[b]]
language turing = filter (`isAccept` turing) $ concatMap xs [0..]
    where
        xs n = replicateM n . Set.toList . inSymbols $ snd turing
