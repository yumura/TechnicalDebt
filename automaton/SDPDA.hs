module SDPDA ( SDPDA
             , stackSymbols
             , inSymbols
             , transition

             , mkSDPDA
             , brSDPDA

             , configuration
             , height

             , move
             , moves
             , isAccept
             , isReject
             , language
             ) where


import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Control.Monad (replicateM)

import qualified Data.Set as Set
import qualified Data.Map as Map


data SDPDAd a b = SDPDAd { stackSymbols :: Set a
                                   , inSymbols    :: Set b
                                   , transition   :: a -> b -> Maybe [a]
                                   }


type SDPDA a b = (Maybe [a], SDPDAd a b)


{--}
mkSDPDA :: (Ord a, Ord b) => Maybe [a] -> [((a, b), [a])] -> Maybe(SDPDA a b)
mkSDPDA ss ts
    | hasTotalFunction dpda = Just (ss, dpda)
    | otherwise             = Nothing
        where
            dpda = mkSDPDAd ss ts


mkSDPDAd :: (Ord a, Ord b) => Maybe [a] -> [((a, b), [a])] -> SDPDAd a b
mkSDPDAd ss ts = SDPDAd
    { stackSymbols = Set.fromList $ ss' ++ map (fst . fst) ts ++ concatMap snd ts
    , inSymbols    = Set.fromList $ map (snd . fst) ts
    , transition   = curry (`Map.lookup` Map.fromList ts)
    } where
        ss' = fromMaybe [] ss


brSDPDA :: (Ord a, Ord b) => SDPDA a b -> ([a], [((a, b), [a])])
brSDPDA (ss, dpda) = (fromMaybe [] ss, [(si, t) |(si, Just t)  <- ts dpda])


hasTotalFunction :: SDPDAd a b -> Bool
hasTotalFunction dpda
    | any (isNothing . snd) (ts dpda) = False
    | otherwise                       = True


ts :: SDPDAd a b -> [((a, b), Maybe [a])]
ts dpda = [((s, i), transition dpda s i) | s <- Set.toList $ stackSymbols dpda, i <- Set.toList $ inSymbols dpda]


{--}
configuration :: SDPDA a b -> Maybe [a]
configuration = fst


height :: SDPDA a b -> Maybe Int
height = fmap length . configuration


{--}
move :: b -> SDPDA a b -> SDPDA a b
move x (ss, dpda) = (f =<< ss, dpda)
        where
            f []     = Nothing
            f (s:ss) = fmap (++ ss) $ transition dpda s x


moves :: [b] -> SDPDA a b -> SDPDA a b
moves xs dpda = foldl (flip ($)) dpda $ map move xs


isAccept :: [b] -> SDPDA a b -> Bool
isAccept xs dpda = (== Just True) . fmap null . configuration $ moves xs dpda


isReject :: [b] -> SDPDA a b -> Bool
isReject xs = not . isAccept xs


language :: (Ord a) => SDPDA a b -> [[b]]
language (ss, dpda) = filter (`isAccept` (ss, dpda)) $ concatMap (\n -> replicateM n . Set.toList $ inSymbols dpda) [0..]
