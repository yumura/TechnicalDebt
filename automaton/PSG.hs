module PSG ( PSG
           , nonterminals
           , terminals
           , rules
           , start

           , mkPSG
           , brPSG
           ) where


import Data.Map (Map)
import Data.Set (Set)
import Data.Either (lefts, rights)
import Data.Function (on)

import qualified Data.Map as Map
import qualified Data.Set as Set


data PSG a b = PSG { rules :: Set ([Either a b], [Either a b])
                   , start :: a
                   } deriving (Eq, Show)


nonterminals :: (Ord a) => PSG a b -> Set a
nonterminals psg = Set.insert (start psg) $ lrs' lefts psg


terminals :: (Ord b) => PSG a b -> Set b
terminals = lrs' rights


lrs' :: (Ord c) => ([Either a b] -> [c]) -> PSG a b -> Set c
lrs' f psg =  Set.fromList . concatMap (uncurry ((++) `on` f)) . Set.toList $ rules psg


{--}
mkPSG :: (Ord a, Ord b) => a -> [([Either a b], [Either a b])] -> PSG a b
mkPSG s rs = PSG
    { rules = Set.fromList rs
    , start = s
    }


brPSG :: PSG a b -> (a, [([Either a b], [Either a b])])
brPSG psg = (start psg, Set.toList $ rules psg) 
