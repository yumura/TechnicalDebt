module Mealy ( MealyD
             , states
             , inSymbols
             , outSymbols
             , transition
             , output
             , Mealy

             , mkMealy
             , brMealyD

             , move
             , moves
             , moveLog
             , stateLog
             , outputLog

             , toGraph
             , toGraphviz
             , toGraphviz'
             , mkDotPng
             , mkDotPng'

             ,toMoore

             ,fromMoore
             ,simplification
             ) where


import Data.Ord (comparing)
import Data.Set (Set, notMember)
import Data.List (sortBy, groupBy)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, catMaybes, listToMaybe)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz, Orient(..))
import Data.Function (on)
import System.Cmd (system)
import Control.Monad (guard)
import Control.Arrow ((&&&))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Moore (Moore, mkMoore, brMooreD)
import qualified Moore


{-
:l Mealy
data State = P | Q deriving (Eq, Ord, Show)
:{
let xs = [ ((P, 0), (P, 0)), ((P, 1), (Q, 0))
         , ((Q, 0), (P, 0)), ((Q, 1), (Q, 1))
         ]
:}
let (Just (s, mm)) = mkMealy (Just P) xs

states mm
inSymbols mm
transition mm P 1
output mm P 1
brMealyD mm

Data.Maybe.isNothing . mkMealy (Just P) $ tail xs
Data.Maybe.isJust $ mkMealy Nothing xs

-}
-------------------------------------------------------------------------------
data MealyD a b c = MealyD { states     :: Set a
                           , inSymbols  :: Set b
                           , transition :: a -> b -> Maybe a
                           , output     :: a -> b -> Maybe c
                           }


type Mealy a b c = (Maybe a, MealyD a b c)


-------------------------------------------------------------------------------
mkMealy :: (Ord a, Ord b, Ord c) => Maybe a -> [((a, b), (a, c))] -> Maybe (Mealy a b c)
mkMealy s tos
    | hasTotalFunction mm = Just (s, mm)
    | otherwise           = Nothing
        where
            mm = mkMealyD s tos

mkMealyD :: (Ord a, Ord b, Ord c) => Maybe a -> [((a, b), (a, c))] -> MealyD a b c
mkMealyD s tos = MealyD
    { states     = Set.fromList $ catMaybes [s] ++ concat [[s, t] | ((s, _), (t, _)) <- tos]
    , inSymbols  = Set.fromList $ map (snd . fst) tos
    , transition = \x y -> fmap fst . Map.lookup (x, y) $ Map.fromList tos
    , output     = \x y -> fmap snd . Map.lookup (x, y) $ Map.fromList tos
    }

hasTotalFunction :: (Ord a) => MealyD a b c -> Bool
hasTotalFunction mm
    | any isNothing ts                           = False
    | any (`notMember` states mm) $ catMaybes ts = False
    | any isNothing os                           = False
    | otherwise                                  = True
        where
            ss = Set.toList $ states mm
            is = Set.toList $ inSymbols mm
            ts = [transition mm s i | s <- ss, i <- is]
            os = [output mm s i | s <- ss, i <- is]


-------------------------------------------------------------------------------
brMealyD :: MealyD a b c -> [((a, b), (a, c))]
brMealyD mm = do
    s <- Set.toList $ states mm
    i <- Set.toList $ inSymbols mm
    let t = transition mm s i
        o = output mm s i
    guard $ isJust t
    guard $ isJust o
    return ((s, i), (fromJust t, fromJust o))


-------------------------------------------------------------------------------
outSymbols :: (Ord c) => MealyD a b c -> Set c
outSymbols mm = Set.fromList . map (snd . snd) $ brMealyD mm


{-
let mm' = (s, mm)
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_22" mm'

-}
toGraph :: (Show a, Show b, Show c) => MealyD a b c -> Gr String String
toGraph mm = mkGraph ns es
    where
        ss = map show . Set.toList $ states mm
        ns = zip [1..] ss
        es = [(f s, f t, g i o) | ((s, i), (t, o)) <- brMealyD mm]
            where
                f x   = (Map.! show x) $ Map.fromList $ zip ss [1..]
                g x y = (' ' : show x) ++ ('/' : show y) ++ " "


toGraphviz :: (Show a, Show b, Show c) => Mealy a b c -> String -> (Double, Double) -> (Int, Int) -> Maybe String
toGraphviz (Nothing, _) _ _  _   = Nothing
toGraphviz (Just s, mm) t wh pwh = Just . addStateColor s mm $ graphviz (toGraph mm) t wh pwh Portrait


toGraphviz' :: (Show a, Show b, Show c) => Mealy a b c -> String -> Maybe String
toGraphviz' mm t = toGraphviz mm t (0, 0) (0, 0)


mkDotPng :: String -> String -> IO ()
mkDotPng fileName gviz = do
    let dotfile = fileName ++ ".dot"
        pngfile = fileName ++ ".png"

    writeFile dotfile gviz
    system $ unwords ["dot", "-Tpng", dotfile, "-o", pngfile]
    return ()


mkDotPng' :: (Show a, Show b, Show c) => String -> Mealy a b c -> IO ()
mkDotPng' fileName mm = do
    let plot = fmap (mkDotPng fileName) $ toGraphviz' mm "MealyMachine"
    fromMaybe (return ()) plot


addStateColor :: Show a => a -> MealyD a b c -> String -> String
addStateColor s mm = u . r (p z) (p w) . r (p x) (p y) . p
    where
        p = Text.pack
        u = Text.unpack
        r = Text.replace

        l = u . r (p "\"") (p "\\\"") . p $ show s
        x = "label = \"" ++ l ++ "\""
        y = x ++ ", style = \"filled\", fillcolor = \"gold\""

        z = "{"
        w = "{\n\tgraph [rankdir = LR];\n\tnode [shape = \"circle\"];\n"


{-
stateLog  [0, 1, 0, 1, 1, 1, 0] mm'
outputLog [0, 1, 0, 1, 1, 1, 0] mm'
stateLog  [0, 1, 0, 999, 1, 1, 0] mm'
outputLog [0, 1, 0, 999, 1, 1, 0] mm'

Control.Monad.sequence $ stateLog  [0, 1, 0, 1, 1, 1, 0] mm'
Control.Monad.sequence $ outputLog [0, 1, 0, 1, 1, 1, 0] mm'
Control.Monad.sequence $ stateLog  [0, 1, 0, 999, 1, 1, 0] mm'
Control.Monad.sequence $ outputLog [0, 1, 0, 999, 1, 1, 0] mm'

-}
move :: b -> Mealy a b c -> Mealy a b c
move x (s', mm) = (flip (transition mm) x =<< s', mm)


moves :: [b] -> Mealy a b c -> Mealy a b c
moves xs mm = foldl (flip ($)) mm $ map move xs


moveLog :: [b] -> Mealy a b c -> [Mealy a b c]
moveLog xs mm = scanl (flip ($)) mm $ map move xs


stateLog :: [b] -> Mealy a b c -> [Maybe a]
stateLog xs mm = map fst $ moveLog xs mm


outputLog :: [b] -> Mealy a b c -> [Maybe c]
outputLog xs mm = zipWith (>>=) (stateLog xs mm) $ map (flip $ output $ snd mm) xs


{-
:l Mealy Moore
data State = P | Q deriving (Eq, Ord, Show)
:{
let tos = [ ((P, 0), (P, 0)), ((P, 1), (Q, 0))
          , ((Q, 0), (P, 0)), ((Q, 1), (Q, 1))
          ]
:}
let (Just mm) = Mealy.mkMealy (Just P) tos
Mealy.mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_210_0" mm

let (Just mm') = Mealy.toMoore mm
Moore.mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_210_1" mm'

let (Just mm'') = Mealy.fromMoore mm'
Mealy.mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_210_2" mm''

let mm''' = Mealy.simplification mm''
Mealy.mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_210_3" mm'''

-}
toMoore :: (Ord a, Ord b, Ord c) => Mealy a b c -> Maybe (Moore (a, c) b c)
toMoore (s', mm)
    | not $ hasTotalFunction mm = Nothing
    | null tos1                 = Nothing
    | otherwise                 = mkMoore s os ts
        where
            tos1 = map snd $ brMealyD mm
            tos2 = (`zip` repeat (snd $ head tos1)) . Set.toList . Set.difference (states mm) . Set.fromList $ map fst tos1
            tos  = tos1 ++ tos2

            s  = listToMaybe $ filter ((== s') . Just . fst) tos
            os = zip tos $ map snd tos
            ts = do
                (s, o) <- tos
                i <- Set.toList $ inSymbols mm
                let (Just t ) = transition mm s i
                    (Just o') = output mm s i
                return (((s, o), i), (t, o'))


fromMoore :: (Ord a, Ord b, Ord c) => Moore a b c -> Maybe (Mealy a b c)
fromMoore (s, mm) = mkMealy s [(si, (t, fromJust $ Moore.output mm t)) | (si, t) <- snd $ brMooreD mm]


simplification :: (Ord a, Ord b, Ord c) => Mealy a b c -> Mealy a b c
simplification (s, mm)
    | Set.null $ states mm = (s, mm)
    | otherwise            = (fmap f s, mm')
        where
            g x = map (transition mm x &&& output mm x) . Set.toList $ inSymbols mm
            f x = (Map.! x) . Map.fromList . concatMap (\xs -> zip xs . repeat $ head xs) . groupBy ((==) `on` g) . sortBy (comparing g) . Set.toList $ states mm
            mm' = mkMealyD (fmap f s) [((f s, i), (f t, o)) | ((s, i), (t, o)) <- brMealyD mm]
