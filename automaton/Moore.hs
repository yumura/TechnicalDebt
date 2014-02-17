module Moore ( MooreD
             , states
             , inSymbols
             , outSymbols
             , transition
             , output
             , Moore

             , mkMoore
             , brMooreD

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

             , reachables
             , isReachable
             , removeUnreach

             , eqL
             , eqS

             , reduce
             , reduceEqS
             , reduceEqK
             ) where


import Data.Set (Set, member, notMember, union)
import Data.List (partition)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, catMaybes)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz, Orient(..))
import System.Cmd (system)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text




data MooreD a b c = MooreD { states     :: Set a
                           , inSymbols  :: Set b
                           , transition :: a -> b -> Maybe a
                           , output     :: a -> Maybe c
                           }


type Moore a b c = (Maybe a, MooreD a b c)


outSymbols :: (Ord c) => MooreD a b c -> Set c
outSymbols mm = Set.fromList . map snd . fst $ brMooreD mm


{-
:l Moore
data State = R | S | T deriving (Eq, Ord, Show)
let os = [(R, 0), (S, 0), (T, 1)]
:{
let ts = [ ((R, 0), R), ((R, 1), S)
         , ((S, 0), R), ((S, 1), T)
         , ((T, 0), R), ((T, 1), T)
         ]
:}

let (Just (s, mm)) = mkMoore (Just R) os ts
states mm
inSymbols mm
transition mm R 1
output mm R
brMooreD mm

Data.Maybe.isNothing $ mkMoore (Just R) (tail os) ts
Data.Maybe.isNothing . mkMoore (Just R) os $ tail ts
Data.Maybe.isJust $ mkMoore Nothing os ts

-}
mkMoore :: (Ord a, Ord b, Ord c) => Maybe a -> [(a, c)] -> [((a, b), a)] -> Maybe (Moore a b c)
mkMoore s os ts
    | hasTotalFunction mm = Just (s, mm)
    | otherwise           = Nothing
        where
            mm = mkMooreD s os ts


mkMooreD :: (Ord a, Ord b, Ord c) => Maybe a -> [(a, c)] -> [((a, b), a)] -> MooreD a b c
mkMooreD s os ts = MooreD
    { states     = Set.fromList $ catMaybes [s] ++ map fst os ++ map (fst . fst) ts ++ map snd ts
    , inSymbols  = Set.fromList $ map (snd . fst) ts
    , transition = \x y -> Map.lookup (x, y) $ Map.fromList ts
    , output     = \x   -> Map.lookup x      $ Map.fromList os
    }


hasTotalFunction :: (Ord a) => MooreD a b c -> Bool
hasTotalFunction mm
    | any isNothing $ map (output mm) ss         = False
    | any isNothing ts                           = False
    | any (`notMember` states mm) $ catMaybes ts = False
    | otherwise                                  = True
        where
            ss = Set.toList $ states mm
            is = Set.toList $ inSymbols mm
            ts = [transition mm s i | s <- ss, i <- is]


{--}
brMooreD :: MooreD a b c -> ([(a, c)], [((a, b), a)])
brMooreD mm = (os, ts)
    where
        os = catMaybes [fmap (\o -> (s, o)) $ output mm s | s <- Set.toList $ states mm]
        ts = catMaybes [fmap (\t -> ((s, i), t)) $ transition mm s i | s <- Set.toList $ states mm, i <- Set.toList $ inSymbols mm]


{-
let mm' = (s, mm)
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_24" mm'
-}
toGraph :: (Show a, Show b, Show c) => MooreD a b c -> Gr String String
toGraph mm = mkGraph ns es
    where
        (os, ts) = brMooreD mm
        ns = zip [1..] [show s ++ '/' : show o | (s, o) <- os]
        es  = [(f s, f t, g i) | ((s, i), t) <- ts]
            where
                f x = (Map.! show x) . Map.fromList $ zip (map (show . fst) os) [1..]
                g x = ' ':show x ++ " "


toGraphviz :: (Show a, Show b, Show c) => Moore a b c -> String -> (Double, Double) -> (Int, Int) -> Maybe String
toGraphviz (Nothing, _) _ _  _   = Nothing
toGraphviz (Just s, mm) t wh pwh = Just . addStateColor s mm $ graphviz (toGraph mm) t wh pwh Portrait


toGraphviz' :: (Show a, Show b, Show c) => Moore a b c -> String -> Maybe String
toGraphviz' mm t = toGraphviz mm t (0, 0) (0, 0)


mkDotPng :: String -> String -> IO()
mkDotPng fileName gviz = do
    let dotfile = fileName ++ ".dot"
        pngfile = fileName ++ ".png"

    writeFile dotfile gviz
    system $ unwords ["dot", "-Tpng", dotfile, "-o", pngfile]
    return ()


mkDotPng' :: (Show a, Show b, Show c) => String -> Moore a b c -> IO()
mkDotPng' fileName mm = do
    let plot = fmap (mkDotPng fileName) $ toGraphviz' mm "MooreMachine"
    fromMaybe (return ()) plot


addStateColor :: (Show a, Show c) => a -> MooreD a b c -> String -> String
addStateColor s mm = u . r (p z) (p w) . r (p x) (p y) . p
    where
        p = Text.pack
        u = Text.unpack
        r = Text.replace

        Just o = output mm s
        l = u . r (p "\"") (p "\\\"") . p $ show s ++ '/' : show o
        x = "label = \"" ++ l ++ "\""
        y = x ++ ", style = \"filled\", fillcolor = \"gold\""

        z = "{"
        w = "{\n\tgraph [rankdir = LR];\n\tnode [shape = \"circle\"];\n"


{-
stateLog [0, 1, 0, 1, 1, 1, 0] mm'
Control.Monad.sequence $ stateLog [0, 1, 0, 1, 1, 1, 0] mm'

outputLog [0, 1, 0, 1, 1, 1, 0] mm'
Control.Monad.sequence $ outputLog [0, 1, 0, 1, 1, 1, 0] mm'

stateLog [0, 1, 0, 999, 1, 1, 0] mm'
Control.Monad.sequence $ stateLog [0, 1, 0, 999, 1, 1, 0] mm'

outputLog [0, 1, 0, 999, 1, 1, 0] mm'
Control.Monad.sequence $ outputLog [0, 1, 0, 999, 1, 1, 0] mm'

-}
move :: b -> Moore a b c -> Moore a b c
move x (s, mm) = (flip (transition mm) x =<< s, mm)


moves :: [b] -> Moore a b c -> Moore a b c
moves xs mm = foldl (flip ($)) mm $ map move xs


moveLog :: [b] -> Moore a b c -> [Moore a b c]
moveLog xs mm = scanl (flip ($)) mm $ map move xs


stateLog :: [b] -> Moore a b c -> [Maybe a]
stateLog xs mm = map fst $ moveLog xs mm


outputLog :: [b] -> Moore a b c -> [Maybe c]
outputLog xs mm = map (output (snd mm) =<<) $ stateLog xs mm


{--}
reachables :: (Ord a) => Moore a b c -> Set a
reachables (Nothing, _) = Set.empty
reachables (Just s, mm) = reachables' mm Set.empty [s]


reachables' :: (Ord a) => MooreD a b c -> Set a -> [a] -> Set a
reachables' mm rs [] = rs
reachables' mm rs xs = reachables' mm rs' xs'
    where
        rs'  = Set.fromList xs `union` rs
        xs'  = filter (`notMember` rs') . catMaybes $ concatMap (\x -> map (transition mm x) . Set.toList $ inSymbols mm) xs


isReachable :: (Ord a) => a -> Moore a b c -> Bool
isReachable x mm = x `member` reachables mm


removeUnreach :: (Ord a, Ord b, Ord c) => Moore a b c -> Moore a b c
removeUnreach (s, mm) = (s, mkMooreD s os' ts')
    where
        (os, ts) = brMooreD mm
        os' = filter (f . fst) os
        ts' = filter (f . snd) $ filter (f . fst . fst) ts

        f = (`isReachable` (s, mm))


{--}
eqL :: (Ord a, Ord a', Ord b, Eq c) => Moore a b c -> Moore a' b c -> Bool
eqL mm nn = eqDFTree Set.empty [(mm, nn)]


eqDFTree :: (Ord a, Ord a', Ord b, Eq c) => Set (Maybe a, Maybe a') -> [(Moore a b c, Moore a' b c)] -> Bool
eqDFTree _  [] = True
eqDFTree rs (((x, mm), (y, nn)):xys)
    | (x, y) `member` rs                     = eqDFTree rs xys
    | inSymbols mm /= inSymbols nn           = False
    | (output mm =<< x) /= (output nn =<< y) = False
    | otherwise                              = eqDFTree (Set.insert (x, y) rs) $ zip (f x mm) (f y nn) ++ xys
        where
            f z ll = map (`move` (z, ll)) . Set.toList $ inSymbols mm


eqS :: (Ord a, Ord b, Eq c) => Moore a b c -> a -> a -> Bool
eqS mm x y
    | x <= y    = eqDFTree' (snd mm) Set.empty [(Just x, Just y)]
    | otherwise = eqDFTree' (snd mm) Set.empty [(Just y, Just x)]


eqDFTree' :: (Ord a, Ord b, Eq c) => MooreD a b c -> Set (Maybe a, Maybe a) -> [(Maybe a , Maybe a)] -> Bool
eqDFTree' mm _  [] = True
eqDFTree' mm rs ((x, y):xys)
    | x == y                                 = eqDFTree' mm rs xys
    | (x, y) `member` rs                     = eqDFTree' mm rs xys
    | (output mm =<< x) /= (output mm =<< y) = False
    | otherwise                              = eqDFTree' mm (Set.insert (x, y) rs) $ zipWith g (f x) (f y) ++ xys
        where
            f z = map (fst . (`move` (z, mm))) . Set.toList $ inSymbols mm
            g x y
                | x <= y    = (x, y)
                | otherwise = (y, x)


{--}
reduceEqS :: (Ord a, Ord b, Ord c) => Moore a b c -> Moore a b c
reduceEqS mm = (fmap f s, mkMooreD (fmap f s) [(f s, o) | (s, o) <- os] [((f s, i), f t) | ((s, i), t) <- ts])
    where
        (s, mm') = removeUnreach mm
        (os, ts) = brMooreD mm'
        f x = (Map.! x) . Map.fromList . concatMap h . g . Set.toList $ states mm'
            where
                g []     = []
                g (x:xs) = let (ys, zs) = partition (eqS mm x) xs in (x:ys) : g zs
                h []     = []
                h (x:xs) = zip (x:xs) $ repeat x


{--}
eqK :: (Ord a, Eq c) => Moore a b c -> Int -> Maybe a -> Maybe a -> Bool
eqK (s, mm) k x y
    | k <= 0    = (output mm =<< x) == (output mm  =<< y)
    | otherwise = and $ zipWith (eqK (s, mm) $ k - 1) (f x) (f y)
        where
            f z = map (fst . (`move` (z, mm))) . Set.toList $ inSymbols mm


eqKGroup ::  (Ord a, Eq c) => Moore a b c -> Int -> [[Maybe a]] -> [[a]]
eqKGroup mm k xss
    | xss' == xss = map catMaybes xss
    | otherwise   = eqKGroup mm (k + 1) xss'
        where
            xss' = concatMap g xss
                where
                    g []     = []
                    g (x:xs) = let (ys, zs) = partition (eqK mm k x) xs in (x:ys) : g zs


reduceEqK :: (Ord a, Ord b, Ord c) => Moore a b c -> Moore a b c
reduceEqK mm = (fmap f s, mkMooreD (fmap f s) [(f s, o) | (s, o) <- os] [((f s, i), f t) | ((s, i), t) <- ts])
    where
        (s, mm')= removeUnreach mm
        (os, ts) = brMooreD mm'
        f x = (Map.! x) . Map.fromList . concatMap g . eqKGroup mm 0 . (:[]) . map Just . Set.toList $ states mm'
            where
                g []     = []
                g (x:xs) = zip (x:xs) $ repeat x


{--}
reduce :: (Ord a, Ord b, Ord c) => Moore a b c -> Moore a b c
reduce = reduceEqK
