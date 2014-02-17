module DFA ( DFAd
           , states
           , inSymbols
           , transition
           , accepts
           , DFA

           , mkDFA
           , brDFAd

           , toGraph
           , toGraphviz
           , toGraphviz'
           , mkDotPng
           , mkDotPng'

           , isAccept
           , isReject
           , language

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
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, catMaybes)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz, Orient(..))
import System.Cmd (system)
import Control.Monad (replicateM, guard)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text

import Moore (Moore, MooreD, mkMoore, brMooreD)
import qualified Moore


type DFAd a b = MooreD a b Bool


type DFA a b = Moore a b Bool


states :: DFAd a b -> Set a
states = Moore.states


inSymbols :: DFAd a b -> Set b
inSymbols = Moore.inSymbols


transition :: DFAd a b -> a -> b -> Maybe a
transition = Moore.transition


output :: DFAd a b -> a -> Maybe Bool
output = Moore.output


accepts :: (Ord a) => DFAd a b -> Set a
accepts = Set.fromList . fst . brDFAd


{-
:l DFA
data State = R | S | T deriving (Eq, Ord, Show)
:{
let ts = [ ((R, 0), R), ((R, 1), S)
         , ((S, 0), R), ((S, 1), T)
         , ((T, 0), R), ((T, 1), T)
         ]
:}
let (Just (s, dfa)) = mkDFA (Just R) [T] ts

states dfa
inSymbols dfa
transition dfa R 1
accepts dfa
brDFAd dfa

Data.Maybe.isNothing . mkDFA (Just R) [T] $ tail ts
Data.Maybe.isJust $ mkDFA Nothing [] ts

-}
mkDFA :: (Ord a, Ord b) => Maybe a -> [a] -> [((a, b), a)] -> Maybe (DFA a b)
mkDFA s as ts = mkMoore s os ts
    where
        os = zip ss $ map (`member` Set.fromList as) ss
            where
                ss = catMaybes [s] ++ as ++ concat [[s, t] | ((s, _), t) <- ts]


brDFAd :: DFAd a b -> ([a], [((a, b), a)])
brDFAd dfa = (map fst $ filter snd os, ts)
    where
      (os, ts) = Moore.brMooreD dfa


{-
let dfa' = (s, dfa)
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_215" dfa'

-}
toGraph :: (Show a, Show b) => DFAd a b -> Gr String String
toGraph dfa = mkGraph ns es
    where
        ss = map show . Set.toList $ states dfa
        ns = zip [1..] ss
        es = [(f s, f t, g i) | ((s, i), t) <- snd $ brDFAd dfa]
            where
                f x = fromJust . Map.lookup (show x) . Map.fromList $ zip ss [1..]
                g x = ' ':show x ++ " "


toGraphviz :: (Show a, Show b) => DFA a b -> String -> (Double, Double) -> (Int, Int) -> Maybe String
toGraphviz (Nothing,  _) _ _  _   = Nothing
toGraphviz (Just s, dfa) t wh pwh = Just . gvizTextConv s dfa $ graphviz (toGraph dfa) t wh pwh Portrait


toGraphviz' :: (Show a, Show b) => DFA a b -> String -> Maybe String
toGraphviz' dfa t = toGraphviz dfa t (0, 0) (0, 0)


mkDotPng :: String -> String -> IO()
mkDotPng fileName gviz = do
    let dotfile = fileName ++ ".dot"
        pngfile = fileName ++ ".png"

    writeFile dotfile gviz
    system $ unwords ["dot", "-Tpng", dotfile, "-o", pngfile]
    return ()


mkDotPng' :: (Show a, Show b) => String -> DFA a b -> IO()
mkDotPng' fileName dfa = do
    let plot = fmap (mkDotPng fileName) $ toGraphviz' dfa "DFA"
    fromMaybe (return ()) plot


gvizTextConv :: Show a => a -> DFAd a b -> String -> String
gvizTextConv s dfa = u . fs . r (p z) (p w) . r (p x) (p y) . p
    where
        p = Text.pack
        u = Text.unpack
        r = Text.replace

        x = "label = \"" ++ (u . r (p "\"") (p "\\\"") . p $ show s) ++ "\""
        y = x ++ ", style = \"filled\", fillcolor = \"gold\""

        z = "{"
        w = "{\n\tgraph [rankdir = LR];\n\tnode [shape = \"circle\"];\n"

        fs = foldl1 (.) . map (\(x, y) -> r (p x) (p y)) $ zip xs ys
            where
                l x = "label = \"" ++ x ++ "\""
                xs = map (l . u . r (p "\"") (p "\\\"") . p . show) . fst $ brDFAd dfa
                ys = map ("shape = \"doublecircle\", " ++) xs


{-
:l DFA
data State = Q0 | Q1 | Q2 deriving (Eq, Ord, Show)
let ts = [ ((Q0, 0), Q0), ((Q0, 1), Q1), ((Q1, 0), Q1), ((Q1, 1), Q2), ((Q2, 0), Q2), ((Q2, 1), Q0)]
let (Just dfa) = mkDFA (Just Q0) [Q0] ts
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_217" dfa

let xs = [[],[1,1,1],[0,1,1,1],[0,0,0,0],[1],[1,1],[0,0,0,1],[1,0,0,1]]
map (`isAccept` dfa) xs
map (`isReject` dfa) xs

take 42 $ language dfa
-}
isAccept :: (Ord a) => [b] -> DFA a b -> Bool
isAccept xs dfa = (== Just True) . fmap (`member` accepts (snd dfa)) . fst $ Moore.moves xs dfa


isReject :: (Ord a) => [b] -> DFA a b -> Bool
isReject xs = not . isAccept xs


language :: (Ord a) => DFA a b -> [[b]]
language (Nothing,  _) = []
language (Just s, dfa) = filter (`isAccept` (Just s, dfa)) $ concatMap (\n -> replicateM n . Set.toList $ inSymbols dfa) [0..]


{-
:l DFA
data State = Q | R | S | T deriving (Eq, Ord, Show)
let ts = [((Q, 0), Q), ((Q, 1), R), ((R, 0), T), ((R, 1), R), ((S, 0), R), ((S, 1), T), ((T, 0), T), ((T, 1), R)]
let (Just m) = mkDFA (Just Q) [T] ts

reachables m
S `isReachable` m
R `isReachable` m

mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_218_1" m
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_218_2" $ removeUnreach m
-}
reachables :: (Ord a) => DFA a b -> Set a
reachables = Moore.reachables


isReachable :: (Ord a) => a -> DFA a b -> Bool
isReachable = Moore.isReachable


removeUnreach :: (Ord a, Ord b) => DFA a b -> DFA a b
removeUnreach = Moore.removeUnreach


{-
:l DFA

data State1 = Q0 | Q1 | Q2 | Q3 deriving (Eq, Ord, Show)
let xs = [((Q0, 0), Q2), ((Q0, 1), Q1), ((Q1, 0), Q1), ((Q1, 1), Q3), ((Q2, 0), Q2), ((Q2, 1), Q1), ((Q3, 0), Q3), ((Q3, 1), Q1)]
let (Just m1) = mkDFA (Just Q0) [Q1, Q3] xs
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_220_1" m1

data State2 = R0 | R1 | R2 deriving (Eq, Ord, Show)
let xs = [((R0, 0), R2), ((R0, 1), R1), ((R1, 0), R1), ((R1, 1), R1), ((R2, 0), R2), ((R2, 1), R1)]
let (Just m2) = mkDFA (Just R0) [R1] xs
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_220_2" m2

data State3 = S0 | S1 deriving (Eq, Ord, Show)
let ys = [ ((S0, 0), S0), ((S0, 1), S1), ((S1, 0), S1), ((S1, 1), S1)]
let (Just m3) = mkDFA (Just S0) [S1] ys
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_220_3" m3

data State4 = T0 | T1 deriving (Eq, Ord, Show)
let zs = [((T0, 0), T1), ((T0, 1), T1), ((T1, 0), T1), ((T1, 1), T1)]
let (Just m4) = mkDFA (Just T0) [T1] zs
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_220_4" m4

m1 `eqL` m1
m1 `eqL` m2
m2 `eqL` m1
m2 `eqL` m3
m1 `eqL` m3
m1 `eqL` m4
-}
eqL :: (Ord a, Ord b, Ord c) => DFA a c -> DFA b c -> Bool
eqL = Moore.eqL


eqS :: (Ord a, Ord b) => DFA a b -> a -> a -> Bool
eqS = Moore.eqS


{-
:l DFA
data State  = A  | B  deriving (Eq, Ord, Show)
data InSymbols = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 deriving (Eq, Ord, Show)
:{
let xs = [ ((Q0, A), Q1), ((Q0, B), Q2)
         , ((Q1, A), Q1), ((Q1, B), Q3)
         , ((Q2, A), Q3), ((Q2, B), Q4)
         , ((Q3, A), Q3), ((Q3, B), Q4)
         , ((Q4, A), Q2), ((Q4, B), Q3)
         , ((Q5, A), Q2), ((Q5, B), Q4)
         ]
:}
let (Just m1) = mkDFA (Just Q0) [Q2, Q3] xs
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_229_1" m1

let m2 = reduceEqS m1
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_229_2" m2

let m3 = reduceEqK m1
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_229_3" m3

let m4 = reduce m1
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/M_229_4" m4

-}
reduce :: (Ord a, Ord b) => DFA a b -> DFA a b
reduce = Moore.reduce


reduceEqS :: (Ord a, Ord b) => DFA a b -> DFA a b
reduceEqS = Moore.reduceEqS


reduceEqK :: (Ord a, Ord b) => DFA a b -> DFA a b
reduceEqK = Moore.reduceEqK

