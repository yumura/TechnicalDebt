module NFAwE ( NFAwEd
             , states
             , inSymbols
             , outSymbols
             , transition
             , eTransition
             , accepts
             , NFAwE

             , mkNFAwE
             , brNFAwEd

             , toGraph
             , toGraphviz
             , toGraphviz'
             , mkDotPng
             , mkDotPng'

             , move
             , moves
             , eMove
             , eClMove
             , move'
             , moves'
             , moves''
             , isAccept
             , isReject
             , language

             , fromNFA
             , toNFA
             , fromDFA
             , toDFA
             ) where

import Data.Maybe (fromJust)
import Data.Set (Set, member, notMember, union)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz, Orient(..))
import Control.Monad (replicateM)
import System.Cmd (system)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text

import NFA (NFA, mkNFA, brNFAd)
import DFA (DFA)
import qualified NFA


data NFAwEd a b = NFAwEd { states      :: Set a
                         , inSymbols   :: Set b
                         , transition  :: a -> b -> Set a
                         , eTransition :: a -> Set a
                         , accepts     :: Set a
                         }


type NFAwE a b = (Set a, NFAwEd a b)


outSymbols :: NFAwE a b -> Set Bool
outSymbols _ = Set.fromList [False, True]


{-
-}
mkNFAwE :: (Ord a, Ord b) => [a] -> [a] -> [(a, [a])] -> [((a, b), [a])] -> NFAwE a b
mkNFAwE ss as es ts = (Set.fromList ss, mkNFAwEd ss as es ts)


mkNFAwEd :: (Ord a, Ord b) => [a] -> [a] -> [(a, [a])] -> [((a, b), [a])] -> NFAwEd a b
mkNFAwEd ss as es ts = NFAwEd
    { states      = Set.fromList $ ss ++ as ++ map (fst . fst) ts ++ concatMap snd ts
    , inSymbols      = Set.fromList $ map (snd . fst) ts
    , transition  = tFun
    , eTransition = tFun'
    , accepts     = Set.fromList as
    } where
        tFun  x y  = f . fmap Set.fromList $ list2Fun ts (x, y)
        tFun' x    = Set.singleton x `union` (f . fmap Set.fromList $ list2Fun es x)
        f (Just z) = z
        f Nothing  = Set.empty


brNFAwEd :: (Ord a, Ord b) => NFAwEd a b -> ([a], [(a, [a])], [((a, b), [a])])
brNFAwEd nfa = (as, es, tos)
    where
        as  = Set.toList $ accepts nfa

        es  = do
            s <- Set.toList $ states nfa
            return (s, Set.toList $ eTransition nfa s)

        tos = do
            s <- Set.toList $ states nfa
            i <- Set.toList $ inSymbols nfa
            return ((s, i), Set.toList $ transition nfa s i)


list2Fun :: (Ord a) => [(a, b)] -> a -> Maybe b
list2Fun xs x = Map.lookup x $ Map.fromList xs


list2Fun' :: (Ord a) => [(a, b)] -> a -> b
list2Fun' xs x = fromJust $ list2Fun xs x


{-
-}
toGraph :: (Show a, Show b) => NFAwEd a b -> Gr String String
toGraph nfa = mkGraph ns (es1 ++ es2)
    where
        ss  = map show . Set.toList $ states nfa
        ns  = zip [1..] ss

        f   = list2Fun' (zip ss [1..]) . show
        g x = ' ':show x ++ " "
        
        es1 = do
            s <- Set.toList $ states nfa
            t <- Set.toList $ eTransition nfa s
            return (f s, f t, "")

        es2 = do
            s <- Set.toList $ states nfa
            i <- Set.toList $ inSymbols nfa
            t <- Set.toList $ transition nfa s i
            return (f s, f t, g i)


toGraphviz :: (Show a, Show b) => NFAwE a b -> String -> (Double, Double) -> (Int, Int) -> String
toGraphviz (ss, nfa) t wh pwh = gvizTextConv ss nfa $ graphviz (toGraph nfa) t wh pwh Portrait


toGraphviz' :: (Show a, Show b) => NFAwE a b -> String -> String
toGraphviz' nfa t = toGraphviz nfa t (0, 0) (0, 0)


mkDotPng :: String -> String -> IO()
mkDotPng fileName gviz = do
    let dotfile = fileName ++ ".dot"
        pngfile = fileName ++ ".png"

    writeFile dotfile gviz
    system $ unwords ["dot", "-Tpng", dotfile, "-o", pngfile]
    return ()


mkDotPng' :: (Show a, Show b) => String -> NFAwE a b -> IO()
mkDotPng' fileName nfa = mkDotPng fileName $ toGraphviz' nfa "DFA"


gvizTextConv :: Show a => Set a -> NFAwEd a b -> String -> String
gvizTextConv ss nfa = u . fs xs2 ys2 . fs xs1 ys1 . r (p x) (p y) . p
    where
        p = Text.pack
        u = Text.unpack
        r = Text.replace

        x = "{"
        y = "{\n\tgraph [rankdir = LR]\n\tnode [shape = \"circle\"];\n"

        xs = map ((\l -> "label = \"" ++ l ++ "\"") . u . r (p "\"") (p "\\\"") . p . show) . Set.toList
        fs z t = foldl1 (.) . map (\(x, y) -> r (p x) (p y)) $ zip z t

        xs1 = xs ss
        ys1 = map (++ ", style = \"filled\", fillcolor = \"gold\"") xs1

        xs2 =  xs $ accepts nfa
        ys2 = map (++ ", shape = \"doublecircle\"") xs2


{-
-}
move :: (Ord a) => b -> NFAwE a b -> NFAwE a b
move x (ss, nfa) = (ts, nfa)
        where
            ts = Set.foldl Set.union Set.empty $ Set.map (\s -> transition nfa s x) ss


moves :: (Ord a) => [b] -> NFAwE a b -> NFAwE a b
moves xs nfa = foldl (flip ($)) nfa $ map move xs


eMove :: (Ord a) => NFAwE a b -> NFAwE a b
eMove (ss, nfa) = (ss `union` es, nfa)
    where
        es = Set.foldl Set.union Set.empty $ Set.map (eTransition nfa) ss


eClMove :: (Ord a) => NFAwE a b -> NFAwE a b
eClMove nfa
    | fst nfa == fst nfa' = nfa
    | otherwise           = eClMove nfa'
        where
            nfa' = eMove nfa


move' :: (Ord a) => b -> NFAwE a b -> NFAwE a b
move' x = move x . eClMove


moves' :: (Ord a) => [b] -> NFAwE a b -> NFAwE a b
moves' xs nfa = foldl (flip ($)) nfa $ map move' xs


moves'' :: (Ord a) => [b] -> NFAwE a b -> NFAwE a b
moves'' xs = eClMove . moves' xs


{-
-}
isAccept :: (Ord a) => [b] -> NFAwE a b -> Bool
isAccept xs (ss, nfa) = any (`member` accepts nfa). Set.toList . fst $ moves'' xs (ss, nfa)


isReject :: (Ord a) => [b] -> NFAwE a b -> Bool
isReject xs = not . isAccept xs


language :: (Ord a) => NFAwE a b -> [[b]]
language nfa = filter (`isAccept` nfa) $ concatMap xs [0..]
    where
        xs n = replicateM n . Set.toList . inSymbols $ snd nfa


{-
-}
toDFA :: (Ord a, Ord b) => NFAwE a b -> DFA [a] b
toDFA nfa = NFA.toDFA $ toNFA nfa


fromDFA :: (Ord a, Ord b) => DFA a b -> NFAwE a b
fromDFA dfa = fromNFA $ NFA.fromDFA dfa


toNFA :: (Ord a, Ord b) => NFAwE a b -> NFA a b
toNFA (ss', nfa)
    | acceptEpsilon (ss', nfa) = mkNFA ss (ss ++ as) ts
    | otherwise                = mkNFA ss as ts
        where
            ss = Set.toList ss'
            as = Set.toList $ accepts nfa
            ts = do
                s <- Set.toList $ states nfa
                i <- Set.toList $ inSymbols nfa
                let t = Set.toList . fst $ moves'' [i] (ss', nfa)
                return ((s, i), t)


fromNFA :: (Ord a, Ord b) => NFA a b -> NFAwE a b
fromNFA (ss, nfa) = mkNFAwE (Set.toList ss) as [] ts
    where
      (as, ts) = brNFAd nfa


acceptEpsilon :: (Ord a) => NFAwE a b -> Bool
acceptEpsilon (ss, nfa) = any (`member` accepts nfa) . Set.toList . fst $ eClMove (ss, nfa)


