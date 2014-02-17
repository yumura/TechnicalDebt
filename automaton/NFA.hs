module NFA ( NFAd
           , NFA
           , states
           , inSymbols
           , transition
           , accepts

           , mkNFA
           , brNFAd

           , toGraph
           , toGraphviz
           , toGraphviz'
           , mkDotPng
           , mkDotPng'

           , removeDead

           , move
           , moves
           , isAccept
           , isReject
           , language

           , fromDFA
           , toDFA
           ) where

import Data.Set (Set, member)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz, Orient(..))
import System.Cmd (system)
import Control.Monad (replicateM)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import DFA (DFA, mkDFA, brDFAd)
import qualified DFA


data NFAd a b = NFAd { states     :: Set a
                     , inSymbols  :: Set b
                     , transition :: a -> b -> Set a
                     , accepts    :: Set a
                     }


type NFA a b = (Set a, NFAd a b)



{-
:l NFA
data State = R0 | R1 | R2 deriving (Eq, Ord, Show)
:{
let ts = [ ((R0, 0), [R0]    )
         , ((R0, 1), [R0, R1])
         , ((R1, 0), []      )
         , ((R1, 1), [R2]    )
         , ((R2, 0), [R2]    )
         , ((R2, 1), []      )
         ]
:}

let (s, nfa) = mkNFA [R0] [R2] ts
states nfa
inSymbols nfa
transition nfa R0 1
brNFAd nfa

-}
mkNFA :: (Ord a, Ord b) => [a] -> [a] -> [((a, b), [a])] -> NFA a b
mkNFA ss as ts = (Set.fromList ss, mkNFAd ss as ts)


mkNFAd :: (Ord a, Ord b) => [a] -> [a] -> [((a, b), [a])] -> NFAd a b
mkNFAd ss as ts = NFAd
    { states     = Set.fromList $ ss ++ as ++ map (fst . fst) ts ++ concatMap snd ts
    , inSymbols  = Set.fromList $ map (snd . fst) ts
    , transition = \x y -> Set.fromList . fromMaybe [] . Map.lookup (x, y) $ Map.fromList ts
    , accepts    = Set.fromList as
    }


brNFAd :: (Ord a, Ord b) => NFAd a b -> ([a], [((a, b), [a])])
brNFAd nfa = (as, tos)
    where
        as  = Set.toList $ accepts nfa
        tos = [((s, i), Set.toList $ transition nfa s i) | s <- Set.toList $ states nfa, i <- Set.toList $ inSymbols nfa]


{-
let nfa' = (s, nfa)
mkDotPng' "/Users/yumura_s/Documents/automaton/dot/NFA_231" nfa'
-}
toGraph :: (Show a, Show b) => NFAd a b -> Gr String String
toGraph nfa = mkGraph ns es
    where
        ss = map show . Set.toList $ states nfa
        ns = zip [1..] ss
        es = do 
            s <- Set.toList $ states nfa
            i <- Set.toList $ inSymbols nfa
            t <- Set.toList $ transition nfa s i
            return (f s, f t, g i)
              where
                f x = (Map.! show x) . Map.fromList $ zip ss [1..]
                g x = ' ':show x ++ " "


toGraphviz :: (Show a, Show b) => NFA a b -> String -> (Double, Double) -> (Int, Int) -> String
toGraphviz (ss, nfa) t wh pwh = gvizTextConv ss nfa $ graphviz (toGraph nfa) t wh pwh Portrait


toGraphviz' :: (Show a, Show b) => NFA a b -> String -> String
toGraphviz' nfa t = toGraphviz nfa t (0, 0) (0, 0)


mkDotPng :: String -> String -> IO()
mkDotPng fileName gviz = do
    let dotfile = fileName ++ ".dot"
        pngfile = fileName ++ ".png"

    writeFile dotfile gviz
    system $ unwords ["dot", "-Tpng", dotfile, "-o", pngfile]
    return ()


mkDotPng' :: (Show a, Show b) => String -> NFA a b -> IO()
mkDotPng' fileName nfa = mkDotPng fileName $ toGraphviz' nfa "NFA"


gvizTextConv :: Show a => Set a -> NFAd a b -> String -> String
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
isAlive :: (Ord a, Ord b) => a -> DFA a b -> Bool
isAlive x (_, dfa) = any (`member` DFA.accepts dfa) . Set.toList $ DFA.reachables (Just x, dfa)


isDead :: (Ord a, Ord b) => a -> DFA a b -> Bool
isDead x = not . isAlive x


removeDead :: (Ord a, Ord b) => DFA a b -> NFA a b
removeDead (s, dfa) = mkNFA (catMaybes [s]) as $ map (\(x, y) -> (x, [y])) . filter ((`isAlive` (s, dfa)) . fst . fst) $ filter ((`isAlive` (s, dfa)) . snd) ts
        where
            (as, ts) = DFA.brDFAd dfa


{-
:l NFA
data State = R0 | R1 | R2 deriving (Eq, Ord, Show)
let nfa = mkNFA [R0] [R2] [((R0, 0), [R0]), ((R0, 1), [R0, R1]), ((R1, 0), []), ((R1, 1), [R2]), ((R2, 0), [R2]), ((R2, 1), [])]

fst $ moves [1] nfa
fst $ moves [1,1] nfa
fst $ moves [1,1,0] nfa
fst $ moves [1,1,0,1] nfa
fst $ moves [1,1,0,1,1] nfa
-}
move :: (Ord a) => b -> NFA a b -> NFA a b
move x (ss, nfa) = (ts, nfa)
        where
            ts = Set.unions . map (flip (transition nfa) x) $ Set.toList ss


moves :: (Ord a) => [b] -> NFA a b -> NFA a b
moves xs nfa = foldl (flip ($)) nfa $ map move xs


{-
:l NFA
data State = R0 | R1 | R2 deriving (Eq, Ord, Show)
let nfa = mkNFA [R0] [R2] [((R0, 0), [R0]), ((R0, 1), [R0, R1]), ((R1, 0), []), ((R1, 1), [R2]), ((R2, 0), [R2]), ((R2, 1), [])]
let is = [[1,1,0,0,0],[1,1,1,0,0],[1,1,1,1,0],[1,1,0,1,1],[1,1,0,1,1],[0],[1],[0,1],[1,0],[1,0]]

map (`isAccept` nfa) is
map (`isReject` nfa) is

take 10 $ language nfa
take 10 . drop 1000 $ language nfa
-}
isAccept :: (Ord a) => [b] -> NFA a b -> Bool
isAccept xs = not . isReject xs


isReject :: (Ord a) => [b] -> NFA a b -> Bool
isReject xs nfa = Set.null . (Set.intersection . accepts $ snd nfa) . fst $ moves xs nfa


language :: (Ord a) => NFA a b -> [[b]]
language nfa = filter (`isAccept` nfa) $ concatMap (\n -> replicateM n . Set.toList . inSymbols $ snd nfa) [0..]


{-
:l NFA
data State = R0 | R1 | R2 deriving (Eq, Ord, Show)
let nfa = mkNFA [R0] [R2] [((R0, 0), [R0]), ((R0, 1), [R0, R1]), ((R1, 0), []), ((R1, 1), [R2]), ((R2, 0), [R2]), ((R2, 1), [])]
let dfa = toDFA nfa
DFA.mkDotPng' "/Users/yumura_s/Documents/automaton/dot/NFA_234" dfa
-}
toDFA :: (Ord a, Ord b) => NFA a b -> DFA [a] b
toDFA (ss, nfa) = fromJust . uncurry (mkDFA (Just $ Set.toList ss)) $ subsetStates nfa [] [] Set.empty [ss]


subsetStates :: (Ord a, Ord b) => NFAd a b -> [[a]] -> [(([a], b), [a])] -> Set(Set a) -> [Set a] -> ([[a]], [(([a], b), [a])])
subsetStates nfa as ts xss [] = (as, ts)
subsetStates nfa as ts xss (ys:yss)
    | ys `member` xss = subsetStates nfa as ts xss yss
    | otherwise       = subsetStates nfa (as' ++ as) (ts' ++ ts) (Set.insert ys xss) (yss' ++ yss)
        where
            is   = Set.toList $ inSymbols nfa
            yss' = map (fst . flip move (ys, nfa)) is
            ts'  = (repeat (Set.toList ys) `zip` is) `zip` map Set.toList yss'
            as'  = [Set.toList ys | isAccept [] (ys, nfa)]


fromDFA :: (Ord a, Ord b) => DFA a b -> NFA a b
fromDFA (s, dfa) = mkNFA (catMaybes [s]) as ts
        where
            (as, ts') = brDFAd dfa
            ts = [(si, [t]) | (si, t) <- ts']

