{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Control.Monad (forM_)

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace, traceShow)
import Data.Maybe (isJust, fromJust)

class Mergeable a where
  (<++>) :: a -> a -> a
  mergeEmpty :: a
infixr 7 <++>

mergeAll :: (Foldable t, Mergeable a) => t a -> a
mergeAll = foldr (<++>) mergeEmpty

instance (Ord a) => Mergeable (Set a) where
  (<++>) = Set.union
  mergeEmpty = Set.empty

instance (Show k, Ord k, Mergeable a) => Mergeable (Map k a) where
  mergeEmpty = Map.empty
  a <++> b = foldr aux Map.empty keys where
    keys = (Map.keysSet a) `Set.union` (Map.keysSet b)
    aux key acc = (merged `Map.union` acc) where
      merged = merge (key `Map.lookup` a) (key `Map.lookup` b)
      merge Nothing  Nothing  = Map.empty
      merge Nothing  (Just b) = Map.singleton key b
      merge (Just a) Nothing  = Map.singleton key a
      merge (Just a) (Just b) = Map.singleton key (a <++> b)

data Terminal = Terminal Char 
              | Epsilon 
              deriving (Eq, Show, Ord)

type NonTerminal = String

data Production = CompleteRule    Terminal NonTerminal
                | TerminalRule    Terminal
                | NonTerminalRule NonTerminal
                deriving (Eq, Show, Ord)

type Rule = (NonTerminal, Production)

data State = State String
           | FinalState String
           | InitialState
           | GenericFinalState 
           deriving (Show, Eq, Ord)

type StateSet = Set State

type NFA = Map State    (Map Terminal StateSet)
type DFA = Map StateSet (Map Terminal StateSet)

epsilonRepresentation = '§'

charToTerminal :: Char -> Terminal
charToTerminal c
  | c == epsilonRepresentation = Epsilon
  | otherwise                  = Terminal c

splitOnFirst :: Char -> String -> (String, Maybe String)
splitOnFirst c (x:xs)
  | x == c    = ("", Just xs)
  | otherwise = let (bef, aft) = splitOnFirst c xs
                in (x:bef, aft)
splitOnFirst _ [] = ("", Nothing)

trimLeft :: String -> String
trimLeft (' ':xs) = trimLeft xs
trimLeft s        = s

trim :: String -> String
trim = reverse . trimLeft . reverse . trimLeft

stringToProduction :: String -> Production
stringToProduction ('\'':c:'\'':[]) = TerminalRule (charToTerminal c)
stringToProduction ('\'':c:'\'':xs) = CompleteRule (charToTerminal c) xs
stringToProduction s                = NonTerminalRule s

-- this function is only called when the input has already
-- been verified
lineToRules :: String -> Set Rule
lineToRules s = rules where 
  (ruleName, Just prods) = splitOnFirst '=' s
  rules = Set.fromList .
          map ((trim ruleName,) . stringToProduction . trim) .
          splitOn " | " $
          prods

linesToRules :: String -> Set Rule
linesToRules = Set.unions . map lineToRules . lines

findFinalStateNames :: Set Rule -> Set String
findFinalStateNames a = Set.map fst . Set.filter isFinal $ a where
  isFinal (head, TerminalRule Epsilon) = True
  isFinal _ = False

  
rulesToNFA :: Set Rule -> NFA
rulesToNFA a = mergeAll . Set.map ruleToMap $ a where
  finalStateNames = findFinalStateNames a
  isFinal name = name `Set.member` finalStateNames
  stringToState s
    | isFinal s = FinalState s
    | otherwise = State s
  ruleToMap :: Rule -> NFA
  ruleToMap (name, TerminalRule Epsilon) = Map.empty
  ruleToMap (name, (TerminalRule t)) =
    Map.singleton 
      (stringToState name) 
      (Map.singleton t (Set.singleton GenericFinalState))
  ruleToMap (name, (NonTerminalRule nt)) =
    Map.singleton 
      (stringToState name) 
      (Map.singleton Epsilon (Set.singleton $ stringToState nt))
  ruleToMap (name, CompleteRule t nt) =
    Map.singleton 
      (stringToState name)
      (Map.singleton t (Set.singleton $ stringToState nt))
    
printNFA :: NFA -> IO ()
printNFA nfa = do
  let states = Map.keys nfa
  forM_ states $ \state -> do
    let map = nfa Map.! state
    let terms = Map.keys map
    print state
    forM_ terms $ \term -> do
      putStr "\t"
      print term
      let targetStates = map Map.! term
      forM_ targetStates $ \ts -> do
        putStr "\t\t"
        print ts

nfaToDfa :: NFA -> DFA
nfaToDfa = Map.mapKeys Set.singleton

main = do
  args <- getArgs
  let inputFile = args !! 0
  inputText <- readFile inputFile
  let rules = linesToRules inputText
  --print . findFinalStateNames $ rules

  let nfa = rulesToNFA rules
  --print nfa
  printNFA nfa

  putStrLn ""
