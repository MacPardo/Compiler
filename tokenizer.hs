{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Control.Monad (forM_)

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace, traceShow)
import Data.Maybe (isJust, fromJust)

prefixTrace :: (Show a) => String -> a -> a
prefixTrace s a = trace (s ++ (show a)) a

mapOfSetsUnions :: (Foldable t, Ord a, Ord b) => t (Map a (Set b)) -> Map a (Set b)
mapOfSetsUnions a = foldr f Map.empty a 
  where f m acc = Map.union (Map.mapWithKey mergeValues acc) m
          where mergeValues k v
                  |isJust mRules = Set.union (fromJust mRules) v
                  |otherwise = v
                  where mRules = Map.lookup k m

--mapOfMapsOfSetsUnions :: (Foldable t, Ord a, 



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
           | UFinalState 
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

  
--updateFinalStateRules :: Set Rule -> Set Rule
rulesToNFA a = traceShow a (Set.map ruleToMap $ a) where
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
      (Map.singleton t (Set.singleton UFinalState))
  ruleToMap (name, (NonTerminalRule nt)) =
    Map.singleton 
      (stringToState name) 
      (Map.singleton Epsilon (Set.singleton $ stringToState nt))
  ruleToMap (name, CompleteRule t nt) =
    Map.singleton 
      (stringToState name)
      (Map.singleton t (Set.singleton $ stringToState nt))

--ruleToNFA :: (State, Production) -> NFA
--ruleToNFA (state, TerminalRule Epsilon)
--  = Map.empty
--ruleToNFA (state, TerminalRule t)
--  = Map.singleton state (Map.singleton t FinalState)
--ruleToNFA (state, NonTerminalRule )

--printRules :: Set Rule -> IO ()
printRules a = do
  forM_ (Set.toList a) $ \(h, b) -> do
    putStrLn $ "rule( " ++ h ++ " ) = " ++ (show b)
    


main = do
  args <- getArgs
  let inputFile = args !! 0
  inputText <- readFile inputFile
  let rules = linesToRules inputText
  print . findFinalStateNames $ rules

  let nfa = rulesToNFA rules
  print nfa

  putStrLn ""
