import Data.List (nub, find)
import qualified Data.Map as Map
import System.IO
import Data.Maybe (fromMaybe, isJust)

type State = Int
type Symbol = Char
type Transition = ((State, Symbol), State)
type Automaton = (Map.Map (State, Symbol) State, State, [State])

addTransition :: Automaton -> Transition -> Automaton
addTransition (transitions, start, finals) ((from, symbol), to) =
  (Map.insert (from, symbol) to transitions, start, finals)

accepts :: Automaton -> String -> Bool
accepts (transitions, start, finals) word =
  let step (Just state) symbol = Map.lookup (state, symbol) transitions
      step Nothing _ = Nothing
      finalState = foldl step (Just start) word
  in case finalState of
       Just state -> state `elem` finals
       Nothing -> False

alphabetFromAutomaton :: Automaton -> [Symbol]
alphabetFromAutomaton (transitions, _, _) = nub [symbol | ((_, symbol), _) <- Map.toList transitions]

readAutomaton :: FilePath -> IO Automaton
readAutomaton path = do
  contents <- readFile path
  let (transitionLines, rest) = splitAt (length (lines contents) - 2) (lines contents)
      transitions = map parseTransition transitionLines
      startState = read $ head rest
      finalStates = map read $ words $ last rest
      transitionMap = Map.fromList transitions
  return (transitionMap, startState, finalStates)

parseTransition :: String -> Transition
parseTransition line =
  let [from, to, [symbol]] = words line
  in ((read from, symbol), read to)

tryPrefixes :: Automaton -> String -> [Symbol] -> String -> Int -> Maybe String
tryPrefixes automaton word alphabet prefix depth
  | accepts automaton (prefix ++ word) = Just prefix
  | depth >= maxDepth = Nothing  -- Максимальна глибина рекурсії досягнута
  | otherwise = case filter isJust results of
      [] -> Nothing
      (Just p:_) -> Just p
  where
    maxDepth = 10  -- Встановлюємо максимальну глибину рекурсії
    possiblePrefixes = [prefix ++ [a] | a <- alphabet]
    results = map (\p -> tryPrefixes automaton word alphabet p (depth + 1)) possiblePrefixes

main :: IO ()
main = do
  putStrLn "Enter the path to the automaton file:"
  filePath <- getLine
  automaton <- readAutomaton filePath
  putStrLn "Enter the word w:"
  word <- getLine
  let alphabet = alphabetFromAutomaton automaton
  -- Початковий виклик функції tryPrefixes з глибиною рекурсії 0
  let prefix = tryPrefixes automaton word alphabet "" 0
  case prefix of
    Just p -> putStrLn $ "The automaton accepts the word with prefix: " ++ p ++ word
    Nothing -> putStrLn "No prefix found that the automaton accepts."
