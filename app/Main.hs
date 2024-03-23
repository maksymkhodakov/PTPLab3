import Data.List (find, nub)
import qualified Data.Map as Map
import Control.Monad (filterM)

type State = Int
type Symbol = Char
type Transition = ((State, Symbol), State)
type Automaton = (Map.Map (State, Symbol) State, State, [State])

-- Функція для додавання переходу до автомата
addTransition :: Automaton -> Transition -> Automaton
addTransition (transitions, start, finals) ((from, symbol), to) =
  (Map.insert (from, symbol) to transitions, start, finals)

-- Функція, що перевіряє, чи автомат приймає слово
accepts :: Automaton -> String -> Bool
accepts (transitions, start, finals) word =
  let step (Just state) (symbol) = Map.lookup (state, symbol) transitions
      step Nothing _ = Nothing
      finalState = foldl step (Just start) word
  in case finalState of
       Just state -> state `elem` finals
       Nothing -> False

-- Генерація всіх комбінацій символів алфавіту
generatePrefixes :: [Symbol] -> Int -> [[Symbol]]
generatePrefixes alphabet maxLen = filter (\x -> length x <= maxLen) $ concatMap (\n -> sequence (replicate n alphabet)) [0..maxLen]

-- Визначення алфавіту автомата
alphabetFromAutomaton :: Automaton -> [Symbol]
alphabetFromAutomaton (transitions, _, _) = nub [symbol | ((_, symbol), _) <- Map.toList transitions]

-- Пошук префікса x такого, що xw приймається автоматом
findPrefix :: Automaton -> String -> Int -> Maybe String
findPrefix automaton word maxLen = find (\prefix -> accepts automaton (prefix ++ word)) (generatePrefixes (alphabetFromAutomaton automaton) maxLen)

main :: IO ()
main = do
  -- Простий лінійний автомат
  let automaton1 = (Map.fromList [((0, 'a'), 1), ((1, 'b'), 2)], 0, [2])
  let word1 = "b"
  putStrLn "\nAutomaton 1 (accepts 'ab'):"
  let maxPrefixLength1 = length word1 + 10 -- Наприклад, дозволяємо префікси на 3 символи довші за слово
  case findPrefix automaton1 word1 maxPrefixLength1 of
    Just prefix -> putStrLn $ "Found: " ++ prefix ++ word1
    Nothing -> putStrLn "No such prefix x exists that xw is accepted by the automaton."

  -- Автомат з петлею
  let automaton2 = (Map.fromList [((0, 'a'), 1), ((1, 'a'), 1), ((1, 'b'), 2), ((2, 'c'), 3)], 0, [3])
  let word2 = "bc"
  putStrLn "\nAutomaton 2 (accepts 'a*bc'):"
  let maxPrefixLength2 = length word2 + 3 -- Наприклад, дозволяємо префікси на 3 символи довші за слово
  case findPrefix automaton2 word2 maxPrefixLength2 of
    Just prefix -> putStrLn $ "Found: " ++ prefix ++ word2
    Nothing -> putStrLn "No such prefix x exists that xw is accepted by the automaton."

  -- Автомат, що приймає парну кількість 'a'
  let automaton3 = (Map.fromList [((0, 'a'), 1), ((1, 'a'), 0)], 0, [0])
  let word3 = "aa"
  putStrLn "\nAutomaton 3 (accepts even number of 'a'):"
  let maxPrefixLength3 = length word3 + 3 -- Наприклад, дозволяємо префікси на 3 символи довші за слово
  case findPrefix automaton3 word3 maxPrefixLength3 of
    Just prefix -> putStrLn $ "Found: " ++ prefix ++ word3
    Nothing -> putStrLn "No such prefix x exists that xw is accepted by the automaton."

  -- Автомат, що приймає слова, що закінчуються на "ab"
  let automaton4 = (Map.fromList [((0, 'a'), 1), ((1, 'b'), 2), ((0, 'b'), 0), ((1, 'a'), 1), ((2, 'a'), 1)], 0, [2])
  let word4 = "ab"
  putStrLn "\nAutomaton 4 (accepts words ending in 'ab'):"
  let maxPrefixLength4 = length word4 + 3 -- Наприклад, дозволяємо префікси на 3 символи довші за слово
  case findPrefix automaton4 word4 maxPrefixLength4 of
    Just prefix -> putStrLn $ "Found: " ++ prefix ++ word4
    Nothing -> putStrLn "No such prefix x exists that xw is accepted by the automaton."

  -- Автомат, що приймає лише слова, починаючи з "abc"
  let automaton5 = (Map.fromList [((0, 'a'), 1), ((1, 'b'), 2), ((2, 'c'), 3)], 0, [3])
  let word5 = "d"
  putStrLn "\nAutomaton 5 (accepts words starting with 'abc'):"
  let maxPrefixLength5 = length word5 + 3 -- Наприклад, дозволяємо префікси на 3 символи довші за слово
  case findPrefix automaton5 word5 maxPrefixLength5 of
    Just prefix -> putStrLn $ "Found: " ++ prefix ++ word5
    Nothing -> putStrLn "No such prefix x exists that xw is accepted by the automaton."
