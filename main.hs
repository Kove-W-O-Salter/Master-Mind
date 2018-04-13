import System.Random
import System.IO
import Data.Char

{---------------------------------+
 | Print str without a newline    |
 | automatically flushing stdout. |
 +---------------------------------}
putString :: String -> IO ()
putString ""
  = error "Cannot print empty String!"

putString str
  = do
       putStr str
       hFlush stdout

{--------------------------------+
 | Convert a String to upperCase |
 +--------------------------------}
toUpperS :: String -> String
toUpperS ""
  = ""
 
toUpperS string
  = map (toUpper) string


{------------------------------------------+
 | Generate the code via indexing          |
 | all posible code combinations at index. |
 +------------------------------------------}
genCode :: Int -> [Char]
genCode index
  = [[a, b, c, d] | a <- range,
                    b <- range,
                    c <- range,
                    d <- range] !! index
  where range = ['A'..'D']


{-------------------------------------------------------+
 | Check if the players guess was correct and, if so    |
 | notify them of their victory, otherwise guess again. |
 +-------------------------------------------------------}
checkGuess :: [Char] -> [Char] -> Int -> Int -> IO ()
checkGuess code guess turn turns
  | guess == code
      = do putStrLn "You Win!"

  | otherwise
      = makeTurn code (turn + 1) turns


{----------------------------------------+
 | Prompt the player to make a guess and |
 | check if their guess was right. If so |
 | leave the game. Otherwise guess again |
 | updating turn to the next.            |
 +----------------------------------------}
makeGuess :: [Char] -> Int -> Int -> IO ()
makeGuess [] _ _
  = error "Cannot make guess with empty List!"

makeGuess code turn turns
  = do
       putString "  Guess: "
       guess <- getLine

       checkGuess code (toUpperS guess) turn turns


{------------------------------------------+
 | Display turn information, check if the  |
 | player is out of turns and if their not |
 | make another guess.                     |
 +------------------------------------------}
makeTurn :: [Char] -> Int -> Int -> IO ()
makeTurn [] _ _
  = error "Cannot make turn on an empty list!"

makeTurn code turn turns
  = do
       -- Displaying turn fraction
       -- (current turn / maximum turns).
       putStrLn ("Turn (" ++ (show turn) ++ "/" ++ (show turns) ++ ")")

       -- Checking if the player is out
       -- of turns and, if so letting
       -- them know they've lost, otherwise
       -- giving the manother guess.
       if turn==turns
          then putStrLn "You Lose!"
          else makeGuess code turn turns


{-------------------------------+
 | Play a game of MstrMnd.      |
 | Where turns specifys         |
 | the number of incorrect      |
 | guesses the player can make. |
 +-------------------------------}
playMsMnd :: Int -> IO ()
playMsMnd turns
  = do
       seed <- newStdGen

       makeTurn (genCode (codeSeed seed)) 1 turns

  where codeRange     = (0 - 1, possibleCodes - 1)
        possibleCodes = (4 ^ 4)
        codeSeed seed = fst (randomR codeRange seed)

  {-----------------------------------------------------------------------+
   | codeRange determines the range of codes (first index, last index).   |
   | The reason that the last index is (4^4) - 1 (255) is that the total  |
   | number of possible outcomes is equal to the length of a code to the  |
   | power of the number of possible digits for each element of the code: |
   | Possible Combinations = Code Length ^ Posible Code Digits.           |
   | The reasoning for a starting index of 0 - 1 is that the range        |
   | excludes the first number in the range.                              |
   +-----------------------------------------------------------------------}


main :: IO ()
main
  = do
       playMsMnd 10
