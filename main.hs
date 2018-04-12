import System.Random
import System.IO
import Data.Char

{----------------------------------
 - Print str without a newline    -
 - automatically flushing stdout. -
 ----------------------------------}
putString :: String -> IO ()
putString ""
  = error "Cannot print empty String!"

putString str
  = do
       putStr str
       hFlush stdout


{-------------------------------------------
 | Generate the code via indexing          |
 | all posible code combinations at index. |
 -------------------------------------------}
genCode :: Int -> [Char]
genCode index
  = [[a, b, c, d] | a <- range,
                    b <- range,
                    c <- range,
                    d <- range] !! index
  where range = ['A'..'D']


{-----------------------------------------
 | Prompt the player to make a guess and |
 | check if their guess was right. If so |
 | leave the game. Otherwise guess again |
 | updating turn to the next.            |
 -----------------------------------------}
makeGuess :: [Char] -> Int -> Int -> IO ()
makeGuess [] _ _
  = error "Cannot make guess with empty List!"

makeGuess code turn turns
  = do
       putString "  Guess: "
       guess <- getLine

       -- Mapping all lowercase digits to upper
       -- case allowing case insensitive guesses.
       let guess'
             = map (toUpper) guess

       -- Checking if the guess
       -- is of an invalid length.
       if (length guess') /= (length code)
          then putStrLn "Code of invalid length!"
          else
               if guess' == code
                  then putStrLn "You Win!"
                  else makeTurn code (turn + 1) turns


{-------------------------------------------
 | Display turn information, check if the  |
 | player is out of turns and if their not |
 | make another guess.                     |
 -------------------------------------------}
makeTurn :: [Char] -> Int -> Int -> IO ()
makeTurn [] _ _
  = error "Cannot make turn on an empty list!"

makeTurn code turn turns
  = do
       -- Displaying turn fraction
       -- (current turn / maximum turns).
       putStrLn ("Turn (" ++ (show turn) ++ "/" ++ (show turns) ++ ")")

       if turn < turns
          then makeGuess code turn turns
          else putStrLn "You Lose!"


{--------------------------------
 | Play a game of MstrMnd.      |
 | Where turns specifys         |
 | the number of incorrect      |
 | guesses the player can make. |
 --------------------------------}
playMstrMnd :: Int -> IO ()
playMstrMnd turns
  = do
       seed <- newStdGen

       -- The number wich determines
       -- wich match shall be used
       let codeSeed
             = fst (randomR codeRange seed)

       makeTurn (genCode codeSeed) 0 turns

  where codeRange = ((-1), (4^4) - 1)

  {---------------------------------------------------
   | codeRange is the range of all posible           |
   | code combinations (0 .. 255). The reasoning     |
   | for 4 ^ 4 is that the total number of posible   |
   | combinations is equal to the length of the code |
   | to the power of the length of the code.         |
   ---------------------------------------------------}


main :: IO ()
main
  = do
       playMstrMnd 10
