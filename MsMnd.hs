module MsMnd where

import System.IO
import System.Random
import Data.Char


{-==========================+
 | Write str to the console |
 | flushing to console.     |
 +==========================-}
fPutStr :: String -> IO ()
fPutStr str
  = do { putStr str;
         hFlush stdout;
  }


{-============================+
 | Convert str to upper case. |
 +============================-}
toUpperS :: String -> String
toUpperS str
  = map (toUpper) str


{-===========================+
 | Get the index'th possible |
 | code combination.         |
 +===========================-}
genCode :: Int -> [Char]
genCode index
  = [[a,b,c,d] | a <- range,
                 b <- range,
                 c <- range,
                 d <- range] !! index
  where range = ['A'..'D']


{-============================================+
 | Get updated display with each digit        |
 | that was correct, in the guess, filled in. |
 +============================================-}
updateDisplay :: String -> String -> String -> String
updateDisplay code guess display
  = [if guessi == codei
       then
         codei
       else
         displayi
      | (codei, guessi, displayi) <- zip3 code guess display]


{-================================+
 | Check if the guess was correct |
 | update the display.            |
 +================================-}
checkGuess :: String -> String -> String -> (Bool, String)
checkGuess code guess display
  = (guess == code,
     updateDisplay code guess display)


{-==========================================+
 | Prompt the user for a guess and check    |
 | if that guess was correct and, if so     |
 | notify the user that they won, otherwise |
 | take another turn.                       |
 +==========================================-}
makeGuess :: String -> String -> Int -> Int -> IO ()
makeGuess code display turn turns
  = do { fPutStr "  Guess: ";
         guess <- getLine;

         let guess'                = toUpperS guess
             (correct, newDisplay) = checkGuess code guess' display

         in if correct
              then
                putStrLn "You Win!"
              else
                makeTurn code newDisplay (turn + 1) turns
  }


{-==============================+
 | Make a turn, displaying turn |
 | data and display String.     |
 +==============================-}
makeTurn :: String -> String -> Int -> Int -> IO ()
makeTurn code display turn turns
  = do { putStrLn ("Turn (" ++ (show turn) ++ "/" ++ (show turns) ++ ") " ++ display);

         if turn == turns
           then
             putStrLn "You Lose!"
           else
             makeGuess code display turn turns
  }

{-======================+
 | Play a game of MsMnd |
 | with turns turns.    |
 +======================-}
playMsMnd :: Int -> IO ()
playMsMnd turns
  = do { seed <- newStdGen;
         makeTurn (genCode (codeSeed seed)) "----" turn turns
  }
  where possibleCodes = (4 ^ 4)
        codeRange     = (0 - 1, possibleCodes - 1)
        codeSeed seed = fst (randomR codeRange seed)
        turn          = 1



