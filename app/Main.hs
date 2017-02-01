module Main where

import Control.Monad
import Uno
import CommandHandler



main :: IO ()
main = do
  putStrLn "Welcome to UNO!"
  eventStore <- newEventStore
  forever $ do
    putStrLn "Type a command:"
    cmdStr <- getLine
    let cmd = (read :: String -> Command) cmdStr
    events <- handleCommand cmd eventStore
    print events


-- PlayCard (Player {_playerNumber = 1}) (DigitCard Yellow Four)
