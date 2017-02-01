module Main where

import Control.Monad
import Uno
import CommandHandler
import Text.Read



main :: IO ()
main = do
  putStrLn "Welcome to UNO!"
  eventStore <- newEventStore
  forever $ do
    putStrLn "Type a command:"
    cmdStr <- getLine
    let maybeCmd = (readMaybe :: String -> Maybe Command) cmdStr
    case maybeCmd of
      Nothing -> print "try again please!"
      Just cmd -> do
          events <- handleCommand cmd eventStore
          print events


-- PlayCard (Player {_playerNumber = 1}) (DigitCard Yellow Four)
