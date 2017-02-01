module Main where

import Control.Monad
import Text.Read

import CommandHandler
import EventStore
import Uno
import Projections


main :: IO ()
main = do
  putStrLn "Welcome to UNO!"
  eventStore <- newEventStore
  forever $ do
    putStr "Current card on table: "
    currentCardOnTable eventStore >>= print
    putStr "Current hand: "
    currentHand >>= print
    putStrLn "Type a command:"
    cmdStr <- getLine
    let maybeCmd = (readMaybe :: String -> Maybe Command) cmdStr
    case maybeCmd of
      Nothing -> print "try again please!"
      Just cmd -> do
          events <- handleCommand cmd eventStore
          print events


-- PlayCard (Player {_playerNumber = 1}) (DigitCard Yellow Four)
-- PlayCard (Player {_playerNumber = 2}) (DigitCard Yellow Five)
