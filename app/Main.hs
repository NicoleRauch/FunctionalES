module Main where

import Control.Monad
import Data.Foldable
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
    putStr "Current Player: "
    player <- currentPlayer eventStore
    print player
    for_ player $ \p -> do
      putStr "Current hand: "
      currentHand p eventStore >>= print
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
