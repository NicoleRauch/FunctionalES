module CommandHandler where

import Data.IORef

import EventStore
import Uno

handleCommand :: Command -> EventStore -> IO [Event]
handleCommand command eventStore = do
  pastEvents <- readEvents eventStore
  let state = foldl evolve initialState pastEvents
  let newEvents = decide command state
  appendEvents eventStore newEvents
  return newEvents

