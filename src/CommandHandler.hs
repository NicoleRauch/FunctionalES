module CommandHandler where

import Data.IORef

import Uno

handleCommand :: Command -> EventStore -> IO [Event]
handleCommand command eventStore = do
  pastEvents <- readEvents eventStore
  let state = foldl evolve initialState pastEvents
  let newEvents = decide command state
  appendEvents eventStore newEvents
  return newEvents


data EventStore = Store (IORef [Event])

newEventStore :: IO EventStore
newEventStore = fmap Store $ newIORef []

readEvents :: EventStore -> IO [Event]
readEvents (Store ref) = readIORef ref

appendEvents :: EventStore -> [Event] -> IO ()
appendEvents (Store ref) events = modifyIORef ref (++ events)
