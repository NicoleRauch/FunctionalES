module EventStore where

import Data.IORef

import Uno

data EventStore = Store (IORef [Event])

newEventStore :: IO EventStore
newEventStore = fmap Store $ newIORef []

readEvents :: EventStore -> IO [Event]
readEvents (Store ref) = readIORef ref

appendEvents :: EventStore -> [Event] -> IO ()
appendEvents (Store ref) events = modifyIORef ref (++ events)
