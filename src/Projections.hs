module Projections where

import qualified Data.Foldable as F
import qualified Data.List as L

import Uno
import EventStore

project :: (a -> Event -> a) -> a -> EventStore -> IO a
project projection initialValue eventStore = do
  events <- readEvents eventStore
  return $ foldl projection initialValue events

currentCardOnTable :: EventStore -> IO (Maybe Card)
currentCardOnTable = project cardOnTable Nothing

cardOnTable :: Maybe Card -> Event -> Maybe Card
cardOnTable _ (CardPlayed _ card) = Just card
cardOnTable currentCard _ = currentCard

currentHand :: Player -> EventStore -> IO [Card]
currentHand player = project (cardsInHand player) []

cardsInHand :: Player -> [Card]  -> Event -> [Card]
cardsInHand player cards (HandsDealt allHands) = F.concat . fmap snd $
  F.find (\pair -> player == fst pair) allHands
cardsInHand player cards (CardPlayed byPlayer cardBeingPlayed)
  | player == byPlayer = L.delete cardBeingPlayed cards
cardsInHand _ cards _ = cards

-- playerOnTurn :: Maybe Player -> Event -> Maybe Player
-- playerOnTurn
