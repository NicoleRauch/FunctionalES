module Uno where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import           Data.Maybe

import Debug.Trace

data Color = Red | Green | Blue | Yellow
  deriving (Show, Read, Eq)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Read, Eq)

{-
data OtherValue = Skip | DrawTwo | DrawFour
  deriving (Show, Eq)
-}

data Card =
  DigitCard Color Digit
  deriving (Show, Read, Eq)


deck :: [Color] -> [Digit] -> [Card]
deck [] _ = []
deck (c:cs) digits = combine c digits ++ deck cs digits
  where
    combine :: Color -> [Digit] -> [Card]
    combine _ [] = []
    combine c (d:ds) = DigitCard c d : combine c ds

newtype Player = Player
  { _playerNumber :: Int
  }
  deriving (Show, Read, Eq, Ord)

data Event =
  DeckShuffled [Card]
  | HandsDealt [(Player, [Card])]
  | PlaceStack [Card]
  | GameStarted GameStartedData -- shuffles and deals, first player determined
  | CardPlayed Player Card
  | InvalidCardPlayed Player Card
  | PlayedBeforeTurn Player Card
  | CardIsNotInHand Player Card
  deriving (Show, Eq)

data GameStartedData = GameStartedData
  { _gameStartedPlayers :: Int
  , _gameStartedFirstCard :: Card
  }
  deriving (Show, Eq)


data Command =
  StartGame Int
  | PlayCard Player Card
  deriving (Show, Read, Eq)

data State = State
  { _stateRemainingStack :: [Card]
  , _stateCardOnTable :: Maybe Card
  , _stateHands :: M.Map Player [Card]
  , _stateNextPlayer :: Maybe Player
  }
  deriving (Show, Eq)

initialState :: State
initialState = State [] Nothing M.empty Nothing

decide :: Command -> State -> [Event]
decide (StartGame count) _ =
  let allCards = deck [Red, Green, Yellow, Blue] [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
      shuffledCards = allCards -- TODO
      (handsDealt, remainingCards) = dealHands count shuffledCards
  in [ DeckShuffled shuffledCards
      , HandsDealt handsDealt
      , PlaceStack (tail remainingCards)
      , GameStarted (GameStartedData count (head remainingCards))
      ]
  where
  numberOfCards = 7
  dealHands :: Int -> [Card] -> ([(Player, [Card])], [Card])
  dealHands numberOfPlayers cards =
    let initialResult = (map (\x -> (x,[])) (map Player [1..numberOfPlayers]), cards)
    in foldr (\num result -> dealOneCard result) initialResult [1..numberOfCards]
    where
    dealOneCard :: ([(Player, [Card])], [Card]) -> ([(Player, [Card])], [Card])
    dealOneCard (players, cards) =
      let (cardsToDeal, remainingCards) = splitAt numberOfPlayers cards
          updatedPlayers = zipWith (\(p, hand) c ->  (p, c:hand)) players cardsToDeal
      in (updatedPlayers, remainingCards)
decide (PlayCard player@(Player playerNumber) card@(DigitCard colorToPlay digitToPlay)) state =
  let maybeCardOnTable = _stateCardOnTable state
      nextPlayer = _stateNextPlayer state
      handOfPlayer = M.findWithDefault [] player (_stateHands state)
  in case (maybeCardOnTable, nextPlayer) of
    (Just cardOnTable@(DigitCard color digit), Just (Player nextPlayerNumber)) ->
        let identicalCard = colorToPlay == color && digitToPlay == digit
            playerHasTurn = playerNumber == nextPlayerNumber
            cardIsInHand = elem card handOfPlayer
        in if not cardIsInHand then [CardIsNotInHand player card]
           else if not (validCard cardOnTable card) then [InvalidCardPlayed player card]
           else if identicalCard then [CardPlayed player card]
           else if not playerHasTurn then [PlayedBeforeTurn player card]
           else [CardPlayed player card]

    (_,_) -> error "Either no card or no player!!!"

validCard (DigitCard c1 d1) (DigitCard c2 d2) = c1 == c2 || d1 == d2

-- decide _ _ = []

evolve :: State -> Event -> State
evolve state (DeckShuffled cards) = state { _stateRemainingStack = cards }
evolve state (HandsDealt hands) = state { _stateHands = M.fromList hands }
evolve state (PlaceStack cards) = state { _stateRemainingStack = cards }
evolve state (GameStarted (GameStartedData num firstCard)) = state
                          { _stateCardOnTable = Just firstCard
                          , _stateNextPlayer = Just (Player 1) }
evolve state (CardPlayed player@(Player num) card) = state
                          { _stateHands = M.update (\cards -> Just (L.delete card cards)) player (_stateHands state)
                          , _stateNextPlayer = Just (Player (num+1 `rem` (M.size (_stateHands state))))
                          , _stateCardOnTable = Just card
                          }
evolve state (InvalidCardPlayed _ _) = state
evolve state (PlayedBeforeTurn _ _) = state
evolve state (CardIsNotInHand _ _) = state

