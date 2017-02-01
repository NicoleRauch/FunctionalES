module UnoSpec where

import Test.Hspec

import qualified Data.Map.Strict as M

import Uno


spec = do
  describe "deck" $ do
    it "multiplies colors and digits" $ do
      deck [Red, Yellow] [Two, Four] `shouldBe` [DigitCard Red Two, DigitCard Red Four, DigitCard Yellow Two, DigitCard Yellow Four]
    it "even for large collections" $ do
      length (deck [Red, Yellow, Blue, Green] [Zero, One, Two, Three, Four]) `shouldBe` 20

  --describe "decide" $ do
    --it "produces events from a command and the state" $ do
      --decide (StartGame (StartGameData 4 (DigitCard (Red, Seven)))) initialState `shouldBe` [GameStarted (GameStartedData 4 (DigitCard (Red, Seven)))]


  describe "weird failures" $ do
    it "when playing the same card multiple times" $ do
      let ev1 = decide (StartGame 4) initialState
      let s1 = foldl evolve initialState ev1
      let ev2 = decide (PlayCard (Player {_playerNumber = 1}) (DigitCard Yellow Four)) s1
      ev2 `shouldBe` [CardPlayed (Player {_playerNumber = 1}) (DigitCard Yellow Four)]
      let s2 = foldl evolve s1 ev2
      let ev3 = decide (PlayCard (Player {_playerNumber = 1}) (DigitCard Yellow Four)) s2
      ev3 `shouldBe` [CardIsNotInHand (Player {_playerNumber = 1}) (DigitCard Yellow Four)]
      let s3 = foldl evolve s2 ev3
      let ev4 = decide (PlayCard (Player {_playerNumber = 1}) (DigitCard Yellow Four)) s3
      ev4 `shouldBe` [CardIsNotInHand (Player {_playerNumber = 1}) (DigitCard Yellow Four)]




  describe "evolve with indirect effects" $ do
    let events = [
              HandsDealt [(Player 1, [DigitCard Blue Seven, DigitCard Blue Nine]), (Player 2, [DigitCard Blue Nine])]
            , GameStarted (GameStartedData 2 (DigitCard Red Seven))
            , CardPlayed (Player 1) (DigitCard Blue Seven)
            ]
    let newState = foldl evolve initialState events

    it "You have to wait for your turn" $ do
      let command = PlayCard (Player 1) (DigitCard Blue Nine)
      decide command newState `shouldBe` [PlayedBeforeTurn (Player 1) (DigitCard Blue Nine)]

    it "You can play a valid card when it is your turn" $ do
      let command = PlayCard (Player 2) (DigitCard Blue Nine)
      decide command newState `shouldBe` [CardPlayed (Player 2) (DigitCard Blue Nine)]

    it "You can only play cards that are in your hand" $ do
      let command = PlayCard (Player 2) (DigitCard Blue Seven)
      decide command newState `shouldBe` [CardIsNotInHand (Player 2) (DigitCard Blue Seven)]

    it "You can play the exact same card at any time" $ do
      let events2 = [
                HandsDealt [(Player 1, [DigitCard Blue Seven, DigitCard Blue Seven]), (Player 2, [DigitCard Blue Nine])]
              , GameStarted (GameStartedData 2 (DigitCard Red Seven))
              , CardPlayed (Player 1) (DigitCard Blue Seven)
              ]
      let newState2 = foldl evolve initialState events2
      let command = PlayCard (Player 1) (DigitCard Blue Seven)
      decide command newState2 `shouldBe` [CardPlayed (Player 1) (DigitCard Blue Seven)]

{-
     5) (The played card ends up on the table) ???



      let state = State remainingStack cardOnTable hands nextPlayer
      evolve state (CardPlayed (Player 1) (DigitCard Blue Seven))


      `shouldBe` state
                                                        { _stateHands = M.fromList [(Player 1, []), (Player 2, [])]
                                                        , _stateCardOnTable = Just (DigitCard Blue Seven)
                                                        , _stateNextPlayer = Just (Player 2)
                                                        }

-}


  describe "valid card" $ do
    it "accepts identical cards" $ do
      validCard (DigitCard Blue Seven) (DigitCard Blue Seven) `shouldBe` True

    it "accepts cards with the same colour" $ do
      validCard (DigitCard Blue Seven) (DigitCard Blue Eight) `shouldBe` True

    it "accepts cards with the same value" $ do
      validCard (DigitCard Yellow Seven) (DigitCard Blue Seven) `shouldBe` True

    it "does not accept different card" $ do
      validCard (DigitCard Blue Seven) (DigitCard Red Nine) `shouldBe` False


  describe "evolve with checking the state" $ do
    let remainingStack = []
    let cardOnTable = (Just (DigitCard Red Seven))
    let nextPlayer = (Just (Player 1))

    it "applies the event to the state" $ do
      evolve initialState (GameStarted (GameStartedData 4 (DigitCard Red Seven))) `shouldBe`
          State {_stateRemainingStack = [], _stateCardOnTable = Just (DigitCard Red Seven), _stateHands = M.fromList [], _stateNextPlayer = Just (Player {_playerNumber = 1})}


    it "updates the card on the table, the player's hand and the next player when a card was played" $ do
      let hands = M.fromList [(Player 1, [DigitCard Blue Seven]), (Player 2, [])]
      let state = State remainingStack cardOnTable hands nextPlayer
      evolve state (CardPlayed (Player 1) (DigitCard Blue Seven)) `shouldBe` state
                                                        { _stateHands = M.fromList [(Player 1, []), (Player 2, [])]
                                                        , _stateCardOnTable = Just (DigitCard Blue Seven)
                                                        , _stateNextPlayer = Just (Player 2)
                                                        }


{-
    it "makes the player draw two penalty cards when they play an invalid card" $ do

    it "makes the player draw two penalty cards when they play out of turn" $ do

    it "makes the player draw two penalty cards when they play a card they do not have in hand" $ do -- how can that happen anyway? ...
-}


  describe "check played card" $ do
    let remainingStack = []
    let cardOnTable = (Just (DigitCard Red Seven))
    let nextPlayer = (Just (Player 1))

    it "a card with the same color as the card on table is valid if the player is the next one" $ do
      let hands = (M.fromList [(Player 1, [DigitCard Red Nine]), (Player 2, [])])
      let state = State remainingStack cardOnTable hands nextPlayer
      decide (PlayCard (Player 1) (DigitCard Red Nine)) state `shouldBe` [CardPlayed (Player 1) (DigitCard Red Nine)]

    it "a card with the same value as the card on table is valid if the player is the next one" $ do
      let hands = (M.fromList [(Player 1, [DigitCard Blue Seven]), (Player 2, [])])
      let state = State remainingStack cardOnTable hands nextPlayer
      decide (PlayCard (Player 1) (DigitCard Blue Seven)) state `shouldBe` [CardPlayed (Player 1) (DigitCard Blue Seven)]

    it "a card with a different value and color as the card on table is invalid even if the player is the next one" $ do
      let hands = (M.fromList [(Player 1, [DigitCard Yellow Nine]), (Player 2, [])])
      let state = State remainingStack cardOnTable hands nextPlayer
      decide (PlayCard (Player 1) (DigitCard Yellow Nine)) state `shouldBe` [InvalidCardPlayed (Player 1) (DigitCard Yellow Nine)]

    it "a card with the same value and color as the card on table is valid even if the player is not the next one" $ do
      let hands = (M.fromList [(Player 1, [DigitCard Yellow Nine]), (Player 2, [DigitCard Red Seven])])
      let state = State remainingStack cardOnTable hands nextPlayer
      decide (PlayCard (Player 2) (DigitCard Red Seven)) state `shouldBe` [CardPlayed (Player 2) (DigitCard Red Seven)]

    it "a different card may not be played by any player other than the next one" $ do
      let hands = (M.fromList [(Player 1, [DigitCard Yellow Nine]), (Player 2, [DigitCard Red Eight])])
      let state = State remainingStack cardOnTable hands nextPlayer
      decide (PlayCard (Player 2) (DigitCard Red Eight)) state `shouldBe` [PlayedBeforeTurn (Player 2) (DigitCard Red Eight)]

    it "a card is invalid if the player does not have it in his hand" $ do
      let hands = (M.fromList [(Player 1, [DigitCard Yellow Nine]), (Player 2, [])])
      let state = State remainingStack cardOnTable hands nextPlayer
      decide (PlayCard (Player 1) (DigitCard Red Eight)) state `shouldBe` [CardIsNotInHand (Player 1) (DigitCard Red Eight)]

