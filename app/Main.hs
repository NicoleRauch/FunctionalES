module Main where


import Uno


loopOnce :: State -> Command -> (State, [Event])
loopOnce state command =
  let events = decide command state
      -- store events
      newState = foldr (flip evolve) state events
  in (newState, events)

main = do


  let (newState, events) = loopOnce initialState (StartGame (StartGameData 4))
  print newState
  return ()

