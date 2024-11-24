{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "Add Player successfully" $ do
        let initialState = Lib2.emptyState
        let player = Lib2.Player "Kirito" 10 5
        case Lib2.stateTransition initialState (Lib2.SpawnPlayer player) of
          Left err -> assertFailure err
          Right (msg, newState) -> do
            msg @?= Just "Player added."
            length (Lib2.players newState) @?= 1,
      testCase "Add duplicate player" $ do
        let initialState = Lib2.emptyState
        let player = Lib2.Player "Kirito" 100 55
        case Lib2.stateTransition initialState (Lib2.SpawnPlayer player) of
          Left err -> assertFailure err
          Right (_, stateAfterAdd) -> do
            -- Try to add the same player again.
            case Lib2.stateTransition stateAfterAdd (Lib2.SpawnPlayer player) of
              Left err -> err @?= "Player with the same name already exists."
              Right _ -> assertFailure "Expected an error for duplicate player.",
      testCase "Remove Player successfully" $ do
        let initialState = Lib2.emptyState
        let player = Lib2.Player "Kirito" 100 55
        case Lib2.stateTransition initialState (Lib2.SpawnPlayer player) of
          Left err -> assertFailure err
          Right (_, stateAfterAdd) -> do
            case Lib2.stateTransition stateAfterAdd (Lib2.RemovePlayer player) of
              Left err -> assertFailure err
              Right (msg, finalState) -> do
                msg @?= Just "Player removed."
                length (Lib2.players finalState) @?= 0,
      testCase "Remove non-existent player" $ do
        let initialState = Lib2.emptyState
        let player = Lib2.Player "Kirito" 100 55
        case Lib2.stateTransition initialState (Lib2.RemovePlayer player) of
          Left err -> err @?= "Player not found."
          Right _ -> assertFailure "Expected an error when removing non-existent player.",
      testCase "Sequence of operations" $ do
        let initialState = Lib2.emptyState
        let player1 = Lib2.Player "Kirito" 100 55
        let player2 = Lib2.Player "Medjed" 200 40
        case Lib2.stateTransition
          initialState
          ( Lib2.Sequence
              [ Lib2.SpawnPlayer player1,
                Lib2.SpawnPlayer player2,
                Lib2.RemovePlayer player1
              ]
          ) of
          Left err -> assertFailure err
          Right (msg, finalState) -> do
            msg @?= Just "Player removed."
            length (Lib2.players finalState) @?= 1, -- Verify one player is left.
      testCase "Remove already removed player" $ do
        let initialState = Lib2.emptyState
        let player = Lib2.Player "Kirito" 100 55
        case Lib2.stateTransition initialState (Lib2.SpawnPlayer player) of
          Left err -> assertFailure err
          Right (_, stateAfterAdd) -> do
            case Lib2.stateTransition stateAfterAdd (Lib2.RemovePlayer player) of
              Left err -> assertFailure err
              Right (_, stateAfterSell) -> do
                case Lib2.stateTransition stateAfterSell (Lib2.RemovePlayer player) of
                  Left err -> err @?= "Player not found."
                  Right _ -> assertFailure "Expected an error when removing a removed player.",
      testCase "Adding multiple players and checking state" $ do
        let initialState = Lib2.emptyState
        let player1 = Lib2.Player "Kirito" 100 55
        let player2 = Lib2.Player "Medjed" 200 55
        let player3 = Lib2.Player "Joker" 100 40

        case Lib2.stateTransition initialState (Lib2.SpawnPlayer player1) of
          Right (Just msg1, stateAfterAdd1) -> do
            msg1 @?= "Player added."
            case Lib2.stateTransition stateAfterAdd1 (Lib2.SpawnPlayer player2) of
              Right (Just msg2, stateAfterAdd2) -> do
                msg2 @?= "Player added."
                case Lib2.stateTransition stateAfterAdd2 (Lib2.SpawnPlayer player3) of
                  Right (Just msg3, stateAfterAdd3) -> do
                    msg3 @?= "Player added."
                    length (Lib2.players stateAfterAdd3) @?= 3
                  Left err -> assertFailure err
              Left err -> assertFailure err
          Left err -> assertFailure err
    ]