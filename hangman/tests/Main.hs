module Main where

import Test.Hspec
import Hangman



main :: IO ()
main = hspec $ describe "Hangman" $ do



  describe "createGame" $

    it "creates a game" $ let
      word = "foo"
      expected = Game word [Nothing, Nothing, Nothing] []
      actual = createGame word
      in
      expected === actual



  describe "guessChar" $ do

    it "Unsuccessful guess updates history" $ let
      word = "foo"
      guess = 'a'
      expected = Game word [Nothing, Nothing, Nothing] [guess]
      actual = guessChar (createGame word) guess
      in
      expected === actual

    it "Successful guess updates history AND mask" $ let
      word = "foo"
      guess = 'f'
      expected = Game word [Just guess, Nothing, Nothing] [guess]
      actual = guessChar (createGame word) guess
      in
      expected === actual

    it "Guess on completed game just returns game" $ let
      word = "foo"
      guess = 'f'
      game = Game word [Just 'f', Just 'o', Just 'o'] "of"
      expected = game
      actual = guessChar game guess
      in
      expected === actual






(===) :: (Eq a, Show a) => a -> a -> Expectation
a === b = shouldBe a b
