{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc
import System.Exit (exitFailure, exitSuccess)

import Port67

main = case (all id [test1, test2]) of
    True  -> exitSuccess
    False -> exitFailure

test1 :: Bool
test1 = breakSentences input == output
  where
    input = Para [Str "This", Space, Str "is", Space, Str "a", Space, Str "test.", Space, Str "Bob."]
    output = Para [Str "This", Space, Str "is", Space, Str "a", Space, Str "test.", interSentenceSpace, Str "Bob."]

test2 :: Bool
test2 = not (breakSentences input == output)
  where
    input = Para [Str "This", Space, Str "is", Space, Str "a", Space, Str "test.", Space, Str "Bob."]
    output = Para [Str "This", Space, Str "is", Space, Str "a", Space, Str "test.", interSentenceSpace, Str "Bob.", interSentenceSpace]
