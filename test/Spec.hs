module Main where

import Test.Hspec

import Data.Time.Calendar

import Calendar.Prime hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "int2day" $ do
    it "maps correctly" $
      let n = 19700101 :: Int
      in int2day n == fromGregorian 1970 1 1
  describe "isDayNumber" $ do
    it "includes leap day" $
      let n = 20200229 :: Int
      in isDayNumber n == True
    it "excludes leap day" $
      let n = 20210229 :: Int
      in isDayNumber n == False
