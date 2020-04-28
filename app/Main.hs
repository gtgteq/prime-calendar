{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Word
import Math.NumberTheory.Primes.Testing
import Options.Generic
-- import System.IO (hPrint, stderr)

import Calendar.Days

data Opt
  = Year Word32
  | Term Word32
  | Range { start :: Word32, end :: Word32 }
  deriving (Generic, Show)
instance ParseRecord Opt

main :: IO ()
main = do
  opt <- getRecord "exports iCalendar file which contains only prime days"
  let _ = opt :: Opt
      days = filter f $ case opt of
        Year y -> [y * 10000 + 101 .. y * 10000 +  1231]
        Term y -> [y * 10000 + 101 .. y * 10000 + 10331]
        Range a b -> [a .. b]
      f = (&&) <$> isDayNumber <*> isPrime . fromIntegral
  -- hPrint stderr days
  LBS.putStr $ generateCalendar "prime" $ zip days $ repeat "prime"
