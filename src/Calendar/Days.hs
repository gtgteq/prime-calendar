{-# LANGUAGE OverloadedStrings #-}

module Calendar.Days where

import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar
import Data.Time.Clock
import Data.Word
import qualified Data.UUID as U
import Text.ICalendar

int2day :: Integral a => a -> Day
int2day n' = fromGregorian y m d
  where
  n = fromIntegral n'
  (y, x) = n `divMod` 10000
  (m, d) = fromIntegral x `divMod` 100

isDayNumber :: Integral a => a -> Bool
isDayNumber n' = 0 < d && d <= dayLimit m
  where
  n = fromIntegral n'
  (y, x) = n `divMod` 10000
  (m, d) = fromIntegral x `divMod` 100
  dayLimit :: Int -> Int
  dayLimit 1 = 31
  dayLimit 2 = if isLeapYear y then 29 else 28
  dayLimit 3 = 31
  dayLimit 4 = 30
  dayLimit 5 = 31
  dayLimit 6 = 30
  dayLimit 7 = 31
  dayLimit 8 = 31
  dayLimit 9 = 30
  dayLimit 10 = 31
  dayLimit 11 = 30
  dayLimit 12 = 31
  dayLimit _ = 0

generateCalendar :: TL.Text -> [(Word32, TL.Text)] -> LBS.ByteString
generateCalendar cat days = printICalendar def $ def
  { vcEvents = M.fromList $ map f days
  }
  where
  f (n, t) =
    let d = int2day n
        ud = UTCTime d 0
        uo = UTCTime (fromGregorian 1958 1 1) 0
        nw :: Word64
        nw = round $ diffUTCTime ud uo
        nw1 = fromIntegral nw
        nw2 = fromIntegral $ nw `shiftR` 32
        uuid = U.fromWords 0 n nw2 nw1
        uidText = TL.fromStrict $ U.toText uuid
    in ((uidText, Just $ Left $ Date d), VEvent
      { veDTStamp       = flip DTStamp def $ UTCTime d 0
      , veUID           = UID uidText def
      , veClass         = def
      , veDTStart       = Just $ flip DTStartDate def $ Date d
      , veCreated       = Nothing
      , veDescription   = Just $ Description (TL.pack (show n) <> " is " <> t) Nothing Nothing def
      , veGeo           = Nothing
      , veLastMod       = Nothing
      , veLocation      = Nothing
      , veOrganizer     = Nothing
      , vePriority      = def
      , veSeq           = def
      , veStatus        = Nothing
      , veSummary       = Just $ Summary t Nothing Nothing def
      , veTransp        = def
      , veUrl           = Nothing
      , veRecurId       = Just $ RecurrenceIdDate (Date d) Nothing def
      , veRRule         = S.empty
      , veDTEndDuration = Nothing
      , veAttach        = S.empty
      , veAttendee      = S.empty
      , veCategories    = S.singleton $ Categories (S.singleton cat) Nothing def
      , veComment       = S.empty
      , veContact       = S.empty
      , veExDate        = S.empty
      , veRStatus       = S.empty
      , veRelated       = S.empty
      , veResources     = S.empty
      , veRDate         = S.empty
      , veAlarms        = S.empty
      , veOther         = S.empty
      })
