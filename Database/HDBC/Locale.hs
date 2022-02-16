{-# LANGUAGE CPP #-}
module Database.HDBC.Locale
    (
     defaultTimeLocale,
     iso8601DateFormat,
     oldIso8601DateFormat
    )

where

#ifdef MIN_TIME_15
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

-- | As the semantic of System.Locale.iso8601DateFormat has changed with
--   old-locale-1.0.0.2 in a non-compatible way, we now define our own
--   (compatible) version of it.
iso8601DateFormat :: Maybe String -> String
iso8601DateFormat mTimeFmt =
    "%0Y-%m-%d" ++ case mTimeFmt of
             Nothing  -> ""
             Just fmt -> ' ' : fmt

-- | HDBC would in versions up to and including 2.4.0.3 use this time format
-- string to serialize timestamps. To keep being able to deserialize timestamps
-- serialized on database engines that keep the representation intact (e.g.
-- SQLite) we keep this format string around, such that we can fall back to it.
oldIso8601DateFormat :: Maybe String -> String
oldIso8601DateFormat mTimeFmt =
    "%Y-%m-%d" ++ case mTimeFmt of
             Nothing  -> ""
             Just fmt -> ' ' : fmt
