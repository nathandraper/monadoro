{-# LANGUAGE DeriveGeneric #-}

import           GHC.Generics
import           System.Clock
import           Control.Concurrent
import           Data.Aeson

data Subject = Subject {
    count :: Int
                       } deriving (Generic, Show)

instance ToJSON Subject where -- This empty instance is possible because of the GHC.Generics and language pragma
instance FromJSON Subject where

sec2nan :: Integer
sec2nan = 1000000000

sec2micro :: Int
sec2micro = 1000000

main :: IO ()
main = do
  startTime <- getTime Monotonic
  putStrLn "Enter minutes for timer:"
  stopTime <- getLine
  runtimer startTime ((read stopTime :: Integer) * 1 * sec2nan)

runtimer :: TimeSpec -> Integer -> IO ()
runtimer startTime stopTime = do
  currentTime <- getTime Monotonic
  if (toNanoSecs $ diffTimeSpec currentTime startTime) >= stopTime
    then do
      putStrLn "time's up!"
    else do
      threadDelay $ sec2micro
      runtimer startTime stopTime
