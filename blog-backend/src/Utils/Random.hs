{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Random
  ( getRandomHash
  ) where

import           RIO

import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )
import qualified Data.ByteString.Char8 as ByteStrC8
import qualified RIO.Text as Text


getRandomHash :: IO Text
getRandomHash = do
  seed <- seedNew
  let seedStr = show $ seedToInteger seed
      hashStr = show $ hashWith SHA256 $ ByteStrC8.pack seedStr
  pure $ Text.pack hashStr

