{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Make
  ( Result(..)
  ) where

import           RIO

import           Data.Aeson as Aeson ( ToJSON(..), (.=), object )


data Result
  = CompilerError Text
  | Html Text
  deriving ( Generic )
instance ToJSON Result where
  toJSON result =
    case result of
      CompilerError txt ->
        object [ "compilerError" .= txt ]

      Html txt ->
        object [ "html" .= txt ]

