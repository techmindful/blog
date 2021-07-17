{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( AppM
  , AppState
  ) where

import           RIO hiding ( Handler )

import           Servant ( Handler )


type AppState = ()


type AppM = ReaderT AppState Handler

