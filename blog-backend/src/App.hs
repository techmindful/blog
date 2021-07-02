{-# LANGUAGE DataKinds #-}            
{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}    
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}         
{-# LANGUAGE TypeOperators #-}         
{-# LANGUAGE UndecidableInstances #-} 


module App where

import qualified Servant
import           Servant
  ( (:>)
  , (:<|>)(..)
  , Context(..)
  , Capture
  , Get
  , Handler
  , PlainText
  , Proxy(..)
  , Put
  , ReqBody
  , ServerT
  , err401
  , hoistServer
  , hoistServerWithContext
  , serve
  , serveWithContext
  )

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Reader ( ReaderT, runReaderT )
import           Control.Concurrent.STM ( TVar, atomically, newTVar, readTVar, writeTVar )
import           Data.Aeson ( FromJSON, ToJSON )
import qualified Data.ByteArray as ByteArray
import qualified Data.Map as Map
import           Data.Map ( Map )
import           GHC.Float ( rationalToFloat )
import           GHC.Generics ( Generic )
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp


type AppState = ()


type API = "blog-apis" :> "emojis-in-elm" :> "unicode-to-path"
                       :> ReqBody '[PlainText] String :> Put '[PlainText] String


type AppM = ReaderT AppState Handler


server :: ServerT API AppM
server =
  unicodeToPathHandler
  where
    unicodeToPathHandler :: String -> AppM String
    unicodeToPathHandler userCode = do
      return "Unicode to path handler"


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Wai.Application
mkApp appState =
  serve api $ hoistServer api ( \x -> runReaderT x appState ) server


runApp :: IO ()
runApp = do

  Warp.run 9000 $ mkApp ()

