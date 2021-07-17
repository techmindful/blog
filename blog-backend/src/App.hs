{-# LANGUAGE DataKinds #-}            
{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}    
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}         
{-# LANGUAGE TypeOperators #-}         
{-# LANGUAGE UndecidableInstances #-} 


module App where

import           Types ( AppM, AppState )
import qualified Blogs.EmojisInElm 
import           ElmTest
  ( ElmTestResp
  , runElmTest
  )

import           RIO hiding ( Handler )
import           Prelude ( putStrLn )

import           Servant
  ( (:>)
  , Handler
  , JSON
  , PlainText
  , Proxy(..)
  , Put
  , ReqBody
  , ServerT
  , hoistServer
  , serve
  )
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp

import           Control.Arrow ( left )
import           Control.Error ( note )
import qualified Data.ByteString.Char8 as ByteStrC8
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( readFile, writeFile )
import           RIO.List ( initMaybe )
import qualified RIO.Text as Text
import           Path
  ( (</>)
  , Rel
  , Dir
  , File
  , Path
  , toFilePath
  )
import qualified Path
import           Path.IO ( createDirIfMissing ) 


type API = Blogs.EmojisInElm.API


server :: ServerT API AppM
server =

  Blogs.EmojisInElm.server


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Wai.Application
mkApp appState =
  serve api $ hoistServer api ( \x -> runReaderT x appState ) server


runApp :: IO ()
runApp = do

  Warp.run 9000 $ mkApp ()

