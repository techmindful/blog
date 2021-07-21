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
import qualified Blogs.Emojis_In_Elm 

import           RIO hiding ( Handler )

import           Servant
  ( Proxy(..)
  , ServerT
  , (:>)
  , hoistServer
  , serve
  )
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp

--import           Optics ( (^.) )


type API = "blog-apis" :> "emojis-in-elm" :> Blogs.Emojis_In_Elm.API


server :: ServerT API AppM
server =

  Blogs.Emojis_In_Elm.server


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Wai.Application
mkApp appState =
  serve api $ hoistServer api ( \x -> runReaderT x appState ) server


runApp :: IO ()
runApp = do

  Warp.run 9000 $ mkApp ()

