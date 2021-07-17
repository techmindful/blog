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


module Blogs.EmojisInElm
  ( API
  , server
  , unicodeToPathHandler
  )
  where

import           Types ( AppM )
import           ElmTest
  ( ElmTestResp
  , MkUserCodeError(..)
  , runElmTest
  , tryMkUserFile
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
import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )
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


type API = "blog-apis" :> "emojis-in-elm" :> "unicode-to-path"
                       :> ReqBody '[PlainText] String :> Put '[Servant.JSON] ElmTestResp


server :: ServerT API AppM
server =

  unicodeToPathHandler


unicodeToPathHandler :: String -> AppM ElmTestResp
unicodeToPathHandler userCode = do

  let elmDirRoot = $(Path.mkRelDir "blog-apis/emojis-in-elm/")
      templatesDirPath = elmDirRoot </> $(Path.mkRelDir "src-templates/")
      templateModuleName = $(Path.mkRelFile "UnicodeToPath")
      usersDirPath = $(Path.mkRelDir "src-users/")

  let codeMod :: Text -> Either MkUserCodeError Text
      codeMod templateCode = do
        withoutEndNewline <- note EmptyTemplate $ initMaybe $ Text.unpack templateCode
        pure $ Text.pack $ withoutEndNewline ++ userCode

  userFileName  <-
    liftIO $ tryMkUserFile
      templatesDirPath
      templateModuleName
      ( elmDirRoot </> usersDirPath )
      codeMod

  elmTestResult <- liftIO $ runElmTest elmDirRoot $ usersDirPath </> userFileName

  liftIO $ putStrLn $ show elmTestResult

  pure elmTestResult

