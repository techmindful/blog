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
  , JSON
  , PlainText
  , Put
  , ReqBody
  , ServerT
  )

import           Control.Error ( note )
--import           Optics ( (^.) )
import           RIO.List ( initMaybe )
import qualified RIO.Text as Text
import           Path
  ( (</>)
  , Rel
  , Dir
  , File
  )
import qualified Path


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

