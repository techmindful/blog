{-# LANGUAGE DataKinds #-}            
{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}


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
  , (:<|>)(..)
  , Get
  , JSON
  , PlainText
  , Put
  , ReqBody
  , ServerT
  )

import           Control.Error ( note )
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( readFile )
import           RIO.List ( initMaybe )
import qualified RIO.Text as Text
import           Path
  ( (</>)
  , Rel
  , Dir
  , File
  )
import qualified Path
import           System.Process as Proc
  ( CreateProcess(..)
  , StdStream(..)
  , createProcess
  , proc
  , std_err
  , std_out
  )


type API = "unicode-to-path" :> ReqBody '[PlainText] String :> Put '[Servant.JSON] ElmTestResp
      :<|> "render" :> Get '[PlainText] Text


server :: ServerT API AppM
server =

  unicodeToPathHandler :<|> renderHandler


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


renderHandler :: AppM Text
renderHandler = liftIO $ do

  ( _, Just _, Just _, _ ) <- createProcess
    ( proc "elm" [ "make", "src-templates/Render.elm", "--optimize", "--output=render.html" ] )
    { std_out = CreatePipe
    , std_err = CreatePipe
    , Proc.cwd = Just "blog-apis/emojis-in-elm/"
    }

  html <- readFile "blog-apis/emojis-in-elm/render.html"

  pure $ decodeUtf8With lenientDecode html

