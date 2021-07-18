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
import           Elm.Files ( tryMkUserFile )
import           Elm.Test
  ( ElmTestResp
  , MkUserFileError(..)
  , runElmTest
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
import           Data.Text ( breakOn )
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( readFile )
import           RIO.List ( initMaybe )
import qualified RIO.Text as Text
import           Path
  ( (</>)
  , Dir
  , File
  , Path
  , Rel
  , toFilePath
  )
import qualified Path
import           System.Process as Proc
  ( CreateProcess(..)
  , StdStream(..)
  , createProcess
  , proc
  , std_err
  , std_out
  , waitForProcess
  )


type API = "unicode-to-path" :> ReqBody '[PlainText] String :> Put '[Servant.JSON] ElmTestResp
      :<|> "render" :> Get '[PlainText] Text


server :: ServerT API AppM
server =

  unicodeToPathHandler :<|> renderHandler


elmRoot :: Path Rel Dir
elmRoot = $(Path.mkRelDir "blog-apis/emojis-in-elm/")


templatesDirPath :: Path Rel Dir
templatesDirPath = elmRoot </> $(Path.mkRelDir "src-templates/")


elmUsersDirPath :: Path Rel Dir
elmUsersDirPath = $(Path.mkRelDir "src-users/")


unicodeToPathHandler :: String -> AppM ElmTestResp
unicodeToPathHandler userCode = do

  let templateModuleName = $(Path.mkRelFile "UnicodeToPath")

  let codeMod :: Text -> Either MkUserFileError Text
      codeMod templateCode = do
        withoutEndNewline <- note EmptyTemplate $ initMaybe $ Text.unpack templateCode
        pure $ Text.pack $ withoutEndNewline ++ userCode

  userFileName  <-
    liftIO $ tryMkUserFile
      templatesDirPath
      templateModuleName
      ( elmRoot </> elmUsersDirPath )
      codeMod

  elmTestResult <- liftIO $ runElmTest elmRoot $ elmUsersDirPath </> userFileName

  liftIO $ putStrLn $ show elmTestResult

  pure elmTestResult


renderHandler :: AppM Text
renderHandler = liftIO $ do

  let templateModuleName = $(Path.mkRelFile "Render")

  let codeMod :: Text -> Either MkUserFileError Text
      codeMod templateCode = do

        let modifyNoColonCase :: Text -> Text
            modifyNoColonCase txt =
              let targetCode = "[ Text str ]"
              in
              -- strip is better than isInfixOf.
              -- It makes sure the line is only "[ Text str ]"
              if not $ Text.strip txt == targetCode then
                txt
              else
                let ( leadingSpaces, _ ) = breakOn targetCode txt
                in
                Text.append leadingSpaces "[ Text \"test\" ]"
                

        pure $ Text.unlines $
          map modifyNoColonCase $ Text.lines templateCode
            

  userFileName <-
    tryMkUserFile templatesDirPath templateModuleName ( elmRoot </> elmUsersDirPath ) codeMod

  let elmUserFilePath = toFilePath $ elmUsersDirPath </> userFileName

  ( _, Just _, Just _, elmMakeProcHandle ) <- createProcess
    ( proc "elm" [ "make", elmUserFilePath, "--optimize", "--output=render.html" ] )
    { std_out = CreatePipe
    , std_err = CreatePipe
    , Proc.cwd = Just $ elmRoot & toFilePath
    }

  _ <- waitForProcess elmMakeProcHandle

  html <- readFile $ toFilePath $ elmRoot </> $(Path.mkRelFile "render.html")

  pure $ decodeUtf8With lenientDecode html

