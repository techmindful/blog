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
import qualified Elm.Make
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
  , JSON
  , PlainText
  , Put
  , ReqBody
  , ServerT
  )

import           Control.Error ( note )
import           Data.Aeson ( FromJSON )
import           Data.Text ( breakOn )
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( hGetContents, readFile, null )
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
  , waitForProcess
  )


type API = "unicode-to-path" :> ReqBody '[PlainText] String :> Put '[Servant.JSON] ElmTestResp
      :<|> "render" :> ReqBody '[Servant.JSON] RenderUserCode :> Put '[Servant.JSON] Elm.Make.Result


server :: ServerT API AppM
server =

  unicodeToPathHandler :<|> renderHandler


elmRoot :: Path Rel Dir
elmRoot = $(Path.mkRelDir "blog-apis/emojis-in-elm/")


templatesDirPath :: Path Rel Dir
templatesDirPath = elmRoot </> $(Path.mkRelDir "src-templates/")


elmRooted_UsersDirPath :: Path Rel Dir
elmRooted_UsersDirPath = $(Path.mkRelDir "src-users/")


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
      ( elmRoot </> elmRooted_UsersDirPath )
      codeMod

  elmTestResult <- liftIO $ runElmTest elmRoot $ elmRooted_UsersDirPath </> userFileName

  liftIO $ putStrLn $ show elmTestResult

  pure elmTestResult


data RenderUserCode = RenderUserCode
  { noColonCase :: Text 
  } deriving ( Generic, Show )
instance FromJSON RenderUserCode


renderHandler :: RenderUserCode -> AppM Elm.Make.Result
renderHandler userCode = liftIO $ do

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
                Text.append leadingSpaces ( userCode & noColonCase )
                

        pure $ Text.unlines $
          map modifyNoColonCase $ Text.lines templateCode
            

  userElmFileName <-
    tryMkUserFile templatesDirPath templateModuleName ( elmRoot </> elmRooted_UsersDirPath ) codeMod

  userHtmlFileName <-  -- Not catching InvalidExtension since ".html" should be valid.
    Path.addExtension ".html" userElmFileName

  let elmRooted_UserHtmlFilePath = elmRooted_UsersDirPath </> userHtmlFileName

  ( _, _, Just herr, elmMakeProcHandle ) <- createProcess
    ( proc "elm"
        [ "make"
        , toFilePath $ elmRooted_UsersDirPath </> userElmFileName
        , "--optimize"
        , "--output=" ++ ( elmRooted_UserHtmlFilePath & toFilePath )
        ]
    )
    { std_err = CreatePipe
    , Proc.cwd = Just $ elmRoot & toFilePath
    }

  err <- hGetContents herr

  _ <- waitForProcess elmMakeProcHandle

  if not $ ByteStr.null err then
    pure $ Elm.Make.CompilerError $ decodeUtf8With lenientDecode err
  else do
    html <- readFile $ toFilePath $ elmRoot </> elmRooted_UserHtmlFilePath
    pure $ Elm.Make.Html $ decodeUtf8With lenientDecode html

