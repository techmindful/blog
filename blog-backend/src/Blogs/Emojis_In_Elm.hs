{-# LANGUAGE DataKinds #-}            
{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}


module Blogs.Emojis_In_Elm
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
  { noColonCase  :: Text 
  , notEmojiCase :: Text
  , isEmojiCase  :: Text
  } deriving ( Generic, Show )
instance FromJSON RenderUserCode


renderHandler :: RenderUserCode -> AppM Elm.Make.Result
renderHandler userCode = liftIO $ do

  let templateModuleName = $(Path.mkRelFile "Render")

  let codeMod :: Text -> Either MkUserFileError Text
      codeMod templateCode = do

        let modifyTemplateLine :: Text -> Text -> Text -> Text
            modifyTemplateLine templateCode' userCode' line =
              -- Find the target template line.
              --    strip is better than isInfixOf
              --    It makes sure the line is only that text.
              if not $ Text.strip line == templateCode' then
                line
              else
                -- Preserve the original leading spaces.
                let ( leadingSpaces, _ ) = breakOn templateCode' line
                in
                -- User code may be multiple lines. Break it into lines,
                -- and insert leading spaces at front for each.
                if Text.any ( == '\n' ) userCode' then
                  userCode' & Text.lines
                            & map ( \userLine -> leadingSpaces <> userLine )
                            & Text.unlines
                -- If user code is single line:
                else
                  leadingSpaces <> userCode' 

        templateCode & Text.lines
                     & map ( modifyTemplateLine
                               "-- Insert noColonCase here."
                               ( userCode & noColonCase )
                           )
                     & map ( modifyTemplateLine
                               "-- Insert notEmojiCase here."
                               ( ":: " <> ( userCode & notEmojiCase ) )
                           )
                     & map ( modifyTemplateLine
                               "-- Insert isEmojiCase here."
                               ( userCode & isEmojiCase )
                           )
                     & Text.unlines
                     & pure


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

