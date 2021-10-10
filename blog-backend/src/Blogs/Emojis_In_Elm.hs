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
import           Elm.Files ( tryMkUserDir )
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
  , err400
  )
import qualified Servant

import           Control.Error ( note )
import           Data.Aeson ( FromJSON )
import           Data.Text ( breakOn, unsnoc )
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( hGetContents, readFile, null )
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


type API = "unicode-to-path" :> ReqBody '[PlainText] Text :> Put '[Servant.JSON] ElmTestResp
      :<|> "render" :> ReqBody '[Servant.JSON] RenderUserCode :> Put '[Servant.JSON] Elm.Make.Result


server :: ServerT API AppM
server =

  unicodeToPathHandler :<|> renderHandler


blogRoot :: Path Rel Dir
blogRoot = $(Path.mkRelDir "blog-apis/emojis-in-elm/")


unicodeToPathHandler :: Text -> AppM ElmTestResp
unicodeToPathHandler userCode = do

  if Text.length userCode >= 100 then
    Servant.throwError err400

  else do

    let root = blogRoot </> $(Path.mkRelDir "unicode-to-path")
    let templateModuleName = $(Path.mkRelFile "UnicodeToPath")

    let codeMod :: Text -> Either MkUserFileError Text
        codeMod templateCode = do
          ( withoutEndNewline, _ ) <- note EmptyTemplate $ unsnoc templateCode
          pure $ withoutEndNewline <> userCode

    elmTestResult <- liftIO $ runElmTest root templateModuleName codeMod

    liftIO $ putStrLn $ show elmTestResult

    pure elmTestResult


data RenderUserCode = RenderUserCode
  { noColonCase  :: Text 
  , notEmojiCase :: Text
  , isEmojiCase  :: Text
  } deriving ( Generic, Show )
instance FromJSON RenderUserCode


renderHandler :: RenderUserCode -> AppM Elm.Make.Result
renderHandler userCode = do

  if   Text.length ( userCode & noColonCase  ) >= 40
    || Text.length ( userCode & notEmojiCase ) >= 100
    || Text.length ( userCode & isEmojiCase  ) >= 120
  then
    Servant.throwError err400

  else liftIO $ do

    let root = blogRoot </> $(Path.mkRelDir "render/")
    let templateModuleName = $(Path.mkRelFile "Render")
    let userCreationsPath = root </> $(Path.mkRelDir "user-creations/")

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
                  userCode' & Text.lines
                            & map ( \userLine -> leadingSpaces <> userLine )
                            & Text.unlines
                            -- If `unlines` added a '\n' at end, where there was none
                            -- Remove it.
                            & ( \txt ->
                                  if Text.takeEnd 1 userCode' == "\n" then
                                    txt
                                  else
                                    Text.dropSuffix "\n" txt
                              )

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


    userDirName <-
      tryMkUserDir
        root
        templateModuleName
        codeMod

    let userDirPath = userCreationsPath </> userDirName

    userElmPath_AfterRoot <-
      Path.addExtension ".elm" $
          $(Path.mkRelDir "src/") </> templateModuleName
    
    ( _, _, Just herr, elmMakeProcHandle ) <- createProcess
      ( proc "elm"
          [ "make"
          , toFilePath userElmPath_AfterRoot
          , "--optimize"
          , "--output=index.html"
          ]
      )
      { std_err = CreatePipe
      , Proc.cwd = Just $ toFilePath userDirPath
      }

    err <- hGetContents herr

    _ <- waitForProcess elmMakeProcHandle

    if not $ ByteStr.null err then
      pure $ Elm.Make.CompilerError $ decodeUtf8With lenientDecode err
    else do
      let userHtmlPath = userDirPath </> $(Path.mkRelFile "index.html")
      html <- readFile $ toFilePath userHtmlPath
      pure $ Elm.Make.Html $ decodeUtf8With lenientDecode html

