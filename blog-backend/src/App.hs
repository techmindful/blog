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

import           ElmTest
  ( ElmTestResp
  --, ElmTestResult
  , resultToResp
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


type AppState = ()


type API = "blog-apis" :> "emojis-in-elm" :> "unicode-to-path"
                       :> ReqBody '[PlainText] String :> Put '[Servant.JSON] ElmTestResp


type AppM = ReaderT AppState Handler


getRandomHash :: IO Text
getRandomHash = do
  seed <- seedNew
  let seedStr = show $ seedToInteger seed
      hashStr = show $ hashWith SHA256 $ ByteStrC8.pack seedStr
  pure $ Text.pack hashStr


data MkUserCodeError
  = EmptyTemplate
  | EmptyModdedCode
  | TemplateUtf8Error 
  deriving ( Generic, Show )
instance Exception MkUserCodeError


{-|
  Try to make a user file. Returns the file name, not path.

  Caller is responsible for providing a valid `Path Rel File` as `templateModuleName`,
  and let the function add an extension that's guaranteed to be valid, ".elm",
  instead of letting function try to split a correct extension out.

  Excluding unlikely ones, these exceptions may be thrown:
  * MkUserCodeError
  * File read/write errors.
-}
tryMkUserFile :: Path Rel Dir
              -> Path Rel File
              -> Path Rel Dir
              -> ( Text -> Either MkUserCodeError Text )
              -> IO ( Path Rel File )
tryMkUserFile templateDirPath templateModuleName userDirPath codeMod = do

  withException
    mkUserFile
    ( \ ( e :: MkUserCodeError ) -> do
        putStrLn $ "[ Exception ] "
                ++ show e
                ++ " occurred on tryMkUserFile."

        putStrLn $ "templateDirPath: " ++ toFilePath templateDirPath
        putStrLn $ "templateModuleName: " ++ toFilePath templateModuleName
        putStrLn $ "userDirPath: " ++ toFilePath userDirPath
        putStrLn $ "With some codeMod."
    )

  where
    mkUserFile = do

      liftIO $ createDirIfMissing False userDirPath

      let ext = ".elm"

      let templateModuleNameText = Text.pack $ templateModuleName & toFilePath

      -- Make template file info.
      --   We'll ignore the possibility of InvalidExtension exception here,
      --   Since we know ".elm" is a valid extension.
      ( templateFileName :: Path Rel File ) <- Path.addExtension ext templateModuleName
      let templateFilePath = templateDirPath </> templateFileName

      -- Make user file info.
      randomHash <- getRandomHash
      let userModuleNameText =
            Text.concat [ templateModuleNameText, "_", randomHash ]
      -- Ignoring InvalidRelFile exception, since appending hash shouldn't cause path exceptions.
      ( userModuleName :: Path Rel File ) <- Path.parseRelFile $ Text.unpack userModuleNameText 
      -- Ignoring InvalidExtension exception just like above with the template.
      userFileName <- Path.addExtension ext userModuleName
      let userFilePath = userDirPath </> userFileName

      let fixModuleLine :: Text -> Either MkUserCodeError Text
          fixModuleLine codeText = do 

            case Text.lines codeText of
              [] -> Left EmptyModdedCode

              ( moduleLine : rest ) ->
                  let
                    newModuleLine =
                      Text.unwords $ map
                        ( \word ->
                            if word == templateModuleNameText then userModuleNameText
                            else word
                        )
                        ( Text.words moduleLine )
                  in
                  Right $ Text.unlines $ newModuleLine : rest


      templateCodeByteStr <- readFile $ templateFilePath & toFilePath

      let resultUserCode :: Either MkUserCodeError Text
          resultUserCode = do

            templateCode <-
              left ( \_ -> TemplateUtf8Error )
                   ( decodeUtf8' templateCodeByteStr )

            moddedCode <- codeMod templateCode
            fixedCode  <- fixModuleLine moddedCode
            pure fixedCode

      case resultUserCode of
        Left mkUserCodeError -> throwIO $ mkUserCodeError
        Right userCode -> do
          writeFile ( userFilePath & toFilePath ) ( encodeUtf8 userCode )
          pure userFileName


server :: ServerT API AppM
server =

  unicodeToPathHandler

  where

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

      pure $ ElmTest.resultToResp elmTestResult


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Wai.Application
mkApp appState =
  serve api $ hoistServer api ( \x -> runReaderT x appState ) server


runApp :: IO ()
runApp = do

  Warp.run 9000 $ mkApp ()

