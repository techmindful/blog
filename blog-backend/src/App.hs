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

import           RIO hiding ( Handler )
import           Prelude ( putStrLn )

import qualified Servant
import           Servant
  ( (:>)
  , (:<|>)(..)
  , Context(..)
  , Capture
  , Get
  , Handler
  , PlainText
  , Proxy(..)
  , Put
  , ReqBody
  , ServerT
  , err401
  , hoistServer
  , hoistServerWithContext
  , serve
  , serveWithContext
  )
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import qualified Web.ClientSession as Session

import           Conduit
import           Control.Arrow ( left )
import           Control.Error ( note )
import           Control.Monad.Except ( ExceptT, liftEither, runExceptT )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Reader ( ReaderT, runReaderT )
import           Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import           Control.Concurrent.STM ( TVar, atomically, newTVar, readTVar, writeTVar )
import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )
import           Data.Aeson ( FromJSON, ToJSON )
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as ByteStrC8
import           Data.Function ( (&) )
import qualified Data.Map as Map
import           Data.Map ( Map )
import           Data.Maybe ( fromMaybe )
import           RIO.ByteString ( readFile, writeFile )
import qualified RIO.Text as Text
import           RIO.Text ( decodeUtf8', encodeUtf8 )
import           RIO.List ( initMaybe )
import           GHC.Float ( rationalToFloat )
import           GHC.Generics ( Generic )
import           Path
  ( (</>)
  , Abs
  , Rel
  , Dir
  , File
  , Path
  , PathException(..)
  , toFilePath
  )
import qualified Path
import           Path.IO ( createDirIfMissing, doesDirExist, doesFileExist )
import           System.Process ( callCommand, createProcess, proc, readProcess )
import qualified System.Process as Proc


type AppState = ()


type API = "blog-apis" :> "emojis-in-elm" :> "unicode-to-path"
                       :> ReqBody '[PlainText] String :> Put '[PlainText] String


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
  | ModdedCodeUtf8Error 
  deriving ( Generic, Show )
instance Exception MkUserCodeError


{-|
  Try to make a user file.

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
              -> ( ByteString -> Either MkUserCodeError ByteString )
              -> IO ( Path Rel File )
tryMkUserFile templateDirPath templateModuleName userDirPath codeMod = do

  withException
    mkUserFile
    ( \ ( e :: MkUserCodeError ) ->
        putStrLn $ "Error: " ++ show e
    )

  where
    mkUserFile = do

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

      let fixModuleLine :: ByteStrC8.ByteString -> Either MkUserCodeError ByteStrC8.ByteString
          fixModuleLine codeByteStr = do

            codeText <- left ( \_ -> ModdedCodeUtf8Error ) ( decodeUtf8' codeByteStr )

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
                  Right $ encodeUtf8 $ Text.unlines $ newModuleLine : rest

      templateCode <- readFile $ templateFilePath & toFilePath

      let resultUserCode :: Either MkUserCodeError ByteString
          resultUserCode = do
            moddedCode <- codeMod templateCode
            fixedCode  <- fixModuleLine moddedCode
            pure fixedCode

      case resultUserCode of
        Left mkUserCodeError -> throwIO $ mkUserCodeError
        Right userCode -> do
          writeFile ( userFilePath & toFilePath ) userCode
          pure userFilePath


server :: ServerT API AppM
server =

  unicodeToPathHandler

  where

    unicodeToPathHandler :: String -> AppM String
    unicodeToPathHandler userCode = do

      let elmDirPath = $(Path.mkRelDir "blog-apis/emojis-in-elm/")

          templatesDirPath   = elmDirPath </> $(Path.mkRelDir "src-templates/")
          templateModuleName = $(Path.mkRelFile "UnicodeToPath")

          usersDirPath = elmDirPath </> $(Path.mkRelDir "src-users/")

      liftIO $ createDirIfMissing False usersDirPath

      let codeMod :: ByteStrC8.ByteString -> Either MkUserCodeError ByteStrC8.ByteString
          codeMod templateCode = do
            -- Trim trailing newline.
            initStr <- note EmptyTemplate $ initMaybe $ ByteStrC8.unpack templateCode
            pure $ ByteStrC8.pack $ initStr ++ userCode

      let testElm :: MonadIO m => Path Rel File -> MaybeT m ()
          testElm path = liftIO $ do

            -- Path after setting directory
            path' <- Path.stripProperPrefix elmDirPath path

            liftIO $ createProcess
              ( proc "elm-test" [ toFilePath path' ] )
              { Proc.cwd = Just $ toFilePath elmDirPath }

            return ()

      userFilePath <- liftIO $ tryMkUserFile templatesDirPath templateModuleName usersDirPath codeMod
      runMaybeT $ testElm userFilePath

      return "Inserted user code"


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Wai.Application
mkApp appState =
  serve api $ hoistServer api ( \x -> runReaderT x appState ) server


runApp :: IO ()
runApp = do

  sessionEncryptionKey <- Session.getDefaultKey

  Warp.run 9000 $ mkApp ()

