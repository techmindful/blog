{-# LANGUAGE DataKinds #-}            
{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}    
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}         
{-# LANGUAGE TypeOperators #-}         
{-# LANGUAGE UndecidableInstances #-} 


module App where

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
import           Control.Exception ( Exception, throw, throwIO, try, catch )
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
import qualified Data.Text as Text
import           Data.Text.Encoding ( decodeUtf8, decodeUtf8', encodeUtf8 )
import           Data.Text.Encoding.Error ( UnicodeException )
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
import           System.IO ( appendFile, readFile )
import           System.Process ( callCommand, createProcess, proc, readProcess )
import qualified System.Process as Proc


type AppState = ()


type API = "blog-apis" :> "emojis-in-elm" :> "unicode-to-path"
                       :> ReqBody '[PlainText] String :> Put '[PlainText] String


type AppM = ReaderT AppState Handler


getRandomHash :: IO String
getRandomHash = do
  seed <- seedNew
  let seedStr = show $ seedToInteger seed
      hash    = show $ hashWith SHA256 $ ByteStrC8.pack seedStr
  return hash


mkUniqueFilePath :: Path Rel Dir
                 -> Maybe String
                 -> Maybe String
                 -> MaybeT IO ( Path Rel File )
mkUniqueFilePath dirPath maybePrefix maybePostfix = do

  randomHash <- liftIO getRandomHash

  let fileNameStr = fromMaybe "" maybePrefix ++ randomHash ++ fromMaybe "" maybePostfix

  fileName <- Path.parseRelFile fileNameStr

  let filePath = dirPath </> fileName

  isCollided <- liftIO $ doesFileExist filePath
  if isCollided then
    mkUniqueFilePath dirPath maybePrefix maybePostfix
  else
    return filePath


data MkUserFileErr
  = UnicodeException_ UnicodeException
  | WrongPrefix
  | EmptyTemplate
  deriving ( Show )
instance Exception MkUserFileErr


mkUserFile :: Path Rel File
           -> Path Rel File
           -> ( ByteStrC8.ByteString -> ByteStrC8.ByteString )
           -> IO ()
mkUserFile templatePath savePath codeMod = do

  let templateFileName = templatePath & Path.filename

  ( templateModuleName_Path, fileExt ) <-
      catch
        ( templateFileName & Path.splitExtension )
        ( \ ( e :: PathException ) -> throwIO WrongPrefix )

  let templateModuleName = templateModuleName_Path & toFilePath

  ( userModuleName_Path, _ ) <-
      catch
        ( savePath & Path.filename & Path.splitExtension )
        ( \ ( e :: PathException ) -> throwIO WrongPrefix )

  let userModuleName = userModuleName_Path & toFilePath

  let fixModuleLine :: ByteStrC8.ByteString -> ByteStrC8.ByteString
      fixModuleLine codeByteStr =

        let
            codeText = decodeUtf8 codeByteStr  --
        in
        case Text.lines codeText of
          [] ->
            throw EmptyTemplate

          ( moduleLine : rest ) ->
            let
              newModuleLine =
                Text.replace  
                  ( Text.pack templateModuleName )
                  ( Text.pack userModuleName )
                  moduleLine
            in
            encodeUtf8 $ Text.unlines $ newModuleLine : rest

  liftIO
      $ runConduitRes
      $ sourceFile ( templatePath & toFilePath )
     .| mapC codeMod
     .| mapC fixModuleLine
     .| sinkFile ( savePath & toFilePath )


server :: ServerT API AppM
server =

  unicodeToPathHandler

  where

    unicodeToPathHandler :: String -> AppM String
    unicodeToPathHandler userCode = do

      let elmDirPath = $(Path.mkRelDir "blog-apis/emojis-in-elm/")

          templatesDirPath = elmDirPath </> $(Path.mkRelDir "src-templates/")
          templateFilePath = templatesDirPath </> $(Path.mkRelFile "WrongPrefix")

          usersDirPath = elmDirPath </> $(Path.mkRelDir "src-users/")

      liftIO $ createDirIfMissing False usersDirPath

      maybeUserFilePath <-
        liftIO $ runMaybeT $ mkUniqueFilePath
          usersDirPath ( Just "UnicodeToPath_" ) ( Just ".elm" )
 
      let testElm :: MonadIO m => Path Rel File -> MaybeT m ()
          testElm path = liftIO $ do

            -- Path after setting directory
            path' <- Path.stripProperPrefix elmDirPath path

            liftIO $ createProcess
              ( proc "elm-test" [ toFilePath path' ] )
              { Proc.cwd = Just $ toFilePath elmDirPath }

            return ()

      case maybeUserFilePath of
        Nothing ->
          return "Error"

        Just userFilePath -> do
          liftIO $
            mkUserFile templateFilePath userFilePath
              ( \byteStr ->
                  ByteStrC8.pack $ ( init $ ByteStrC8.unpack byteStr ) ++ userCode
              )
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
  putStrLn $ show sessionEncryptionKey

  Warp.run 9000 $ mkApp ()

