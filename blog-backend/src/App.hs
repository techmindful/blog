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
import           GHC.Float ( rationalToFloat )
import           GHC.Generics ( Generic )
import           Path ( (</>), Abs, Rel, Dir, File, Path )
import qualified Path
import           Path.IO ( createDirIfMissing, doesDirExist, doesFileExist )
import           System.IO ( appendFile, readFile )


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


server :: ServerT API AppM
server =

  unicodeToPathHandler

  where

    unicodeToPathHandler :: String -> AppM String
    unicodeToPathHandler userCode = do

      let elmDirPath = $(Path.mkRelDir "blog-apis/emojis-in-elm/")

          templatesDirPath = elmDirPath </> $(Path.mkRelDir "src-templates/")
          templateFilePath = templatesDirPath </> $(Path.mkRelFile "UnicodeToPath.elm")

          usersDirPath = elmDirPath </> $(Path.mkRelDir "src-users/")

      liftIO $ createDirIfMissing False usersDirPath

      maybeUserFilePath <-
        liftIO $ runMaybeT $ mkUniqueFilePath
          usersDirPath ( Just "UnicodeToPath_" ) ( Just ".elm" )

      let mkUserFile :: MonadIO m => Path Rel File -> m ()
          mkUserFile path =
            liftIO
              $ runConduitRes
              $ sourceFile ( templateFilePath & Path.toFilePath )
             .| mapC ( \str -> ByteStrC8.pack $ ByteStrC8.unpack str ++ userCode )
             .| sinkFile ( path & Path.toFilePath )

      case maybeUserFilePath of
        Nothing ->
          return "Error"

        Just path -> do
          mkUserFile path
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

