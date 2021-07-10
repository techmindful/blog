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

import           Servant
  ( (:>)
  , Handler
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
import           Data.Aeson as Aeson ( FromJSON(..), decodeStrict, withObject, (.:) )
import qualified Data.ByteString.Char8 as ByteStrC8
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( hGetContents, null, readFile, writeFile )
import           RIO.List ( headMaybe, initMaybe )
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
import           System.Process ( CreateProcess(..), StdStream( CreatePipe ), createProcess, proc )
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
  | TemplateUtf8Error 
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
          pure userFilePath


data ElmTestResult
  = CompileFailure ByteString
  | TestFailure ElmTestExpected ElmTestActual
  | Pass
  | JsonError
  deriving ( Generic, Show )

newtype ElmTestExpected = ElmTestExpected
  { getElmTestExpected :: Text }
  deriving ( Generic, Show )
newtype ElmTestActual = ElmTestActual
  { getElmTestActual   :: Text }
  deriving ( Generic, Show )


data ElmTestJson = ElmTestJson
  { event  :: Text
  , status :: Text
  , failures :: [ ElmTestJson_Failure ]
  } deriving ( Generic, Show )
instance FromJSON ElmTestJson

data ElmTestJson_Failure = ElmTestJson_Failure
  { reason :: ElmTestJson_Failure_Reason
  } deriving ( Generic, Show )
instance FromJSON ElmTestJson_Failure

data ElmTestJson_Failure_Reason = ElmTestJson_Failure_Reason
  { data_ :: ElmTestJson_Failure_Reason_Data
  } deriving ( Generic, Show )
-- Hand-writing instance, because "data" field name collides with reserved keyword.
instance FromJSON ElmTestJson_Failure_Reason where
  parseJSON =
    withObject "data" $ \v ->
      ElmTestJson_Failure_Reason <$> v .: "data"

data ElmTestJson_Failure_Reason_Data = ElmTestJson_Failure_Reason_Data
  { expected :: Text
  , actual   :: Text
  } deriving ( Generic, Show )
instance FromJSON ElmTestJson_Failure_Reason_Data

--{"event":"testCompleted","status":"fail","labels":["UnicodeToPath_fb05414c9993917f91a9c9b25d012aa0f0293e51eb33567c19ca51eceb4a07f4","Emoji unicode matches the image file path."],"failures":[{"given":null,"message":"Expect.equal","reason":{"type":"Equality","data":{"expected":"\"/static/noto-emoji/32/emoji_u1f600.png\"","actual":"\"test\"","comparison":"Expect.equal"}}}],"duration":"1"}

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

      let codeMod :: Text -> Either MkUserCodeError Text
          codeMod templateCode = do
            withoutEndNewline <- note EmptyTemplate $ initMaybe $ Text.unpack templateCode
            pure $ Text.pack $ withoutEndNewline ++ userCode

      let testElm :: Path Rel File -> IO ElmTestResult
          testElm path = do

            -- Path after setting directory
            path' <- Path.stripProperPrefix elmDirPath path

            ( _, Just hout, Just herr, _ ) <- liftIO $ createProcess
              ( proc "elm-test" [ toFilePath path', "--report", "json" ] )
              { std_out = CreatePipe
              , std_err = CreatePipe
              , Proc.cwd = Just $ toFilePath elmDirPath
              }

            -- If compiling succeeds, stdout gives test events in jsons.
            out <- hGetContents hout
            -- If compiling fails, stderr gives compiler errors.
            err <- hGetContents herr

            -- Compile fails
            if not $ ByteStr.null err then
              pure $ CompileFailure err
            -- Compile succeeds.
            else do
              -- Filter out test events unrelated to the result.
              let  decodedJsons :: [ ElmTestJson ]
                   decodedJsons = mapMaybe Aeson.decodeStrict $ ByteStrC8.lines out

              case decodedJsons of
                -- Single result:
                elmTestJson : [] ->

                  -- Test fails.
                  if ( elmTestJson & status ) == "fail" then do

                    let maybeResult :: Maybe ( ElmTestExpected, ElmTestActual )
                        maybeResult = do

                          failure <- headMaybe $ elmTestJson & failures

                          -- TODO: Use lenses.
                          let data__ = failure & reason & data_

                          pure
                            ( ElmTestExpected $ data__ & expected
                            , ElmTestActual   $ data__ & actual
                            )

                    case maybeResult of
                      Nothing -> pure JsonError
                      Just ( expected_, actual_ ) ->
                        pure $ TestFailure expected_ actual_

                  -- Test passes.
                  else
                    pure Pass
                          
                [] ->
                  pure JsonError

                _ : _ -> do
                  putStrLn "Multiple test runs?"
                  pure JsonError


      userFilePath <- liftIO $ tryMkUserFile templatesDirPath templateModuleName usersDirPath codeMod

      elmTestResult <- liftIO $ testElm userFilePath

      liftIO $ putStrLn $ show elmTestResult

      return "Inserted user code"


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Wai.Application
mkApp appState =
  serve api $ hoistServer api ( \x -> runReaderT x appState ) server


runApp :: IO ()
runApp = do

  Warp.run 9000 $ mkApp ()

