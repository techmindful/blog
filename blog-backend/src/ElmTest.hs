{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElmTest
  ( ElmTestResp
  , MkUserFileError(..)
  , runElmTest
  , tryMkUserFile
  ) where

import           Utils.Random ( getRandomHash )

import           RIO hiding ( Handler )
import           Prelude ( putStrLn )

import           Control.Arrow ( left )
import           Data.Aeson as Aeson
  ( FromJSON(..)
  , Value(..)
  , ToJSON(..)
  , (.=)
  , (.:)
  , decodeStrict
  , object
  , withObject
  )
import qualified Data.ByteString.Char8 as ByteStrC8
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( hGetContents, null, readFile, writeFile )
import           RIO.List ( headMaybe )
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
import           System.Process as Proc
  ( CreateProcess(..)
  , StdStream(..)
  , createProcess
  , proc
  , std_err
  , std_out
  )


data ElmTestResp
  = CompileFailure Text
  | GotResults [ ElmTestResult ]
  | InternalJsonError
  deriving ( Generic, Show )
instance ToJSON ElmTestResp where
  toJSON resp =
    case resp of
      CompileFailure compilerMsg ->
        object [ "compilerError" .= compilerMsg ]

      GotResults results ->
        toJSONList results

      InternalJsonError ->
        Null


data ElmTestResult
  = Fail ElmTestExpected ElmTestActual
  | Pass
  deriving ( Generic, Show )
instance ToJSON ElmTestResult where
  toJSON result =
    case result of
      Fail expected_ actual_ ->
        object [ "pass" .= False
               , "expected" .= ( expected_ & getElmTestExpected )
               , "actual"   .= ( actual_   & getElmTestActual   )
               ]

      Pass ->
        object [ "pass" .= True ]


newtype ElmTestExpected = ElmTestExpected
  { getElmTestExpected :: Text }
  deriving ( Generic, Show )


newtype ElmTestActual = ElmTestActual
  { getElmTestActual :: Text }
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


-- | Run elm-test under `elmDirRoot`,
--   on elm file at `elmFilePath`.
runElmTest :: Path Rel Dir
           -> Path Rel File
           -> IO ElmTestResp
runElmTest elmDirRoot elmFilePath = do

  ( _, Just hout, Just herr, _ ) <- liftIO $ createProcess
    ( proc "elm-test" [ elmFilePath & toFilePath, "--report", "json", "--fuzz", "1" ] )
    { std_out = CreatePipe
    , std_err = CreatePipe
    , Proc.cwd = Just $ toFilePath elmDirRoot
    }

  -- If compiling succeeds, stdout gives test events in jsons.
  out <- hGetContents hout
  -- If compiling fails, stderr gives compiler errors.
  err <- hGetContents herr

  -- Compile fails
  if not $ ByteStr.null err then
    pure $ CompileFailure $ decodeUtf8With lenientDecode err
  -- Compile succeeds.
  else do
    -- Filter out test events unrelated to the result.
    let  decodedJsons :: [ ElmTestJson ]
         decodedJsons = mapMaybe Aeson.decodeStrict $ ByteStrC8.lines out

    case decodedJsons of
      [] ->
        pure InternalJsonError

      elmTestJsons -> do
        
        let maybeResults = map elmTestJsonToResult elmTestJsons

        if any isNothing maybeResults then
          pure InternalJsonError
        else
          pure $ GotResults $ catMaybes maybeResults

 
elmTestJsonToResult :: ElmTestJson -> Maybe ElmTestResult
elmTestJsonToResult elmTestJson =
  
  if ( elmTestJson & status ) == "fail" then do

    failure <- headMaybe $ elmTestJson & failures

    -- TODO: Use lenses.
    let data__ = failure & reason & data_

    pure $ Fail ( ElmTestExpected $ data__ & expected )
                ( ElmTestActual   $ data__ & actual   )

  else
    pure Pass


data MkUserFileError
  = EmptyTemplate
  | EmptyModdedCode
  | TemplateUtf8Error 
  deriving ( Generic, Show )
instance Exception MkUserFileError


{-|
  Try to make a user file. Returns the file name, not path.

  Caller is responsible for providing a valid `Path Rel File` as `templateModuleName`,
  and let the function add an extension that's guaranteed to be valid, ".elm",
  instead of letting function try to split a correct extension out.

  Excluding unlikely ones, these exceptions may be thrown:
  * MkUserFileError
  * File read/write errors.
-}
tryMkUserFile :: Path Rel Dir
              -> Path Rel File
              -> Path Rel Dir
              -> ( Text -> Either MkUserFileError Text )
              -> IO ( Path Rel File )
tryMkUserFile templateDirPath templateModuleName userDirPath codeMod = do

  withException
    mkUserFile
    ( \ ( e :: MkUserFileError ) -> do
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

      let fixModuleLine :: Text -> Either MkUserFileError Text
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

      let resultUserCode :: Either MkUserFileError Text
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
