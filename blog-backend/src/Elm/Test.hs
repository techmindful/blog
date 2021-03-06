{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Elm.Test
  ( ElmTestResp
  , MkUserFileError(..)
  , runElmTest
  ) where

import           Elm.Files ( MkUserFileError(..), tryMkUserDir )

import           RIO hiding ( Handler )

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
import           RIO.ByteString as ByteStr ( hGetContents, null )
import           RIO.List ( headMaybe )
import           Path
  ( Rel
  , Dir
  , File
  , Path
  , addExtension
  , mkRelDir
  , toFilePath
  , (</>)
  )
import           Path.IO ( removeDirRecur )
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
           -> ( Text -> Either MkUserFileError Text )
           -> IO ElmTestResp
runElmTest root templateModuleName codeMod = do

  -- Create user dir.
  userDirName <- liftIO $
    tryMkUserDir
      root
      templateModuleName
      codeMod

  -- Get the paths.
  userFileFullName <- Path.addExtension ".elm" templateModuleName
  let userDirPath =
        root </> $(Path.mkRelDir "user-creations/")
             </> userDirName

  -- Run elm-test.
  ( _, Just hout, Just herr, _ ) <- liftIO $ createProcess
    ( proc
        "elm-test"
        [ toFilePath $ $(Path.mkRelDir "src/") </> userFileFullName
        , "--report", "json", "--fuzz", "1"
        ]
    )
    { std_out = CreatePipe
    , std_err = CreatePipe
    , Proc.cwd = Just $ toFilePath userDirPath
    }

  -- If compiling succeeds, stdout gives test events in jsons.
  out <- hGetContents hout
  -- If compiling fails, stderr gives compiler errors.
  err <- hGetContents herr

  let elmTestResp :: ElmTestResp
      elmTestResp =
        -- Compile fails
        if not $ ByteStr.null err then
          CompileFailure $ decodeUtf8With lenientDecode err
        -- Compile succeeds.
        else do
          -- Filter out test events unrelated to the result.
          let  decodedJsons :: [ ElmTestJson ]
               decodedJsons = mapMaybe Aeson.decodeStrict $ ByteStrC8.lines out

          case decodedJsons of
            [] ->
              InternalJsonError

            elmTestJsons -> do
              
              let maybeResults = map elmTestJsonToResult elmTestJsons

              if any isNothing maybeResults then
                InternalJsonError
              else
                GotResults $ catMaybes maybeResults

  -- Clean up.
  removeDirRecur userDirPath

  pure elmTestResp

 
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

