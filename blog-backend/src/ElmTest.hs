{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ElmTest
  ( ElmTestResult
  , runElmTest
  ) where

import           RIO
import           Prelude ( putStrLn )

import           Data.Aeson as Aeson
  ( FromJSON(..)
  , Value( Null )
  , ToJSON(..)
  , (.:)
  , (.=)
  , decodeStrict
  , object
  , withObject
  )
import qualified Data.ByteString.Char8 as ByteStrC8
import           RIO.ByteString as ByteStr ( hGetContents, null ) 
import           RIO.List ( headMaybe )
import           Path
  ( Rel
  , Dir
  , File
  , Path
  , toFilePath
  )
import           System.Process ( CreateProcess(..), StdStream( CreatePipe ), createProcess, proc )
import qualified System.Process as Proc


data ElmTestResult
  = CompileFailure Text
  | TestFailure ElmTestExpected ElmTestActual
  | Pass
  | InternalJsonError
  deriving ( Generic, Show )
instance ToJSON ElmTestResult where
  toJSON result =
    case result of
      CompileFailure compilerMsg ->
        object [ "compilerError" .= compilerMsg ]

      TestFailure _ actual_ ->
        object [ "pass" .= False, "actual" .= ( actual_ & getElmTestActual ) ]

      Pass ->
        object [ "pass" .= True ]

      InternalJsonError ->
        Null


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
           -> IO ElmTestResult
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
            Nothing -> pure InternalJsonError
            Just ( expected_, actual_ ) ->
              pure $ TestFailure expected_ actual_

        -- Test passes.
        else
          pure Pass
                
      [] ->
        pure InternalJsonError

      _ : _ -> do
        putStrLn "Multiple test runs?"
        pure InternalJsonError
