{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm.Files
  ( MkUserFileError(..)
  , tryMkUserFile
  ) where


import           Utils.Random ( getRandomHash )

import           RIO hiding ( Handler )
import           Prelude ( putStrLn )

import           Control.Arrow ( left )
--import           Optics ( (^.) )
import           RIO.ByteString as ByteStr ( readFile, writeFile )
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
