{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Elm.Files
  ( MkUserFileError(..)
  , tryMkUserDir
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
import           Path.IO ( copyDirRecur ) 


data MkUserFileError
  = EmptyTemplate
  | EmptyModdedCode
  | TemplateUtf8Error 
  deriving ( Generic, Show )
instance Exception MkUserFileError


{-|
  Try to make a user creation dir.

  This function assumes the template dir is "template/",
  and user dir is "user-creations/".

  @root is the parent dir of "template/" and "user-creations/".

  @templateModuleName is the target template module that user is modifying.

  @return the dir name, which is effectively the random hash with type `Path Rel Dir`, after root.
  This is because if caller wants to full path with @root,
  they already have it as the param and can concat the two.

  Caller is responsible for providing a valid `Path Rel File` as `templateModuleName`,
  and let the function add an extension that's guaranteed to be valid, ".elm",
  instead of letting function try to split a correct extension out.

  Excluding unlikely ones, these exceptions may be thrown:
  * MkUserFileError
  * File read/write errors.
-}
tryMkUserDir :: Path Rel Dir
              -> Path Rel File
              -> ( Text -> Either MkUserFileError Text )
              -> IO ( Path Rel Dir )
tryMkUserDir root templateModuleName codeMod = do

  withException
    mkUserDir
    ( \ ( e :: MkUserFileError ) -> do
        putStrLn $ "[ Exception ] "
                ++ show e
                ++ " occurred on tryMkUserDir."

        putStrLn $ "root: " ++ toFilePath root
        putStrLn $ "templateModuleName: " ++ toFilePath templateModuleName
        putStrLn $ "With some codeMod."
    )

  where
    mkUserDir = do

      let ext = ".elm"

      let templateDirPath = root </> $(Path.mkRelDir "template/")

      -- Make template file info.
      --   We'll ignore the possibility of InvalidExtension exception here,
      --   Since we know ".elm" is a valid extension.
      ( templateFileFullName :: Path Rel File ) <- Path.addExtension ext templateModuleName
      let templateFilePath =
            templateDirPath </> $(Path.mkRelDir "src/") </> templateFileFullName


      -- Make user dir info.
      randomHash <- getRandomHash
      -- Ignoring InvalidRelDir exception, since a hash should be valid dir name.
      userDirName <- Path.parseRelDir $ Text.unpack randomHash
      let userDirPath =
            root </> $(Path.mkRelDir "user-creations/")
                 </> userDirName

      copyDirRecur templateDirPath userDirPath
            
      -- User file is the user's module created by modifying template module.
      let userFilePath =
            userDirPath </> $(Path.mkRelDir "src/") </> templateFileFullName


      templateCodeByteStr <- readFile $ toFilePath templateFilePath

      let resultUserCode :: Either MkUserFileError Text
          resultUserCode = do

            templateCode <-
              left ( \_ -> TemplateUtf8Error )
                   ( decodeUtf8' templateCodeByteStr )

            moddedCode <- codeMod templateCode
            pure moddedCode

      case resultUserCode of
        Left mkUserCodeError -> throwIO $ mkUserCodeError
        Right userCode -> do
          writeFile ( toFilePath userFilePath ) ( encodeUtf8 userCode )
          pure userDirName
