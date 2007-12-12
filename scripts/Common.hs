module Common (
    (</>), getSourceFiles
  ) where

import Control.Monad
import System.Directory

(</>) :: FilePath -> FilePath -> FilePath
xs </> ys = xs ++ "/" ++ ys

getExtension :: FilePath -> String
getExtension = drop 1 . dropWhile (/= '.')

getSourceFiles :: FilePath -> IO [FilePath]
getSourceFiles root = do
  contents <- getDirectoryContents root
  flat <- forM (filter good contents) $ \file -> do
    let newFile = root </> file
    isDir <- doesDirectoryExist newFile
    isFile <- doesFileExist newFile
    case (isDir, isFile) of
      (True, False) -> getSourceFiles $ newFile
      (False, True) -> do
        return $ case getExtension newFile of
          "ml" -> [newFile]
          "mli" -> [newFile]
          _ -> []
      _ -> return []
  return $ concat flat
  where
    good ('.':_) = False
    good _ = True
