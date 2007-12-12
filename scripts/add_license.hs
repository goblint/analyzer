#!/usr/bin/runghc

import System.Directory
import System.Environment
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Common ((</>), getSourceFiles)

getLicenseFileName :: FilePath -> String -> IO (Maybe FilePath)
getLicenseFileName path file = do
  let lfp = path </> file
  b <- doesFileExist lfp
  if b then return (Just lfp)
       else return Nothing

prepLicenseText :: B.ByteString -> B.ByteString
prepLicenseText = B.unlines . g . map f . B.lines
  where
    f line = (B.pack " * ") `B.append` line
    g xs = [B.pack "(* "] ++ xs ++ [B.pack " *)"]

processFile :: B.ByteString -> FilePath -> IO ()
processFile header file = do
  code <- B.readFile file
  B.writeFile file (header `B.append` B.pack "\n" `B.append` code)

main :: IO ()
main = do
  args <- getArgs
  cur <- getCurrentDirectory
  case args of
    [fname, dirname] -> do
      t <- getLicenseFileName cur fname
      b <- doesDirectoryExist dirname
      case (t, b) of
        (Just license, True) -> do
          header <- liftM prepLicenseText (B.readFile license)
          files <- getSourceFiles (cur </> dirname)
          mapM_ (processFile header) files
        _ -> return ()
    _ -> return ()
