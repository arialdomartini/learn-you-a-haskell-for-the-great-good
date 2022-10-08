module FastCopy(copyFile) where

import qualified Data.ByteString.Lazy as B
import Control.Exception (bracketOnError)
import System.IO
import System.Directory(removeFile, renameFile)

copyFile :: [String] -> IO ()
copyFile args = do
  let (f:t: _) = args
  copy f t

copy :: String -> String -> IO ()
copy from to = do
  content <- B.readFile from
  bracketOnError
        (openTempFile "." (from ++ ".temp"))   -- setup
        (\(tempFile, h) -> do         -- on failure
            hClose h
            removeFile tempFile)
        (\(tempFile, h) -> do         -- on success
            B.hPutStr h content
            hClose h
            renameFile tempFile to)
