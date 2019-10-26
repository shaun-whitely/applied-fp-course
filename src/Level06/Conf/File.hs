{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)
import           Control.Monad.IO.Class     (liftIO)

import qualified Data.Attoparsec.ByteString as AB

import qualified System.IO                  as SIO

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM (runAppM), liftEither)
import           Level06.Types              (ConfigError (BadConfFile, FileReadError),
                                             PartialConf (PartialConf), partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile path = do
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
    exOrContents <- liftIO $ try $ SIO.openBinaryFile path SIO.ReadMode >>= BS.hGetContents
    let ceOrContents = first FileReadError exOrContents
    liftEither ceOrContents

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile path = do
  file <- readConfFile path
  let errorOrDecoded = D.pureDecodeFromByteString AB.parseOnly partialConfDecoder file
  liftEither $ first (BadConfFile . fst) errorOrDecoded

-- Go to 'src/Level06/Conf.hs' next.
