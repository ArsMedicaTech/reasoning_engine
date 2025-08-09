{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Short         as BSS
import qualified Data.Text                     as TS
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy.Encoding       as TLE
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import qualified Data.Vector                   as V
import qualified Data.HashMap.Strict           as HM
import           System.Environment            (getArgs)
import           Data.Binary                   (Binary(..), encodeFile)
import Data.Hashable (Hashable)

import           Data.List                     (isPrefixOf)
import           Control.Monad                 (forM_)
import           Text.Read                     (readMaybe)

-- | Type aliases
type ConceptId = Integer
type Term      = BSS.ShortByteString

type OntologyMap = HM.HashMap ConceptId Term

instance (Binary k, Binary v, Hashable k) => Binary (HM.HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get

-- | Fields in `sct2_Description_Full-en_US.txt` (tab-delimited)
-- id	effectiveTime	active	moduleId	conceptId	languageCode	typeId	term	caseSignificanceId
parseLine :: TS.Text -> Maybe (ConceptId, Term)
parseLine line =
  case TS.splitOn "\t" line of
    (_:_:active:_:cid:lang:typeId:term:_) ->
      if active == "1" && lang == "en" && typeId == "900000000000003001"  -- preferred
        then do
          cid' <- readMaybe (TS.unpack cid)
          Just (cid', BSS.toShort (TE.encodeUtf8 term))
        else Nothing
    _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      putStrLn $ "Reading from: " ++ inputFile
      putStrLn $ "Writing to:   " ++ outputFile

      raw <- TIO.readFile inputFile
      let ls     = drop 1 $ TS.lines raw  -- skip header
          terms  = map parseLine ls
          final  = HM.fromList [x | Just x <- terms]

      putStrLn $ "Loaded " ++ show (HM.size final) ++ " preferred terms."
      encodeFile outputFile final

    _ -> do
      putStrLn "Usage: build <sct2_Description_Full-en_US.txt> <ontology.bin>"
