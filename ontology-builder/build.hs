{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import System.Environment (getArgs)
import Data.Foldable (foldl')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Char (ord)

-- The 'cassava' library for robust TSV parsing
import Data.Csv

-- Data.Binary for creating the final output file
import Data.Binary (Binary(..), encodeFile)

---------------------------------------------------
-- ## 1. Shared Data Structures ##
---------------------------------------------------
-- These types define the structure of our final, in-memory ontology.

type TermId = T.Text

data RelationshipType
  = IsA
  | CausedBy
  | FindingSite
  -- Add other relationship types as needed
  deriving (Show, Eq, Generic)

data Relationship = Relationship
  { relationshipType :: RelationshipType
  , targetId       :: TermId
  } deriving (Show, Eq, Generic)

data Term = Term
  { termId        :: TermId
  , name          :: T.Text
  , relationships :: [Relationship]
  } deriving (Show, Eq, Generic)

type Ontology = Map.Map TermId Term

---------------------------------------------------
-- ## 2. Binary Instances for Serialization ##
---------------------------------------------------
-- These allow us to write our rich data structures to a file.

instance Binary RelationshipType
instance Binary Relationship
instance Binary Term
-- The Map instance is provided by the Binary library when k/v are instances

---------------------------------------------------
-- ## 3. SNOMED Logic and Main Function ##
---------------------------------------------------

-- Helper to map SNOMED's numeric IDs to our types
-- Note: SNOMED IDs are just Text, not Integers, as they can be large.
snomedRelIdToType :: T.Text -> Maybe RelationshipType
snomedRelIdToType "116680003" = Just IsA
snomedRelIdToType "246075003" = Just CausedBy
snomedRelIdToType "363698007" = Just FindingSite
snomedRelIdToType _           = Nothing -- Ignore others for now

-- TSV parsing options for SNOMED files
tsvOptions :: DecodeOptions
tsvOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [conceptFile, descriptionFile, relationshipFile, outputFile] -> do
      putStrLn "Starting ontology build..."

      -- ## PASS 1: CONCEPTS ##
      -- Create a skeleton map from the Concepts file.
      putStrLn "  -> Pass 1: Loading concepts..."
      conceptData <- BL.readFile conceptFile
      let conceptRecords = decodeWith tsvOptions NoHeader conceptData :: Either String (V.Vector (T.Text, T.Text, T.Text))
      let initialOntology = case conceptRecords of
            Left err -> error $ "Concept parsing error: " ++ err
            Right v  -> foldl' (\acc row ->
                let (cid, _effectiveTime, active) = (V.head row, V.head (V.tail row), V.head (V.tail (V.tail row))) -- simplified for brevity
                in if active == "1"
                   then Map.insert cid (Term cid "" []) acc
                   else acc
              ) Map.empty v
      putStrLn $ "Loaded " ++ show (Map.size initialOntology) ++ " active concepts."

      -- ## PASS 2: DESCRIPTIONS ##
      -- Populate the 'name' field using Preferred Terms.
      putStrLn "  -> Pass 2: Populating names..."
      descData <- BL.readFile descriptionFile
      let descRecords = decodeWith tsvOptions NoHeader descData :: Either String (V.Vector (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text))
      let ontologyWithNames = case descRecords of
            Left err -> error $ "Description parsing error: " ++ err
            Right v -> foldl' (\acc row ->
                let (_id, _effTime, active, _modId, cid, _lang, typeId, term, _case) =
                      (row V.! 0, row V.! 1, row V.! 2, row V.! 3, row V.! 4, row V.! 5, row V.! 6, row V.! 7, row V.! 8)
                in if active == "1" && typeId == "900000000000013009" -- The correct ID for Preferred Term
                   then Map.adjust (\t -> t { name = term }) cid acc
                   else acc
              ) initialOntology v
      putStrLn "Populated term names."

      -- ## PASS 3: RELATIONSHIPS ##
      -- Build the connections between concepts.
      putStrLn "  -> Pass 3: Building relationships..."
      relData <- BL.readFile relationshipFile
      let relRecords = decodeWith tsvOptions NoHeader relData :: Either String (V.Vector (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text))
      let finalOntology = case relRecords of
            Left err -> error $ "Relationship parsing error: " ++ err
            Right v -> foldl' (\acc row ->
                let (_id, _effTime, active, _modId, sourceId, destId, _relGroup, typeId, _charType, _modifier) =
                      (row V.! 0, row V.! 1, row V.! 2, row V.! 3, row V.! 4, row V.! 5, row V.! 6, row V.! 7, row V.! 8, row V.! 9)
                in if active == "1"
                   then case snomedRelIdToType typeId of
                          Just rt ->
                            let newRel = Relationship rt destId
                            in Map.adjust (\t -> t { relationships = newRel : relationships t }) sourceId acc
                          Nothing -> acc -- Ignore relationship types we don't handle
                   else acc
              ) ontologyWithNames v
      putStrLn "Built relationship graph."

      -- ## SERIALIZE TO FILE ##
      putStrLn $ "Writing final ontology to: " ++ outputFile
      encodeFile outputFile finalOntology
      putStrLn "Build complete."

    _ -> putStrLn "Usage: build <concept.txt> <description.txt> <relationship.txt> <output.bin>"
