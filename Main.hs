{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Web.Scotty.Trans (ScottyT, scottyT, status, param, json, middleware, lift)
import qualified Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status (status404)
import Data.Aeson (ToJSON)
import qualified Data.Text.Lazy as T
import Data.Binary (Binary(..), decodeFile)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Control.Monad (forM_)

---------------------------------------------------
-- ## 1. ALIGNED DATA STRUCTURES ##
---------------------------------------------------

type TermId = T.Text

data RelationshipType
  = IsA
  | CausedBy
  | FindingSite
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

type Ontology = Map TermId Term

-- Instances for API (JSON) and file loading (Binary)
instance ToJSON RelationshipType
instance ToJSON Relationship
instance ToJSON Term

instance Binary RelationshipType
instance Binary Relationship
instance Binary Term


---------------------------------------------------
-- ## 2. SIMPLIFIED LOADING ##
---------------------------------------------------

loadOntology :: IO Ontology
loadOntology = decodeFile "/app/ontology.bin"


---------------------------------------------------
-- ## 3. REASONING FUNCTIONS ##
---------------------------------------------------

getTransitiveRelations :: Ontology -> RelationshipType -> TermId -> [TermId]
getTransitiveRelations ontology relType tid = case Map.lookup tid ontology of
  Nothing -> []
  Just term ->
    let direct = [targetId r | r <- relationships term, relationshipType r == relType]
        indirect = concatMap (getTransitiveRelations ontology relType) direct
    in nub (direct ++ indirect)

getAncestors :: Ontology -> TermId -> [TermId]
getAncestors ontology = getTransitiveRelations ontology IsA

isA :: Ontology -> TermId -> TermId -> Bool
isA ontology childId parentId = parentId `elem` getAncestors ontology childId

findByTarget :: Ontology -> RelationshipType -> TermId -> [Term]
findByTarget ontology relType target =
  Map.foldr (\term acc ->
    if any (\r -> relationshipType r == relType && targetId r == target) (relationships term)
    then term : acc
    else acc
  ) [] ontology



main :: IO ()
main = do
  ontology <- loadOntology

  putStrLn "Sample keys in ontology:"
  mapM_ print (take 10 (Map.keys ontology))

  let runner = flip runReaderT ontology
  scottyT 8080 runner $ do
    middleware logStdoutDev
    
    Scotty.get "/healthz" $ do
      json ("OK" :: T.Text)

    Scotty.get "/reasoning/term/:id" $ do
      termId   <- param "id"          -- termId :: T.Text   (lazy)
      ontology <- lift ask            -- ontology :: Map T.Text Term
      case Map.lookup termId ontology of
        Just term -> json term
        Nothing   -> do
          status status404
          json ("Not Found" :: T.Text)

-- | Helpers
readMaybeTL :: T.Text -> Maybe Integer
readMaybeTL = readMaybe . T.unpack
