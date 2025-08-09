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


-- Recursively get all ancestors ("is-a" hierarchy)
getAncestors :: Ontology -> TermId -> [TermId]
getAncestors ontology tid =
  case Map.lookup tid ontology of
    Nothing -> []
    Just term ->
      let directParents = parents term
          indirect = concatMap (getAncestors ontology) directParents
      in nub (directParents ++ indirect)

-- Inherit properties from ancestors
inheritProperties :: Ontology -> TermId -> [Property]
inheritProperties ontology tid =
  let ancestors = getAncestors ontology tid
      ancestorProps = concatMap (\aid -> maybe [] properties (Map.lookup aid ontology)) ancestors
      directProps = maybe [] properties (Map.lookup tid ontology)
  in nub (directProps ++ ancestorProps)


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
