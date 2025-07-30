{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty.Trans                 ( ScottyT, ActionT, scottyT, status
                                          , middleware, param, json )
import qualified Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status (status404)
import Text.Read (readMaybe)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Short        as  BSS
import qualified Data.HashMap.Strict          as  HM
import           Data.Binary                     ( Binary(..), decodeFile )
import qualified Data.Binary as Bin
import           Data.Hashable                   ( Hashable )
import Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader      ( ReaderT, ask, runReaderT )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)
import qualified Data.Text as TS

-- Clinical Concept IDs
type TermId = T.Text

-- Term with name and relationships
data Term = Term
  { termId :: TermId
  , name :: T.Text
  , parents :: [TermId]     -- "is-a" relationships
  , properties :: [Property]
  } deriving (Show, Generic)

-- Arbitrary key-value metadata for the term
data Property
  = Property T.Text T.Text  -- e.g., "causedBy" "SARS-CoV-2"
  deriving (Show, Eq)

-- A simple term store as a map
type Ontology = Map TermId Term

-- Example mini-ontology (like SNOMED)
exampleOntology :: Ontology
exampleOntology = Map.fromList
  [ ("C1", Term "C1" "Infection" [] [])
  , ("C2", Term "C2" "Viral Infection" ["C1"] [])
  , ("C3", Term "C3" "COVID-19" ["C2"] [Property "causedBy" "SARS-CoV-2"])
  , ("C4", Term "C4" "Bacterial Infection" ["C1"] [])
  ]

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

-- API endpoints
getNameById :: TermId -> T.Text
getNameById "C3" = "COVID-19"
getNameById "C2" = "Viral Infection"
getNameById "C1" = "Infection"
getNameById _    = "Unknown Term"

main :: IO ()
main = scotty 8080 $ do
  middleware logStdoutDev

  get "/reasoning/term/:id" $ do
    termId <- param "id"
    let name = getNameById termId
    json (Map.fromList [("id", termId), ("name", name)] :: Map.Map T.Text T.Text)
