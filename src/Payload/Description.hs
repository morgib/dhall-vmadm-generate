{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Payload.Description
  ( PayloadDescription(..)
  , PropDesc(..)
  , PropDescName(..)
  , SubPropDescName(..)
  , SubObjectDescription(..)
  , SubObjectId(..)
  , SubPropDesc(..)
  , PropDescType(..)
  , SubPropDescType(..)
  , CrudDesc(..)
  , payloadDescriptions
  )
where

import           Control.Lens
import qualified Data.Text                     as T

import qualified PropTable.Types               as P
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           Control.Arrow                  ( (&&&) )
import           Data.Hashable                  ( Hashable(..) )
import           Data.Maybe                     ( mapMaybe )

-- | A description of a payload's structure from which a formal
-- payload type and set of subobject types can be produced
data PayloadDescription = PayloadDescription
  { properties :: [PropDesc]
  , subObjects :: HM.HashMap SubObjectId SubObjectDescription
  }

-- | A description of a property within a particular payload
data PropDesc = PropDesc
  { propType :: PropDescType
  , propName :: PropDescName
  , isRequired :: Bool
  , crudDesc :: Maybe CrudDesc
  }

-- | An identifier for a property description
newtype PropDescName = PropDescName { propDescName :: T.Text }

data CrudDesc  =
  BaseProp T.Text
  | AddProp T.Text | UpdateProp T.Text | RemoveProp T.Text

-- | A description of a property's type
data PropDescType = SimpleType P.PropType | DerivedType SubObjectId

-- | A description of a subproperty within a particular object and
-- payload
data SubPropDesc = SubPropDesc
  { subPropType :: SubPropDescType
  , subPropName :: SubPropDescName
  , subIsRequired :: Bool
  }

-- | An identifier for a subobject property description
newtype SubPropDescName = SubPropDescName { subPropDescName :: T.Text }

-- | A description of a subobject property's type
newtype SubPropDescType = SubPropDescType P.PropType

-- | An identifier for a derived subproperty object type
newtype SubObjectId = SubObjectId T.Text
  deriving (Eq, Hashable)

-- Subobject is shorthand for subproperty object
-- | A description of a subproperty object's structure from which a
-- formal type can be produced
newtype SubObjectDescription = SubObjectDescription
  { subproperties :: HM.HashMap P.SubAction [SubPropDesc]
  }

-- | Generate the set of payload descriptions for all targets
-- available in the given property table
payloadDescriptions
  :: [P.PropTableItem] -> HM.HashMap P.Target PayloadDescription
payloadDescriptions items =
  HM.fromList . map (id &&& describePayload propDefs subPropDefs) $ targets
 where
  targets     = collectTargets items
  propDefs    = collectPropDefs items
  subPropDefs = collectSubPropDefs items

-- | Produce a description of the given target's payload from the sets
-- of all property and subproperty definitions
describePayload
  :: [(P.PropName, P.PropDef)]
  -> HM.HashMap SubObjectId [(SubPropDescName, P.SubPropDef)]
  -> P.Target
  -> PayloadDescription
describePayload propDefs subPropDefs target = PayloadDescription
  { properties = describeProperties target (HM.keys subPropDefs) propDefs
  , subObjects = describeSubObjects target subPropDefs
  }

-- | Process property definitions into property descriptions for the
-- given target
describeProperties
  :: P.Target -> [SubObjectId] -> [(P.PropName, P.PropDef)] -> [PropDesc]
describeProperties target subIds = mapMaybe (uncurry descProp)
  . filter matchTarget
 where
  matchTarget = maybe False (elem target) . preview allowed
  allowed     = _2 . P.payload . _Just . P.allowed . P._AllowedRequired
  descProp :: P.PropName -> P.PropDef -> Maybe PropDesc
  descProp name def = do
    propType' <- def ^? P.payload . _Just . P.type_
    let propType = if view (P._PropName . to SubObjectId) name `elem` subIds
          then DerivedType (view (P._PropName . to SubObjectId) name)
          else SimpleType propType'
    let propName' = view P._PropName name
    let propName  = PropDescName propName'
    let getReq    = P.payload . _Just . P.required . P._AllowedRequired
    let crudDesc  = markCrudProp subIds propName'
    isRequired <- elem target <$> preview getReq def
    pure $ PropDesc { propType, propName, isRequired, crudDesc }

-- | Categorize property id as part of a CRUD family or as a
-- standalone property
markCrudProp :: [SubObjectId] -> T.Text -> Maybe CrudDesc
markCrudProp subIds name = if SubObjectId name `elem` subIds
  then Just $ case T.breakOn "_" name of
    (base    , ""   ) -> BaseProp base  -- Base case for base names with no underscores
    ("add"   , base_) -> AddProp . T.drop 1 $ base_
    ("update", base_) -> UpdateProp . T.drop 1 $ base_
    ("remove", base_) -> RemoveProp . T.drop 1 $ base_
    (a       , b    ) -> BaseProp $ a <> b  -- Base case for base names with underscores
  else Nothing

-- | Produce descriptions of the given target's subobjects from the
-- set of all subproperty definitions
describeSubObjects
  :: P.Target
  -> HM.HashMap SubObjectId [(SubPropDescName, P.SubPropDef)]
  -> HM.HashMap SubObjectId SubObjectDescription
describeSubObjects target = HM.map descSubObj
 where
  descSubObj :: [(SubPropDescName, P.SubPropDef)] -> SubObjectDescription
  descSubObj props =
    let matchProps = filter matchTarget props
    in  SubObjectDescription
          { subproperties = HM.fromList
                            . map (id &&& flip describeSubProperties matchProps)
                            . filter (compatible (target ^. P._Targetlike . _2))
                            $ [P.SubAdd, P.SubReceive, P.SubUpdate]
          }
  matchTarget = maybe False (any (match target)) . preview allowed
  match (P.Targetlike brand action) (P.Targetlike brand' subAction) =
    brand == brand' && compatible action subAction
  allowed = _2 . P.payload . _Just . P.allowed . P._AllowedRequired
  compatible P.Create  P.SubAdd     = True
  compatible P.Receive P.SubReceive = True
  compatible P.Update  P.SubAdd     = True
  compatible P.Update  P.SubUpdate  = True
  compatible _         _            = False

-- | Process subproperty definitions into subproperty descriptions for
-- the given sub action
describeSubProperties
  :: P.SubAction -> [(SubPropDescName, P.SubPropDef)] -> [SubPropDesc]
describeSubProperties subAction = mapMaybe (uncurry descSubProp)
 where
  descSubProp :: SubPropDescName -> P.SubPropDef -> Maybe SubPropDesc
  descSubProp name def = do
    propType' <- def ^? P.payload . _Just . P.type_
    let getAllowed = P.payload . _Just . P.allowed . P._AllowedRequired . to
          (map (view $ P._Targetlike . _2))
    allowed <- elem subAction <$> preview getAllowed def
    if allowed then pure () else Nothing
    let getReq = P.payload . _Just . P.required . P._AllowedRequired . to
          (map (view $ P._Targetlike . _2))
    req <- elem subAction <$> preview getReq def
    pure $ SubPropDesc { subPropType   = SubPropDescType propType'
                       , subPropName   = name
                       , subIsRequired = req
                       }

-- | Collect all definitions for payload properties
collectPropDefs :: [P.PropTableItem] -> [(P.PropName, P.PropDef)]
collectPropDefs = mapMaybe nameDefPair
 where
  nameDefPair item = (,) <$> preview propName item <*> preview propDef item
  propName = P._PropDefItem . _1
  propDef  = P._PropDefItem . _2

-- | Collect all definitions for subproperties, grouped by discovered
-- object type identifiers
collectSubPropDefs
  :: [P.PropTableItem]
  -> HM.HashMap SubObjectId [(SubPropDescName, P.SubPropDef)]
collectSubPropDefs items =
  HM.fromList . map (id &&& flip collect items) . collectSubObjectIds $ items
 where
  collect
    :: SubObjectId -> [P.PropTableItem] -> [(SubPropDescName, P.SubPropDef)]
  collect objId = mapMaybe nameDefPair . filter (matchObj objId)
  nameDefPair item =
    (,) <$> preview subPropName item <*> preview subPropDef item
  matchObj objId = (== Just objId) . preview subObjName
  subObjName = P._SubPropDefItem . _1 . P._SubPropName . _1 . to SubObjectId
  subPropName =
    P._SubPropDefItem . _1 . P._SubPropName . _2 . to SubPropDescName
  subPropDef = P._SubPropDefItem . _2

-- | Collect unique values of subobject names present in the property table
collectSubObjectIds :: [P.PropTableItem] -> [SubObjectId]
collectSubObjectIds = map SubObjectId . nub . toListOf (folded . subPropName)
  where subPropName = P._SubPropDefItem . _1 . P._SubPropName . _1

-- | Collect unique values of target present in the property table
collectTargets :: [P.PropTableItem] -> [P.Target]
collectTargets = nub . concat . toListOf (folded . allowed)
 where
  allowed =
    P._PropDefItem . _2 . P.payload . _Just . P.allowed . P._AllowedRequired
