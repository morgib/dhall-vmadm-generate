{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Payload.Dhall.Type
  ( allSources
  , PackageFile(..)
  , SimpleId(..)
  , Source
  )
where

import           Dhall.Core                     ( Expr(..) )
import qualified Dhall.Map                     as Map
import           Data.Void                      ( Void )
import qualified Data.HashMap.Strict           as HM
import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                , first
                                                , second
                                                )
import qualified Data.Text                     as T

import           Payload.Description            ( PayloadDescription(..)
                                                , PropDesc(..)
                                                , PropDescType(..)
                                                , PropDescName(..)
                                                , SubPropDescName(..)
                                                , SubPropDesc(..)
                                                , SubPropDescType(..)
                                                , SubObjectDescription(..)
                                                , SubObjectId(..)
                                                , CrudDesc(..)
                                                )
import qualified PropTable.Types               as P

-- | A location identifier for a source object
data PackageFile = PayloadFile P.Target | SubObjFile P.Target T.Text P.SubAction | SimpleFile SimpleId | PackageDotDhall

-- | An identifier for a simple type definition
data SimpleId = SimpleList | FlatObject | FlatValue

-- | A source expression located by type 'loc', importing type 'im'
type Source loc im = (loc, Expr Void im)

-- | Produce sources for all targets and common types
allSources
  :: HM.HashMap P.Target PayloadDescription -> [Source PackageFile PackageFile]
allSources =
  reflect (mkPackageDotDhall . map fst)
    . (simpleTypes ++)
    . map (second defaultOptionals)
    . concatMap (uncurry targetSources)
    . HM.toList
 where
  reflect f xs = f xs : xs
  simpleTypes =
    [ (SimpleFile SimpleList, simpleList)
    , (SimpleFile FlatObject, assocList flatValue)
    , (SimpleFile FlatValue , flatValue)
    ]

setNested
  :: [T.Text]
  -> T.Text
  -> Expr s a
  -> Map.Map T.Text (Expr s a)
  -> Map.Map T.Text (Expr s a)
setNested []             key value top = Map.insert key value top
setNested (child : path) key value top = case Map.lookup child top of
  Nothing ->
    Map.insert child (RecordLit $ setNested path key value Map.empty) top
  Just (RecordLit childMap) ->
    Map.insert child (RecordLit $ setNested path key value childMap) top
  Just _ ->
    error
      $  "called setNested with a child key "
      ++ show child
      ++ " containing a non-record existing value"

mkPackageDotDhall :: [PackageFile] -> Source PackageFile PackageFile
mkPackageDotDhall locs = (PackageDotDhall, RecordLit top)
 where
  top = foldr insertLocation Map.empty locs
  insertLocation
    :: PackageFile
    -> Map.Map T.Text (Expr Void PackageFile)
    -> Map.Map T.Text (Expr Void PackageFile)
  insertLocation loc = case loc of
    PayloadFile (P.Targetlike brand action) ->
      setNested [brandLabel brand, actionLabel action] "Payload" (Embed loc)
    SubObjFile (P.Targetlike brand action) objId subAct -> setNested
      [brandLabel brand, actionLabel action, objId]
      (subActionLabel subAct)
      (Embed loc)
    SimpleFile simp -> setNested [] (simpleLabel simp) (Embed loc)
    PackageDotDhall -> id
  brandLabel = \case
    P.Joyent        -> "joyent"
    P.JoyentMinimal -> "joyent-minimal"
    P.LX            -> "lx"
    P.Bhyve         -> "bhyve"
    P.KVM           -> "kvm"
  actionLabel = \case
    P.Create  -> "create"
    P.Update  -> "update"
    P.Receive -> "receive"
  subActionLabel = \case
    P.SubAdd     -> "add"
    P.SubReceive -> "receive"
    P.SubUpdate  -> "update"
  simpleLabel = \case
    SimpleList -> "SimpleList"
    FlatObject -> "FlatObject"
    FlatValue  -> "FlatValue"


-- | Produce the set of sources which are derived from the given
-- target payload description
targetSources
  :: P.Target -> PayloadDescription -> [Source PackageFile PackageFile]
targetSources target PayloadDescription { properties, subObjects } =
  payloadSource target properties : subObjectSources target subObjects

-- | Produce source for the given payload
payloadSource :: P.Target -> [PropDesc] -> Source PackageFile PackageFile
payloadSource target =
  (PayloadFile target, ) . Record . Map.fromList . map procProp
 where
  procProp :: PropDesc -> (T.Text, Expr Void PackageFile)
  procProp p =
    propDescName . propName &&& optionalize p . mkPropType target $ p
  optionalize PropDesc { isRequired } = if isRequired then id else App Optional

-- | Produce sources for the given subobjects
subObjectSources
  :: P.Target
  -> HM.HashMap SubObjectId SubObjectDescription
  -> [Source PackageFile PackageFile]
subObjectSources target =
  map (uncurry subObjFileName *** Record . Map.fromList . map procSubProp)
    . concatMap (uncurry tagSubObj . fmap HM.toList)
    . HM.toList
    . HM.map subproperties
 where
  tagSubObj subObjId = map (first (subObjId, ))
  subObjFileName (SubObjectId label) subAct = SubObjFile target label subAct
  procSubProp :: SubPropDesc -> (T.Text, Expr Void PackageFile)
  procSubProp p =
    subPropDescName . subPropName &&& optionalize p . mkSubPropType $ p
  optionalize SubPropDesc { subIsRequired } =
    if subIsRequired then id else App Optional

-- | Map property description to a Dhall type for the corresponding field
mkPropType :: P.Target -> PropDesc -> Expr s PackageFile
mkPropType curTarget PropDesc { propType, crudDesc } = case propType of
  SimpleType P.Boolean    -> Bool
  SimpleType P.Integer_   -> Integer
  SimpleType P.String_    -> Text
  SimpleType P.UUID       -> Text
  SimpleType P.FlatObject -> Embed $ SimpleFile FlatObject
  SimpleType P.List       -> Embed $ SimpleFile SimpleList
  DerivedType (SubObjectId objLabel) ->
    App List . flip Field "Type" . Embed $ SubObjFile curTarget
                                                      objLabel
                                                      (subAction crudDesc)
  SimpleType P.ObjectArray -> Union $ Map.singleton "ObjectArray" Nothing
  SimpleType P.Zpool       -> Union $ Map.singleton "Zpool" Nothing
 where
  subAction Nothing =
    error
      $  "It's currently assumed that a property"
      <> " with a derived type is given a CRUD description"
  subAction (Just cd) = case cd of
    BaseProp n -> case curTarget of
      P.Targetlike _ P.Create -> P.SubAdd
      P.Targetlike _ P.Receive -> P.SubReceive
      _ -> error $ "Assume base props only in add/receive: " <> T.unpack n
    AddProp    _ -> P.SubAdd
    UpdateProp _ -> P.SubUpdate
    RemoveProp _ -> error "Assume no derived remove props"

-- | Map subproperty description to a Dhall type for the corresponding field
mkSubPropType :: SubPropDesc -> Expr s PackageFile
mkSubPropType SubPropDesc { subPropType } = case subPropType of
  SubPropDescType P.Boolean     -> Bool
  SubPropDescType P.Integer_    -> Integer
  SubPropDescType P.String_     -> Text
  SubPropDescType P.UUID        -> Text
  SubPropDescType P.FlatObject  -> Embed $ SimpleFile FlatObject
  SubPropDescType P.List        -> Embed $ SimpleFile SimpleList
  SubPropDescType P.ObjectArray -> Union $ Map.singleton "ObjectArray" Nothing
  SubPropDescType P.Zpool       -> Union $ Map.singleton "Zpool" Nothing

-- | Make a default instance for a record type, assigning Nones at all
-- top level optionals
defaultOptionals :: Expr Void a -> Expr Void a
defaultOptionals (Record kv) = RecordLit $ Map.fromList
  [("Type", Record kv), ("default", RecordLit (Map.mapMaybe toDefault kv))]
 where
  toDefault (App Optional t) = Just (App None t)
  toDefault _                = Nothing
defaultOptionals x = x -- identity on non-record types

-- | The type of a list property, which can be a list of integers or a
-- list of strings in the json payload
simpleList :: Expr Void a
simpleList = Union $ Map.fromList
  [("IntList", Just $ App List Integer), ("TextList", Just $ App List Text)]

-- | The type of an association list of some value type
assocList :: Expr Void a -> Expr Void a
assocList valueType =
  App List $ Record . Map.fromList $ [("mapKey", Text), ("mapValue", valueType)]

-- | The value type used by association lists
flatValue :: Expr Void a
flatValue = Union $ Map.fromList
  [ ("FlatText"   , Just Text)
  , ("FlatInteger", Just Integer)
  , ("FlatBool"   , Just Bool)
  ]
