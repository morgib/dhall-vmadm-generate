{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}
module PropTable.Types where

import qualified Data.Aeson                    as A
import           Data.Aeson                     ( FromJSON(..)
                                                , withBool
                                                , withText
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                )
import qualified Data.Aeson.Types              as A
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic(..) )
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromMaybe )
import           Control.Lens            hiding ( List )
import           Data.Hashable                  ( Hashable(..) )

-- This file is organized following the structure of the documentation
-- at the top of proptable.js, where possible. Empty sections are
-- present for attributes which do not require any special code to be
-- handled.

trueUnit :: String -> a -> A.Value -> A.Parser a
trueUnit label unit = withBool label $ \case
  True  -> pure unit
  False -> fail $ label ++ ": not handling false case"

newtype PropName = PropName T.Text
  deriving (Show, Generic)

makePrisms ''PropName

instance FromJSON PropName

data SubPropName = SubPropName T.Text T.Text
  deriving (Show, Generic)

makePrisms ''SubPropName

-- * Property: deprecated
data Deprecated = Deprecated
  deriving (Show)

instance FromJSON Deprecated where
  parseJSON = trueUnit "Deprecated" Deprecated

-- * Property: flattenable
data Flattenable = Array | ArrayHashKey | HashKey
  deriving (Show)

makePrisms ''Flattenable

instance FromJSON Flattenable where
  parseJSON = withText "Flattenable" $ \case
    "array"          -> pure Array
    "array_hash_key" -> pure ArrayHashKey
    "hash_key"       -> pure HashKey
    _                -> fail "Flattenable: invalid string"

-- * Property: ignore
data Ignore = Ignore
  deriving (Show)

instance FromJSON Ignore where
  parseJSON = trueUnit "Ignore" Ignore

-- * Property: json
newtype JSONPath = JSONPath T.Text
  deriving (Show, Generic)

makePrisms ''JSONPath

instance FromJSON JSONPath

-- * Property: load_depends
newtype LoadDepends = LoadDepends PropName
  deriving (Show)

makePrisms ''LoadDepends

instance FromJSON LoadDepends where
  parseJSON = fmap LoadDepends . parseJSON @PropName

-- * Property: loadValueTranslator
newtype LoadValTrans = LoadValTrans T.Text
  deriving (Show, Generic)

makePrisms ''LoadValTrans

instance FromJSON LoadValTrans

-- * Property: payload
-- ** Subproperty: allowed
data Action = Create | Receive | Update
  deriving (Show, Eq, Generic, Hashable)

instance FromJSON Action where
  parseJSON = withText "Action" $ \case
    "create"  -> pure Create
    "receive" -> pure Receive
    "update"  -> pure Update
    s         -> fail $ "Action: invalid string \"" ++ T.unpack s ++ "\""

data SubAction = SubAdd | SubReceive | SubUpdate
  deriving (Show, Eq, Generic, Hashable)

makePrisms ''SubAction

instance FromJSON SubAction where
  parseJSON = withText "SubAction" $ \case
    "add"     -> pure SubAdd
    "receive" -> pure SubReceive
    "update"  -> pure SubUpdate
    s         -> fail $ "SubAction: invalid string \"" ++ T.unpack s ++ "\""

data Brand = Bhyve | Joyent | JoyentMinimal | KVM | LX
  deriving (Show, Eq, Generic, Hashable)

parseBrand :: T.Text -> A.Parser Brand
parseBrand = \case
  "bhyve"          -> pure Bhyve
  "joyent"         -> pure Joyent
  "joyent-minimal" -> pure JoyentMinimal
  "kvm"            -> pure KVM
  "lx"             -> pure LX
  _                -> fail "Brand: invalid string"

data Targetlike action = Targetlike Brand action
  deriving (Show, Eq, Generic, Hashable)

makePrisms ''Targetlike

type Target = Targetlike Action
type SubTarget = Targetlike SubAction

newtype AllowedRequired action = AllowedRequired [Targetlike action]
  deriving (Show)

makePrisms ''AllowedRequired

instance FromJSON action => FromJSON (AllowedRequired action) where
  parseJSON =
    withObject "AllowedRequired"
      $ fmap (AllowedRequired . concat)
      . traverse (uncurry collect)
      . HM.toList
   where
    collect
      :: FromJSON action => T.Text -> A.Value -> A.Parser [Targetlike action]
    collect label value = do
      brand <- parseBrand label
      fmap (Targetlike brand) <$> parseJSON value

-- ** Subproperty: check_as
-- ** Subproperty: required
-- ** Subproperty: keep_zero
data KeepZero = KeepZero
  deriving (Show)

instance FromJSON KeepZero where
  parseJSON = trueUnit "KeepZero" KeepZero

-- ** Subproperty: type
data PropType = ObjectArray | Boolean | FlatObject | Integer_ | List | String_ | UUID | Zpool
  deriving (Show, Eq)

makePrisms ''PropType

instance FromJSON PropType where
  parseJSON = withText "PropType" $ \case
    "object-array" -> pure ObjectArray
    "boolean"      -> pure Boolean
    "flat-object"  -> pure FlatObject
    "integer"      -> pure Integer_
    "list"         -> pure List
    "string"       -> pure String_
    "uuid"         -> pure UUID
    "zpool"        -> pure Zpool
    s              -> fail $ "PropType: invalid string \"" ++ T.unpack s ++ "\""

-- ** Subproperty: valueValidator
newtype ValueValidator = ValueValidator T.Text
  deriving (Show, Generic)

makePrisms ''ValueValidator

instance FromJSON ValueValidator

-- ** The property
data Payload action = Payload
  { _allowed        :: AllowedRequired action
  , _checkAs        :: Maybe PropName
  , _keepZero       :: Maybe KeepZero
  , _required       :: AllowedRequired action
  , _type_          :: PropType
  , _valueValidator :: Maybe ValueValidator
  }
  deriving (Show)

makeLenses ''Payload
instance FromJSON action => FromJSON (Payload action) where
  parseJSON = withObject "Payload" $ \v ->
    Payload
      <$> v
      .:  "allowed"
      <*> v
      .:? "check_as"
      <*> v
      .:? "keep_zero"
      <*> (fromMaybe (AllowedRequired []) <$> v .:? "required")
      <*> v
      .:  "type"
      <*> v
      .:? "valueValidator"

-- * Property: sysinfo
newtype Sysinfo = Sysinfo T.Text
  deriving (Show, Generic)

makePrisms ''Sysinfo

instance FromJSON Sysinfo

-- * Property: zfs
newtype ZFSField = ZFSField T.Text
  deriving (Show, Generic)

makePrisms ''ZFSField

instance FromJSON ZFSField

newtype ZFSType = ZFSType T.Text
  deriving (Show, Generic)

makePrisms ''ZFSType

instance FromJSON ZFSType

data ZFS = ZFS [ZFSField] [ZFSType]
  deriving (Show)

makePrisms ''ZFS

instance FromJSON ZFS where
  parseJSON = withObject "ZFS" $ \v -> ZFS <$> v .: "fields" <*> v .: "types"

-- * Property: zoneinfo
newtype Zoneinfo = Zoneinfo T.Text
  deriving (Show, Generic)

makePrisms ''Zoneinfo

instance FromJSON Zoneinfo

newtype Zonexml = Zonexml T.Text
  deriving (Show, Generic)

makePrisms ''Zonexml

-- * Property: zonexml
instance FromJSON Zonexml

newtype ZonexmlDepends = ZonexmlDepends T.Text
  deriving (Show, Generic)

makePrisms ''ZonexmlDepends

-- * Property: zonexml_depends
instance FromJSON ZonexmlDepends

-- * The entire property definition
data GenericDef action = GenericDef
  { _deprecated          :: Maybe Deprecated
  , _flattenable         :: Maybe Flattenable
  , _ignore              :: Maybe Ignore
  , _json                :: Maybe JSONPath
  , _loadDepends         :: [LoadDepends]
  , _loadValueTranslator :: Maybe LoadValTrans
  , _payload             :: Maybe (Payload action)
  , _sysinfo             :: Maybe Sysinfo
  , _zfs                 :: Maybe ZFS
  , _zoneinfo            :: Maybe Zoneinfo
  , _zonexml             :: Maybe Zonexml
  , _zonexmlDepends      :: Maybe ZonexmlDepends
  }
  deriving (Show)

makeLenses ''GenericDef

type PropDef = GenericDef Action
type SubPropDef = GenericDef SubAction

instance FromJSON action => FromJSON (GenericDef action) where
  parseJSON = withObject "Property" $ \v ->
    GenericDef
      <$> v
      .:? "deprecated"
      <*> v
      .:? "flattenable"
      <*> v
      .:? "ignore"
      <*> v
      .:? "json"
      <*> (fromMaybe [] <$> v .:? "load_depends")
      <*> v
      .:? "loadValueTranslator"
      <*> v
      .:? "payload"
      <*> v
      .:? "sysinfo"
      <*> v
      .:? "zfs"
      <*> v
      .:? "zoneinfo"
      <*> v
      .:? "zonexml"
      <*> v
      .:? "zonexmlDepends"

data PropTableItem = PropDefItem PropName PropDef
                   | SubPropDefItem SubPropName SubPropDef
                   deriving (Show)

makePrisms ''PropTableItem
