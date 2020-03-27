{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
module PropTable.Parse where

import           Data.Bifunctor                 ( second )
import qualified Data.Aeson                    as A
import           Data.Aeson                     ( FromJSON(..)
                                                , withObject
                                                )
import qualified Data.Aeson.Types              as A
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic(..) )
import qualified Data.HashMap.Strict           as HM
import           Control.Lens            hiding ( List )
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                    ( either )

import           PropTable.Types

keysToAttrs :: T.Text -> A.Value -> A.Parser [A.Object]
keysToAttrs idAttr = withPairs "keyToAttrs" $ traverse (uncurry keyToAttr)
 where
  keyToAttr :: T.Text -> A.Value -> A.Parser A.Object
  keyToAttr idVal =
    A.withObject "keyToAttr" $ pure . HM.insert idAttr (A.String idVal)

unObject :: A.Value -> A.Parser [(T.Text, A.Value)]
unObject = withObject "unObject" $ pure . HM.toList

withPairs :: String -> ([A.Pair] -> A.Parser a) -> A.Value -> A.Parser a
withPairs t f = withObject t $ f . HM.toList

parseProperty :: T.Text -> A.Value -> A.Parser PropTableItem
parseProperty name v = if ".*." `T.isInfixOf` name
  then
    SubPropDefItem
        (uncurry SubPropName . second (T.drop 3) . T.breakOn ".*." $ name)
      <$> parseJSON v
  else PropDefItem (PropName name) <$> parseJSON v

newtype PropTable = PropTable { properties :: [PropTableItem] }
  deriving (Show, Generic)

makePrisms ''PropTable

instance A.FromJSON PropTable where
  parseJSON = A.withObject "PropTable" $ \v -> do
    propVal <- v A..: "properties"
    withPairs "propVal"
              (fmap PropTable . traverse (uncurry parseProperty))
              propVal

loadProperties :: String -> IO [PropTableItem]
loadProperties file =
  BL.readFile file >>= either fail (pure . properties) . A.eitherDecode
