{-# LANGUAGE OverloadedStrings #-}
module Fixups.PropTable
  ( fixupProptable
  )
where

import           Control.Lens

import           PropTable.Types

import qualified Data.Text                     as T

fixupProptable :: [PropTableItem] -> [PropTableItem]
fixupProptable = map propFix
 where
  propFix prop = if prop ^? propName == Just "pci_devices"
    then prop & allowedL %~ filter (/= Targetlike Bhyve Update)
    else prop
  propName :: Fold PropTableItem T.Text
  propName = _PropDefItem . _1 . _PropName
  allowedL :: Traversal' PropTableItem [Targetlike Action]
  allowedL = _PropDefItem . _2 . payload . _Just . allowed . _AllowedRequired
