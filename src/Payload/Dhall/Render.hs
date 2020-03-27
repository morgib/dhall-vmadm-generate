{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Payload.Dhall.Render
  ( writeSources
  , resolvePaths
  )
where

import qualified System.Directory              as Directory
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           Dhall.Core                     ( Import(..)
                                                , ImportMode(..)
                                                , ImportType(..)
                                                , ImportHashed(..)
                                                , File(..)
                                                , FilePrefix(..)
                                                , Directory(..)
                                                , pretty
                                                )

import           Control.Arrow                  ( (***) )

import qualified PropTable.Types               as P
import           Payload.Dhall.Type             ( PackageFile(..)
                                                , SimpleId(..)
                                                , Source
                                                )

-- | Write a list of source expressions to the given 'root' directory
writeSources :: FilePath -> [Source FilePath Import] -> IO ()
writeSources root = mapM_ (uncurry render)
 where
  render p e = do
    let path = root </> p
    Directory.createDirectoryIfMissing True (takeDirectory path)
    T.IO.writeFile path . pretty $ e

-- | Transform a given source file description, resolving the abstract
-- 'PackageFile' values in the source path and in imports to concrete
-- relative file paths
resolvePaths :: Source PackageFile PackageFile -> Source FilePath Import
resolvePaths =
  (pathFromPieces *** fmap (uncurry relativeImport))
    . annotateRelative
    . (topLevelPath *** fmap topLevelPath)
 where
  annotateRelative :: Source File File -> Source File (File, File)
  annotateRelative (caller, expr) = (caller, fmap (caller, ) expr)
  pathFromPieces :: File -> FilePath
  pathFromPieces File { directory, file } =
    foldr1 (flip (</>)) . map T.unpack $ file : components directory

-- | Helper to make a 'File' tersely
mkFile :: [T.Text] -> T.Text -> File
mkFile d f = File { directory = Directory d, file = f }

-- | Given a calling file and an imported file, with paths relative to
-- some common root, produce a relative 'Import' value
relativeImport :: File -> File -> Import
relativeImport caller imported = importCodeNoHash . relImport $ rel
  (length . extractPath $ caller)
  (extractPath caller)
  (extractPath imported)
 where
  extractPath = reverse . components . directory
  -- Recursively prune common prefix, determine depth to '..' from
  rel _ []       is       = (0, is)
  rel d _        []       = (d, [])
  rel d (c : cs) (i : is) = if i == c then rel (d - 1) cs is else (d, i : is)
  -- Use the results of the rel operation
  relImport (d, is) = if d <= 0
    then Local Here $ mkFile (reverse is) (file imported)
    else Local Parent
      $ mkFile (reverse is ++ replicate (d - 1) "..") (file imported)
  importCodeNoHash imp = Import
    { importHashed = ImportHashed { hash = Nothing, importType = imp }
    , importMode   = Code
    }

-- | Map the abstract 'PackageFile' into a concrete location in the
-- source tree
topLevelPath :: PackageFile -> File
topLevelPath = \case
  PayloadFile target -> mkFile (targetLabel target) "Payload"
  SubObjFile target objId subAct ->
    mkFile (objId : targetLabel target) (subActionLabel subAct)
  SimpleFile simp -> mkFile [] (simpleLabel simp)
  PackageDotDhall -> mkFile [] "package.dhall"
 where
  targetLabel (P.Targetlike brand action) =
    [actionLabel action, brandLabel brand]
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
