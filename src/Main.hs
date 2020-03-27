{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Dhall.Core
import           Payload.Description            ( payloadDescriptions )
import           Payload.Dhall.Render           ( writeSources
                                                , resolvePaths
                                                )
import           Payload.Dhall.Type             ( allSources )
import           PropTable.Parse                ( loadProperties )
import           Fixups.PropTable               ( fixupProptable )
import           Text.Pretty.Simple             ( pPrint )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  props <- fixupProptable <$> (head <$> getArgs >>= loadProperties)
  putStrLn $ "(Main) Loaded " ++ show (length props) ++ " properties"
  let descs = payloadDescriptions props
  let lib   = map resolvePaths . allSources $ descs
  pPrint . map (fmap Dhall.Core.pretty) $ lib
  writeSources "./build/" lib
