{-|
Module      : ExHack.Renderer.Types
Description : Types associated to the HTML renderer.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}

module ExHack.Renderer.Types (
    Col,
    HighLightError(..),
    HighlightedSymbolOccurs(..),
    HighlightedSourceCodeFile(..),
    HomePagePackage(..),
    Line,
    ModuleName(..),
    PackageName(..),
    Route(..),
    SymbolName,
    SymbolOccurs(..),
    renderRoute
) where

import           Control.Monad.Catch (Exception)
import           Data.Text           (Text, pack, replace, unpack)
import           Database.Selda      (RowID)
import           Network.URI         (escapeURIString, isReserved)
import           Text.Hamlet         (Render)

import           ExHack.Types        (ModuleNameT, PackageNameT,
                                      SourceCodeFile (..))

newtype PackageName            = PackageName (RowID, Text)
newtype ModuleName             = ModuleName (RowID, Text)
data HighlightedSourceCodeFile = HighlightedSourceCodeFile 
                                    Text ModuleNameT PackageNameT deriving (Eq, Show)
type SymbolName                = Text
type Col                       = Int
type Line                      = Int

-- | Renderer's routing datatype
data Route = 
    HomePage
    | PackagePage PackageName
    | ModulePage PackageName ModuleName

-- | Datatype used to populate the home page template.
data HomePagePackage = HomePagePackage PackageName Int

-- | Datatype used to populate the module HTML template whith non highlighted code.
data SymbolOccurs = SymbolOccurs SymbolName [(Col, Line, SourceCodeFile)]

-- | Datatype used to populate the module HTML template whith highlighted code.
data HighlightedSymbolOccurs = HighlightedSymbolOccurs SymbolName [(Col, Line, HighlightedSourceCodeFile)]

-- | Exception raised during the code highlight process.
newtype HighLightError = HighLightError String deriving (Eq, Show)

instance Exception HighLightError

escapeUrlSegment :: Text -> Text
escapeUrlSegment = pack . escapeURIString isReserved . unpack

-- | Render a 'Route' to a proper HTTP URL.
renderRoute :: Render Route
renderRoute HomePage _           = "/"
renderRoute (PackagePage (PackageName (_,pn))) _ = "/packages/" <> escapeUrlSegment pn <> "/"
renderRoute (ModulePage (PackageName (_,pn)) (ModuleName (_,mn))) _ = 
    "/packages/" <> escapeUrlSegment pn <> "/" <> escapeUrlSegment (replace "." "-" mn) <> "/"
