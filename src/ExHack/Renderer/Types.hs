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
    HomePagePackage(..),
    Line,
    ModuleName(..),
    PackageName(..),
    Route(..),
    SymbolName,
    SymbolOccurs(..),
    renderRoute
) where

import           Data.Text      (Text, pack, unpack)
import           Database.Selda (RowID)
import           Network.URI    (escapeURIString, isReserved)
import           Text.Hamlet    (Render)

import           ExHack.Types   (SourceCodeFile (..))

newtype PackageName = PackageName (RowID, Text)
newtype ModuleName  = ModuleName (RowID, Text)
type SymbolName  = Text
type Col         = Int
type Line        = Int

-- | Renderer's routing datatype
data Route = 
    HomePage
    | PackagePage PackageName
    | ModulePage PackageName ModuleName

-- | Datatype used to populate the home page template.
data HomePagePackage = HomePagePackage PackageName Int

-- | Datatype used to populate the module HTML template.
data SymbolOccurs = SymbolOccurs SymbolName [(Col, Line, SourceCodeFile)]

escapeUrlSegment :: Text -> Text
escapeUrlSegment = pack . escapeURIString isReserved . unpack

-- | Render a 'Route' to a proper HTTP URL.
renderRoute :: Render Route
renderRoute HomePage _           = "/"
renderRoute (PackagePage (PackageName (_,pn))) _ = "/packages/" <> escapeUrlSegment pn <> "/"
renderRoute (ModulePage (PackageName (_,pn)) (ModuleName (_,mn))) _ = 
    "/packages/" <> escapeUrlSegment pn <> "/" <> escapeUrlSegment mn <> "/"
