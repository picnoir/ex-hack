module ExHack.Renderer.Types (
    Col,
    Line,
    HomePagePackage(..),
    ModuleName,
    PackageName,
    Route(..),
    SymbolName,
    SymbolOccurs(..)
) where

import           Data.Text    (Text)

import           ExHack.Types (SourceCodeFile (..))

type PackageName = Text
type SymbolName  = Text
type ModuleName  = Text
type Col = Int
type Line = Int

data Route = 
    HomePage
    | PackagePage PackageName
    | ModulePage PackageName ModuleName

data HomePagePackage = HomePagePackage PackageName Int

data SymbolOccurs = SymbolOccurs SymbolName [(Col, Line, SourceCodeFile)]
