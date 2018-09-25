{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ExHack.Renderer.Html (
    renderHomePage,
    renderModulePage,
    renderPackagePage
) where

import           Data.Text   (Text)
import           Text.Hamlet (HtmlUrl, hamletFile)

type CodeExample = Text
type ModuleName  = Text
type PackageDesc = Text
type PackageName = Text
type SymbolName  = Text

data Route = 
    HomePage
    | PackagePage PackageName
    | ModulePage PackageName ModuleName

data HomePagePackage = HomePagePackage PackageName PackageDesc Int Int

data SymbolOccurs = SymbolOccurs SymbolName [CodeExample]

getHeader :: Text -> HtmlUrl Route
getHeader pageTitle = $(hamletFile "./src/ExHack/Renderer/templates/header.hamlet")

menu :: HtmlUrl Route
menu = $(hamletFile "./src/ExHack/Renderer/templates/menu.hamlet")

renderHomePage :: [HomePagePackage] -> HtmlUrl Route
renderHomePage packages = 
    $(hamletFile "./src/ExHack/Renderer/templates/homePage.hamlet")
  where
    header = getHeader "The Haskell Examples Database"

renderPackagePage :: HomePagePackage -> [ModuleName] ->  HtmlUrl Route
renderPackagePage pack@(HomePagePackage pn _ _ _) mods = 
    $(hamletFile "./src/ExHack/Renderer/templates/packagePage.hamlet")
  where
    header = getHeader $ pn <> " usage examples"

renderModulePage :: ModuleName -> [SymbolOccurs] -> HtmlUrl Route
renderModulePage mname soccs = 
    $(hamletFile "./src/ExHack/Renderer/templates/modulePage.hamlet")
  where
    header = getHeader "ToTitle"
