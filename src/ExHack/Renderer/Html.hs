{-|
Module      : ExHack.Renderer.Html
Description : HTML renderer.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ExHack.Renderer.Html (
    homePageTemplate,
    modulePageTemplate,
    packagePageTemplate
) where

import           Data.Text             (Text)
import           Text.Hamlet           (HtmlUrl, hamletFile)

import           ExHack.Renderer.Types (HomePagePackage (..), ModuleName (..),
                                        PackageName (..), Route (..),
                                        SymbolOccurs (..))
import           ExHack.Types          (SourceCodeFile (..))

getHeader :: Text -> HtmlUrl Route
getHeader pageTitle = $(hamletFile "./src/ExHack/Renderer/templates/header.hamlet")

menu :: HtmlUrl Route
menu = $(hamletFile "./src/ExHack/Renderer/templates/menu.hamlet")

homePageTemplate :: [HomePagePackage] -> HtmlUrl Route
homePageTemplate packages = 
    $(hamletFile "./src/ExHack/Renderer/templates/homePage.hamlet")
  where
    header = getHeader "The Haskell Examples Database"

packagePageTemplate :: HomePagePackage -> [ModuleName] ->  HtmlUrl Route
packagePageTemplate pack@(HomePagePackage (PackageName (_,pn)) _) mods = 
    $(hamletFile "./src/ExHack/Renderer/templates/packagePage.hamlet")
  where
    header = getHeader $ pn <> " usage examples"

modulePageTemplate :: HomePagePackage -> ModuleName -> [SymbolOccurs] -> HtmlUrl Route
modulePageTemplate _ (ModuleName (_,mname)) soccs = 
    $(hamletFile "./src/ExHack/Renderer/templates/modulePage.hamlet")
  where
    header = getHeader $ mname <> " usage examples"
