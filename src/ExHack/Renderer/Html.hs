{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ExHack.Renderer.Html (
    renderHomePage,
    renderModulePage,
    renderPackagePage
) where

import           Data.Text             (Text)
import           Text.Hamlet           (HtmlUrl, hamletFile)

import           ExHack.Renderer.Types (HomePagePackage (..), ModuleName,
                                        Route (..), SymbolOccurs (..))
import           ExHack.Types          (SourceCodeFile (..))

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
renderPackagePage pack@(HomePagePackage pn _) mods = 
    $(hamletFile "./src/ExHack/Renderer/templates/packagePage.hamlet")
  where
    header = getHeader $ pn <> " usage examples"

renderModulePage :: HomePagePackage -> ModuleName -> [SymbolOccurs] -> HtmlUrl Route
renderModulePage _ mname soccs = 
    $(hamletFile "./src/ExHack/Renderer/templates/modulePage.hamlet")
  where
    header = getHeader $ mname <> " usage examples"
