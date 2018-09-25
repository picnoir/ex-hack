{-# LANGUAGE TemplateHaskell #-}
module ExHack.Renderer.Html (
    renderHomePage,
    renderModulePage,
    renderPackagePage
) where

import           Text.Hamlet (HtmlUrl, hamletFile)

type Route = String

getHeader :: String -> HtmlUrl Route
getHeader pageTitle = $(hamletFile "./src/ExHack/Renderer/templates/header.hamlet")

menu :: HtmlUrl Route
menu = $(hamletFile "./src/ExHack/Renderer/templates/menu.hamlet")

renderHomePage :: HtmlUrl Route
renderHomePage = $(hamletFile "./src/ExHack/Renderer/templates/homePage.hamlet")
    where
        packages = undefined :: [(String, String, String)]
        header   = getHeader "The Haskell Examples Database"

renderPackagePage :: HtmlUrl Route
renderPackagePage = $(hamletFile "./src/ExHack/Renderer/templates/packagePage.hamlet")
    where
        header = getHeader "Title"
        mods   = undefined :: [String]
        pname  = undefined :: String
        pdesc  = undefined :: String

renderModulePage :: HtmlUrl Route
renderModulePage = $(hamletFile "./src/ExHack/Renderer/templates/modulePage.hamlet")
    where
        header = getHeader "ToTitle"
        syms = undefined :: [(String, String, String)]
        mname = undefined :: String
