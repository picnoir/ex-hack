{-|
Module      : ExHack.Renderer.Html
Description : HTML renderer.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module ExHack.Renderer.Html (
    addLineMarker,
    highLightCode,
    homePageTemplate,
    modulePageTemplate,
    packagePageTemplate
) where

import           Control.Monad.Catch    (MonadMask, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T (Text, lines, pack, unlines,
                                              unpack)
import           Safe                   (headMay)
import           System.Exit            (ExitCode (..))
import           System.Process         (readProcessWithExitCode)
import           Text.Blaze.Html        (preEscapedToHtml)
import           Text.Hamlet            (HtmlUrl, hamletFile)

import           ExHack.Renderer.Types  (HighLightError (..),
                                         HighlightedSourceCodeFile (..),
                                         HighlightedSymbolOccurs (..),
                                         HomePagePackage (..), ModuleName (..),
                                         PackageName (..), Route (..))
import           ExHack.Types           (ModuleNameT (..), PackageNameT (..))

-- | Highlights the source file using pygments.
highLightCode :: forall m. (MonadIO m, MonadMask m) => T.Text -> m T.Text
highLightCode t = do
    (ec,out,err) <- liftIO $ readProcessWithExitCode 
        "pygmentize" 
        ["-l", "haskell", "-f", "html"] 
        $ T.unpack t
    case ec of
        ExitSuccess -> pure $ T.pack out
        ExitFailure _ -> throwM $ HighLightError err

-- | Adds an arrow pointing to the selected symbol.
addLineMarker :: Int -> T.Text -> T.Text
addLineMarker line t = let slm = headMay end
    in maybe t (\l -> T.unlines $ start <> [wrapL l] <> drop 1 end) slm  
  where
    xs = T.lines t
    start = take (line - 1) xs
    end = drop (line - 1) xs
    wrapL txt = "<span class=\"occ-line\">" <> txt <> "</span>" 

getHeader :: T.Text -> HtmlUrl Route
getHeader pageTitle = $(hamletFile "./src/ExHack/Renderer/templates/header.hamlet")

menu :: HtmlUrl Route
menu = $(hamletFile "./src/ExHack/Renderer/templates/menu.hamlet")

-- | Template rendered in order to create the home page.
homePageTemplate :: [HomePagePackage] -> HtmlUrl Route
homePageTemplate packages = 
    $(hamletFile "./src/ExHack/Renderer/templates/homePage.hamlet")
  where
    header = getHeader "The Haskell Examples Database"

-- | Template rendered in order to create a package's page.
packagePageTemplate :: HomePagePackage -> [ModuleName] ->  HtmlUrl Route
packagePageTemplate pack@(HomePagePackage (PackageName (_,pn)) _) mods = 
    $(hamletFile "./src/ExHack/Renderer/templates/packagePage.hamlet")
  where
    header = getHeader $ pn <> " usage examples"

-- | Template rendered in order to create a module's page.
modulePageTemplate :: HomePagePackage -> ModuleName -> [HighlightedSymbolOccurs] -> HtmlUrl Route
modulePageTemplate _ (ModuleName (_,mname)) soccs = 
    $(hamletFile "./src/ExHack/Renderer/templates/modulePage.hamlet")
  where
    header = getHeader $ mname <> " usage examples"
