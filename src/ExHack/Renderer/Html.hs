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

highLightCode :: forall m. (MonadIO m, MonadMask m) => T.Text -> m T.Text
highLightCode t = do
    (ec,out,err) <- liftIO $ readProcessWithExitCode 
        "/nix/store/w7p41gs88mkpk8k0bwbgg7bc3970fpbl-python3.6-Pygments-2.2.0/bin/pygmentize" ["-l", "haskell", "-f", "html"] $ T.unpack t
    case ec of
        ExitSuccess -> pure $ T.pack out
        ExitFailure _ -> throwM $ HighLightError err

addLineMarker :: Int -> T.Text -> T.Text
addLineMarker line t = let (_, nt) = (\l -> foldr replaceOcc (length l - 1, []) l) $ T.lines t
                        in T.unlines nt
  where
    replaceOcc :: T.Text -> (Int, [T.Text]) -> (Int, [T.Text])
    replaceOcc x (i, xs) = if i /= line then (i-1, x:xs) else (i-1, (wrapL x):xs)
    wrapL l = "<span class=\"occ-line\">" <> l <> "</span>"

getHeader :: T.Text -> HtmlUrl Route
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

modulePageTemplate :: HomePagePackage -> ModuleName -> [HighlightedSymbolOccurs] -> HtmlUrl Route
modulePageTemplate _ (ModuleName (_,mname)) soccs = 
    $(hamletFile "./src/ExHack/Renderer/templates/modulePage.hamlet")
  where
    header = getHeader $ mname <> " usage examples"
