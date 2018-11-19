{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens.Getter    ((^.))
import           Options.Applicative    (execParser, failureCode, help, helper,
                                         info, long, metavar, short, strOption,
                                         value, (<**>))
import           System.Directory       (XdgDirectory (XdgData),
                                         createDirectoryIfMissing,
                                         getXdgDirectory, listDirectory,
                                         removeDirectoryRecursive)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (NoBuffering),
                                         hSetBuffering, stdin, stdout)


import           ExHack.ProcessingSteps (dlAssets, genGraphDep, generateDb,
                                         generateHtmlPages, indexSymbols,
                                         parseStackage, retrievePkgsExports,
                                         saveGraphDep)
import           ExHack.Types           (CabalFilesDir (..), Config (..),
                                         DatabaseHandle, DatabaseStatus (..),
                                         HtmlDir (..), StackageFile (..),
                                         TarballsDir (..), WorkDir (..),
                                         cabalFilesDir, getDatabaseHandle,
                                         htmlDir, newDatabaseHandle, runStep,
                                         tarballsDir, workDir)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    c <- parseOpts
    createConfigDirs c
    dbInit <- shouldBypassDBInit (_dbHandle c) $ runStep generateDb c
    let ci = c {_dbHandle= dbInit}  :: Config 'Initialized
    descs <- runStep parseStackage ci
    shouldBypassAssetsDl (_tarballsDir c) (_cabalFilesDir c) $ runStep (dlAssets descs) ci 
    !pkgs <- runStep (genGraphDep descs) ci
    dbGraph <- shouldBypassGraphDepsGen dbInit $ runStep (saveGraphDep pkgs) ci
    let cg = ci {_dbHandle=dbGraph} :: Config 'DepsGraph
    dbExprt <- runStep (retrievePkgsExports pkgs) cg
    let ce = cg {_dbHandle=dbExprt} :: Config 'PkgExports
    dbIdx <- runStep (indexSymbols pkgs) ce
    let cidx = ce {_dbHandle=dbIdx} :: Config 'IndexedSyms
    runStep generateHtmlPages cidx
    pure ()

createConfigDirs :: Config 'New -> IO ()
createConfigDirs c = do
    let TarballsDir tbd   = c ^. tarballsDir
        CabalFilesDir cbd = c ^. cabalFilesDir
        WorkDir wd        = c ^. workDir
        HtmlDir htd       = c ^. htmlDir
    createDirectoryIfMissing True tbd
    createDirectoryIfMissing True cbd
    createDirectoryIfMissing True wd
    createDirectoryIfMissing True htd

shouldBypassDBInit :: DatabaseHandle 'New -> IO (DatabaseHandle 'Initialized) -> IO (DatabaseHandle 'Initialized)
shouldBypassDBInit dbh s =
    promptUser "Do you wanna empty and init the database?" 
               s
               (pure . snd $ getDatabaseHandle dbh)

shouldBypassAssetsDl :: TarballsDir -> CabalFilesDir -> IO () -> IO ()
shouldBypassAssetsDl (TarballsDir fpt) (CabalFilesDir fpc) s = do
    dirT <- listDirectory fpt
    dirC <- listDirectory fpc
    if null (dirT <> dirC) 
        then s 
        else promptUser "Your assets folder is not empty. Do you want to empty it and re-download everything?"
                        (removeDirectoryRecursive fpt >> removeDirectoryRecursive fpc >> s)
                        (pure ())

shouldBypassGraphDepsGen :: DatabaseHandle 'Initialized -> IO (DatabaseHandle 'DepsGraph) -> IO (DatabaseHandle 'DepsGraph)
shouldBypassGraphDepsGen h s =
    promptUser "Do you wanna save the dependancy graph to the db?"
               s
               (pure . snd $ getDatabaseHandle h)

promptUser :: String -> IO a -> IO a -> IO a
promptUser str true false = do
    putStrLn (str <> " [y/N]")
    res <- getLine
    if null res || head (words res) == "n" || head (words res) == "N"
        then false
        else true

parseOpts :: IO (Config 'New)
parseOpts = do
    dataDir <- getXdgDirectory XdgData "ex-hack"
    let dfttarballs = dataDir </> "tarballs"
        dftcabal    = dataDir </> "cabal-files"
        dftworkdir  = dataDir </> "workdir"
        dftdb       = dataDir </> "database.sqlite"
        dftoutdir   = dataDir </> "output"
    let parser = Config 
            <$> (newDatabaseHandle <$> strOption
                (long "database-filepath"
                <> short 'd'
                <> value dftdb
                <> metavar "DB_FILEPATH"
                <> help "Path to the database file"))
            <*> (StackageFile <$> strOption
                (long "stackage-filepath"
                <> short 's'
                <> metavar "STACKAGE_FILE"
                <> help "Stackage build plan file"))
            <*> (TarballsDir <$> strOption
                (long "tarballs-directory"
                <> short 't'
                <> value dfttarballs
                <> metavar "TARBALLS_DIR"
                <> help "Directory in which the tarballs will be downloaded"))
            <*> (CabalFilesDir <$> strOption
                (long "cabal-files-directory"
                <> short 'c'
                <> value dftcabal
                <> metavar "CABALFILES_DIR"
                <> help "Directory in which the cabal files will be downloaded"))
            <*> (WorkDir <$> strOption
                (long "working-directory"
                <> short 'w'
                <> value dftworkdir
                <> metavar "WORKDIR"
                <> help "Directory used to unpack/build the packages"))
            <*> (HtmlDir <$> strOption
                (long "output-directory"
                <> short 'w'
                <> value dftoutdir
                <> metavar "OUTDIR"
                <> help "Directory where exhack HTML documentation will be saved"))
    execParser $ info (parser <**> helper) (failureCode 1) 
