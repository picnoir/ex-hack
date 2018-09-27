{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative    (execParser, failureCode, help, info,
                                         long, metavar, short, strOption,
                                         switch, value)
import           System.Directory       (XdgDirectory (XdgData),
                                         createDirectoryIfMissing,
                                         getXdgDirectory, listDirectory,
                                         removeDirectoryRecursive)
import           System.FilePath        ((</>))


import           ExHack.ProcessingSteps (dlAssets, genGraphDep, generateDb,
                                         generateHtmlPages, indexSymbols,
                                         parseStackage, retrievePkgsExports,
                                         saveGraphDep)
import           ExHack.Types           (CabalFilesDir (..), Config (..),
                                         DatabaseHandle, DatabaseStatus (..),
                                         StackageFile (..), TarballsDir (..),
                                         WorkDir (..), getDatabaseHandle,
                                         newDatabaseHandle, runStep)

main :: IO ()
main = do
    c <- parseOpts
    dbInit <- shouldBypassDBInit (_dbHandle c) $ runStep generateDb c
    let ci = c {_dbHandle= dbInit} :: Config 'Initialized
    descs <- runStep parseStackage ci
    shouldBypassAssetsDl (_tarballsDir c) (_cabalFilesDir c) $ runStep (dlAssets descs) ci 
    pkgs <- runStep (genGraphDep descs) ci
    dbGraph <- shouldBypassGraphDepsGen dbInit $ runStep (saveGraphDep pkgs) ci
    let cg = ci {_dbHandle=dbGraph} :: Config 'DepsGraph
    (dbExprt,pe) <- runStep (retrievePkgsExports pkgs) cg
    let ce = cg {_dbHandle=dbExprt} :: Config 'PkgExports
    dbIdx <- runStep (indexSymbols pe) ce
    let cidx = ce {_dbHandle=dbIdx} :: Config 'IndexedSyms
    runStep generateHtmlPages cidx
    pure ()

defaultConf :: IO (Config 'New)
defaultConf = do
    dataDir <- getXdgDirectory XdgData "ex-hack"
    let tarballs = dataDir </> "tarballs"
        cabal    = dataDir </> "cabal-files"
        workdir  = dataDir </> "workdir"
    createDirectoryIfMissing True tarballs
    createDirectoryIfMissing True cabal
    createDirectoryIfMissing True workdir
    pure $ Config (newDatabaseHandle $ dataDir </> "database.sqlite")
                  (StackageFile "./data/lts-10.5.yaml") 
                  (TarballsDir tarballs) 
                  (CabalFilesDir cabal)
                  (WorkDir workdir)
                  True

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
            <*> switch 
                (long "create-dirs"
                <> short 'c'
                <> help "Create the tarball, cabal and working directories if they do not exists on the filesystem")
    execParser $ info parser (failureCode 1) 
