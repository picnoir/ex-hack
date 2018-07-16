module Log ( 
  logProgress,
  logTitle
) where


logProgress :: String -> String -> IO ()
logProgress prefix l = putStrLn (prefix ++ l)

logTitle :: String -> IO ()
logTitle txt = line >> putStrLn ("* " ++ txt ++ " *") >> line
  where line = putStrLn (replicate (length txt + 4) '*')
