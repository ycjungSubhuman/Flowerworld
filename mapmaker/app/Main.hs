module Main where
    import Options.Applicative
    import Data.Semigroup ((<>))
    import System.IO
    import System.Directory
    import Data.Time.Format
    import Data.Time
    import Data.Aeson
    import System.FilePath.Windows
    import qualified Data.ByteString.Lazy as BS
    import qualified MapData as M
    import System.FSNotify
    import Control.Concurrent (threadDelay)
    import Control.Monad (forever)
    import FilenameLexer

    data Args = Compile FilePath FilePath
                | CompileAll FilePath FilePath
                | Watch FilePath FilePath

    argumentParser :: Parser Args
    argumentParser = subparser
        ( command "compile"
            (info (parser Compile "src" "dst") 
                (progDesc "Compile a .map file to .json"))
        <> command "compileall"
            (info (parser CompileAll "srcDir" "dstDir") 
                (progDesc "Compile All .map in srcDir to .json in dstDir."))
        <> command "watch"
            (info (parser Watch "srcDir" "dstDir") 
                (progDesc "While running, automatically compile all .map files in srcDir to .json files in dstDir"))
        )

    parser t input output = t 
        <$> strOption 
            ( short 'i'
            <> metavar input)
        <*> strOption
            ( short 'o'
            <> metavar output)

    main :: IO ()
    main = program =<< execParser (info (helper <*> argumentParser) (progDesc "A Compiler for Map Making DSL"))

    program :: Args -> IO ()
    program args = case args of
        Compile src dst -> compile src dst
        CompileAll srcDir dstDir -> compileAll srcDir dstDir
        Watch srcDir dstDir -> watch srcDir dstDir

    watch :: FilePath -> FilePath -> IO ()
    watch srcDir dstDir = do
        -- First compile all sources in 'srcDir'
        putStrLn $ "Compiling All Maps in " ++ srcDir ++ " to " ++ dstDir ++ "..."
        compileAll srcDir dstDir
        -- Start watching
        putStrLn $ "Started watching " ++ srcDir ++ "..."
        withManager $ \mgr -> do
            watchDir mgr srcDir (const True) (update dstDir)
            forever $ threadDelay 1000000

    update :: FilePath -> Event -> IO ()
    update dstDir event =
        case event of
            Added src t -> do
                if ".map" == (takeExtension src) then do
                    let dst = src2dst src dstDir
                    putStrLn $ "<" ++ (readDayTime t) ++ ">" ++ " File Added " ++ src
                    threadDelay 1000
                    compile src dst
                else return ()
            Modified src t -> do
                if ".map" == (takeExtension src) then do
                    let dst = src2dst src dstDir
                    putStrLn $ "<" ++ (readDayTime t) ++ ">" ++ " File Modified " ++ src
                    compile src dst
                else return ()
            Removed src t -> do
                if ".map" == (takeExtension src) then do
                    let dst = src2dst src dstDir
                    putStrLn $ "<" ++ (readDayTime t) ++ ">" ++ " File Removed " ++ src
                    removeFile dst
                else return ()

    compileAll :: FilePath -> FilePath -> IO()
    compileAll srcDir dstDir = do
        oldFiles <- listDirectory dstDir
        (\f -> removeFile $ dstDir </> f) `mapM_` oldFiles
        files <- listDirectory srcDir
        let targets = (\f -> (srcDir </> f, src2dst f dstDir)) `map` ((\f -> ".map" == (takeExtension f)) `filter` files)
        (\(src, dst) -> compile src dst) `mapM_` targets

    src2dst :: FilePath -> FilePath -> FilePath
    src2dst src dstDir = dstDir </> (takeBaseName src) <.> "json"

    src2worldstage :: FilePath -> (String, String)
    src2worldstage src = 
        let base = takeBaseName src
        in case lexFilename base of
            Just result -> result
            Nothing -> ("0", "0")

    readDayTime :: UTCTime -> String
    readDayTime t = formatTime defaultTimeLocale "%T" t

    compile :: FilePath -> FilePath -> IO ()
    compile src dst = do
        putStrLn $ "Compiling " ++ src ++ ": "
        srcHandle <- openFile src ReadMode
        hSetEncoding srcHandle utf8_bom
        srcString <- hGetContents srcHandle
        let (world, stage) = src2worldstage src
        let res = encode `fmap` M.src2map (world,stage) srcString
        case res of
            Right m -> 
                putStrLn "Success" >> BS.writeFile dst m
            Left e -> putStrLn e
        hClose srcHandle

