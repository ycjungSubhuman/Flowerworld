module Main where
    import Options.Applicative
    import Data.Semigroup ((<>))
    import System.IO
    import Data.Aeson
    import qualified Data.ByteString.Lazy as BS
    import qualified MapData as M

    data Args = Compile FilePath FilePath
                | Watch FilePath FilePath

    argumentParser :: Parser Args
    argumentParser = subparser
        ( command "compile"
            (info (parser Compile "src" "dst") 
                (progDesc "Compiles a .map file to .json"))
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
        Watch srcDir dstDir -> watch srcDir dstDir

    watch :: FilePath -> FilePath -> IO ()
    watch srcDir dstDir = putStrLn "wow"

    compile :: FilePath -> FilePath -> IO ()
    compile src dst = do
        putStrLn $ "Compiling " ++ src ++ ": "
        srcHandle <- openFile src ReadMode
        hSetEncoding srcHandle utf8_bom
        srcString <- hGetContents srcHandle
        let res = encode `fmap` M.src2map srcString
        case res of
            Right m -> 
                putStrLn "Success" >> BS.writeFile dst m
            Left e -> putStrLn e
        hClose srcHandle

