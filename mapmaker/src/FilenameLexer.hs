module FilenameLexer (lexFilename) where
    import Text.ParserCombinators.Parsec
    import Text.Parsec.Char


    lexFilename :: String -> Maybe (String, String)
    lexFilename name = 
        case parse lexer "" name of
            Right result -> Just result
            _ -> Nothing 
        
    lexer :: CharParser st (String, String)
    lexer = do
        string "map-"
        world <- many1 digit
        string "-"
        stage <- many1 digit
        return (world, stage)

