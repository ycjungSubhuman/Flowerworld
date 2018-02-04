module MapDSL.DSLParser(
    parseMapText
    ) where
    import Text.ParserCombinators.Parsec hiding (spaces, space)
    import Text.Parsec.Char hiding (spaces, space)
    import MapDSL.DSLAst
    import MapDSL.DSLValidate
    import Control.Applicative ((<$), (<$>))

    -- Custom 'spaces' for horizontal spaces only
    spaces :: CharParser st ()
    spaces = () <$ many space

    space :: CharParser st ()
    space = () <$ (oneOf " \t")

    spacesOrLineEnds :: CharParser st ()
    spacesOrLineEnds = () <$ many (try space <|> (() <$ endOfLine))

    parseMapText :: String -> Either String (Expr Validated)
    parseMapText input = 
        let unvalidated = parse stmtListParser "" input
        in validate unvalidated

    stmtListParser :: CharParser st (Expr Unvalidated)
    stmtListParser =
        do 
            manyStmt <- many singleStmtParser
            spacesOrLineEnds
            eof
            return $ Expr manyStmt

    singleStmtParser :: CharParser st Stmt
    singleStmtParser = 
        try titleStmtParser <|> try goalCountStmtParser <|> try patternStmtParser <|> try blockStmtParser
        <?> "single statement"

    titleStmtParser :: CharParser st Stmt
    titleStmtParser =
        do 
            spacesOrLineEnds
            string "title"
            space
            title <- many (noneOf "\r\n")
            return $ TitleStmt title

    goalCountStmtParser :: CharParser st Stmt
    goalCountStmtParser =
        do 
            spacesOrLineEnds
            string "goalcount"
            space
            t <- goalCountParser
            return $ GoalCountStmt t
    goalCountParser :: CharParser st Int
    goalCountParser = read <$> many1 digit

    patternStmtParser :: CharParser st Stmt
    patternStmtParser =
        do 
            spacesOrLineEnds
            string "pattern"
            space
            labels <- attrParser `sepBy` (spaces >> char ',' >> spaces)
            return $ PatternStmt labels

    blockStmtParser :: CharParser st Stmt
    blockStmtParser = 
        do 
            spacesOrLineEnds
            string "block"
            mat <- matParser
            return $ BlockStmt mat

    matParser :: CharParser st [[Cell]]
    matParser = filter (not . null) <$> rowParser `sepBy` (many1 endOfLine)

    rowParser :: CharParser st [Cell]
    rowParser = do
        row <- many (try cellParser)
        spaces
        return row

    cellParser :: CharParser st Cell
    cellParser = do
        cell <- labelParser 
        (char '|')
        return cell

    labelParser :: CharParser st Cell
    labelParser = do
        spaces
        label <- attrParser `sepBy` try (spaces >> char ',' >> spaces)
        spaces
        return $ if null label then [EMPTY] else label

    attrParser :: CharParser st Label
    attrParser = 
        START <$ try (string "^")
        <|> ANY <$ try (string "*")
        <|> A <$ string "A"
        <|> B <$ string "B"
        <|> C <$ string "C"
        <|> D <$ string "D"
        <|> E <$ string "E"
        <|> F <$ string "F"
        <|> GOAL <$ try (string "$")
        <|> G <$ string "G"





