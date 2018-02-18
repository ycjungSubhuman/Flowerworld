{-# LANGUAGE Rank2Types #-}
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
            <|> try itemStmtParser
        <?> "single statement"

    simpleFieldParser :: String -> (forall st. CharParser st b) -> (b -> Stmt) -> CharParser st Stmt
    simpleFieldParser keyword innerParser stmtBuilder =
        do
            spacesOrLineEnds
            string keyword
            space
            inner <- innerParser
            spacesOrLineEnds
            return $ stmtBuilder inner

    itemStmtParser = simpleFieldParser "item" itemParser (\items -> ItemStmt items)
    itemParser :: CharParser st Items
    itemParser =
        do
            springs <- read <$> many1 digit
            string "/("
            glassCounts <- (read <$> many1 digit) `sepBy` (spaces >> char ',' >> spaces)
            string ")/"
            watches <- read <$> many1 digit
            return (springs, glassCounts, watches)

    titleStmtParser = simpleFieldParser "title" (many $ noneOf "\r\n") (\title -> TitleStmt title)

    goalCountStmtParser = simpleFieldParser "goalcount" goalCountParser (\gc -> GoalCountStmt gc)
    goalCountParser :: CharParser st Int
    goalCountParser = read <$> many1 digit

    patternStmtParser :: CharParser st Stmt
    patternStmtParser = simpleFieldParser "pattern" patternParser (\pattern -> PatternStmt pattern)
    patternParser :: CharParser st Pattern
    patternParser = patternLabelParser `sepBy` (spaces >> char ',' >> spaces)
    patternLabelParser :: CharParser st Label
    patternLabelParser =
        do
            l <- attrParser
            spaces
            return l

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





