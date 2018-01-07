module MapDSL.DSLAstHelper where
    import MapDSL.DSLAst
    import Data.Maybe

    stmtToTitle :: Stmt -> Maybe Title
    stmtToTitle stmt =
        case stmt of
            TitleStmt title -> Just title
            _ -> Nothing

    stmtToMapType :: Stmt -> Maybe MapType
    stmtToMapType stmt = 
        case stmt of
            TypeStmt t -> Just t
            _ -> Nothing

    stmtToPattern :: Stmt -> Maybe Pattern
    stmtToPattern stmt = 
        case stmt of
            PatternStmt pattern -> Just pattern
            _ -> Nothing

    stmtToBlock :: Stmt -> Maybe Block
    stmtToBlock stmt =
        case stmt of
            BlockStmt t -> Just t
            _ -> Nothing

    extractSingleField :: (Stmt -> Maybe a) -> Expr b -> a
    extractSingleField f expr = 
            head $ extractMultipleField f expr

    extractMultipleField :: (Stmt -> Maybe a) -> Expr b -> [a]
    extractMultipleField f expr =
        let Expr stmts = expr
        in 
            catMaybes $ map (\stmt -> do
            v <- f stmt
            return v) stmts

    titleOf :: Expr a -> Title
    titleOf expr = extractSingleField stmtToTitle expr

    mapTypeOf :: Expr a -> MapType
    mapTypeOf expr = extractSingleField stmtToMapType expr

    patternOf :: Expr a -> Pattern
    patternOf expr = extractSingleField stmtToPattern expr

    blocksOf :: Expr a -> [Block]
    blocksOf expr = extractMultipleField stmtToBlock expr
