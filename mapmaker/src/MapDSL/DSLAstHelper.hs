module MapDSL.DSLAstHelper where
    import MapDSL.DSLAst
    import Data.Maybe

    stmtToTitle :: Stmt -> Maybe Title
    stmtToTitle stmt =
        case stmt of
            TitleStmt title -> Just title
            _ -> Nothing

    stmtToPattern :: Stmt -> Maybe Pattern
    stmtToPattern stmt = 
        case stmt of
            PatternStmt pattern -> Just pattern
            _ -> Nothing

    stmtToGoalCount :: Stmt -> Maybe GoalCount
    stmtToGoalCount stmt = 
        case stmt of
            GoalCountStmt goalCount -> Just goalCount
            _ -> Nothing

    stmtToBlock :: Stmt -> Maybe Block
    stmtToBlock stmt =
        case stmt of
            BlockStmt t -> Just t
            _ -> Nothing
    stmtToItems :: Stmt -> Maybe Items
    stmtToItems stmt =
        case stmt of
            ItemStmt items -> Just items
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
    commentOf :: Expr a -> Comment
    commentOf expr =
        let comments = extractMultipleField stmtToComment expr
        in let lineCount = length comments
        in case lineCount of
            0 -> ""
            _ -> head comments

    patternOf :: Expr a -> Pattern
    patternOf expr = extractSingleField stmtToPattern expr

    itemsOf :: Expr a -> Items
    itemsOf expr =
        let itemStmtLines = extractMultipleField stmtToItems expr
        in let lineCount = length itemStmtLines
        in case lineCount of
            0 -> (0, [0, 0, 0, 0], 0)
            _ -> head itemStmtLines

    goalCountOf :: Expr a -> GoalCount
    goalCountOf expr = extractSingleField stmtToGoalCount expr

    blocksOf :: Expr a -> [Block]
    blocksOf expr = extractMultipleField stmtToBlock expr
