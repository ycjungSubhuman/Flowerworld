{-# LANGUAGE OverloadedStrings #-}
module MapDSL.DSLValidate where
    import MapDSL.DSLAst
    import MapDSL.DSLAstHelper
    import Data.Text.Format
    import Text.ParserCombinators.Parsec (ParseError)
    import Data.List
    import Data.Maybe
    import Control.Applicative

    data Unvalidated
    data Validated

    titleErrorMsg :: Int -> String
    titleErrorMsg actualCount = 
        show $ format "It has {} 'title' statements. should be 1." [actualCount]

    mapTypeErrorMsg :: Int -> String
    mapTypeErrorMsg actualCount =
        show $ format "It has {} 'title' statements. should be 1." [actualCount]

    patternCountErrorMsg :: Int -> String
    patternCountErrorMsg actualCount =
        show $ format "It has {} 'pattern' statements. should be 1." [actualCount]

    blockCountErrorMsg :: MapType -> Int -> String
    blockCountErrorMsg mapType actualCount =
        show $ format "Map type is {}, but the map contains {} map blocks" [show mapType, show actualCount]

    startCountErrorMsg :: Int -> String
    startCountErrorMsg actualCount = 
        show $ format "The map contains {} STARTs. should be 1" [actualCount]

    goalCountErrorMsg :: MapType -> Int -> String
    goalCountErrorMsg mapType actualCount = 
        show $ format "Map type is {}, but the map contains {} GOALs." [show mapType, show actualCount]

    blockSizeErrorMsg = "One or more blocks have zero rows. should have at least one row" :: String
    badPatternErrorMsg = 
        "The pattern contains invalid labels. Each of them should be one of " ++ (show concreteLabels) :: String
    invalidMapHeader = "[Invalid Map Statement] " :: String
    parseErrorHeader = "[Parse Error] " :: String

    checks = [
        hasOneTitle, 
        sequenceCheck hasOnePattern hasValidPattern,
        sequenceCheck hasOneMapType $ 
            sequenceCheck hasValidBlockCount $
                sequenceCheck hasValidBlockSize $
                    sequenceCheck hasValidStart hasValidGoal
            ]

    sequenceCheck :: (Expr a -> Maybe String) -> (Expr a -> Maybe String) -> Expr a -> Maybe String
    sequenceCheck checkA checkB expr = 
        let a = checkA expr
        in case a of
            Just msg -> Just msg
            Nothing -> checkB expr

    hasOne :: (Stmt -> Maybe b) -> (Int -> String) -> Expr a -> Maybe String
    hasOne singleAttrGen msgGen expr =
        let count = length $ extractMultipleField singleAttrGen expr
        in let valid = 1 == count
        in if valid 
            then Nothing 
            else Just $ msgGen count

    hasOneTitle :: Expr a -> Maybe String
    hasOneTitle expr = hasOne stmtToTitle titleErrorMsg expr

    hasOneMapType :: Expr a -> Maybe String
    hasOneMapType expr = hasOne stmtToMapType mapTypeErrorMsg expr

    concreteLabels = [A, B, C, D, E, F, G]

    hasOnePattern :: Expr a -> Maybe String
    hasOnePattern expr = hasOne stmtToPattern patternCountErrorMsg expr

    hasValidPattern :: Expr a -> Maybe String
    hasValidPattern expr =
        let pattern = patternOf expr
        in let valid = all (\p -> elem p concreteLabels) pattern
        in if valid
            then Nothing
            else Just $ badPatternErrorMsg

    hasValidBlockCount :: Expr a -> Maybe String
    hasValidBlockCount expr =
        let mapType = mapTypeOf expr
        in let isConstruct = mapType == CONSTRUCT  
        in let isValidCount = if isConstruct then (>=1) else (==1)
        in let blockCount = length $ blocksOf expr
        in let valid = isValidCount blockCount
        in if valid
            then Nothing
            else Just $ blockCountErrorMsg mapType blockCount

    hasValidBlockSize :: Expr a -> Maybe String
    hasValidBlockSize expr =
        let blockRowCounts = length `map` blocksOf expr
        in let valid = all (>0) blockRowCounts 
        in if valid
            then Nothing
            else Just $ blockSizeErrorMsg

    hasValidStart :: Expr a -> Maybe String
    hasValidStart expr =
        let blocks = blocksOf expr
        in let cells = flattenBlocks blocks
        in let starts = (==START) `filter` cells
        in let count = length starts
        in let valid = 1 == count
        in if valid
            then Nothing
            else Just $ startCountErrorMsg count

    hasValidGoal :: Expr a -> Maybe String
    hasValidGoal expr =
        let blocks = blocksOf expr
        in let cells = flattenBlocks blocks
        in let goals = (==GOAL) `filter` cells
        in let mapType = mapTypeOf expr
        in let isValidCount = if mapType==DODGE then (==0) else (==1)
        in let count = length goals
        in let valid = isValidCount count
        in if valid
            then Nothing
            else Just $ goalCountErrorMsg mapType count

    flattenBlocks bs = (concat . concat . concat) bs

    validate :: (Either ParseError (Expr Unvalidated)) -> Either String (Expr Validated)
    validate (Right expr) =
        let checkResults = (\c -> c expr) `map` checks
        in let errorStrings = (\s -> invalidMapHeader ++ s) `map` (catMaybes checkResults)

        in case errorStrings of
            [] -> 
                let Expr stmts = expr
                in Right $ Expr stmts
            errors ->  Left $ intercalate "\n" errors

    validate (Left e) = Left $ parseErrorHeader ++ (show e)

