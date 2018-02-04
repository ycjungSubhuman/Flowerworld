module MapDSL.DSLAst where
    type Program = Stmt

    newtype Expr a = Expr [Stmt]
    data Stmt = TitleStmt Title
                | GoalCountStmt GoalCount
                | PatternStmt Pattern
                | BlockStmt Block
                deriving (Eq, Show)

    type GoalCount = Int
    type Title = String
    type Block = [[Cell]]
    type Cell = [Label]
    type Pattern = [Label]

    data Label = START
                | A | B | C | D | E | F | G
                | EMPTY
                | ANY
                | GOAL
                deriving (Eq, Show)
