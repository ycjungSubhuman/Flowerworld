module MapDSL.DSLAst where
    type Program = Stmt

    newtype Expr a = Expr [Stmt]
    data Stmt = TitleStmt Title
                | GoalCountStmt GoalCount
                | PatternStmt Pattern
                | ItemStmt Items
                | BlockStmt Block
                deriving (Eq, Show)

    type GoalCount = Int
    type Title = String
    type Block = [[Cell]]
    type Items = (Int, [Int], Int)
    type Cell = [Label]
    type Pattern = [Label]

    data Label = START
                | A | B | C | D | E | F | G
                | EMPTY
                | ANY
                | GOAL
                | Ga | Gb | Gc | Gd 
                | Sp
                deriving (Eq, Show)
