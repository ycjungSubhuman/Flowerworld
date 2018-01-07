module MapDSL.DSLAst where
    type Program = Stmt

    newtype Expr a = Expr [Stmt]
    data Stmt = TitleStmt Title
                | TypeStmt Type
                | PatternStmt Pattern
                | BlockStmt Block
                deriving (Eq, Show)

    type Title = String
    type Type = MapType
    type Block = [[Cell]]
    type Cell = [Label]
    type Pattern = [Label]

    data MapType = NORMAL | CONSTRUCT | DODGE
                    deriving (Eq, Show)

    data Label = START
                | A | B | C | D | E | F | G
                | EMPTY
                | ANY
                | GOAL
                deriving (Eq, Show)
