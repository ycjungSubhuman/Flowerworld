{-# LANGUAGE DeriveGeneric #-}
module MapData(src2map) where
    import GHC.Generics
    import Data.Aeson
    import qualified MapDSL.DSLAst as Ast
    import qualified Data.Map as Dict
    import MapDSL.DSLAstHelper
    import MapDSL.DSLValidate
    import MapDSL.DSLParser

    instance ToJSON Map where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON MapBlock where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON Cell where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON Label where
        toEncoding = genericToEncoding defaultOptions

    src2map :: (String, String) -> String -> Either String Map
    src2map (world,stage) srcString = (expr2map (world,stage)) `fmap` parseMapText srcString

    {- Map Data Representation for Deserialization in Unity -}
    data Map = Map {
            main :: MapBlock,
            subMaps :: [MapBlock],
            title :: String,
            world :: String,
            stage :: String,
            goalCount :: Int,
            springsAvailable :: Int,
            glassAvailable :: Dict.Map String Int,
            watchesAvailable :: Int,
            pattern :: [Label]
        } deriving (Generic, Show)

    data MapBlock = MapBlock {
            mat :: [[Cell]]
        } deriving (Generic, Show)

    data Cell = Cell {
            label :: Label
        } deriving (Generic, Show)

    data Label = Label {
            value :: Int
        } deriving (Generic, Show)

    expr2map :: (String,String) -> Ast.Expr Validated -> Map
    expr2map (world,stage) expr =
        let title = titleOf expr
        in let astPattern = patternOf expr
        in let astBlocks = blocksOf expr
        in let goalCount = goalCountOf expr
        in let mapblocks = astblock2mapblock `map` astBlocks
        in let (springsAvailable, glassCounts, watchesAvailable) = itemsOf expr
        in let glassAvailable = glassCountList2Map glassCounts
        in let (main:submaps) = mapblocks
        in let pattern = (\l -> astlabels2label [l]) `map` astPattern
        in Map {
            main = main,
            subMaps = submaps,
            title = title,
            world = world,
            stage = stage,
            springsAvailable = springsAvailable,
            glassAvailable = glassAvailable,
            watchesAvailable = watchesAvailable,
            goalCount = goalCount,
            pattern = pattern
            }

    glassCountList2Map :: [Int] -> Dict.Map String Int
    glassCountList2Map glassCounts = 
        let labels = ["A", "B", "C", "D"]
        in let pairList = zip labels glassCounts
        in Dict.fromList pairList

    astblock2mapblock :: Ast.Block -> MapBlock
    astblock2mapblock block = MapBlock { mat = (astrow2row `map` block) }

    astrow2row :: [Ast.Cell] -> [Cell]
    astrow2row row = astcell2cell `map` row

    astcell2cell :: Ast.Cell -> Cell
    astcell2cell cell = Cell { label = (astlabels2label cell) }

    astlabels2label :: [Ast.Label] -> Label
    astlabels2label label = Label { value = (sum $ astlabel2int `map` label) }

    astlabel2int :: Ast.Label -> Int
    astlabel2int label = 
        case label of
          Ast.EMPTY -> 0
          Ast.START -> 1
          Ast.A -> 2 ^ 1
          Ast.B -> 2 ^ 2
          Ast.C -> 2 ^ 3
          Ast.D -> 2 ^ 4
          Ast.E -> 2 ^ 5
          Ast.F -> 2 ^ 6
          Ast.G -> 2 ^ 7
          Ast.ANY -> sum $ (\x -> 2 ^ x) `map` range
                    where range = [1..7]
          Ast.GOAL -> 2 ^ 30

