import MapDSL.DSLParser
import MapDSL.DSLAst
import MapDSL.DSLAstHelper
import MapDSL.DSLValidate
import Test.Hspec
import System.IO
import Data.Text.Format
import Control.Exception
import Control.Monad

readMapText :: String -> IO String
readMapText mapName = 
    let mapFileName = "resources/" ++ mapName ++ ".map"
    in do
        handle <- openFile mapFileName ReadMode
        hSetEncoding handle utf8_bom
        hGetContents handle


testMapGroundTruth1 = ([A,B,C,D],
                        [[[[A,START], [B], [C], [D]],
                        [[EMPTY], [A]],
                        [[EMPTY], [A,GOAL]]]]
                        )

testMapGroundTruth2 = ([A,B,C,D],
                        [[[[A,START], [B], [C], [D]],
                        [[EMPTY], [A]],
                        [[EMPTY], [A]],
                        [[EMPTY], [G,GOAL]]]]
                        )

testMapGroundTruth3 = ([A,B,C,D],
                        [[[[A,START], [B], [C], [D]],
                        [[EMPTY], [A]],
                        [[EMPTY], [A]],
                        [[EMPTY], [G,GOAL]]],
                        [[[A], [B], [C], [D]],
                        [[EMPTY], [A]],
                        [[EMPTY], [A]],
                        [[EMPTY], [G]]]]
                        )

testMapGroundTruth4 = testMapGroundTruth2

notitleMsg = invalidMapHeader ++ (titleErrorMsg 0)
nomaptypeMsg = invalidMapHeader ++ (mapTypeErrorMsg 0)
nopatternMsg = invalidMapHeader ++ (patternCountErrorMsg 0)
badpatternMsg = invalidMapHeader ++ badPatternErrorMsg
noblockMsg = invalidMapHeader ++ (blockCountErrorMsg NORMAL 0)
noblock2Msg = invalidMapHeader ++ (blockCountErrorMsg CONSTRUCT 0)
toomanyblockMsg = invalidMapHeader ++ (blockCountErrorMsg NORMAL 2)
emptyblockMsg = invalidMapHeader ++ blockSizeErrorMsg
nogoalMsg = invalidMapHeader ++ (goalCountErrorMsg NORMAL 0)
twogoalMsg = invalidMapHeader ++ (goalCountErrorMsg NORMAL 2)
goalindodgeMsg = invalidMapHeader ++ (goalCountErrorMsg DODGE 1)
nostartMsg = invalidMapHeader ++ (startCountErrorMsg 0)
twostartMsg = invalidMapHeader ++ (startCountErrorMsg 2)

runTestOnMap mapFileName expectedValidationResult = 
    it (mapFileName ++ " matches groundtruth") $ do
        txt <- readMapText mapFileName

        let parseResult = parseMapText txt

        case parseResult of
            Right expr ->
                let blocks = blocksOf expr
                in let pattern = patternOf expr
                in let Right (patternGt, mapGt) = expectedValidationResult
                in (pattern, blocks) `shouldBe` (patternGt, mapGt)
            Left msg ->
                let Left msgGts = expectedValidationResult
                in msg `shouldBe` msgGts

main :: IO ()
main = hspec $ do
    describe "parseMapText" $ do
        runTestOnMap "testmap1" $ Right testMapGroundTruth1
        runTestOnMap "testmap2" $ Right testMapGroundTruth2
        runTestOnMap "testmap3" $ Right testMapGroundTruth3
        runTestOnMap "testmap4" $ Right testMapGroundTruth4

    describe "validate parseMapText" $ do
        runTestOnMap "invalid-notitle" $ Left notitleMsg
        runTestOnMap "invalid-nomaptype" $ Left nomaptypeMsg
        runTestOnMap "invalid-nopattern" $ Left nopatternMsg
        runTestOnMap "invalid-badpattern" $ Left badpatternMsg
        runTestOnMap "invalid-noblock" $ Left noblockMsg
        runTestOnMap "invalid-noblock2" $ Left noblock2Msg
        runTestOnMap "invalid-toomanyblock" $ Left toomanyblockMsg
        runTestOnMap "invalid-emptyblock" $ Left emptyblockMsg
        runTestOnMap "invalid-nogoal" $ Left nogoalMsg
        runTestOnMap "invalid-twogoal" $ Left twogoalMsg
        runTestOnMap "invalid-goalindodge" $ Left goalindodgeMsg
        runTestOnMap "invalid-nostart" $ Left nostartMsg
        runTestOnMap "invalid-twostart" $ Left twostartMsg


            
