using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using Core.Map.Generator.DSL;
using Core.Map;
using Sprache;

public class StringMapGeneratorTest {

    string testMap1 =
@"title UTF-8

type NORMAL

block

    A,START   |   B  |    C   |   A   |    A   |
    A     |      A   |        |       |
    B     | 
";
	[Test]
	public void BasicTest() {
        var m = DSLGrammar.StageStatement.Parse (testMap1);
        Assert.AreEqual("UTF-8 문자 타이틀 테스트", m.Title);
        Assert.AreEqual(Map.Type.NORMAL, m.MapType);
        var cell1Label = m.Main.GetCell (new Vector2Int (0, 0)).CellLabel;
        var cell2Label = m.Main.GetCell (new Vector2Int (2, 0)).CellLabel;
        Assert.IsTrue (
            cell1Label.FallIn (MapBlock.Cell.Label.START)
            && cell1Label.FallIn (MapBlock.Cell.Label.A)
            );
        Assert.IsFalse (
            cell1Label.FallIn (MapBlock.Cell.Label.B)
            );
        Assert.IsTrue (
            cell2Label.FallIn (MapBlock.Cell.Label.B)
            );
        Assert.IsFalse (
            cell2Label.FallIn (MapBlock.Cell.Label.A)
            );
	}
}
