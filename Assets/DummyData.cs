using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Core.Map;

public class DummyData
{
    public static Cell cell1 = new Cell (Label.A);
    public static List<Cell> row1 = new List<Cell> { cell1, cell1, cell1 };
    public static List<List<Cell>> mat1 = new List<List<Cell>> { row1, row1 };
    public static MapBlock block1 = new MapBlock (mat1);
    public static Map m1 = new Map
    (
        "dummy",
        Map.Type.NORMAL,
        new List<Label> { Label.A, Label.B },
        new List<MapBlock> { block1, block1, block1 }
    );
}
