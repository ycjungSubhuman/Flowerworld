using UnityEngine;
using System.Collections.Generic;
using System.Linq;


namespace Core.Map
{
    /* Map 조각 하나를 표현하는 데이터 구조. */
    public class MapBlock
    {
        private Cell [,] map;

        public MapBlock(IEnumerable<IEnumerable<Cell>> block)
        {
            var iteratedBlock = block.Select (row => row.ToArray ()).ToArray ();
            var rowCount = iteratedBlock.Count ();
            var maxRowLength = iteratedBlock.Select (row => row.Count ()).Max ();

            map = new Cell [rowCount, maxRowLength];

            for(int i=0; i<rowCount; i++)
            {
                var row = iteratedBlock [i];
                for(int j=0; j<row.Count(); j++)
                {
                    map [i, j] = row [j];
                    map [i, j].Position = new Vector2Int (i, j);
                }
            }
        }

        public Cell GetCell(Vector2Int position)
        {
            return map [position.x, position.y];
        }

        public class Cell
        {
            public Vector2Int Position { get; set; } // 왼쪽 위가 (0, 0)인 좌표계에서 표시된 칸의 위치.
            public Label CellLabel { get { return label; } }

            private Label label;
            private MapBlock Parent { get; set; }

            public Cell(Label label)
            {
                this.label = label;
            }

            /* 각 칸의 색, 또는 역할을 나타내는 라벨 */
            public class Label
            {
                private static Label genLabel(int value)
                {
                    return new Label (value);
                }
                /*
                 * 각 레이블 클래스 별로 int의 한 개의 비트를 차지하는 식으로 구현된다.
                 * 따라서 레이블을 32개 종류 이상으로 만들 것이라면 구현 방식을 바꾸어야 한다. 
                 * 
                 * 새로운 레이블을 추가하고 싶다면 'genLabel(1 << (위에서 나타나지 않은 0~31 사이의 정수))'를 static readonly 필드로 추가해준다.
                 */
                public static readonly Label EMPTY = genLabel (0);           // 절대 갈 수 없는 블럭, 또는 빈 공간을 나타내는 블럭
                public static readonly Label START = genLabel (1 << 0);
                public static readonly Label A = genLabel (1 << 1);
                public static readonly Label B = genLabel (1 << 2);
                public static readonly Label C = genLabel (1 << 3);
                public static readonly Label D = genLabel (1 << 4);
                public static readonly Label E = genLabel (1 << 5);
                public static readonly Label F = genLabel (1 << 6);
                public static readonly Label G = genLabel (1 << 7);
                public static readonly Label ANY = A.Or(B).Or(C).Or(D).Or(E).Or(F).Or(G);       // 아무 색이나 다 허용. 단, 알파벳 레이블만 허용한다.
                public static readonly Label GOAL = genLabel (1 << 31);

                private int value;

                private Label(int value)
                {
                    this.value = value;
                }

                /**
                 * 이 Label 객체가 주어진 'label'에 속하는지 검사한다. 
                 *
                 * eg)
                 * Label.A.FallIn(Label.A); // =True
                 * Label.WILDCARD.FallIn(Label.A); // =True
                 */
                public bool FallIn(Label label)
                {
                    return (label.value & this.value) != 0;
                }
                
                /**
                 * 현재 Label 객체에 'label'과 합쳐진 속성을 지닌 Label을 리턴한다.  
                 *
                 * eg)
                 * 시작점과 A라벨 속성을 동시에 지니도록 만들고 싶을 때
                 *
                 * var a = Label.START.Or(Label.A);
                 * ...use a...
                 */
                public Label Or(Label label)
                {
                    return new Label (label.value | this.value);
                }

                public static Label Combined(IEnumerable<Label> labels)
                {
                    return labels.Aggregate ((a, b) => a.Or(b));
                }
            }
        }
    }
}
