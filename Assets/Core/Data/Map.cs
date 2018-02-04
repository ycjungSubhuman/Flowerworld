using UnityEngine;
using System.Collections.Generic;
using System.Linq;

namespace Assets.Core.Data
{

    /** 스테이지 하나에 해당하는 맵을 나타낸다. */
    public class Map
    {
        public MapBlock main;
        public List<Label> pattern = new List<Label>();
        public string title { get; set; }
        public int goalCount;

        public Map()
        {
        }

        public Map(string title, IEnumerable<Label> pattern, IEnumerable<MapBlock> blocks)
        {
            this.title = title;
            this.main = blocks.First ();
            this.pattern = pattern.ToList();
        }

        /** 현재 맵의 position에 있는 cell의 label을 리턴 */
        public Label LabelOf(Vector2Int position)
        {
            return main.LabelOf (position);
        }

        /** l 레이블을 가지는 cell들의 position을 리턴 */
        public IEnumerable<Vector2Int> PositionsOf(Label l)
        {
            return main.PositionsOf (l);
        }

        /** pos가 맵 안에 있는지 검사 */
        public bool IsInside(Vector2Int pos)
        {
            var minX = 0;
            var minY = 0;
            var maxX = main.mat.Count;
            var maxY = main.mat.Select (row => row.Count).Max ();

            return pos.x >= minX 
                && pos.x < maxX 
                && pos.y >= minY 
                && pos.y < maxY;
        }
    }
}
