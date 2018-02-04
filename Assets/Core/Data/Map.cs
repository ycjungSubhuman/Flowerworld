using UnityEngine;
using System.Collections.Generic;
using System.Linq;

namespace Assets.Core.Data
{

    /** 스테이지 하나에 해당하는 맵을 나타낸다. */
    public class Map
    {
        public MapBlock main;
        //TODO : BRIDGE를 위해 만들어뒀지만, 안하기로 결정될 경우 없애기
        public List<MapBlock> subMaps = new List<MapBlock>();
        public List<Label> pattern = new List<Label>();
        public string title { get; set; }
        public Type mapType { get; set; }

        public Map()
        {
        }

        // TODO : BRIDGE를 위해 만들어 둔 필드들. 결정 후 없애기
        private Dictionary<MapBlock, Vector2Int> offsetMap = new Dictionary<MapBlock, Vector2Int> ();
        private List<MapBlock> activeMaps = new List<MapBlock> ();

        public Map(string title, Type type, IEnumerable<Label> pattern, IEnumerable<MapBlock> blocks)
        {
            this.title = title;
            this.mapType = type;
            this.main = blocks.First ();
            this.subMaps = blocks.Skip (1).ToList ();
            this.pattern = pattern.ToList();

            Init ();
        }

        // MUST Call after Deserialization
        public void Init()
        {
            offsetMap [this.main] = new Vector2Int (0, 0);
            activeMaps.Add (main);
        }

        /** 현재 맵에 놓여진 모든 블락을 고려한 position에 있는 cell의 label을 리턴 */
        public Label LabelOf(Vector2Int position)
        {
            var labels = from mb in activeMaps
                         select mb.LabelOf (position + offsetMap [mb]);

            Debug.Assert (labels.Count () == 1);
            return labels.First ();
        }

        /** l 레이블을 가지는 모든 맵에 놓여진 블락들의 position을 리턴 */
        public IEnumerable<Vector2Int> GlobalPositionsOf(Label l)
        {
            return activeMaps.SelectMany (mb => 
                    mb.LocalPositionsOf (l)
                        .Select (pos => pos + offsetMap [mb]));
        }

        /** pos가 맵 안에 있는지 검사 */
        public bool IsInside(Vector2Int pos)
        {
            var activeOffsets = activeMaps.Select (mb => offsetMap [mb]);
            var minX = activeOffsets.Select (o => o.x).Min ();
            var minY = activeOffsets.Select (o => o.y).Min ();
            var maxX = activeMaps.Select (mb =>
                        offsetMap [mb].x + mb.mat.Count ()
                        ).Max ();
            var maxY = activeMaps.Select (mb =>
                        offsetMap [mb].y + mb.mat [0].Count ()
                        ).Max ();

            return pos.x >= minX 
                && pos.x < maxX 
                && pos.y >= minY 
                && pos.y < maxY;
        }

        public enum Type
        {
            NORMAL, 
            CONSTRUCT,
            DODGE,
        }
    }
}
