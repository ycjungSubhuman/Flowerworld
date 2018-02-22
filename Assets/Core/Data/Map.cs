using UnityEngine;
using System.Collections.Generic;
using System.Linq;

namespace Assets.Core.Data
{

    /** 스테이지 하나에 해당하는 맵을 나타낸다. */
    public class Map
    {
        public MapBlock main;
        //깔 유리들
        List<List<Label>> Glass = new List<List<Label>>();
        //필드 위에 올라와 있는 아이템들
        List<List<Label>> Item = new List<List<Label>>();
        // public MapBlock Glass;

        public List<Label> pattern = new List<Label>();
        public string title { get; set; }
        public int goalCount;

        public int springsAvailable;
        /**
         * 각 유리에 접근하는 법
         * glassAvailable["A"]  : A 글래스의 개수
         * glassAvailable["B"]  : B 글래스의 개수
         * ...
         */
        public Dictionary<string, int> glassAvailable;
        public int watchesAvailable;
        public string world;
        public string stage;

        public Map()
        {
            //Debug.Log( "Loaded" );
        }

        public Map(string title, IEnumerable<Label> pattern, IEnumerable<MapBlock> blocks)
        {
           // Debug.Log( "Loaded" );
            this.title = title;
            this.main = blocks.First ();
            GlassMap();
        }
        //유리 라벨링
        public void GlassMap() {
            Debug.Log( "GlassMapLoaded" );
           // main.mat.CopyTo( this.Glass );

            this.pattern = pattern.ToList();

            for( int i = 0; i < main.mat.Count; i++ ) {
                List<Label> temp_Cell = new List<Label>();
                for( int j = 0; j < main.mat[ i ].Count; j++ ) {
                    temp_Cell.Add( Label.ANY );
                }
                Glass.Add( temp_Cell );
            }
        }

        /** 현재 맵의 position에 있는 cell의 label을 리턴 */
        public Label LabelOf(Vector2Int position)
        {
            return main.LabelOf (position);
        }
        public Label GlassLabelOf( Vector2Int position ) {
            return Glass[ position.x ][ position.y ];
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
