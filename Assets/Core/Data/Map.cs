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
        List<List<GameObject>> Glass_GameObject = new List<List<GameObject>>();
        //필드 위에 올라와 있는 아이템들
        List <List<Label>> Item = new List<List<Label>>();
        List<List<GameObject>> Item_GameObject = new List<List<GameObject>>();
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
        public SortedList<string, int> glassAvailable;
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

        //유리 초기화
        public void GlassMap() {
            Debug.Log( "GlassMapLoaded" );
            // main.mat.CopyTo( this.Glass );
            GameObject Glass_prefab = Resources.Load<GameObject>( "prefabs/ImageGlassCell" );
            this.pattern = pattern.ToList();

            for( int i = 0; i < main.mat.Count; i++ ) {
                List<Label> temp_Cell = new List<Label>();
                List<GameObject> temp_GameObject = new List<GameObject>();

                for( int j = 0; j < main.mat[ i ].Count; j++ ) {
                    temp_Cell.Add( Label.ANY );

                    GameObject temp_Glass = Object.Instantiate( Glass_prefab );

                    temp_Glass.transform.parent = GameObject.Find( "Cell_Row_" + i.ToString() ).transform.Find( "Cell_Column_" + j.ToString() ).transform;
                    temp_Glass.transform.localPosition = new Vector2( 0, 0 );
                    temp_Glass.GetComponent<SpriteRenderer>().sortingLayerName = "GlassCell";
                    temp_GameObject.Add( temp_Glass );
                    temp_Glass.GetComponent<GlassSpriteRenderer>().Set_State( "ANY" );
                }
                Glass.Add( temp_Cell );
                Glass_GameObject.Add( temp_GameObject );
            }
        }

        //유리 설치
        public void Deploy_Glass(Vector2Int GlassPos,Label GlassType) {

            Glass[ GlassPos.x ][ GlassPos.y ] = GlassType;
            if( GlassType.FallIn( Label.A ) ) {
                Glass_GameObject[ GlassPos.x ][ GlassPos.y ].GetComponent<GlassSpriteRenderer>().Set_State( "A" );
            } else if( GlassType.FallIn( Label.B ) ) {
                Glass_GameObject[ GlassPos.x ][ GlassPos.y ].GetComponent<GlassSpriteRenderer>().Set_State( "B" );
            } else if( GlassType.FallIn( Label.C ) ) {
                Glass_GameObject[ GlassPos.x ][ GlassPos.y ].GetComponent<GlassSpriteRenderer>().Set_State( "C" );
            } else if( GlassType.FallIn( Label.D ) ) {
                Glass_GameObject[ GlassPos.x ][ GlassPos.y ].GetComponent<GlassSpriteRenderer>().Set_State( "D" );
            }
        }
        //유리 철거
        public void Remove_Glass( Vector2Int GlassPos ) {
            Glass[ GlassPos.x ][ GlassPos.y ] = Label.ANY;
            Glass_GameObject[ GlassPos.x ][ GlassPos.y ].GetComponent<GlassSpriteRenderer>().Set_State( "ANY" );
        }
        //유리에 금내기
        public void Crack_Glass( Vector2Int GlassPos ) {
            Glass[ GlassPos.x ][ GlassPos.y ] = Label.ANY;
            Glass_GameObject[ GlassPos.x ][ GlassPos.y ].GetComponent<GlassSpriteRenderer>().Set_Broken();
        }

        //유리 전체 철거
        public void Remove_All_Glass() {
            for( int i = 0; i < main.mat.Count; i++ ) {
                for( int j = 0; j < main.mat[ i ].Count; j++ ) {
                    Glass[ i ][ j] = Label.ANY;
                    Glass_GameObject[ i ][ j ].GetComponent<GlassSpriteRenderer>().Set_State( "ANY" );
                }
            }
        }
        //아이템 설치


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
        public bool IsGlassDeployed( Vector2Int pos ) {
            return Glass[ pos.x ][ pos.y ].Value == Label.ANY.Value;
        }
        public bool IsEmpty( Vector2Int pos ) {
            return main.mat[ pos.x ][ pos.y ].label.Value == Label.EMPTY.Value;
        }
    }
}
