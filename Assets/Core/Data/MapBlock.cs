using UnityEngine;
using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;


namespace Assets.Core.Data
{
    /* Map 조각 하나를 표현하는 데이터 구조. */
    public class MapBlock
    {
        public List<List<Cell>> mat;

        public MapBlock(List<List<Cell>> block)
        {
            mat = block;
        }

        public Label LabelOf(Vector2Int position)
        {
            return mat [position.x] [position.y].label;
        }

        public IEnumerable<Vector2Int> LabelLocalPositionsOf(Label l)
        {
            List<Vector2Int> result = new List<Vector2Int> ();
            for (int i=0; i<mat.Count; i++)
            {
                for (int j=0; j<mat[i].Count; j++)
                {
                    if ( l.FallIn (mat [i] [j].label) )
                    {
                        result.Add (new Vector2Int (i, j));
                    }
                }
            }
            return result;
        }

        public Cell GetCell(Vector2Int position)
        {
            return mat[position.x][position.y];
        }

    }
}
