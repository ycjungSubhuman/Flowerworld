using UnityEngine;
using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;


namespace Core.Map
{
    /* Map 조각 하나를 표현하는 데이터 구조. */
    public class MapBlock
    {
        [JsonProperty]
        private List<List<Cell>> mat;

        public MapBlock(List<List<Cell>> block)
        {
            mat = block;
        }

        public Cell GetCell(Vector2Int position)
        {
            return mat[position.x][position.y];
        }

    }
}
