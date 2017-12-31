using UnityEngine;
using System.Collections.Generic;

namespace Core.Map
{
    using Generator;

    /** 스테이지 하나에 해당하는 맵을 나타낸다. */
    public class Map
    {
        public MapBlock Main;
        public List<MapBlock> SubMaps = new List<MapBlock>();

    }
}
