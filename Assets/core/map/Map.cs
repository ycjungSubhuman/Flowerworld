using UnityEngine;
using System.Collections.Generic;
using System.Linq;

namespace Core.Map
{
    using Generator;

    /** 스테이지 하나에 해당하는 맵을 나타낸다. */
    public class Map
    {
        public MapBlock Main;
        public List<MapBlock> SubMaps = new List<MapBlock>();
        public string Title { get; set; }
        public Type MapType { get; set; }

        public Map(string title, Type type, IEnumerable<MapBlock> blocks)
        {
            Title = title;
            MapType = type;
            Main = blocks.First ();
            SubMaps = blocks.Skip (1).ToList ();
        }

        public enum Type
        {
            NORMAL, 
            CONSTRUCT,
            DODGE,
        }
    }
}
