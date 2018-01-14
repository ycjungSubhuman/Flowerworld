using UnityEngine;
using System.Collections.Generic;
using System.Linq;

namespace Assets.Core.Data
{

    /** 스테이지 하나에 해당하는 맵을 나타낸다. */
    public class Map
    {
        public MapBlock main;
        public List<MapBlock> subMaps = new List<MapBlock>();
        public List<Label> pattern = new List<Label>();
        public string title { get; set; }
        public Type mapType { get; set; }

        public Map() { }

        public Map(string title, Type type, IEnumerable<Label> pattern, IEnumerable<MapBlock> blocks)
        {
            this.title = title;
            this.mapType = type;
            this.main = blocks.First ();
            this.subMaps = blocks.Skip (1).ToList ();
            this.pattern = pattern.ToList();
        }

        public enum Type
        {
            NORMAL, 
            CONSTRUCT,
            DODGE,
        }
    }
}
