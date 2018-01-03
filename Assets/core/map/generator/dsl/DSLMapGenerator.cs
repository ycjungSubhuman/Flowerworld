using UnityEngine;
using System.Collections;
using Sprache;

namespace Core.Map.Generator.DSL
{
    /** 
     * 맵 제작을 위한 간단한 텍스트 기반 맵 생성기.
     * 
     */
    public class DSLMapGenerator : MapGenerator
    {
        private string source;

        public DSLMapGenerator(string source)
        {
            this.source = source;
        }

        public Map Generate()
        {
            throw new System.NotImplementedException ();
        }
    }
}
