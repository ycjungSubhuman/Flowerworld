using Assets.Core.Data;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Util
{
    /** 맵 파일들의 리스트에 접근하는 함수들의 모음 */
    public class MapFileUtil
    {

        public static List<TextAsset> GetAllMapSources()
        {
            TextAsset Maplist_Asset = Resources.Load<TextAsset>("maplist");
            string[] Maplist = Encoding.ASCII.GetString(Maplist_Asset.bytes).Split(' ');
            Debug.Log(Maplist);
            var maps = Resources.LoadAll<TextAsset> ("maps");
            return maps.ToList ();
        }
        public static string mapTitleOfFile(TextAsset mapSource)
        {
            var map = JsonConvert.DeserializeObject<Map> (mapSource.text);
            return map.title;
        }
        public static string mapWorld(TextAsset mapSource)
        {
            var map = JsonConvert.DeserializeObject<Map>(mapSource.text);
            return map.world;
        }
        public static string mapStage(TextAsset mapSource)
        {
            var map = JsonConvert.DeserializeObject<Map>(mapSource.text);
            return map.stage;
        }
    }
}
