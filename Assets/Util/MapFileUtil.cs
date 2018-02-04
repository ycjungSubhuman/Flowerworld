using Assets.Core.Data;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Util
{
    public class MapFileUtil
    {
        public static List<TextAsset> GetAllMapSources()
        {
            var maps = Resources.LoadAll<TextAsset> ("maps");
            return maps.ToList ();
        }
        public static string mapTitleOfFile(TextAsset mapSource)
        {
            var map = JsonConvert.DeserializeObject<Map> (mapSource.text);
            return map.title;
        }
    }
}
