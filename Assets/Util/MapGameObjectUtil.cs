using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Util
{
    class MapGameObjectUtil
    {
        public static GameObject GetCellGameObject(GameObject map, Vector2Int pos)
        {
            var row = map.transform.GetChild (pos.x);
            var cell = row.transform.GetChild (pos.y);
            return cell.gameObject;
        }

        public static List<GameObject> GetAllCells(GameObject map)
        {
            var result = new List<GameObject> ();

            for (int i=0; i<map.transform.childCount; i++)
            {
                var row = map.transform.GetChild (i);
                for (int j=0; j<row.transform.childCount; j++)
                {
                    result.Add (row.transform.GetChild(j).gameObject);
                }
            }
            return result;
        }
    }
}
