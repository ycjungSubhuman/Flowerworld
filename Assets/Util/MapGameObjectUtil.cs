using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Util
{
    /**
     * 2차원 배열 형태로 구성되어있는 스테이지 셀들에 대한 접근 함수들 
     * TODO : StageScript 안으로 병합
     */ 
    class MapGameObjectUtil
    {
        /** map의 pos에 있는 Cell을 나타내는 GameObject를 불러온다 */
        public static GameObject GetCellGameObject(GameObject map, Vector2Int pos)
        {
            var row = map.transform.GetChild (pos.x);
            var cell = row.transform.GetChild (pos.y);
            return cell.gameObject;
        }

        /** map에 있는 모든 Cell들을 불러온다 */
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
