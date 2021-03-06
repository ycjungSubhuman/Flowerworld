﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;
using Assets.Core.Data;

namespace Assets.Core.Drawer
{
    /** MapBlock 하나를 받아서 UnityEngine 상의 GameObject로 만들어준다 */
    class MapBlockDrawer : IMapBlockDrawer
    {
        const float horitozontalSpace = 0.02f;
        const float verticalSpace = 0.02f;

        ICellDrawer cellDrawer;
        float cellWidth;
        float cellHeight;

        public MapBlockDrawer(ICellDrawer cellDrawer)
        {
            this.cellDrawer = cellDrawer;
            this.cellWidth = cellDrawer.Width ();
            this.cellHeight = cellDrawer.Height ();
        }

        public GameObject Draw(MapBlock m)
        {
            GameObject rootGameObject = GameObject.Find ("Cell_Head");
            for (int i=0; i<m.mat.Count; i++)
            {
                var row = m.mat [i];
                var rowGameObject = new GameObject ();
                rowGameObject.name = "Cell_Row_" + i.ToString();
                rowGameObject.transform.parent = rootGameObject.transform;
                rowGameObject.transform.localPosition = 
                    new Vector2 (0, ((m.mat.Count / 2f) - i)*(cellHeight+verticalSpace));
                for (int j=0; j<row.Count; j++)
                {
                    var cell = row [j];
                    var cellGameObject = cellDrawer.Draw (cell);
                    cellGameObject.name = "Cell_Column_" + j.ToString();
                    cellGameObject.transform.parent = rowGameObject.transform;
                    cellGameObject.transform.localPosition = 
                        new Vector2 (( -1 * (row.Count / 2f) + j + 0.5f) *(cellWidth+horitozontalSpace), 0);
                }
            }

            return rootGameObject;
        }
    }
}
