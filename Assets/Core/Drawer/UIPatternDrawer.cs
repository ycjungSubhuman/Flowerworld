using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;
using UnityEngine.UI;

namespace Assets.Core.Drawer
{
    class UIPatternDrawer : IUIPatternDrawer
    {
        float X_Ratio = 1.2f;
        float Y_Ratio = 0f;
        IUICellDrawer cellDrawer;
        public UIPatternDrawer(IUICellDrawer cellDrawer)
        {
            this.cellDrawer = cellDrawer;
        }

        public GameObject Draw(List<Label> pattern,int Maxindex)
        {
           float Pos_Calib = (Maxindex - 5) * (-0.5f) * X_Ratio;

            var root = new GameObject ();
            root.AddComponent<RectTransform> ();
            for(int i=0; i<pattern.Count(); i++) {
                var uiCell = cellDrawer.Draw (pattern [i], i, pattern.Count());
                uiCell.transform.SetParent(root.transform);
                if(i==0)
                    uiCell.transform.localPosition = new Vector2 ((i) * X_Ratio + Pos_Calib, (i - 1f) * Y_Ratio);
                else
                    uiCell.transform.localPosition = new Vector2 ((i + 0.1f) * X_Ratio + Pos_Calib, (i + 1f) * Y_Ratio);

            }
            return root;
        }
    }
}
