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
        IUICellDrawer cellDrawer;
        public UIPatternDrawer(IUICellDrawer cellDrawer)
        {
            this.cellDrawer = cellDrawer;
        }

        public GameObject Draw(List<Label> pattern)
        {
            var root = new GameObject ();
            root.AddComponent<RectTransform> ();
            for(int i=0; i<pattern.Count(); i++) {
                var uiCell = cellDrawer.Draw (pattern [i], i, pattern.Count());
                uiCell.transform.SetParent(root.transform);
                if(i == 0)
                       uiCell.transform.localPosition = new Vector2 ((i) *0.8f, (i + 0.5f) * -0.2f);
                else
                    uiCell.transform.localPosition = new Vector2 ((i+ 2f) * 0.8f, (i+ 2f) * -0.2f);

            }
            return root;
        }
    }
}
