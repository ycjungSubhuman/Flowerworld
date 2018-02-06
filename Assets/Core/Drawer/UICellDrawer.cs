using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;

namespace Assets.Core.Drawer
{
    class UICellDrawer : IUICellDrawer
    {
        public GameObject Draw(Label label, int index,int Maxindex)
        {
            var uiCell = GameObject.Instantiate(Resources.Load<GameObject> ("prefabs/UICell"));
            var material = uiCell.GetComponent<MeshRenderer> ().material;
            material.SetColor ("_LineColor", CellColor.getLineColorOf (label));
            material.SetColor ("_BackgroundColor", CellColor.getBackgroundColorOf (label));
            material.SetFloat ("_Offset", UnityEngine.Random.value);
            uiCell.GetComponent<UICellScript> ().index = index;
            uiCell.GetComponent<UICellScript>().Maxindex = Maxindex;
            return uiCell;
        }
    }
}
