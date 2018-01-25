using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;
using Assets.Core.Data;

namespace Assets.Core.Drawer
{
    class CellDrawer : ICellDrawer
    {
        public GameObject Draw(Cell cell)
        {
            var cellPrefab = Resources.Load<GameObject>("prefabs/Cell");
            var cellGameObject = GameObject.Instantiate (cellPrefab);
            var material = cellGameObject.GetComponent<MeshRenderer> ().material;
            material.SetColor ("_LineColor", CellColor.getLineColorOf (cell.label));
            material.SetColor ("_BackgroundColor", CellColor.getBackgroundColorOf (cell.label));
            material.SetFloat ("_Offset", UnityEngine.Random.value);

            // Add Goal Mark to Goal Cell
            if(cell.label.FallIn(Label.GOAL))
            {
                var goalMarker = GameObject.Instantiate (cellGameObject);
                var goalMarkerMaterial = Resources.Load<Material> ("materials/BorderMaterial");
                goalMarker.GetComponent<MeshRenderer> ().material = goalMarkerMaterial;
                goalMarker.transform.parent = cellGameObject.transform;
            }

            return cellGameObject;
        }

        public float Width()
        {
            return 1f;
        }
        public float Height()
        {
            return 1f;
        }
    }
}
