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
            material.SetColor ("_LineColor", getLineColorOf (cell.label));
            material.SetColor ("_BackgroundColor", getBackgroundColorOf (cell.label));
            material.SetFloat ("_Offset", UnityEngine.Random.value);
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

        static Dictionary<int, Color> lineColorMap =
            new Dictionary<int, Color> ()
            {
                {Label.A.Value, rgb(196, 0, 103)},
                {Label.B.Value, rgb(144, 224, 0)},
                {Label.C.Value, rgb(241, 129, 0)},
                {Label.D.Value, rgb(3, 103, 152)},
                {Label.ANY.Value, rgb(33, 33, 33)},
                {Label.EMPTY.Value, rgba(255, 255, 255, 0)},
            };

        static Dictionary<int, Color> subColorMap =
            new Dictionary<int, Color> ()
            {
                {Label.A.Value, rgb(143, 0, 75)},
                {Label.B.Value, rgb(104, 162, 0)},
                {Label.C.Value, rgb(175, 94, 0)},
                {Label.D.Value, rgb(0, 75, 111)},
                {Label.ANY.Value, rgb(100, 100, 182)},
                {Label.EMPTY.Value, rgba(255, 255, 255, 0)},
            };

        static Dictionary<int, Color> bgColorMap =
            new Dictionary<int, Color> ()
            {
                {Label.A.Value, rgb(240, 0, 127)},
                {Label.B.Value, rgb(160, 249, 0)},
                {Label.C.Value, rgb(255, 137, 0)},
                {Label.D.Value, rgb(8, 154, 225)},
                {Label.ANY.Value, rgb(0, 0, 0)},
                {Label.EMPTY.Value, rgba(200, 200, 200, 0)},
            };

        private static Color rgb(int r, int g, int b)
        {
            return new Color (r / 255.0f, g / 255.0f, b / 255.0f);
        }

        private static Color rgba(int r, int g, int b, int a)
        {
            return new Color (r / 255.0f, g / 255.0f, b / 255.0f, a / 255.0f);
        }

        private static Color getLineColorOf (Label label)
        {
            return lineColorMap [label.Value 
                & (~Label.START.Value)
                & (~Label.GOAL.Value) ];
        }

        private static Color getBackgroundColorOf (Label label)
        {
            return bgColorMap [label.Value 
                & (~Label.START.Value)
                & (~Label.GOAL.Value) ];
        }

    }
}
