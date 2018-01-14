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
            var cellGameObject = GameObject.CreatePrimitive (PrimitiveType.Quad);
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
