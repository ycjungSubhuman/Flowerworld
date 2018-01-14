using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;
using Assets.Core.Data;

namespace Assets.Core.Drawer
{
    interface ICellDrawer
    {
        GameObject Draw(Cell cell);
        float Height();
        float Width();
    }
}
