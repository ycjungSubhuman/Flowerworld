using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;

namespace Assets.Core.Drawer
{
    interface IMapBlockDrawer
    {
        GameObject Draw(MapBlock mapBlock);
    }
}
