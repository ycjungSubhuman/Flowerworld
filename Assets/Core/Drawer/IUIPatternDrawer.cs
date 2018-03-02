using Assets.Core.Data;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Core.Drawer
{
    interface IUIPatternDrawer
    {
        GameObject Draw(List<Label> pattern,int Maxindex);
    }
}
