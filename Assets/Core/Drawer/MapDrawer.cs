using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;

namespace Assets.Core.Drawer
{
    class MapDrawer : IMapDrawer
    {
        IMapBlockDrawer mapBlockDrawer;

        public MapDrawer(IMapBlockDrawer mapBlockDrawer)
        {
            this.mapBlockDrawer = mapBlockDrawer;
        }

        public GameObject Draw(Map map)
        {
            //TODO : Construct mode
            return mapBlockDrawer.Draw (map.main);
        }
    }
}
