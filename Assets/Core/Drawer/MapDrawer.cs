using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;

namespace Assets.Core.Drawer
{
    //맵을 그리는 Class, 컨셉에 따라 다른 Class를 선언하는 것도 생각해보자
    class MapDrawer : IMapDrawer
    {
        IMapBlockDrawer mapBlockDrawer;

        public MapDrawer(IMapBlockDrawer mapBlockDrawer)
        {
            this.mapBlockDrawer = mapBlockDrawer;
        }

        public GameObject Draw(Map map)
        {
            //진짜로 그리고 리턴
            return mapBlockDrawer.Draw (map.main);
        }
    }
}
