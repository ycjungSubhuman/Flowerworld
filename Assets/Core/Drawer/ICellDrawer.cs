using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;
using Assets.Core.Data;

namespace Assets.Core.Drawer
{
    /** 각 셀의 데이터 표현을 Scene상의 GameObject로 만들어주는 오브젝트의 인터페이스 */
    interface ICellDrawer
    {
        GameObject Draw(Cell cell);
        float Height();
        float Width();
    }
}
