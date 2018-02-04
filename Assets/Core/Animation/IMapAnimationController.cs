using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Core.Animation
{
    /** 스테이지의 특정 pos에 있는 GameObject에 대한 애니메이션을 컨트롤하는 컨트롤러 */
    public interface IMapAnimationController
    {
        void SetTrigger(Vector2Int pos, string name);
        void SetBool(Vector2Int pos, string name, bool value);
        void SetFloat(Vector2Int pos, string name, bool value);
    }
}
