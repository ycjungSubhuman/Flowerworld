using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Core.Animation
{
    public interface IMapAnimationController
    {
        void SetTrigger(Vector2Int pos, string name);
        void SetBool(Vector2Int pos, string name, bool value);
        void SetFloat(Vector2Int pos, string name, bool value);
    }
}
