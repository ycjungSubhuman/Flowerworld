using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;
using Assets.Util;

namespace Assets.Core.Animation
{
    public class MapAnimationController : IMapAnimationController
    {
        private GameObject mapGameObject;

        public MapAnimationController(GameObject mapGameObject)
        {
            this.mapGameObject = mapGameObject;
        }

        public void SetBool(Vector2Int pos, string name, bool value)
        {
            throw new NotImplementedException ();
        }

        public void SetFloat(Vector2Int pos, string name, float value)
        {
            throw new NotImplementedException ();
        }

        public void SetTrigger(Vector2Int pos, string name)
        {
            var cell = MapGameObjectUtil.GetCellGameObject (mapGameObject, pos);
            cell.GetComponent<Animator> ().SetTrigger (name);
        }
    }
}
