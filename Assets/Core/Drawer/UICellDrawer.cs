using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;

namespace Assets.Core.Drawer
{
    class UICellDrawer : IUICellDrawer {
        public bool ImageCell = true;

        public GameObject Draw( Label label, int index, int Maxindex ) {
            GameObject uiCell;
            Material material;
            Sprite sprite;
            if( !ImageCell ) {
                uiCell = GameObject.Instantiate( Resources.Load<GameObject>( "prefabs/UICell" ) );
                material = uiCell.GetComponent<MeshRenderer>().material;
                material.SetColor( "_LineColor", CellColor.getLineColorOf( label ) );
                material.SetColor( "_BackgroundColor", CellColor.getBackgroundColorOf( label ) );
                material.SetFloat( "_Offset", UnityEngine.Random.value );

                uiCell.GetComponent<CellScript>().index = index;
                uiCell.GetComponent<CellScript>().Maxindex = Maxindex;
            } else {
                uiCell = GameObject.Instantiate( Resources.Load<GameObject>( "prefabs/UIImageCell" ) );
                //Cell의 label에 따라 다른 이미지를 가져온다
                sprite = FlowerFetcher( label );
                uiCell.GetComponent<SpriteRenderer>().sprite = sprite;
                uiCell.GetComponent<CellScript>().index = index;
                uiCell.GetComponent<CellScript>().Maxindex = Maxindex;
            }
            return uiCell;
        }
        Sprite FlowerFetcher( Label label ) {

            string FlowerName;
            if( label.Value == Label.A.Value ) {
                FlowerName = "A";
            } else if( label.Value == Label.B.Value ) {
                FlowerName = "B";
            } else if( label.Value == Label.C.Value ) {
                FlowerName = "C";
            } else if( label.Value == Label.D.Value ) {
                FlowerName = "D";
            } else {
                FlowerName = "D";
            }
            Sprite temp = Resources.Load<Sprite>( "Sprite/UICellFlower/" + FlowerName );

            return temp;
        }
    }
}
