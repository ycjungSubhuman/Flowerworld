using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;
using Assets.Core.Data;

namespace Assets.Core.Drawer
{
    class CellDrawer : ICellDrawer
    {
        public bool ImageCell = true;
        public GameObject Draw(Cell cell)
        {
            GameObject cellPrefab;
            GameObject cellGameObject;
            Material material;
            Sprite sprite;
            if( !ImageCell ) {
                cellPrefab = Resources.Load<GameObject>( "prefabs/Cell" );
                cellGameObject = GameObject.Instantiate( cellPrefab );
                material = cellGameObject.GetComponent<MeshRenderer>().material;
                material.SetColor( "_LineColor", CellColor.getLineColorOf( cell.label ) );
                material.SetColor( "_BackgroundColor", CellColor.getBackgroundColorOf( cell.label ) );
                material.SetFloat( "_Offset", UnityEngine.Random.value );

                // Add Goal Mark to Goal Cell
                if( cell.label.FallIn( Label.GOAL ) ) {
                    var goalMarker = GameObject.Instantiate( cellGameObject );
                    var goalMarkerMaterial = Resources.Load<Material>( "materials/BorderMaterial" );
                    goalMarker.GetComponent<MeshRenderer>().material = goalMarkerMaterial;
                    goalMarker.transform.parent = cellGameObject.transform;
                }

            } else {
                cellPrefab = Resources.Load<GameObject>( "prefabs/ImageCell" );
                cellGameObject = GameObject.Instantiate( cellPrefab );
                //Cell의 label에 따라 다른 이미지를 가져온다
                sprite = FlowerFetcher( cell.label );
                cellGameObject.GetComponent<SpriteRenderer>().sprite = sprite;

                if( cell.label.FallIn( Label.GOAL ) ) {
                    var goalMarker = GameObject.Instantiate( Resources.Load<GameObject>("prefabs/Goal") );
                    goalMarker.transform.parent = cellGameObject.transform;
                    var lp = goalMarker.transform.localPosition;
                    goalMarker.transform.localPosition = new Vector3 (0, 0, lp.z);
                }
                cellGameObject.GetComponent<SpriteRenderer>().sortingLayerName = "Cell";
            }
            return cellGameObject;
        }

        Sprite FlowerFetcher( Label label ) {

            string FlowerName;
            if( label.FallIn(Label.A) ) {
                FlowerName = "A";
            } else if( label.FallIn( Label.B ) ) {
                FlowerName = "B";
            } else if( label.FallIn( Label.C ) ) {
                FlowerName = "C";
            } else if( label.FallIn( Label.D ) ) {
                FlowerName = "D";
            } else {
                FlowerName = "NONE";
            }
            if( label.Value == Label.ANY.Value )
                FlowerName = "ANY";
            Sprite temp = Resources.Load<Sprite>( "Sprite/CellFlower/" + FlowerName );

            return temp;
        }

        public float Width()
        {
            return 1f;
        }
        public float Height()
        {
            return 1f;
        }
    }
}
