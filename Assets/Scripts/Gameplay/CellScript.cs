using Assets.Core.Data;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Assets.Core.Animation.Coroutines;
/** 그래픽 상으로 나타나는 Cell을 켜고 끄는데 활용되는 스크립트 */

public class CellScript : MonoBehaviour {

    public int index;
    public int Maxindex=5;
    float X_Ratio = 1.2f;
    float Y_Ratio = 0f;
    float Pos_Calib;

    /*
     * (레거시 코드 삭제)
    void Pos_Reset() {
      
        Vector2 currScenePos = gameObject.transform.localPosition;
        Vector2 newScenePos;
        if( index == 0 )
            newScenePos = new Vector2( (index) * X_Ratio + Pos_Calib, ( index - 1f ) * Y_Ratio );
        else
            newScenePos = new Vector2( ( index + 0.1f ) * X_Ratio + Pos_Calib, ( index -1f ) * Y_Ratio );
        if( this.index == 0 )
            gameObject.GetComponent<Animator>().SetBool( "Onoff", true );
        else
            gameObject.GetComponent<Animator>().SetBool( "Onoff", false );
        StartCoroutine( Move.QuadOut( gameObject, currScenePos, newScenePos, 0.5f ) );
        Debug.Log ("Pos_Reset Called");
    }
    */
    void SetOnoff( int index ) {

        if( index == this.index ) {
            gameObject.GetComponent<Animator>().SetBool( "Onoff", true );
        } else {
            gameObject.GetComponent<Animator>().SetBool( "Onoff", false );
        }

        StopAllCoroutines();
        Vector2 currScenePos = gameObject.transform.localPosition;
        Vector2 newScenePos = currScenePos;

        /*
        if( index <= this.index )
            if( this.index - index == 0 )
                newScenePos = new Vector2( ( this.index - index ) * X_Ratio + Pos_Calib, ( this.index - index - 1f ) * Y_Ratio );
            else
                newScenePos = new Vector2( ( this.index - index ) * X_Ratio + Pos_Calib, ( this.index - index ) * Y_Ratio );
        else
           if( Maxindex + this.index - index == 0 )
            newScenePos = new Vector2( ( Maxindex + this.index - index ) * X_Ratio + Pos_Calib, ( Maxindex + this.index - index - 1 ) * Y_Ratio );
        else
            newScenePos = new Vector2( ( Maxindex + this.index - index  ) * X_Ratio + Pos_Calib, ( Maxindex + this.index - index  ) * Y_Ratio );

       
        */
        StartCoroutine (Move.QuadOut (gameObject, currScenePos, newScenePos, 0.5f));
    }
    public void Set_Maxindex(int v)
    {
        Maxindex = v;
        Pos_Calib = (Maxindex - 5) * (-0.5f) * X_Ratio;
    }
}
