using Assets.Core.Data;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Assets.Core.Animation.Coroutines;
/** 그래픽 상으로 나타나는 Cell을 켜고 끄는데 활용되는 스크립트 */

public class CellScript : MonoBehaviour {

    public int index;
    public int Maxindex;
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
    void Pos_Reset()
    {
        Vector2 currScenePos = gameObject.transform.localPosition;
        Vector2 newScenePos;
        if (index == 0)
             newScenePos = new Vector2((index + 0.2f )* 0.8f,( index - 1f) * -0.2f);
        else
             newScenePos = new Vector2 ((index+1.5f) * 0.8f, (index+ 2f) * -0.2f);
        if (this.index == 0)
            gameObject.GetComponent<Animator>().SetBool("Onoff", true);
        else
            gameObject.GetComponent<Animator>().SetBool("Onoff", false);
        StartCoroutine(Move.QuadOut(gameObject, currScenePos, newScenePos, 0.5f));
    }
    void SetOnoff(int index)
    {
        if ( index == this.index )
        {
            gameObject.GetComponent<Animator> ().SetBool ("Onoff", true);
        }
        else
        {
            gameObject.GetComponent<Animator> ().SetBool ("Onoff", false);
        }

        StopAllCoroutines ();
        Vector2 currScenePos = gameObject.transform.localPosition;
        Vector2 newScenePos;
        if ( index <= this.index )
            if ( this.index - index == 0 )
                newScenePos = new Vector2 ((this.index - index + 0.2f) * 0.8f, (this.index - index - 1f) * -0.2f);
            else
                newScenePos = new Vector2 ((this.index - index+ 2f) * 0.8f, (this.index - index+ 2f) * -0.2f);
        else
           if ( Maxindex + this.index - index == 0 )
            newScenePos = new Vector2 ((Maxindex + this.index - index + 0.2f) * 0.8f, (Maxindex + this.index - index -1) * -0.2f);
        else
            newScenePos = new Vector2 ((Maxindex + this.index - index+ 2f) * 0.8f, (Maxindex + this.index - index+ 2f) * -0.2f);
        StartCoroutine (Move.QuadOut (gameObject, currScenePos, newScenePos, 0.5f));

    }

}
