using Assets.Core.Data;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/** 그래픽 상으로 나타나는 Cell을 켜고 끄는데 활용되는 스크립트 */
public class UICellScript : MonoBehaviour {

    public int index;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    void SetOnoff(int index)
    {
        if (index == this.index)
        {
            gameObject.GetComponent<Animator> ().SetBool ("Onoff", true);
        }
        else
        {
            gameObject.GetComponent<Animator> ().SetBool ("Onoff", false);
        }
    }

}
