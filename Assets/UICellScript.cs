using Assets.Core.Data;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

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
