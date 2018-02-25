using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class StageSelectScrollScript : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    public void Restore ()
    {
        GameObject.Find ("StageSelectionInitializer").GetComponent<StageSelectionInitializerScript>().Restore();
    }
}
