using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BackToMain : MonoBehaviour {
    public void onGotoStageSelect()
    {
        GameObject.Find ("Player").GetComponent<PlayerControlScript> ().onGotoStageSelect ();
    }
}
