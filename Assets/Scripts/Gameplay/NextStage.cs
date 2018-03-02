using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class NextStage : MonoBehaviour {
    public void onNextStage()
    {
        GameObject.Find ("Player").GetComponent<PlayerControlScript> ().onNextStage ();
    }
}
