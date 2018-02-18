using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class UIAnimationTestScript : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}

    private void Update() {

    }

    // Update is called once per frame
    private void OnMouseEnter() {
        GetComponent<Animator>().SetTrigger( "Shake" );
    }
    private void OnMouseExit() {
        GetComponent<Animator>().SetTrigger( "Out" );
    }
    private void OnMouseOver() {
        if( Input.GetMouseButtonDown( 0 ) ) {
            GetComponent<Animator>().SetTrigger( "Vibrate" );
        } else if( Input.GetMouseButtonDown( 1 ) ) {
            GetComponent<Animator>().SetTrigger( "Stomp" );
        }
    }
}
