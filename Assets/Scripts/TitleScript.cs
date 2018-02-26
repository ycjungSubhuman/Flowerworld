using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class TitleScript : MonoBehaviour {

    private void Start() {
        StartCoroutine( KeyChecker() );
    }

    // Update is called once per frame
    void Update () {
        //StartCoroutine( KeyChecker() );
    }
    IEnumerator KeyChecker() {

        bool Title_Shown = true;


        while(true) {
            if( Input.anyKeyDown ) {
                if( GameObject.Find( "Title" ) ) {

                    GameObject.Find( "Title" ).SetActive( false );
                    //Title_Shown = false;
                } else {
                    SceneManager.LoadScene( "main" );
                }
            }
            yield return new WaitForFixedUpdate();
        }
    }
}
