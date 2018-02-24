using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class TitleScript : MonoBehaviour {
	
	// Update is called once per frame
	void Update () {
        StartCoroutine( KeyChecker() );
    }
    IEnumerator KeyChecker() {

        bool Title_Shown = true;


        while(true) {
            if( Input.anyKey ) {
                if(Title_Shown) {

                    GameObject.Find( "Title_BackGround" ).SetActive( false );
                    Title_Shown = false;
                }
                else
                    SceneManager.LoadScene( "main" );
            }
            yield return new WaitForSeconds( 0.05f );
        }
    }
}
