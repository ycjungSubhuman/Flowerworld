using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GetOut : MonoBehaviour {

    public void onGetOut()
    {
        Application.Quit ();
    }

    void Update()
    {
        if(Input.GetKeyDown(KeyCode.Escape))
        {
            onGetOut ();
        }
    }
}
