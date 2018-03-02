using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ClearNotification : MonoBehaviour
{
    public void DisableClearNotification()
    {
        var clearNotification = gameObject;
        for ( int i = 0; i < clearNotification.transform.childCount; i++ )
        {
            clearNotification.transform.GetChild (i).gameObject.SetActive (false);
        }
        clearNotification.GetComponent<Animator> ().SetBool ("On", false);
    }
    public void EnableClearNotification()
    {
        var clearNotification = gameObject;
        for ( int i = 0; i < clearNotification.transform.childCount; i++ )
        {
            clearNotification.transform.GetChild (i).gameObject.SetActive (true);
        }
        clearNotification.GetComponent<Animator> ().SetBool ("On", true);
    }
}
