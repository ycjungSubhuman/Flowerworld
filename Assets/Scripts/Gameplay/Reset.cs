using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;

public class Reset : MonoBehaviour
{
    public void onReset()
    {
        var player = GameObject.Find ("Player");
        player.GetComponent<PlayerControlScript> ().onReset ();

        GameObject.Find ("StageInitializer").GetComponent<ItemManager> ().onReset ();
        GameObject.Find ("Clear_Notification").GetComponent<Animator> ().SetBool ("On", false);
    }
}
