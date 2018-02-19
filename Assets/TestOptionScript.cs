using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UI = UnityEngine.UI;

public class TestOptionScript : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        var dropdown = GetComponent<UI.Dropdown> ();
        var options = new List<string> ()
        {
            "Spring Ready",
            "Spring Use",
            "Glass Ready",
            "Glass Use",
            "Watch Ready",
            "Watch Use",
            "Watch LastUse",
        };
        dropdown.AddOptions(options);

        dropdown.onValueChanged.AddListener ((selectionIndex) => {
            switch(selectionIndex)
            {
                case 0:
                    SpringReady ();
                    break;
                case 1:
                    break;

                case 2:
                    break;
                case 3:
                    break;

                case 4:
                    break;
                case 5:
                    break;
                case 6:
                    break;
            }
        });
	}

    private void SpringReady()
    {
        //
    }
}
