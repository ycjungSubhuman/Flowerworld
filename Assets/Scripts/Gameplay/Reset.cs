using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Reset : MonoBehaviour
{

    public bool Pressed = false;
    private bool Hovered = false;

    public bool Is_Reset;
    public bool Is_BacktoMain;

    //Slider ResetProgress;
    float ResetValue=0.0f;

    public GameObject Player;

    float SLIDER_DELTA;//초당 0.5


    // Use this for initialization
    void Start()
    {
        //ResetProgress = transform.Find("ProgressBar").GetComponent<Slider>();
        SLIDER_DELTA = 1f;
       // StartCoroutine(DetectMouseHovering());
    }

    // Update is called once per frame
    void Update()
    {
        //Debug.Log( ResetValue );
        if (Player == null)
            Player = GameObject.FindWithTag("Player");

        if (Pressed || Hovered)
        {
            ResetValue += SLIDER_DELTA;
            if (ResetValue > 1f)
                ResetValue = 1f;
        }
        else
        {
            ResetValue -= SLIDER_DELTA;
            if (ResetValue < 0)
                ResetValue = 0;
        }
        if (ResetValue >= 1f)
        {
            Hovered = false;
            if( Is_Reset ) {
                Player.GetComponent<PlayerControlScript>().onResetKey();
      
                GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>().Reset_SpringCount();
                GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>().TurnOff_Glass();
         
            }
            if (Is_BacktoMain)
                Player.GetComponent<PlayerControlScript>().onGotoStageSelect();
            ResetValue = 0;
        }

        if( Input.GetMouseButtonDown( 0 ) ) {
            RaycastHit hit;
            Ray ray = Camera.main.ScreenPointToRay( Input.mousePosition );
            if( Physics.Raycast( ray, out hit, 100.0f ) ) {
                Hovered = true;
            }
        }
    }
    //호버링 감지 방식 변경중(Legacy)
    /*
    IEnumerator DetectMouseHovering()
    {
        while (true)
        {
            RaycastHit hit;
            Ray ray = Camera.main.ScreenPointToRay(Input.mousePosition);
            if (Physics.Raycast(ray, out hit, 100.0f))
            {
                if (hit.collider.gameObject == this.gameObject)
                    Hovered = true;
                else
                    Hovered = false;
            }
            yield return new WaitForSeconds(0.2f);
        }
    }*/
}
