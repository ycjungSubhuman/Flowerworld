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

    Slider ResetProgress;

    public GameObject Player;

    float SLIDER_DELTA;//초당 0.5


    // Use this for initialization
    void Start()
    {
        ResetProgress = transform.Find("ProgressBar").GetComponent<Slider>();
        SLIDER_DELTA = 2f;
       // StartCoroutine(DetectMouseHovering());
    }

    // Update is called once per frame
    void Update()
    {
        if (Player == null)
            Player = GameObject.FindWithTag("Player");
        if (Pressed || Hovered)
        {
            ResetProgress.value += SLIDER_DELTA * Time.deltaTime;
            if (ResetProgress.value > 1f)
                ResetProgress.value = 1f;
        }
        else
        {
            ResetProgress.value -= SLIDER_DELTA * Time.deltaTime;
            if (ResetProgress.value < 0)
                ResetProgress.value = 0;
        }
        if (ResetProgress.value >= 1f)
        {
            if( Is_Reset ) {
                Player.GetComponent<PlayerControlScript>().onResetKey();
                GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>().Reset_SpringCount();
            }
            if (Is_BacktoMain)
                Player.GetComponent<PlayerControlScript>().onGotoStageSelect();
            ResetProgress.value = 0;
        }


    }
    private void OnMouseOver()
    {
        Hovered = true;
    }
    private void OnMouseExit()
    {
        Hovered = false;
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
