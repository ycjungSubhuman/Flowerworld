using UnityEngine;
using UnityEngine.UI;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Assets.Core.Data;
using Assets.Util;
using Assets.Core.Sound;
using Assets.Core.Animation.Coroutines;

/**
 * 플레이어의 컨트롤에 대한 반응을 처리하는 스크립트
 */
public class PlayerControlScript : MonoBehaviour
{
    /* Initialize public members on instantiation */
    public IPlayerSoundController soundController;
    [HideInInspector]
    public GameObject stageRoot;

    private Vector2Int pos;
    private StageScript stage;

    private GameObject Reset, BacktoMain;
    int PosDelta;

    void Start()
    {
        Reset = GameObject.Find("ResetIndicator");
        BacktoMain = GameObject.Find("BacktoMainIndicator");

        Debug.Assert (stageRoot != null);
        stage = stageRoot.GetComponent<StageScript> ();
        pos = stage.GetInitPosition ();
        Vector2 initScenepos = stage.ScenePosOf (pos);
        gameObject.transform.localPosition = initScenepos;
        stage.UpdateStage (pos);
    }

    void Update()
    {
        Vector2Int newPos = pos;

        if(Input.GetKeyDown(KeyCode.UpArrow))
        {
            newPos += new Vector2Int( PosDelta * -1, 0);
        }
        else if (Input.GetKeyDown(KeyCode.DownArrow))
        {
            newPos += new Vector2Int( PosDelta, 0);
        }
        else if (Input.GetKeyDown(KeyCode.RightArrow))
        {
            newPos += new Vector2Int(0, PosDelta );
        }
        else if (Input.GetKeyDown(KeyCode.LeftArrow))
        {
            newPos += new Vector2Int(0, PosDelta * -1);
        }

        updatePlayerPosition (newPos);

        if (Input.GetKey(KeyCode.R))
        {
            Reset.GetComponent<Reset>().Pressed = true;
        }
        else
        {
            Reset.GetComponent<Reset>().Pressed = false;
        }
        if (Input.GetKey(KeyCode.Escape))
        {
            BacktoMain.GetComponent<Reset>().Pressed = true;
        }
        else
        {
            BacktoMain.GetComponent<Reset>().Pressed = false;
        }   
    }

    public void Set_SpringState(bool On) {
        if( On )
            PosDelta = 2;
        else
            PosDelta = 1;
    }
    public void onGotoStageSelect()
    {
        onResetKey();
        StartCoroutine(LoadYourAsyncScene());
    }
    IEnumerator LoadYourAsyncScene()
    {
        // The Application loads the Scene in the background at the same time as the current Scene.
        //This is particularly good for creating loading screens. You could also load the Scene by build //number.
        AsyncOperation asyncLoad = UnityEngine.SceneManagement.SceneManager.LoadSceneAsync("main");

        //Wait until the last operation fully loads to return anything
        while (!asyncLoad.isDone)
        {
            yield return null;
        }
    }
    public void onResetKey()
    {
        updatePlayerPosition (stage.GetInitPosition(), true);
        stage.ResetStage ();
        stage.UpdateStage (pos);
        soundController.OnRestart ();
    }

    void updatePlayerPosition(Vector2Int newPos, bool isReset=false)
    {
        if (newPos.Equals(pos))
        {
            return;
        }

        if(stage.IsValidPos(newPos) || isReset)
        {
            playMoveSound ();
            stage.UpdateStage (newPos);
            movePlayer (newPos);
        }
        else
        {
            refuseMove ();
        }
    }

    void movePlayer(Vector2Int newPos)
    {
        Vector2 currScenePos = gameObject.transform.position;
        Vector2 newScenePos = stage.ScenePosOf (newPos);

        StopAllCoroutines ();
        StartCoroutine (Move.QuadOut(gameObject, currScenePos, newScenePos, 0.5f));

        this.pos = newPos;
        gameObject.GetComponent<Animator> ().SetTrigger ("Move");
        stage.AnimateCellStomp (newPos);
    }

    void playMoveSound()
    {
        soundController.OnLabelSound (stage.CurrentPatternLabel());
    }

    void refuseMove()
    {
        gameObject.GetComponent<Animator> ().SetTrigger ("Refuse");
        soundController.OnRefuse ();
    }
}
