using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Assets.Core.Data;
using Assets.Util;
using Assets.Core.Sound;
using Assets.Core.Animation.Coroutines;

public class PlayerControlScript : MonoBehaviour
{
    /* Initialize public members on instantiation */
    public IPlayerSoundController soundController;
    [HideInInspector]
    public GameObject stageRoot;

    private Vector2Int pos;
    private StageScript stage;

    void Start()
    {
        Debug.Assert (stageRoot != null);
        stage = stageRoot.GetComponent<StageScript> ();
        pos = stage.GetInitPosition ();
        stage.UpdateStage ();
    }

    void Update()
    {
        Vector2Int newPos = pos;

        if(Input.GetKeyDown(KeyCode.UpArrow))
        {
            newPos += new Vector2Int(-1, 0);
        }
        else if (Input.GetKeyDown(KeyCode.DownArrow))
        {
            newPos += new Vector2Int(1, 0);
        }
        else if (Input.GetKeyDown(KeyCode.RightArrow))
        {
            newPos += new Vector2Int(0, 1);
        }
        else if (Input.GetKeyDown(KeyCode.LeftArrow))
        {
            newPos += new Vector2Int(0, -1);
        }

        updatePlayerPosition (newPos);

        if(Input.GetKeyDown(KeyCode.R))
        {
            onResetKey ();
        }
    }

    private void onResetKey()
    {
        updatePlayerPosition (stage.GetInitPosition());
        stage.ResetStage ();
        stage.UpdateStage ();
        soundController.OnRestart ();
    }

    void updatePlayerPosition(Vector2Int newPos)
    {
        if (newPos.Equals(pos))
        {
            return;
        }

        if(stage.IsValidPos(newPos))
        {
            playMoveSound ();
            stage.UpdateStage ();
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
