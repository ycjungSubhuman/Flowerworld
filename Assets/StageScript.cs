using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;
using Assets.Core.Data;
using Assets.Util;
using Assets.Core.Animation;

public class StageScript : MonoBehaviour {

    /* Initialize public members on instantiation */
    public Map map;
    public IMapAnimationController mapAnimationController;

    private int patternIndex = -1;

	// Use this for initialization
	void Start () {
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    public void ResetStage()
    {
        patternIndex = -1;
    }
    public Vector2Int GetInitPosition()
    {
        Debug.Assert (map != null);
        var startPositions = map.GlobalPositionsOf (Label.START);
        Debug.Assert (startPositions.Count () == 1);
        return startPositions.First ();
    }
    public bool IsValidPos(Vector2Int newPos)
    {
        return map.IsInside (newPos) && checkLogic (newPos);
    }
    public Vector2 ScenePosOf(Vector2Int pos)
    {
        var targetCell =  MapGameObjectUtil.GetCellGameObject (gameObject, pos);
        Vector2 scenePos = targetCell.transform.position;
        return scenePos;
    }
    public void AnimateCellStomp(Vector2Int pos)
    {
        mapAnimationController.SetTrigger (pos, "Stomp");
    }
    public void UpdateStage()
    {
        updatePattern ();
        updateCellColor ();
        updateUI ();
    }
    public Label CurrentPatternLabel()
    {
        return map.pattern [patternIndex];
    }
    private void updatePattern()
    {
        patternIndex = (patternIndex + 1) % map.pattern.Count;
    }
    private void updateCellColor()
    {
        var newAvailabePositions = map.GlobalPositionsOf (map.pattern [patternIndex]);
        foreach (var go in MapGameObjectUtil.GetAllCells (gameObject))
        {
            go.GetComponent<Animator> ().SetBool ("Onoff", false);
        }
        foreach(var pos in newAvailabePositions)
        {
            var go = MapGameObjectUtil.GetCellGameObject (gameObject, pos);
            go.GetComponent<Animator> ().SetBool ("Onoff", true);
        }
    }
    private void updateUI()
    {
        var patternUI = GameObject.Find ("PatternUI");
        for (int i=0; i<patternUI.transform.childCount; i++)
        {
            patternUI.transform.GetChild (i).SendMessage ("SetOnoff", patternIndex);
        }
    }
    private bool checkLogic(Vector2Int newPos)
    {
        var validLabel = map.pattern [patternIndex];
        var actualLabel = map.LabelOf (newPos);
        return actualLabel.FallIn (validLabel);
    }
}
