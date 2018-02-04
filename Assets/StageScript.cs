using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;
using Assets.Core.Data;
using Assets.Util;
using Assets.Core.Animation;

/**
 * 스테이지의 로직과 스테이지 로직 상태에 따른 그래픽 이벤트 발생을 관리하는 스크립트
 */
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

    /** 스테이지를 초기 상태로 되돌린다 */
    public void ResetStage()
    {
        patternIndex = -1;
    }
    /** START 레이블의 포지션을 불러온다 */
    public Vector2Int GetInitPosition()
    {
        Debug.Assert (map != null);
        var startPositions = map.GlobalPositionsOf (Label.START);
        Debug.Assert (startPositions.Count () == 1);
        return startPositions.First ();
    }
    /** newPos가 현재 스테이지 상태에서 다음 위치로 적합한지 체크한다 */
    public bool IsValidPos(Vector2Int newPos)
    {
        return map.IsInside (newPos) && checkLogic (newPos);
    }
    /** 
     * 그래픽 상으로 나타나는 Cell의 Transform Position을 불러온다. 
     * TODO : 추후에 Logic과 분리 필요
     */ 
    public Vector2 ScenePosOf(Vector2Int pos)
    {
        var targetCell =  MapGameObjectUtil.GetCellGameObject (gameObject, pos);
        Vector2 scenePos = targetCell.transform.position;
        return scenePos;
    }
    /** pos에 있는 Cell의 Stomp 애니메이션을 시작한다 */
    public void AnimateCellStomp(Vector2Int pos)
    {
        mapAnimationController.SetTrigger (pos, "Stomp");
    }
    /** position에 플레이어가 갔을 때 스테이지를 업데이트한다 */
    public void UpdateStage(Vector2Int position)
    {
        updatePattern ();
        updateCellColor ();
        updateUI ();
        if(checkGoal (position))
        {
            //TODO : 게임 끝났을 때 할 것 
            Debug.Log ("GOAL");
        }
    }
    /** 현재 패턴에서 활성화된 레이블을 가져온다 */ 
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
    private bool checkGoal (Vector2Int position)
    {
        return map.LabelOf (position).FallIn (Label.GOAL);
    }
    private bool checkLogic(Vector2Int newPos)
    {
        var validLabel = map.pattern [patternIndex];
        var actualLabel = map.LabelOf (newPos);
        return actualLabel.FallIn (validLabel);
    }
}
