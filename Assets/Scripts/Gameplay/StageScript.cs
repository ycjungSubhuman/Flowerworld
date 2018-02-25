using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
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
    private int moveCount = 0;

    static public bool Cleared = false;

	// Use this for initialization
	void Start () {
        initGoalCount ();
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    /** 스테이지를 초기 상태로 되돌린다 */
    public void ResetStage()
    {
        patternIndex = -1;
        moveCount = 0;
    }
    /** START 레이블의 포지션을 불러온다 */
    public Vector2Int GetInitPosition()
    {
        Debug.Assert (map != null);
        var startPositions = map.PositionsOf (Label.START);
        Debug.Assert (startPositions.Count () == 1);
        return startPositions.First ();
    }
    /** newPos가 현재 스테이지 상태에서 다음 위치로 적합한지 체크한다 */
    public bool IsValidPos(Vector2Int newPos)
    {
        return map.IsInside (newPos) && checkLogic (newPos);
    }
    public bool IsValidGlassPos(Vector2Int newPos) {
        return ((map.IsInside( newPos ) && map.IsGlassDeployed(newPos)) && !map.IsEmpty(newPos));
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
        //mapAnimationController.SetTrigger (pos, "Stomp");
    }
    //플레이어의 위치는 그대로인데 스테이지 하이라이팅만 바꿔야 할 떄 호출한다.
    public void UpdateMapHighlight(Vector2Int position, int Delta)
    {
        updateCellColor (position, Delta);
    }
    //유리 하이라이팅
    public void UpdateGlassHighlight( Vector2Int position, int Delta ) {
        updateGlassColor( position, Delta );
    }
    /** position에 플레이어가 갔을 때 스테이지를 업데이트한다 */
    public void UpdateStage(Vector2Int position,int Delta)
    {
        updatePattern();
        updateCellColor(position,Delta);
        updateUI ();

        updateMoveCount ();
        moveCount++;
        if(checkGoal (position))
        {
            Cleared = true;
            GameObject.Find( "Clear_Notification" ).GetComponent<Animator>().SetBool( "On", true );

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
    private void updateGlassColor( Vector2Int position, int Delta ) {

        var newAvailabePositions = map.PositionsOf( Label.ANY );
        foreach( var go in MapGameObjectUtil.GetAllCells( gameObject ) ) {
            go.GetComponent<Animator>().SetBool( "Glass", false);
            go.GetComponent<Animator>().SetBool( "Onoff", false );
        }
        foreach( var pos in newAvailabePositions ) {
            //이 셀의 상하좌우 중 한칸에 플레이어가 있을 때만
            if( ( ( pos.x - position.x ) * ( pos.x - position.x ) == Delta * Delta || ( pos.y - position.y ) * ( pos.y - position.y ) == Delta * Delta ) && ( pos.x - position.x ) * ( pos.y - position.y ) == 0 ) {
                GameObject go = MapGameObjectUtil.GetCellGameObject( gameObject, pos );

                go.GetComponent<Animator>().SetBool( "Onoff", true );
                go.GetComponent<Animator>().SetBool( "Glass", true );
            }
        }
    }
    private void updateCellColor(Vector2Int position,int Delta)
    {
        var newAvailabePositions = map.PositionsOf (map.pattern [patternIndex]);
        foreach (var go in MapGameObjectUtil.GetAllCells (gameObject))
        {
            go.GetComponent<Animator>().SetBool( "Glass", false );
            go.GetComponent<Animator> ().SetBool ("Onoff", false);
        }
        foreach(var pos in newAvailabePositions)
        {
            //이 셀의 상하좌우 중 한칸에 플레이어가 있을 때만
            if ( ((pos.x - position.x) * (pos.x - position.x) == Delta * Delta || (pos.y - position.y) * (pos.y - position.y) == Delta * Delta ) && (pos.x - position.x) * (pos.y - position.y) == 0)
            {
                GameObject go;

                   go = MapGameObjectUtil.GetCellGameObject (gameObject, pos);

                if( map.GlassLabelOf( pos ).Value == Label.ANY.Value)
                   go.GetComponent<Animator> ().SetBool ("Onoff", true);
                else if(map.GlassLabelOf( pos ).Value == map.pattern[ patternIndex ].Value )
                   go.GetComponent<Animator>().SetBool( "Onoff", true );

            }
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
        //기본적으로는 이미 지정된 Label의 데이터와 비교해서 True/false를 리턴한다.
        //마법신발이 적용되어있으면 무조건 True를 리턴하고
        //newPos에 색유리가 깔려 있으면 그 지점을 대신 비교한다.

        var validLabel = map.pattern [patternIndex];//다음에 갈 수 있는 칸

        var actualLabel = map.LabelOf (newPos);//플레이어가 입력한 칸(여기로 가고 싶어요)
        var glassLabel = map.GlassLabelOf( newPos );

        if( glassLabel.Value == Label.ANY.Value )
            return actualLabel.FallIn( validLabel );
        else
            return glassLabel.FallIn( validLabel );

    }
    void initGoalCount()
    {
       // var text = GameObject.Find ("GoalCountText").GetComponent<Text>();
        //text.text = map.goalCount.ToString ();
    }
    void updateMoveCount()
    {
        var text = GameObject.Find ("MoveCountText").GetComponent<Text>();
        text.text = moveCount.ToString ();
    }
}
