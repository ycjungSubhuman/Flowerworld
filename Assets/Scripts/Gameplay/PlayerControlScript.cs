using UnityEngine;
using UnityEngine.UI;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Assets.Core.Data;
using Assets.Util;
using Assets.Core.Sound;
using Assets.Core.Animation.Coroutines;
using Assets.Scripts;

/**
 * 플레이어의 컨트롤에 대한 반응을 처리하는 스크립트
 */
public class PlayerControlScript : MonoBehaviour
{
    /* Initialize public members on instantiation */
    public IPlayerSoundController soundController;
    [HideInInspector]
    public GameObject stageRoot;

    private GameObject GlassDeploy_Barrier;

    private Vector2Int pos;
    private StageScript stage;

    private GameObject Reset, BacktoMain;
    private ItemManager IM;

    private bool Glass_Selecting = false;
    private bool Glass_Deploying = false;

    private bool Already_Deployed = false;




    int PosDelta;

    void Start()
    {
        GlassDeploy_Barrier = GameObject.Find( "GlassDeploy_Barrier" );
        GlassDeploy_Barrier.SetActive( false );
        Reset = GameObject.Find("ResetIndicator");
        BacktoMain = GameObject.Find("BacktoMainIndicator");
        IM = GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>();

        Debug.Assert (stageRoot != null);
        stage = stageRoot.GetComponent<StageScript> ();
        pos = stage.GetInitPosition ();
        Vector2 initScenepos = stage.ScenePosOf (pos);
        gameObject.transform.localPosition = initScenepos;
        stage.UpdateStage (pos,1);
    }

    void Update()
    {
        Vector2Int newPos = pos;
        Vector2Int glassPos = pos;

        if( !StageScript.Cleared ) {
            if( !Glass_Selecting ) {
                if( Input.GetKeyDown( KeyCode.UpArrow ) ) {
                    newPos += new Vector2Int( PosDelta * -1, 0 );
                } else if( Input.GetKeyDown( KeyCode.DownArrow ) ) {
                    newPos += new Vector2Int( PosDelta, 0 );
                } else if( Input.GetKeyDown( KeyCode.RightArrow ) ) {
                    newPos += new Vector2Int( 0, PosDelta );
                } else if( Input.GetKeyDown( KeyCode.LeftArrow ) ) {
                    newPos += new Vector2Int( 0, PosDelta * -1 );
                }
            }
            if( Input.GetKeyDown( KeyCode.A ) ) {
                IM.Onclick_ToggleSpring();

                if( PosDelta == 1 )
                    gameObject.GetComponent<Animator>().SetBool( "SpringOn", false );
                else
                    gameObject.GetComponent<Animator>().SetBool( "SpringOn", true );
            }
            if( Input.GetKeyDown( KeyCode.S ) ) {
                if( IM.Is_GlassAvailable( "A" ) ) {
                    IM.Toggle_Glass( "A" );
                    Toggle_Glass();
                }
            }
            if( Input.GetKeyDown( KeyCode.D ) ) {
                if( IM.Is_GlassAvailable( "B" ) ) {
                    IM.Toggle_Glass( "B" );
                    Toggle_Glass();
                }
            }
            if( Input.GetKeyDown( KeyCode.F ) ) {
                if( IM.Is_GlassAvailable( "C" ) ) {
                    IM.Toggle_Glass( "C" );
                    Toggle_Glass();
                }
            }


            //if ( Glass_Deploying )
            if( Glass_Selecting ) {
                stage.UpdateGlassHighlight( pos, PosDelta );
                GlassDeploy_Barrier.SetActive( true );

                if( Input.GetKeyUp( KeyCode.UpArrow ) ) {
                    glassPos += new Vector2Int( PosDelta * -1, 0 );
                } else if( Input.GetKeyUp( KeyCode.DownArrow ) ) {
                    glassPos += new Vector2Int( PosDelta, 0 );
                } else if( Input.GetKeyUp( KeyCode.RightArrow ) ) {
                    glassPos += new Vector2Int( 0, PosDelta );
                } else if( Input.GetKeyUp( KeyCode.LeftArrow ) ) {
                    glassPos += new Vector2Int( 0, PosDelta * -1 );
                }

                //glassPos가 변경되었다 : 유리를 놓을 것이다.
                if( glassPos != pos && !Already_Deployed ) {
                    //놓을 수 있는 위치인가?
                    if( stage.IsValidGlassPos( glassPos ) ) {
                        ItemManager Current_ItemManager = GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>();
                        //ItemManager에게 지금 활성화된 유리를 받아온다
                        string Current_Glass_Name = Current_ItemManager.Current_Glass();
                        soundController.OnGlassCreate ();

                        if( Current_ItemManager.Is_GlassAvailable( Current_Glass_Name ) ) {
                            //유리를 놓는 함수를 호출한다(현재 켜진 유리를 Deploy하는 함수)
                            Label temp = new Label();
                            if( Current_Glass_Name == "A" )
                                temp = Label.A;
                            if( Current_Glass_Name == "B" )
                                temp = Label.B;
                            if( Current_Glass_Name == "C" )
                                temp = Label.C;
                            if( Current_Glass_Name == "D" )
                                temp = Label.D;
                            stage.map.Deploy_Glass( glassPos, temp );
                            Current_ItemManager.Use_Glass( Current_Glass_Name );

                            Already_Deployed = true;
                            Glass_Selecting = false;
                            Glass_Deploying = false;
                            stage.UpdateMapHighlight( pos, PosDelta );
                        }
                    }
                }
            }
            if( !Glass_Selecting )
                GlassDeploy_Barrier.SetActive( false );

            updatePlayerPosition( newPos );

        }

        if( Input.GetKeyDown( KeyCode.R ) ) {
            Reset.GetComponent<Reset>().Pressed = true;
            PosDelta = 1;
        } else {
            Reset.GetComponent<Reset>().Pressed = false;
        }
        if( Input.GetKeyDown( KeyCode.Escape ) ) {
            BacktoMain.GetComponent<Reset>().Pressed = true;
        } else {
            BacktoMain.GetComponent<Reset>().Pressed = false;
        }
        //WASD로 자신의 상하좌우 중 한칸에 현재 선택한 유리를 설치 가능.
    }


    void Toggle_Glass() {
        Glass_Selecting = !Glass_Selecting;
        stage.UpdateMapHighlight( pos, PosDelta );
    }

    public void Set_SpringState(bool On) {
        if( On )
            PosDelta = 2;
        else
            PosDelta = 1;
        
        stage.UpdateMapHighlight (pos, PosDelta);
    }
    public void onGotoStageSelect()
    {
        StartCoroutine(LoadYourAsyncScene());
        TitleMusicScript.Instance.PlayMusic ();
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
        GameObject.Find( "Clear_Notification" ).GetComponent<Animator>().SetBool( "On", false );
        StageScript.Cleared = false;
        GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>().Reset_Glassinfo();
        updatePlayerPosition (stage.GetInitPosition(), true);
        stage.ResetStage ();
        stage.UpdateStage (pos,1);
        soundController.OnRestart ();
        //모든 유리를 철거한다.
        stage.map.Remove_All_Glass();

        Already_Deployed = false;
    }

    //
    void updatePlayerPosition(Vector2Int newPos, bool isReset=false)
    {
        
        if (newPos.Equals(pos) || StageScript.Cleared)
        {
            return;
        }
        //일반적으로 갈 수 있는 장소인가?
        bool IsValidPos = stage.IsValidPos( newPos );


        if(IsValidPos || isReset) {           

            if ( !stage.map.IsGlassNotDeployed (newPos) ) {
                soundController.OnGlassDestroy ();
            }
           
            //현재 위치에 유리가 있다면 깨부순다
            stage.map.Remove_Glass( pos );

            playMoveSound ();
            stage.UpdateStage (newPos,1);
            movePlayer (newPos);
            //움직인 뒤에 스프링이 켜져있으면 끄고 1 차감한다.
            if(PosDelta != 1 && !isReset) {
                IM.Set_Spring( false );
                IM.Change_SpringValue( -1 );
            }

            //움직인 위치에 유리가 있다면 이미지를 바꿔준다
            stage.map.Crack_Glass (pos);


            //유리를 다시 설치할 수 있게 바꿔준다
            Already_Deployed = false;

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
        gameObject.GetComponent<Animator>().SetBool( "SpringOn", false );
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
