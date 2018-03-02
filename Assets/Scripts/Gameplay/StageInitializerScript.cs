using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;
using UnityEngine.UI;
using Newtonsoft.Json;
using Assets.Core.Data;
using Assets.Core.Drawer;
using Assets.Core.Sound;
using Assets.Core.Animation;

//씬 초기화 후 게임을 초기화해주는 스크립트
public class StageInitializerScript : MonoBehaviour {

    //Main씬에서 넘겨준 데이터
    private Assets.Configuration configuration = Assets.Configuration.Instance;

    //스프링 개수
    int SpringCount;

    Map map;
    GameObject player;

    // Use this for initialization
    void Start() {
        // 맵 불러오기
        StageScript.Cleared = false;
        TextAsset json = configuration.activatedMapSource;
        if( json == null ) {
            // 디버그용 (GameplayScene을 바로 플레이할 때)
            json = Resources.Load<TextAsset>( "maps/map-1-1" );
        }
        map = JsonConvert.DeserializeObject<Map>( json.text );

        // 맵 Scene에 그리기

        //CellDrawer : 한 셀
        //MapBlockDrawer : 그리드
        //MapDrawer : 나머지 전부
        var mapDrawer = new MapDrawer( new MapBlockDrawer( new CellDrawer() ) );
        var mapGameObject = mapDrawer.Draw( map );
        mapGameObject.AddComponent<StageScript>();
        var stageScript = mapGameObject.GetComponent<StageScript>();


        stageScript.map = map;

        stageScript.mapAnimationController = new MapAnimationController( mapGameObject );

        //맵의 유리 정보 초기화
        map.GlassMap();
        //맵에서 사용가능한 스프링 개수 받아와서 할당
        SpringCount = map.springsAvailable;


        //UI 생성
        var uiPatternDrawer = new UIPatternDrawer( new UICellDrawer() );
        var uiPattern = uiPatternDrawer.Draw( map.pattern );
        uiPattern.transform.SetParent( GameObject.Find( "PatternRoot" ).transform );
        uiPattern.GetComponent<RectTransform>().anchoredPosition = new Vector2( 0, 0 );
        uiPattern.name = "PatternUI";


        Text Tutorial = GameObject.Find( "Tutorial" ).GetComponent<Text>();
        Tutorial.text = map.comment;


        //플레이어 생성 및 초기화 
        player = GameObject.Instantiate( Resources.Load<GameObject>( "prefabs/player" ) );
        player.name = "Player";
        var playerScript = player.GetComponent<PlayerControlScript>();
        playerScript.stageRoot = mapGameObject;
        playerScript.soundController = new PlayerSoundController( player );

  
        Invoke( "Set_ItemValue", 0.2f );

        //스테이지 이름
        GameObject.Find ("StageName").GetComponent<Text> ().text = configuration.mapName;
        GameObject.Find ("Clear_Notification").GetComponent<ClearNotification> ().DisableClearNotification ();

        itemVisualization ();
    }
    IEnumerator iconRoutine(Sprite sprite, int pos)
    {
        const float duration = 1.5f;
        float t = 0f;
        var fg = GameObject.Find ("FG_Canvas");
        var icon = GameObject.Instantiate (Resources.Load<GameObject> ("prefabs/ItemIcon"));
        icon.GetComponent<Image> ().sprite = sprite;
        icon.transform.SetParent (fg.transform);
        while ( t < duration )
        {
            t += Time.deltaTime;
            yield return null;
        }
        while (true)
        {
            if (Input.anyKey || Input.GetMouseButton(0))
            {
                break;
            }
            yield return null;
        }
        icon.GetComponent<Animator> ().SetInteger ("pos", pos);
        t = 0;
        StartCoroutine (destroyDelay (icon));
        yield return null;
    }
    IEnumerator destroyDelay(GameObject obj)
    {
        const float postDelay = 1f;
        float t = 0;
        while ( t < postDelay )
        {
            t += Time.deltaTime;
            yield return null;
        }
        Destroy (obj);
    }

    IEnumerator springRoutine()
    {
        return iconRoutine (Resources.Load<Sprite>("Sprite/UI/Spring"), 1);
    }
    IEnumerator glassARoutine()
    {
        return iconRoutine (Resources.Load<Sprite> ("Sprite/UI/GlassA"), 2);
    }
    IEnumerator glassBRoutine()
    {
        return iconRoutine (Resources.Load<Sprite> ("Sprite/UI/GlassB"), 3);
    }
    IEnumerator glassCRoutine()
    {
        return iconRoutine (Resources.Load<Sprite> ("Sprite/UI/GlassC"), 4);
    }
    IEnumerator finishRoutine()
    {
        Debug.Log ("Called");
        player.GetComponent<PlayerControlScript> ().StartControl ();
        GameObject.Find ("ItemBarrier").GetComponent<Animator> ().SetBool ("on", false);
        yield return null;
    }

    List<IEnumerator> genItemRoutines()
    {
        var li = new List<IEnumerator> ();
        if(map.springsAvailable > 0)
        {
            li.Add (springRoutine());
        }
        if(map.glassAvailable["A"] > 0)
        {
            li.Add (glassARoutine ());
        }
        if ( map.glassAvailable ["B"] > 0 )
        {
            li.Add (glassBRoutine ());
        }
        if ( map.glassAvailable ["C"] > 0 )
        {
            li.Add (glassCRoutine ());
        }
        li.Add (finishRoutine ());
        return li;
    }

    IEnumerator sequence(IEnumerator a, IEnumerator b)
    {
        while(a.MoveNext())
        {
            yield return null;
        }
        while(b.MoveNext ())
        {
            yield return null;
        }
    }

    void itemVisualization ()
    {
        var itemRoutines = genItemRoutines ();
        var seq = itemRoutines.Aggregate ((a, b) => { return sequence (a, b); });
        StartCoroutine (seq);
    }

    void Set_ItemValue() {
        SortedList<string, int> temp = new SortedList<string, int>();
        GetComponent<ItemManager>().Set_InitSpringCount( SpringCount );
        temp = map.glassAvailable;
        GetComponent<ItemManager>().Set_Glassinfo( temp );
        GetComponent<ItemManager>().Set_Mapinfo( this.map );
    }
}
