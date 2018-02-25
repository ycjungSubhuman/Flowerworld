using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using Assets.Core.Data;
/*
 * ItemManager Class
 * 아이템을 맵에 배치하는 것, 아이템이 플레이어와 충돌하면 아이템을 인벤토리에 추가하는 것
 * 아이템 인벤토리를 관리하는 것, 아이템 On/Off시 상수를 관리하는 Class
 */

public class ItemManager : MonoBehaviour {

    public bool Spring = false;
    GameObject Player;
    Map map;
    int SpringCount;
    int Init_SpringCount;
    static readonly int GLASS_VARIATION = 3;
    static readonly int ITEM_VARIATION = 4;
    SortedList<string, int> Init_GlassCount = new SortedList<string, int>();
    SortedList<string, int> GlassCount = new SortedList<string, int> () { { "A", 0 }, { "B", 0 }, { "C", 0 } };
    Text SpringCount_Text;
    Text[] GlassCount_Text = new Text[ GLASS_VARIATION ];
    string[] FlowerType = { "A", "B", "C", "D", "E", "F" };

    Animator[] LightBox_Anim = new Animator[ ITEM_VARIATION ];
    string[] LightBox_Name = { "A", "S", "D", "F" };
    GameObject[] Glass_UI;

    public string Queued_Glass;

    // Use this for initialization
    void Start()
    {
        SpringCount = 0;
        Spring = false;
        Player = GameObject.FindWithTag ("Player");
        Set_Spring (false);
        Debug.Log( "LightBox_" + LightBox_Name[ 0 ] );
        for(int i=0;i< ITEM_VARIATION;i++) {
            Debug.Log( "LightBox_" + LightBox_Name[ i ] );
            LightBox_Anim[ i ] = GameObject.Find( "LightBox_" + LightBox_Name[ i ] ).GetComponent<Animator>();
            LightBox_Anim[ i ].SetBool( "On", false );
        }
    }

    void Update() {
        Item_StateUpdater();
        //Text_Updater ();
    }
    public void Set_Mapinfo( Map map_data ) {
        map = map_data;
    }

    //아이템이 있으면 Lightbox를 꺼주는 역할
    void Item_StateUpdater() {
        if( LightBox_Anim[ 0] == null) {
            for( int i = 0; i < ITEM_VARIATION; i++ ) {
                Debug.Log( "LightBox_" + LightBox_Name[ i ] );
                LightBox_Anim[ i ] = GameObject.Find( "LightBox_" + LightBox_Name[ i ] ).GetComponent<Animator>();
                LightBox_Anim[ i ].GetComponent<Animator>().SetBool( "On", false );
            }
        }
        LightBox_Anim[ 0 ].SetBool( "On", SpringCount <= 0 );
        LightBox_Anim[ 1 ].SetBool( "On", GlassCount[ "A" ] <= 0 );
        LightBox_Anim[ 2 ].SetBool( "On", GlassCount[ "B" ] <= 0 );
        LightBox_Anim[ 3 ].SetBool( "On", GlassCount[ "C" ] <= 0 );
    }

    public void Text_Updater()
    {

        if(SpringCount_Text == null)
        SpringCount_Text = GameObject.Find ("SpringCount_Text").GetComponent<Text> ();
        if ( GlassCount_Text[0] == null )
            for ( int i = 0; i < GLASS_VARIATION; i++ )
        {
            GlassCount_Text [i] = GameObject.Find ("GlassCount_Text_" + FlowerType [i]).GetComponent<Text> ();
        }

        SpringCount_Text.text = SpringCount.ToString ();

        for ( int i = 0; i < GLASS_VARIATION; i++ )
        {
            if(GlassCount.ContainsKey( FlowerType[ i ] ) )
            GlassCount_Text [i].text = GlassCount [FlowerType [i]].ToString ();
        }
    }

    //색유리 아이템 관련 함수

    //유리의 초기 정보(각 유리별 사용 가능 개수)
    public void Set_Glassinfo( SortedList<string, int> Info ) {
        foreach( KeyValuePair<string, int> GlassInfo in Info ) {
            Init_GlassCount.Add( GlassInfo.Key, GlassInfo.Value );
            GlassCount[GlassInfo.Key] = GlassInfo.Value;
        }

        //플레이어의 머리 위에 유리 UI를 띄워준다.
        Glass_UI = new GameObject[ GLASS_VARIATION ];
        for(int i = 0; i < GLASS_VARIATION; i++ ) {

        }
    }

    public void Reset_Glassinfo() {
        foreach( KeyValuePair<string, int> GlassInfo in Init_GlassCount ) {
            GlassCount[ GlassInfo.Key ] = GlassInfo.Value;
        }
        Queued_Glass = null;
    }


    public string Current_Glass() {
        return Queued_Glass;
    }


    //유리 쓸 수 있나요?(쓸 개수가 남아있나요)
    public bool Is_GlassAvailable(string FlowerType) {
        if( !GlassCount.ContainsKey( FlowerType ) || GlassCount[ FlowerType ] <= 0 )
            return false;
        else
            return true;
    }

    //유리를 썼어요
    public void Use_Glass( string FlowerType ) {
        if( GlassCount.ContainsKey( FlowerType ) )
            GlassCount[ FlowerType ]--;
    }

    //유리를 비활성화 상태로 만들어 줍니다
    public void TurnOff_Glass() {
        Queued_Glass = null;
    }
    public void Toggle_Glass( string FlowerType ) {
        Queued_Glass = FlowerType;
    }




    //스프링 아이템 (기능) 관련 함수
    public void Onclick_ToggleSpring() {
        GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>().Toggle_Spring();
    }
    public void Set_InitSpringCount(int value) {
        
        Init_SpringCount = value;
        SpringCount = Init_SpringCount;
    }

    public void Reset_SpringCount() {
        SpringCount = Init_SpringCount;
    }

    //남은 스프링의 개수를 설정하는 Method
    public void Set_SpringValue(int value ) {
        SpringCount = value;
    }
    //남은 스프링의 개수를 변경하는 Method(입력한 값 만큼 더합니다)
    public void Change_SpringValue(int delta) {
        SpringCount += delta;
    }
    public void Toggle_Spring() {
        Spring = !Spring;
        if( SpringCount <= 0 )
            Spring = false;
        Player.GetComponent<PlayerControlScript>().Set_SpringState( Spring );
    }
    public void Set_Spring(bool Is_On) {
        if( SpringCount <= 0 && Is_On)
            return;
        Spring = Is_On;
        Player.GetComponent<PlayerControlScript>().Set_SpringState( Spring );
    }
    void init(int S=0) {
        SpringCount = S;
    }
}
