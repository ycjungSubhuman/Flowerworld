using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
/*
 * ItemManager Class
 * 아이템을 맵에 배치하는 것, 아이템이 플레이어와 충돌하면 아이템을 인벤토리에 추가하는 것
 * 아이템 인벤토리를 관리하는 것, 아이템 On/Off시 상수를 관리하는 Class
 */

public class ItemManager : MonoBehaviour {

    public bool Spring = false;
    GameObject Player;
    int SpringCount;
    int Init_SpringCount;
    Text Temp_SpringCount;


	// Use this for initialization
	void Start () {
        Spring = false;
        Player = GameObject.FindWithTag( "Player" );
        Set_Spring( false );
        Temp_SpringCount = GameObject.Find( "Temp_SpringCount" ).GetComponent<Text>();
	}

    private void Update() {
        Temp_SpringCount.text = SpringCount.ToString();
    }

    //스프링 아이템 (기능) 관련 함수

    public void Temp_Onclick_ToggleSpring() {
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
