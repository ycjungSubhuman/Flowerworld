using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/*
 * ItemManager Class
 * 아이템을 맵에 배치하는 것, 아이템이 플레이어와 충돌하면 아이템을 인벤토리에 추가하는 것
 * 아이템 인벤토리를 관리하는 것, 아이템 On/Off시 상수를 관리하는 Class
 */

public class ItemManager : MonoBehaviour {

    public bool Spring = false;
    GameObject Player;
    int SpringCount;
	// Use this for initialization
	void Start () {
        Spring = false;
        Player = GameObject.FindWithTag( "Player" );
        Set_Spring( false );
	}
    public void Temp_Onclick_ToggleSpring() {
        GameObject.Find( "StageInitializer" ).GetComponent<ItemManager>().Toggle_Spring();
    }
    public void Toggle_Spring() {
        Spring = !Spring;
        Player.GetComponent<PlayerControlScript>().Set_SpringState( Spring );
    }
    public void Set_Spring(bool Is_On) {
        Spring = Is_On;
        Player.GetComponent<PlayerControlScript>().Set_SpringState( Spring );
    }
    void init(int S=0) {
        SpringCount = S;
    }
}
