using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GlassSpriteRenderer : MonoBehaviour {

    //렌더할 그림의 이름
    string State;
    //금갔니?
    bool cracked = false;


    // Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    public void Set_Broken() {
        Sprite temp = Resources.Load<Sprite>( "Sprite/GlassFlower/Broken_" + State );
        GetComponent<SpriteRenderer>().sprite = temp;
        cracked = true;
    }

    public void Set_State(string v) {
        State = v;
        Sprite temp = Resources.Load<Sprite>( "Sprite/GlassFlower/" + State );
        GetComponent<SpriteRenderer>().sprite = temp;
    }
}
