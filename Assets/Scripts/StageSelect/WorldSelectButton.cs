using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class WorldScrollRectButton : MonoBehaviour {

    //이 월드의 이름
    string WorldName;
    //월드에 포함된 리스트들
    List<TextAsset> StageList = new List<TextAsset>();

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
    void Init(string W,List<TextAsset> S)
    {
        WorldName = W;
        StageList = S;
    }
}
