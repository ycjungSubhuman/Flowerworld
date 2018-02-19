using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class WorldSelectButton : MonoBehaviour {

    //이 월드의 이름
    string WorldName;
    //월드에 포함된 리스트들
    List<TextAsset> WorldList = new List<TextAsset>();

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    //같은 월드로 분류된 스테이지 정보를 전달합니다(TextAsset에는 스테이지의 모든 정보가 전달됩니다)
    public void Init(string W, List<TextAsset> S)
    {
        WorldName = W;
        WorldList = S;

        Set_Text(W);
    }

    public void Set_Text(string W)
    {
        transform.Find("Text").gameObject.GetComponent<Text>().text = W;
    }
}
