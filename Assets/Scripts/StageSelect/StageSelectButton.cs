using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class StageSelectButton : MonoBehaviour {

    //스테이지 이름
    string StageName;
    //스테이지 정보
    TextAsset Stage;
    
    public void Init(string S,TextAsset T)
    {
        StageName = S;
        Stage = T;
        Set_Text(S);
    }

    void Set_Text(string W)
    {
        transform.Find("Text").gameObject.GetComponent<Text>().text = W;
    }
}
