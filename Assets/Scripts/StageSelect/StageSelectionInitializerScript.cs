using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
using Assets.Util;
using Assets;


// 디버그용 스테이지 선택 씬 초기화 스크립트 (main 씬)
public class StageSelectionInitializerScript : MonoBehaviour {

    List<TextAsset> mapList = new List<TextAsset> ();
    Dropdown dropdown;

	// Use this for initialization
	void Start () {
        //미리 저장된 Map 파일을 읽은 뒤
        List<TextAsset> mapSources = MapFileUtil.GetAllMapSources ();
        GameObject mapSelection = GameObject.Find ("MapSelection");
        GameObject startButton = GameObject.Find ("StartButton");
        Button button = startButton.GetComponent<Button> ();
        dropdown = mapSelection.GetComponent<Dropdown> ();

        //파일 명: map-(대분류)-(소분류)
        //분류하고
        //버튼을 만든다
        
        //Dropbox에 넣어준다
        foreach (TextAsset mapSource in mapSources)
        {
            var title = MapFileUtil.mapTitleOfFile (mapSource);
            dropdown.options.Add (new Dropdown.OptionData(title));
            mapList.Add (mapSource);
            button.onClick.AddListener (StartStage);
        }
        dropdown.value = 0;
        dropdown.RefreshShownValue ();
	}

    void StartStage()
    {
        var selection = mapList [dropdown.value];
        Configuration.Instance.activatedMapSource = selection;
        SceneManager.LoadScene ("GameplayScene");
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
