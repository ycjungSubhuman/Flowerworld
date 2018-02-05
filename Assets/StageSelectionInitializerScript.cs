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
        var mapSources = MapFileUtil.GetAllMapSources ();
        var mapSelection = GameObject.Find ("MapSelection");
        var startButton = GameObject.Find ("StartButton");
        var button = startButton.GetComponent<Button> ();
        dropdown = mapSelection.GetComponent<Dropdown> ();

        foreach (var mapSource in mapSources)
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
