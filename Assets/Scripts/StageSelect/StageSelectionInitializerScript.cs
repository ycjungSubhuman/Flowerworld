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

    public GameObject WorldScroll;
    public GameObject StageScroll;

    RectTransform WorldScrollContentRect;
    float WorldScrollRect_Height;

	// Use this for initialization
	void Start () {
        WorldScrollContentRect = WorldScroll.transform.GetChild(0).GetChild(0).GetComponent<RectTransform>();
        WorldScrollRect_Height = WorldScrollContentRect.rect.height;
        //미리 저장된 Map 파일을 읽은 뒤
        List<TextAsset> mapSources = MapFileUtil.GetAllMapSources ();
        GameObject mapSelection = GameObject.Find ("MapSelection");
        GameObject startButton = GameObject.Find ("StartButton");
        Button button = startButton.GetComponent<Button> ();
        dropdown = mapSelection.GetComponent<Dropdown> ();

        //파일 명: map-(대분류)-(소분류)
        //KeyValueDictionary 만들어서 쓰자
        //string : 대분류를 나타내는 문자열
        List<KeyValuePair<string, List<TextAsset>>> StageList = new List<KeyValuePair<string, List<TextAsset>>>();
        
        //맵들을 대분류에 맞게 분류한다.
        foreach (TextAsset mapSource in mapSources)
        {
            //맵을 찾았다
            string World = MapFileUtil.mapWorld(mapSource);
            bool Make_New = true;
            //대분류에 해당하는 Pair가 리스트에 있는가?
            foreach (KeyValuePair<string, List<TextAsset>> Pair in StageList)
            {
                if (Pair.Key == World)
                {
                    Pair.Value.Add(mapSource);
                    Make_New = false;
                    break;

                }
            }
            if (Make_New)
            {
                KeyValuePair<string, List<TextAsset>> temp = new KeyValuePair<string, List<TextAsset>>(World, new List<TextAsset>());
                temp.Value.Add(mapSource);
                StageList.Add(temp);
            }
        }

        //Scrollrect의 사이즈를 조절한다
        WorldScrollContentRect.sizeDelta = new Vector2(WorldScrollRect_Height * (StageList.Count + 0.1f), WorldScrollContentRect.sizeDelta.y);
        //이제 대분류 버튼을 만들어 보자.

        for (int i=0;i<StageList.Count;i++)
        {
            //버튼을 크롭하고
            //부모를 설정하고
            //Recttransform을 설정하고
            //Init을 실행하고
        }
      
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

    void Gen_Button(bool Is_WorldButton)
    {
        GameObject ScrollRect;
        if (Is_WorldButton)
            ScrollRect = GameObject.Find("WorldSelectScrollRect");
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
