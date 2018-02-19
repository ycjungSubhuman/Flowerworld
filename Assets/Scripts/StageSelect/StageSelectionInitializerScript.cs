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
                    //있으면 그 리스트에 추가하고 종료
                    Pair.Value.Add(mapSource);
                    Make_New = false;
                    break;

                }
            }
            //리스트를 뒤져봤지만 대분류에 해당하는 Pair가 없다
            if (Make_New)
            {
                //새로운 Pair를 만든다. 그리고 첫번째 원소를 넣어준다.
                KeyValuePair<string, List<TextAsset>> temp = new KeyValuePair<string, List<TextAsset>>(World, new List<TextAsset>());
                temp.Value.Add(mapSource);
                StageList.Add(temp);
            }
        }
        //Scrollrect의 사이즈를 조절한다
        WorldScrollContentRect.sizeDelta = new Vector2(WorldScrollRect_Height * (StageList.Count + 0.1f), WorldScrollContentRect.sizeDelta.y);
        //이제 대분류 버튼을 만들어 보자.

        //맵 상에 있는 대분류 버튼을 찾는다.
        GameObject WorldButtonTemplate = WorldScrollContentRect.gameObject.transform.Find("Button").gameObject;
        //비활성화한다.
        if (WorldButtonTemplate != null)
        {
            WorldButtonTemplate.SetActive(false);
            for (int i = 0; i < StageList.Count; i++)
            {
                GameObject temp = Instantiate(WorldButtonTemplate);
                RectTransform tempRect = temp.GetComponent<RectTransform>();
                //부모를 설정하고
                temp.transform.SetParent(WorldScrollContentRect.gameObject.transform);
                //버튼의 크기를 계산한 뒤
                float ButtonHeight = WorldScrollContentRect.rect.height * 0.8f;
                //Recttransform을 설정하고
                tempRect.SetSizeWithCurrentAnchors(RectTransform.Axis.Horizontal, ButtonHeight);
                tempRect.SetSizeWithCurrentAnchors(RectTransform.Axis.Vertical, ButtonHeight);
                tempRect.pivot = new Vector2(0, 0.5f);
                tempRect.anchoredPosition = new Vector2((16 + ButtonHeight) * i + 16, 0f);
                //활성화 시켜준 뒤
                tempRect.gameObject.SetActive(true);
                //Init을 실행한다.
                temp.GetComponent<WorldSelectButton>().Init(StageList[i].Key, StageList[i].Value);
            
            }

            //버튼을 만든다
        }

        /* 레거시 코드 */
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

    //적절한 버튼을 만들어서 리턴한다.
    GameObject Gen_Button(bool Is_WorldButton,string Name)
    {
        GameObject ScrollRect,Button;
        if (Is_WorldButton)
            ScrollRect = GameObject.Find("WorldSelectScrollRect");
        else
            ScrollRect = GameObject.Find("StageSelectScrollRect");

        Button = ScrollRect.transform.Find("Viewport").Find("Content").Find("Button").gameObject;

        return Button;
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
