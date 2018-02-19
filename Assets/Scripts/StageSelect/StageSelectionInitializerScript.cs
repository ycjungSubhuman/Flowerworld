using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
using Assets.Util;
using Assets;
using UnityEngine.Events;

// 디버그용 스테이지 선택 씬 초기화 스크립트 (main 씬)
public class StageSelectionInitializerScript : MonoBehaviour {

    List<TextAsset> mapList = new List<TextAsset> ();
    Dropdown dropdown;

    public GameObject WorldScroll;
    public GameObject StageScroll;

    RectTransform WorldScrollContentRect;
    RectTransform StageScrollContentRect;
    GameObject StageButtonTemplate;
    GameObject Protector;

    float WorldScrollRect_Height;

    SortedList<string, List<TextAsset>> WorldList = new SortedList<string, List<TextAsset>>();


    // Use this for initialization
    void Start () {


        WorldScrollContentRect = WorldScroll.transform.GetChild(0).GetChild(0).GetComponent<RectTransform>();
        StageScrollContentRect = StageScroll.transform.GetChild(0).GetChild(0).GetComponent<RectTransform>();

        StageButtonTemplate = StageScrollContentRect.gameObject.transform.Find("Button").gameObject;
        StageButtonTemplate.SetActive(false);

        Protector = GameObject.Find("Protector");
        Protector.SetActive(false);

        StageScroll.SetActive(false);

        WorldScrollRect_Height = WorldScrollContentRect.rect.height;
        //미리 저장된 Map 파일을 읽은 뒤 분류한다
        List<TextAsset> mapSources = MapFileUtil.GetAllMapSources();

        Classify_Map(out WorldList, mapSources);
        //Scrollrect의 사이즈를 조절한다
        WorldScrollContentRect.sizeDelta = new Vector2(WorldScrollRect_Height * (WorldList.Count + 0.1f), WorldScrollContentRect.sizeDelta.y);

        //이제 대분류 버튼을 만들어 보자.
        Create_WorldSelect(WorldList);



        /* 레거시 코드 */

        GameObject mapSelection = GameObject.Find("MapSelection");
        GameObject startButton = GameObject.Find("StartButton");
        Button button = startButton.GetComponent<Button>();
        dropdown = mapSelection.GetComponent<Dropdown>();


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
    public void OnClick_WorldButton(string WorldName)
    {
        int i = WorldList.IndexOfKey(WorldName);
        Protector.SetActive(true);
        //Debug.Log(WorldName + " Pressed");
        StageScroll.SetActive(true);
        //위치를 지정한다.
        float WorldButtonWidth = WorldScrollContentRect.rect.height * 0.8f;
        float WorldScrollX = WorldScroll.transform.Find("Viewport").Find("Content").gameObject.GetComponent<RectTransform>().rect.x;
        StageScroll.GetComponent<RectTransform>().anchoredPosition = new Vector2(WorldScrollX + (16 + WorldButtonWidth) * i + 16, 0f);
        Debug.Log(i);
        Debug.Log((16 + WorldButtonWidth) * i + 16);
        //버튼을 만들어준다.
        Create_StageSelect(WorldName);

    }



    //이미 만들어진 StageSelect 창을 버튼으로 채워주는 Method
    void Create_StageSelect(string World)
    {

        if (WorldList.ContainsKey(World))
        {
            //월드 정보를 받아온다.
            List<TextAsset> StageList = WorldList[World];
            int i = 0;
            //각 스테이지 정보 별로 버튼을 만들어준다.
            foreach(TextAsset Stage in StageList)
            {
                //버튼의 이름
                string title = MapFileUtil.mapTitleOfFile(Stage);

                GameObject temp = Instantiate(StageButtonTemplate);
                temp.SetActive(true);
                RectTransform tempRect = temp.GetComponent<RectTransform>();

                //부모를 설정하고
                temp.transform.SetParent(StageScrollContentRect.gameObject.transform);
                //버튼의 크기를 계산한 뒤
                float ButtonHeight = StageScrollContentRect.rect.width * 0.45f;
                float ButtonWIdth = StageScrollContentRect.rect.width * 0.8f;
                //Recttransform을 설정하고
                tempRect.SetSizeWithCurrentAnchors(RectTransform.Axis.Horizontal, ButtonWIdth);
                tempRect.SetSizeWithCurrentAnchors(RectTransform.Axis.Vertical, ButtonHeight);
                tempRect.pivot = new Vector2(0.5f, 1f);
                tempRect.anchoredPosition = new Vector2(0f, ((8 + ButtonHeight) * i + 8) * -1f);
                //활성화 시켜준 뒤
                tempRect.gameObject.SetActive(true);
                //Init을 실행한다.
                temp.GetComponent<StageSelectButton>().Init(title,Stage);

                UnityAction Call = delegate { StartStage(Stage);  };
                temp.GetComponent<Button>().onClick.AddListener(Call);
                i++;
            }
  
        }
        else
        {
            Debug.Log("Selected World (" + World + ") isn't Exist!");
        }
    }

    void Create_WorldSelect(SortedList<string, List<TextAsset>> WorldList)
    {
        //맵 상에 있는 대분류 버튼을 찾는다.
        GameObject WorldButtonTemplate = WorldScrollContentRect.gameObject.transform.Find("Button").gameObject;
        //비활성화한다.
        if (WorldButtonTemplate != null)
        { 
            int i = 0;
            WorldButtonTemplate.SetActive(false);
            foreach (KeyValuePair<string, List<TextAsset>> Stage in WorldList)
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
                temp.GetComponent<WorldSelectButton>().Init(Stage.Key, Stage.Value);


                UnityAction Call = delegate { OnClick_WorldButton(Stage.Key); };
                temp.GetComponent<Button>().onClick.AddListener(Call);
                
                i++;
            }
        }

    }

    void Classify_Map(out SortedList<string, List<TextAsset>> WorldList, List<TextAsset> mapSources)
    {
        SortedList<string, List<TextAsset>> tempWorldList = new SortedList<string, List<TextAsset>>();
        //파일 명: map-(대분류)-(소분류)
        //KeyValueSortedList 만들어서 쓰자
        //string : 대분류를 나타내는 문자열
        //맵들을 대분류에 맞게 분류한다.
        foreach (TextAsset mapSource in mapSources)
        {
            //맵을 찾았다
            string World = MapFileUtil.mapWorld(mapSource);
           // bool Make_New = true;

            //대분류에 해당하는 Pair가 리스트에 있는가?
            if(tempWorldList.ContainsKey(World))
            {
                //있으면 그 리스트에 추가하고 종료
                tempWorldList[World].Add(mapSource);
            }
            else
            {
                //새로운 Pair를 만든다. 그리고 첫번째 원소를 넣어준다.
                List<TextAsset> temp = new List<TextAsset>();
                temp.Add(mapSource);
                tempWorldList.Add(World, temp);
            }
            /*
            foreach (KeyValuePair<string, List<TextAsset>> Pair in tempWorldList)
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
                tempWorldList.Add(temp);
            }
            */
        }
        WorldList = tempWorldList;
    }

    void StartStage(TextAsset selection)
    {
        Configuration.Instance.activatedMapSource = selection;
        SceneManager.LoadScene("GameplayScene");
    }

    void StartStage()
    {
        var selection = mapList [dropdown.value];
        Configuration.Instance.activatedMapSource = selection;
        SceneManager.LoadScene ("GameplayScene");
    }
}
