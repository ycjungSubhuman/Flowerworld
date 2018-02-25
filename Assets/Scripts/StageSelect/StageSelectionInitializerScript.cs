using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
using Assets.Util;
using Assets;
using UnityEngine.Events;
using Assets.Core.Animation.Coroutines;
using System;
using Assets.Scripts;

// 디버그용 스테이지 선택 씬 초기화 스크립트 (main 씬)
public class StageSelectionInitializerScript : MonoBehaviour
{

    List<TextAsset> mapList = new List<TextAsset> ();
    Dropdown dropdown;

    public GameObject WorldScroll;
    public GameObject StageScroll;

    RectTransform WorldScrollContentRect;
    RectTransform StageScrollContentRect;
    GameObject StageButtonTemplate;
    GameObject Protector;

    List<GameObject> Prv_StageButton = new List<GameObject> ();

    float WorldScrollRect_Height;

    SortedList<string, List<TextAsset>> WorldList = new SortedList<string, List<TextAsset>> ();
    public Action Restore = () => { };

    void Start()
    {

        Prv_StageButton.Clear ();
        WorldScrollContentRect = WorldScroll.transform.GetChild (0).GetChild (0).GetComponent<RectTransform> ();
        StageScrollContentRect = StageScroll.transform.GetChild (0).GetChild (0).GetComponent<RectTransform> ();

        StageButtonTemplate = StageScrollContentRect.gameObject.transform.Find ("Button").gameObject;
        StageButtonTemplate.SetActive (false);

        Protector = GameObject.Find ("Protector");
        Protector.SetActive (false);

        StageScroll.SetActive (false);

        WorldScrollRect_Height = WorldScrollContentRect.rect.height;
        List<TextAsset> mapSources = MapFileUtil.GetAllMapSources ();

        Classify_Map (out WorldList, mapSources);
        WorldScrollContentRect.sizeDelta = new Vector2 (WorldScrollRect_Height * (WorldList.Count + 0.1f), WorldScrollContentRect.sizeDelta.y);

        Create_WorldSelect (WorldList);
    }
    public void OnClick_WorldButton(string WorldName)
    {
        int i = WorldList.IndexOfKey (WorldName);
        Protector.SetActive (true);

        StageScroll.SetActive (true);
        //위치를 지정한다.
        float WorldButtonWidth = WorldScrollContentRect.rect.height * 0.8f;
        float WorldScrollX = WorldScroll.transform.Find ("Viewport").Find ("Content").gameObject.GetComponent<RectTransform> ().rect.x;

        StageScroll.GetComponent<RectTransform> ().anchoredPosition = 
            new Vector2 (
                WorldScrollContentRect.transform.GetChild(i+1).GetComponent<RectTransform>().anchoredPosition.x + 380f, 
                StageScroll.GetComponent<RectTransform>().anchoredPosition.y-80f);
        Debug.Log (i);
        Debug.Log ((16 + WorldButtonWidth) * i + 16);
        Create_StageSelect (WorldName);
    }




    //이미 만들어진 StageSelect 창을 버튼으로 채워주는 Method
    void Create_StageSelect(string World)
    {


        if ( WorldList.ContainsKey (World) )
        {
            //이미 만들어진 버튼을 비활성화 한다.
            if ( Prv_StageButton.Count != 0 )
            {
                for ( int j = 0; j < Prv_StageButton.Count; j++ )
                {
                    Destroy (Prv_StageButton [j]);
                }
            }
            Prv_StageButton.Clear ();


            List<TextAsset> StageList = WorldList [World];
            ResetButtonPositions ();
            StopAllCoroutines ();

            int i = 0;
            foreach ( TextAsset Stage in StageList )
            {
                string title = MapFileUtil.mapTitleOfFile (Stage);

                GameObject temp = Instantiate (StageButtonTemplate);
                temp.SetActive (true);
                RectTransform tempRect = temp.GetComponent<RectTransform> ();


                temp.transform.SetParent (StageScrollContentRect.gameObject.transform);
                float ButtonHeight = 80f;
                float ButtonWIdth = 250f;
                tempRect.SetSizeWithCurrentAnchors (RectTransform.Axis.Horizontal, ButtonWIdth);
                tempRect.SetSizeWithCurrentAnchors (RectTransform.Axis.Vertical, ButtonHeight);
                tempRect.pivot = new Vector2 (0.5f, 1f);




                StartCoroutine (
                    Move.QuadOut (
                        (v) => { tempRect.anchoredPosition = v; },
                        new Vector2 (0f, 0f),
                        new Vector2 (0f, ((8 + ButtonHeight) * i + 8) * -1f),
                        0.5f)
                    );

                // 0번 인덱스는 템플릿 버튼이 차지하고 있기 때문에 1을 더했다.
                GameObject worldButton = WorldScrollContentRect.gameObject.transform.GetChild (WorldList.IndexOfKey (World) + 1).gameObject;
                float worldButtonXPos = worldButton.GetComponent<RectTransform> ().anchoredPosition.x;
                float worldButtonYPos = worldButton.GetComponent<RectTransform> ().anchoredPosition.y;
                StartCoroutine (
                    Move.QuadOut (
                        (v) => { worldButton.GetComponent<RectTransform>().anchoredPosition = v; },
                        new Vector2 (worldButtonXPos, 0f),
                        new Vector2 (worldButtonXPos, 150f),
                        0.5f)
                );
                float stageScrollXPos = StageScroll.GetComponent<RectTransform> ().anchoredPosition.x;
                float stageScrollYPos = StageScroll.GetComponent<RectTransform> ().anchoredPosition.y;
                StartCoroutine (
                    Move.QuadOut (
                        (v) => { StageScroll.GetComponent<RectTransform>().anchoredPosition = v; },
                        new Vector2 (stageScrollXPos, stageScrollYPos),
                        new Vector2 (stageScrollXPos, stageScrollYPos + 180f),
                        0.5f)
                );

                Restore = () =>
                {
                    float _stageScrollXPos = StageScroll.GetComponent<RectTransform> ().anchoredPosition.x;
                    float _stageScrollYPos = StageScroll.GetComponent<RectTransform> ().anchoredPosition.y;
                    StartCoroutine (
                        Move.QuadOut (
                            (v) => { StageScroll.GetComponent<RectTransform> ().anchoredPosition = v; },
                            new Vector2 (_stageScrollXPos, _stageScrollYPos),
                            new Vector2 (_stageScrollXPos, 0f),
                            0.5f)
                    );

                    // 0번 인덱스는 템플릿 버튼이 차지하고 있기 때문에 1을 더했다.
                    float _worldButtonXPos = worldButton.GetComponent<RectTransform> ().anchoredPosition.x;
                    float _worldButtonYPos = worldButton.GetComponent<RectTransform> ().anchoredPosition.y;
                    StartCoroutine (
                        Move.QuadOut (
                            (v) => { worldButton.GetComponent<RectTransform> ().anchoredPosition = v; },
                            new Vector2 (_worldButtonXPos, _worldButtonYPos),
                            new Vector2 (_worldButtonXPos, 0f),
                            0.5f)
                    );
                };

                tempRect.gameObject.SetActive (true);
                temp.GetComponent<StageSelectButton> ().Init (title, Stage);

                UnityAction Call = delegate { StartStage (Stage); TitleMusicScript.Instance.StopMusic (); };
                
                temp.GetComponent<Button> ().onClick.AddListener (Call);
                i++;

                Prv_StageButton.Add (temp);
            }

        }
        else
        {
            Debug.Log ("Selected World (" + World + ") does not Exist!");
        }
    }

    void ResetButtonPositions()
    {
        var stageScrollRect = StageScroll.GetComponent<RectTransform> ();
        stageScrollRect.anchoredPosition = new Vector2 (stageScrollRect.anchoredPosition.x, 0);
        for ( int i = 1; i < WorldScrollContentRect.transform.childCount; i++ )
        {
            var worldButtonRect = WorldScrollContentRect.transform.GetChild (i).GetComponent<RectTransform> ();
            worldButtonRect.anchoredPosition = new Vector2 (worldButtonRect.anchoredPosition.x, 0);
        }
    }

    void Create_WorldSelect(SortedList<string, List<TextAsset>> WorldList)
    {
        //맵 상에 있는 대분류 버튼을 찾는다.
        GameObject WorldButtonTemplate = WorldScrollContentRect.gameObject.transform.Find ("Button").gameObject;

        if ( WorldButtonTemplate != null )
        {
            int i = 0;
            WorldButtonTemplate.SetActive (false);
            foreach ( KeyValuePair<string, List<TextAsset>> Stage in WorldList )
            {
                GameObject temp = Instantiate (WorldButtonTemplate);
                RectTransform tempRect = temp.GetComponent<RectTransform> ();
                temp.transform.SetParent (WorldScrollContentRect.gameObject.transform);
                float ButtonHeight = WorldScrollContentRect.rect.height * 0.8f;
                tempRect.SetSizeWithCurrentAnchors (RectTransform.Axis.Horizontal, ButtonHeight);
                tempRect.SetSizeWithCurrentAnchors (RectTransform.Axis.Vertical, ButtonHeight);
                tempRect.pivot = new Vector2 (0.5f, 0.5f);
                tempRect.anchoredPosition = new Vector2 ((300f) * (i-1), 0f);
                tempRect.gameObject.SetActive (true);
                temp.GetComponent<WorldSelectButton> ().Init (Stage.Key, Stage.Value);

                UnityAction Call = delegate { OnClick_WorldButton (Stage.Key); };
                temp.GetComponent<Button> ().onClick.AddListener (Call);

                i++;
            }
        }

    }

    void Classify_Map(out SortedList<string, List<TextAsset>> WorldList, List<TextAsset> mapSources)
    {
        SortedList<string, List<TextAsset>> tempWorldList = new SortedList<string, List<TextAsset>> ();
        //파일 명: map-(대분류)-(소분류)
        //KeyValueSortedList 만들어서 쓰자
        //string : 대분류를 나타내는 문자열
        //맵들을 대분류에 맞게 분류한다.
        foreach ( TextAsset mapSource in mapSources )
        {
            string World = MapFileUtil.mapWorld (mapSource);

            if ( tempWorldList.ContainsKey (World) )
            {
                tempWorldList [World].Add (mapSource);
            }
            else
            {
                List<TextAsset> temp = new List<TextAsset> ();
                temp.Add (mapSource);
                tempWorldList.Add (World, temp);
            }
        }
        WorldList = tempWorldList;
    }

    void StartStage(TextAsset selection)
    {
        Configuration.Instance.activatedMapSource = selection;
        SceneManager.LoadScene ("GameplayScene");
    }

    void StartStage()
    {
        var selection = mapList [dropdown.value];
        Configuration.Instance.activatedMapSource = selection;
        SceneManager.LoadScene ("GameplayScene");
    }
}
