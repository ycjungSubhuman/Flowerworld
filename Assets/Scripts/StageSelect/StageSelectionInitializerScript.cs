using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
using System.Linq;
using Assets.Util;
using Assets;
using UnityEngine.Events;
using Assets.Core.Animation.Coroutines;
using System;
using Assets.Scripts;
using System.Text.RegularExpressions;

// 디버그용 스테이지 선택 씬 초기화 스크립트 (main 씬)
public class StageSelectionInitializerScript : MonoBehaviour
{

    List<TextAsset> mapList = new List<TextAsset> ();
    GameObject Protector;

    //string : 대분류를 나타내는 문자열
    SortedList<string, List<TextAsset>> WorldDict;
    public Action Restore = () => { };

    const float stagePanelMargin = 105.8f;
    const float stagePanelMarginOffset = -16.6f;
    const float stagePanelInitOffset = 100f;
    const float stageTextXOffset = 35f;

    float buttonSlideDelta(int childCount)
    {
        return 55f * childCount;
    }

    GameObject tutorialGameObject;
    GameObject world1GameObject;
    GameObject world2GameObject;
    GameObject world3GameObject;
    List<GameObject> worldButtons;

    Dictionary<GameObject, float> offsetMap = new Dictionary<GameObject, float> ();
    string currentOpenWorldName;

    void Start()
    {
        tutorialGameObject = GameObject.Find ("WorldButton1");
        world1GameObject = GameObject.Find ("WorldButton2");
        world2GameObject = GameObject.Find ("WorldButton3");
        world3GameObject = GameObject.Find ("WorldButton4");
        Protector = GameObject.Find ("Protector");
        worldButtons = new List<GameObject> () { tutorialGameObject, world1GameObject, world2GameObject, world3GameObject };

        Protector.SetActive (false);
        List<TextAsset> mapSources = MapFileUtil.GetAllMapSources ();
        WorldDict = Classify_Map (mapSources);
        genStageButtons ();

        Protector.GetComponent<Button> ().onClick.AddListener (() => { StopAllCoroutines (); RestoreButtonPositions (); });
    }

    private void RestoreButtonPositions()
    {
        var i = WorldDict.IndexOfKey (currentOpenWorldName);
        var parentGameObject = worldButtons [i];

        animateButton (parentGameObject, 0f, true);
        for ( int j = 1; j < parentGameObject.transform.childCount; j++ )
        {
            var child = parentGameObject.transform.GetChild (j).gameObject;

            child.SetActive (false);
            animateButton (child, stagePanelInitOffset, true);
        }
    }

    private void sortSources(List<TextAsset> sources)
    {
        sources.Sort ((t1, t2) =>
        {
            var title1 = MapFileUtil.mapTitleOfFile (t1);
            var title2 = MapFileUtil.mapTitleOfFile (t2);
            Func<String, KeyValuePair<int, int>> extract = (title) =>
            {
                var regex = @"(World\s(\d+)-(\d+))|(Tutorial (\d+))";
                var match = Regex.Match (title, regex);
                if ( match.Groups [1].Success )
                {
                    return new KeyValuePair<int, int> (int.Parse (match.Groups [2].Value), int.Parse (match.Groups [3].Value));
                }
                else
                {
                    return new KeyValuePair<int, int> (0, int.Parse (match.Groups [5].Value));
                }
            };

            var p1 = extract (title1);
            var p2 = extract (title2);
            var world1 = p1.Key;
            var world2 = p2.Key;
            var stage1 = p1.Value;
            var stage2 = p2.Value;

            if ( world1 != world2 )
            {
                return world1.CompareTo (world2);
            }
            else
            {
                return stage1.CompareTo (stage2);
            }

        });
    }

    private void genStageButtons()
    {
        foreach (var p in WorldDict)
        {
            var world = p.Key;
            var sources = p.Value;
            var i = WorldDict.IndexOfKey (world);
            sortSources (sources);
            
            var stageNames = sources.Select (s => MapFileUtil.mapTitleOfFile (s)).ToList ();
            var parent = worldButtons [i];

            for ( int j = 0; j < stageNames.Count (); j++ )
            {
                var button = GameObject.Instantiate (Resources.Load<GameObject> ("prefabs/StagePanel"));
                button.transform.parent = parent.transform;

                var stageName = stageNames [j];
                var text = GameObject.Instantiate (Resources.Load<GameObject> ("prefabs/StageText"));
                text.GetComponent<Text> ().text = stageName;
                text.transform.parent = button.transform;
                text.GetComponent<RectTransform> ().anchoredPosition = new Vector2 (stageTextXOffset, 0f);

                offsetMap [button] = -j * stagePanelMargin + stagePanelMarginOffset;
                button.GetComponent<RectTransform> ().anchoredPosition = new Vector2 (0f, stagePanelInitOffset);
                button.SetActive (false);
                int ind = j;
                button.GetComponent<Button> ().onClick.AddListener (() => { StartStage (sources, ind); });
            }

            parent.GetComponent<Button> ().onClick.AddListener (() => { OnClick_WorldButton (world); });
        }
    }

    private void animateButton(GameObject button, float goalY, bool startFromCurrentPos, float startY=0f)
    {
        var currentPosition = button.GetComponent<RectTransform> ().anchoredPosition;
        if(startFromCurrentPos)
        {
            startY = currentPosition.y;
        }
        StartCoroutine (
            Move.QuadOut ((v) => { button.GetComponent<RectTransform> ().anchoredPosition = v; },
            new Vector3 (currentPosition.x, startY, -10f),
            new Vector3 (currentPosition.x, goalY, -10f),
            0.5f
            ));
    }

    public void OnClick_WorldButton(string WorldName)
    {
        if ( currentOpenWorldName != null )
        {
            StopAllCoroutines ();
            RestoreButtonPositions ();
        }

        int i = WorldDict.IndexOfKey (WorldName);
        currentOpenWorldName = WorldName;
        Protector.SetActive (true);
        var buttonGameObject = worldButtons [i];
        var childButtonCount = buttonGameObject.transform.childCount - 1; //The first GameObject is Test. Ommiting.
        animateButton (buttonGameObject, buttonSlideDelta (childButtonCount), false, 0f);
        for ( int j = 1; j < buttonGameObject.transform.childCount; j++ )
        {
            var child = buttonGameObject.transform.GetChild (j).gameObject;
            child.SetActive (true);
            animateButton (child, offsetMap [child], false, 0f);
        }


    }

    SortedList<string, List<TextAsset>> Classify_Map(List<TextAsset> mapSources)
    {
        SortedList<string, List<TextAsset>> tempWorldList = new SortedList<string, List<TextAsset>> ();
        //파일 명: map-(대분류)-(소분류)
        //KeyValueSortedList 만들어서 쓰자
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
        return tempWorldList;
    }

    void StartStage(List<TextAsset> selection,int index)
    {
        Configuration.Instance.activatedMapSource = selection[index];
        Configuration.List = selection;
        Configuration.Instance.mapName = MapFileUtil.mapTitleOfFile (selection[index]);
        TitleMusicScript.Instance.StopMusic ();
        SceneManager.LoadScene ("GameplayScene");
    }
}