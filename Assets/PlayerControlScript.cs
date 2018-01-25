using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Assets.Core.Data;
using Assets.Util;

public class PlayerControlScript : MonoBehaviour
{
    public Map map;
    public GameObject mapGameObject;

    public AudioClip Refuse;
    public AudioClip Restart;
    public AudioClip A;
    public AudioClip B;
    public AudioClip C;

    private Vector2Int pos;
    private int patternIndex = -1;

    void Start()
    {
        pos = getInitPosition ();

        Debug.Assert (mapGameObject != null);
        soundMap = new Dictionary<int, AudioClip>() {
            { Label.A.Value, A },
            { Label.B.Value, B },
            { Label.C.Value, C },
        };
        updateCurrentLabel ();
    }

    Vector2Int getInitPosition()
    {
        Debug.Assert (map != null);
        var startPositions = map.LabelGlobalPositionsOf (Label.START);
        Debug.Log (startPositions.Count());
        Debug.Assert (startPositions.Count () == 1);
        return startPositions.First ();
    }

    void Update()
    {
        Vector2Int newPos = pos;

        if(Input.GetKeyDown(KeyCode.UpArrow))
        {
            newPos += new Vector2Int(-1, 0);
        }
        else if (Input.GetKeyDown(KeyCode.DownArrow))
        {
            newPos += new Vector2Int(1, 0);
        }
        else if (Input.GetKeyDown(KeyCode.RightArrow))
        {
            newPos += new Vector2Int(0, 1);
        }
        else if (Input.GetKeyDown(KeyCode.LeftArrow))
        {
            newPos += new Vector2Int(0, -1);
        }

        updatePlayerPosition (newPos);

        if(Input.GetKeyDown(KeyCode.R))
        {
            resetPlayer ();
        }
    }

    private void resetPlayer()
    {
        updatePlayerPosition (getInitPosition());
        patternIndex = -1;
        updateCurrentLabel ();
        gameObject.GetComponent<AudioSource> ().PlayOneShot (Restart, 0.4f);
    }

    void updatePlayerPosition(Vector2Int newPos)
    {
        if (newPos.Equals(pos))
        {
            return;
        }

        if(map.IsInside(newPos) && checkLogic(newPos))
        {
            playMoveSound ();
            updateCurrentLabel ();
            movePlayer (newPos);
        }
        else
        {
            refuseMove ();
        }
    }

    void movePlayer(Vector2Int newPos)
    {
        Vector2 currScenePos = gameObject.transform.position;
        var targetCell =  MapGameObjectUtil.GetCellGameObject (mapGameObject, newPos);
        Vector2 newScenePos = targetCell.transform.position;

        StopAllCoroutines ();
        StartCoroutine (MoveInterpolate(currScenePos, newScenePos, 0.5f));

        this.pos = newPos;
        gameObject.GetComponent<Animator> ().SetTrigger ("Move");
        targetCell.GetComponent<Animator> ().SetTrigger ("Stomp");
    }

    Dictionary<int, AudioClip> soundMap;
    void playMoveSound()
    {
        var label = map.pattern [patternIndex];
        gameObject.GetComponent<AudioSource> ().PlayOneShot (soundMap[label.Value]);
    }

    IEnumerator MoveInterpolate(Vector2 start, Vector2 goal, float duration)
    {
        float t = 0;
        float time = 0;

        while(time <= duration)
        {
            time += Time.deltaTime;
            t = time / duration;
            gameObject.transform.position = Vector2.Lerp (start, goal, -(t-1)*(t-1)+1);
            yield return null;
        }

        gameObject.transform.position = goal;
    }

    void refuseMove()
    {
        gameObject.GetComponent<Animator> ().SetTrigger ("Refuse");
        gameObject.GetComponent<AudioSource> ().PlayOneShot (Refuse);
    }

    void updateCurrentLabel()
    {
        this.patternIndex = (this.patternIndex + 1) % map.pattern.Count;

        // Update Cell Color
        var newAvailabePositions = map.LabelGlobalPositionsOf (map.pattern [patternIndex]);
        foreach (var go in MapGameObjectUtil.GetAllCells (mapGameObject))
        {
            go.GetComponent<Animator> ().SetBool ("Onoff", false);
        }
        foreach(var pos in newAvailabePositions)
        {
            var go = MapGameObjectUtil.GetCellGameObject (mapGameObject, pos);
            go.GetComponent<Animator> ().SetBool ("Onoff", true);
        }

        // Update UI Cell Color
        var patternUI = GameObject.Find ("PatternUI");
        for (int i=0; i<patternUI.transform.childCount; i++)
        {
            patternUI.transform.GetChild (i).SendMessage ("SetOnoff", patternIndex);
        }
    }

    bool checkLogic(Vector2Int newPos)
    {
        var validLabel = map.pattern [patternIndex];
        var actualLabel = map.LabelOf (newPos);
        return actualLabel.FallIn (validLabel);
    }

    void Init(Map map, GameObject mapGameObject)
    {
        this.map = map;
        this.mapGameObject = mapGameObject;
    }
}
