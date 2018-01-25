using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using Newtonsoft.Json;
using Assets.Core.Data;
using Assets.Core.Drawer;

public class MapScript : MonoBehaviour {

	// Use this for initialization
	void Start () {
        TextAsset json = Resources.Load<TextAsset> ("maps/map1");
        var map = JsonConvert.DeserializeObject<Map> (json.text);
        map.Init ();
        var mapDrawer = new MapDrawer(new MapBlockDrawer(new CellDrawer()));
        var mapGameObject = mapDrawer.Draw (map);
        var uiPatternDrawer = new UIPatternDrawer (new UICellDrawer ());
        var uiPattern = uiPatternDrawer.Draw (map.pattern);
        uiPattern.transform.parent = GameObject.Find ("PatternRoot").transform;
        uiPattern.GetComponent<RectTransform>().anchoredPosition = new Vector2 (0, 0);
        uiPattern.name = "PatternUI";

        var player = GameObject.Instantiate(Resources.Load<GameObject> ("prefabs/player"));
        var playerScript = player.GetComponent<PlayerControlScript> ();
        playerScript.map = map;
        playerScript.mapGameObject = mapGameObject;
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
