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
        var mapDrawer = new MapDrawer(new MapBlockDrawer(new CellDrawer()));
        mapDrawer.Draw (map);
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
