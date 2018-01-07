using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using Newtonsoft.Json;
using Core.Map;

public class MapScript : MonoBehaviour {

	// Use this for initialization
	void Start () {
        string js = JsonConvert.SerializeObject (DummyData.m1, new Newtonsoft.Json.Converters.StringEnumConverter ());
        File.WriteAllText ("test.json", js);
        string map = File.ReadAllText ("testmap.json");
        var readmap = JsonConvert.DeserializeObject<Map> (map);
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
