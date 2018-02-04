using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets
{
    //씬과 씬 사이에 데이터 전달하는데 사용하는 싱글톤 오브젝트.
    //StageInitializerScript에서 사용
    public class Configuration
    {
        private static readonly Configuration instance = new Configuration ();

        private Configuration() { }

        public static Configuration Instance
        {
            get
            {
                return instance;
            }
        }

        public TextAsset activatedMapSource;
    }
}
