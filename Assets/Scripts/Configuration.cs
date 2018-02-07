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

        /** GameplayScene 의 초기화 과정에서 참조하는 맵 소스.
         * GameplayScene을 불러오기 전에 미리 정해놓으면 
         * 로딩 후 해당 소스의 스테이지가 Scene에 나타나게 된다.
         */
        public TextAsset activatedMapSource;
    }
}
