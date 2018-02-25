using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Assets.Scripts
{
    public class TitleMusicScript : MonoBehaviour
    {
        private static TitleMusicScript instance = null;
        public static TitleMusicScript Instance
        {
            get { return instance; }
        }

        void Awake()
        {
            if ( instance != null && instance != this )
            {
                Destroy (this.gameObject);
                return;
            }
            else
            {
                instance = this;
            }
            DontDestroyOnLoad (this.gameObject);
        }

        private IEnumerator InterpolateVolume(float duration, float start, float goal)
        {
            GetComponent<AudioSource> ().volume = start;
            float time = 0;
            while(time < duration)
            {
                time += Time.deltaTime;
                GetComponent<AudioSource> ().volume = start + (time / duration) * (goal - start);
                yield return null;
            }
            GetComponent<AudioSource> ().volume = goal;
        }

        // Use this for initialization
        public void PlayMusic()
        {
            StartCoroutine (InterpolateVolume(2f, 0f, 1f));
        }
        public void StopMusic()
        {
            StartCoroutine (InterpolateVolume (2f, 1f, 0f));
        }
    }
}
