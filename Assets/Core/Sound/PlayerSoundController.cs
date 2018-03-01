using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;
using UnityEngine;

namespace Assets.Core.Sound
{
    /** 상황에 따른 사운드 맵. 새로운 상황에 따른 사운드를 추가하고 싶다면
     *  IPlayerSoundController, PlayerSoundController를 참조하여 수정을 가하거나, 새로운
     *  SoundController를 만든다.
     *  MonoBehaviour에서 사용하면 된다.(PlayerScript 사용 예시 참조) 
     *  
     *  사운드 종류나, 플레이 되는 방식을 약간 바꾸고 싶다면 이 코드를 수정하거나,
     *  IPlayerSoundController를 구현하는 새로운 클래스를 작성해서 PlayerScript에서 사용
     */
    public class PlayerSoundController : IPlayerSoundController
    {
        private Dictionary<int, AudioClip> soundMap;
        private AudioSource audioSource;

        private AudioClip A;
        private AudioClip B;
        private AudioClip C;
        private AudioClip D;
        private AudioClip E;
        private AudioClip F;
        private AudioClip G;
        private AudioClip Refuse;
        private AudioClip Restart;
        private AudioClip GlassCreate;
        private AudioClip GlassDestroy;
        private AudioClip GlassReady;
        private AudioClip GlassUnReady;
        private AudioClip SpringUse;
        private AudioClip SpringReady;
        private AudioClip SpringUnReady;

        public PlayerSoundController(GameObject gameObject)
        {
            audioSource = gameObject.GetComponent<AudioSource> ();
            Debug.Assert (audioSource != null);

            A = Resources.Load<AudioClip> ("sounds/A");
            B = Resources.Load<AudioClip> ("sounds/B");
            C = Resources.Load<AudioClip> ("sounds/C");
            D = Resources.Load<AudioClip> ("sounds/D");
            E = Resources.Load<AudioClip> ("sounds/E");
            F = Resources.Load<AudioClip> ("sounds/F");
            G = Resources.Load<AudioClip> ("sounds/G");
            Refuse = Resources.Load<AudioClip> ("sounds/refuse");
            Restart = Resources.Load<AudioClip> ("sounds/restart");
            GlassCreate = Resources.Load<AudioClip> ("sounds/replace");
            GlassDestroy = Resources.Load<AudioClip> ("sounds/glass_break");
            GlassReady = Resources.Load<AudioClip> ("sounds/glass_ready");
            GlassUnReady = Resources.Load<AudioClip> ("sounds/glass_unready");
            SpringUse = Resources.Load<AudioClip> ("sounds/spring");
            SpringReady = Resources.Load<AudioClip> ("sounds/spring_ready");
            SpringUnReady = Resources.Load<AudioClip> ("sounds/spring_unready");

            soundMap = new Dictionary<int, AudioClip> ()
            {
                {Label.A.Value,  A},
                {Label.B.Value,  B},
                {Label.C.Value,  C},
                {Label.D.Value,  D},
                {Label.E.Value,  E},
                {Label.F.Value,  F},
                {Label.G.Value,  G},
            };
        }

        public void OnGlassCreate()
        {
            audioSource.PlayOneShot (GlassCreate);
        }

        public void OnGlassDestroy()
        {
            audioSource.PlayOneShot (GlassDestroy);
        }

        public void OnGlassReady()
        {
            audioSource.PlayOneShot (GlassReady, 0.5f);
        }

        public void OnGlassUnReady()
        {
            audioSource.PlayOneShot (GlassUnReady);
        }

        public void OnLabelSound(Label l)
        {
            audioSource.PlayOneShot (soundMap [l.Value]);
        }

        public void OnRefuse()
        {
            audioSource.PlayOneShot (Refuse, 0.7f);
        }

        public void OnRestart()
        {
            audioSource.PlayOneShot (Restart);
        }

        public void OnSpringReady()
        {
            audioSource.PlayOneShot (SpringReady, 0.5f);
        }

        public void OnSpringUnReady()
        {
            audioSource.PlayOneShot (SpringUnReady, 0.5f);
        }

        public void OnSpringUse()
        {
            audioSource.PlayOneShot (SpringUse);
        }
    }
}
