using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;

namespace Assets.Core.Sound
{
    public interface IPlayerSoundController
    {
        void OnRestart();
        void OnRefuse();
        void OnLabelSound(Label l);
    }
}
