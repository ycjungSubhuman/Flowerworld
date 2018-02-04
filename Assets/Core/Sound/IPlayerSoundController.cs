using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Assets.Core.Data;

namespace Assets.Core.Sound
{
    /** 플레이어의 이벤트에 따라 사운드를 재생해주는 오브젝트의 인터페이스 */
    public interface IPlayerSoundController
    {
        void OnRestart();
        void OnRefuse();
        void OnLabelSound(Label l);
    }
}
