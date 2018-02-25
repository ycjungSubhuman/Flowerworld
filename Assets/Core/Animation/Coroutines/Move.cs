using System;
using System.Collections;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Core.Animation.Coroutines
{
    /** GameObject의 움직임을 나타내는 코루틴들의 모음 */
    public class Move
    {
        public static IEnumerator QuadOut(GameObject gameObject, Vector2 start, Vector2 goal, float duration)
        {
            //움직임을 기술하는 함수(지금은 볼록한 2차함수)
            Func<float,float> tModifier = t => -(t - 1) * (t - 1) + 1;
            return Interpolate (gameObject, start, goal, duration, tModifier);
        }

        public static IEnumerator QuadOut(Action<Vector2> update, Vector2 start, Vector2 goal, float duration)
        {
            //움직임을 기술하는 함수(지금은 볼록한 2차함수)
            Func<float, float> tModifier = t => -(t - 1) * (t - 1) + 1;
            return Interpolate (update, start, goal, duration, tModifier);
        }

        private static IEnumerator Interpolate(GameObject gameObject, Vector2 start, Vector2 goal, float duration, Func<float, float> tModifier)
        {
            return Interpolate ((v) => { gameObject.transform.localPosition = v; }, start, goal, duration, tModifier);
        }

        private static IEnumerator Interpolate(Action<Vector2> update, Vector2 start, Vector2 goal, float duration, Func<float, float> tModifier)
        {
            float t = 0;
            float time = 0;

            while ( time <= duration )
            {
                time += Time.deltaTime;
                t = time / duration;
                update(Vector2.Lerp (start, goal, tModifier (t)));
                yield return null;
            }

            update(goal);
        }
    }
}
