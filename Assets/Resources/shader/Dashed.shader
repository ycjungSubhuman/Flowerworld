Shader "Unlit/Cell"
{
	Properties
	{
		_Color ("Main Color", Color) = (1.0, 0.5, 0.5, 1)
	}
	SubShader
	{
		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma target 3.0
			#include "UnityCG.cginc"
			#include "SDF.cginc"
			#include "lib/noiseSimplex.cginc"

			struct v2f {
				float4 pos : SV_POSITION;
			};

			fixed4 _Color;

			v2f vert (appdata_base v) 
			{
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				return o;
			}

			fixed4 frag (v2f i) : SV_TARGET
			{
				float2 uv = 0.5 - i.pos / float2(_ScreenParams.x, _ScreenParams.y);
				float aspectRatio = _ScreenParams.x / _ScreenParams.y;
				float2 p = uv * float2(aspectRatio, 1);

				float d = 10000;
				for (int i=0; i<100; i++)
				{
					float x = 1 - i * 0.02;
					float t = (_Time.y*2 + i*0.2);
					float width = 0.003*(1.8+sin(t));
					float l = sdfLine(float2(x, 0), float2(x, 1), width, p);
					d = sdfUnion(l, d);
				}

				float4 bg = float4(1, 1, 1, 1);
				float4 layer1 = sdfRenderFill(_Color, d);

				float4 result = float4(lerp(bg.rgb, layer1.rgb, layer1.a), 1);
				return result;
			}

			ENDCG
		}
	}
}
