Shader "Unlit/Cell"
{
	Properties
	{
		_LineColor ("Line Color", Color) = (1.0, 0.5, 0.5, 1)
		_BackgroundColor ("Background Color", Color) = (0.0, 0.5, 0.5, 1)
		_Offset ("Animation Offset", Float) = 0
		_Brightness ("Brightness", Float) = 1
	}
	SubShader
	{
		Blend SrcAlpha OneMinusSrcAlpha
		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"
			#include "SDF.cginc"
			#include "lib/noiseSimplex.cginc"

			struct v2f {
				float4 pos : SV_POSITION;
			};

			fixed4 _LineColor;
			fixed4 _BackgroundColor;
			float _Offset;
			float _Brightness;

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

				float d = 10000; // Just any big value
				for (int i=0; i<100; i++)
				{
					float x = 1 - i * 0.02;
					float t = (_Time.y*2 + i*0.2) + 0.2*snoise(float2(_Time.y, 1)) + 10*_Offset;
					float width = 0.003*(1.8+sin(t));
					float l = sdfLine(float2(x, 0), float2(x, 1), width, p);
					d = sdfUnion(l, d);
				}

				float4 bg = lerp(float4(0, 0, 0, 1), _BackgroundColor, _Brightness*0.6 + 0.4);
				float4 layer1 = sdfRenderFill(lerp(float4(0,0,0,1), _LineColor, _Brightness*0.9 + 0.1), d);

				float4 result = float4(lerp(bg.rgb, layer1.rgb, layer1.a), bg.a);
				return result;
			}

			ENDCG
		}
	}
}
