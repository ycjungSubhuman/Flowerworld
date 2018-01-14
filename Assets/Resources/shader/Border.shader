Shader "Unlit/Border"
{
	Properties
	{
		_LIneColor ("Line Color", Color) = (0.1, 0.2, 0.6)
		_BorderWidth ("Border Width", Float) = 0.5
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

			struct appdata
			{
				float4 vertex : POSITION;
				float2 uv : TEXCOORD0;
			};

			struct v2f
			{
				float2 uv : TEXCOORD0;
				float4 vertex : SV_POSITION;
			};

			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.uv = v.uv;
				return o;
			}

			float4 _LIneColor;
			float _BorderWidth;
			
			fixed4 frag (v2f i) : SV_Target
			{
				float2 uv = i.uv;

				const int count = 10;
				/*
				for (int i=0; i<count/2; i++) 
				{
					float pos = float(4*i*i) * ((1-_BorderWidth) / count) / 100;
					float rect1 = sdfBox(float2(pos, pos), float2(1-pos, 1-pos), uv);
					float rect2 = sdfBox(float2(pos+0.001, pos+0.001), float2(1-pos, 1-pos), uv);
				}*/
				float test = sdfBox(float2(0,0), float2(0.5, 0.5), uv);

				float4 bg = float4(1,1,1,1);
				float4 layer1 = sdfRenderFill(float4(0,0,0,1), test);

				float4 result = float4(lerp(bg.rgb, layer1.rgb, layer1.a), bg.a);
				return result;
			}
			ENDCG
		}
	}
}
