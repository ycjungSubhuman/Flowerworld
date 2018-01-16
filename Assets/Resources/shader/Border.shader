Shader "Unlit/Border"
{
	Properties
	{
		_LIneColor ("Line Color", Color) = (0.1, 0.2, 0.6)
		_BorderWidth ("Border Width", Float) = 0.2
		_Speed ("Speed", Float) = 0.
	}
	SubShader
	{
		Tags {"Queue"="Transparent" "IgnoreProjector"="True" "RenderType"="Transparent"}
        ZWrite Off
		Blend SrcAlpha OneMinusSrcAlpha

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"
			#include "SDF.cginc"
			#define COUNT 100

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
			float _Speed;

			float rectOf(float ind, float2 uv) 
			{
				float t = _Time.y;
				float pos = sin(0.06*ind - _Speed*t);

				float2 rectLeftBottom = float2(pos, pos);
				float2 rectRightTop = float2(1-pos, 1-pos);
				return sdfBox(rectLeftBottom, rectRightTop, uv);
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				float2 uv = i.uv;

				float d = 10000000;
				for (uint j=0; j<COUNT/2; j++) 
				{
					float ind1 = float(2*j);
					float ind2 = float(2*j+1);
					float rect1 = rectOf(ind1, uv);
					float rect2 = rectOf(ind2, uv);
					float rect = sdfDiff(rect1, rect2);
					d = sdfUnion(rect, d);
				}

				float4 bg = float4(1,1,1,1);
				float mask = sdfBox(float2(_BorderWidth, _BorderWidth), float2(1-_BorderWidth, 1-_BorderWidth), uv);
				float res = sdfDiff(d, mask);
				float4 layer1 = sdfRenderFill(_LIneColor, res);

				float4 result = float4(lerp(bg.rgb, layer1.rgb, layer1.a), layer1.a);
				return result;
			}
			ENDCG
		}
	}
}
