Shader "Unlit/Border"
{
	Properties
	{
		_LIneColor ("Line Color", Color) = (0.1, 0.2, 0.6)
		_BorderWidth ("Border Width", Float) = 0.2
		_Thickness ("Thickness", Float) = 0.3
		_Speed ("Speed", Float) = 0.5
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
			float _Thickness;
			
			fixed4 frag (v2f i) : SV_Target
			{
				float2 uv = i.uv;

				float2 offset = abs(uv - float2(0.5, 0.5));
				float r = max(offset.x, offset.y);
				float t = _Time.y;
				float v = step(0, sin((r - _Speed*t)/(0.1*_Thickness)));
				float4 layer0 = float4(v, v, v, v);
				float mask = sdfBox(float2(_BorderWidth, _BorderWidth), float2(1-_BorderWidth, 1-_BorderWidth), uv);
				float4 layer1 = sdfRenderFill(float4(0,0,0,1), mask);
				float4 res = float4(layer0.rgb, min(layer0.a, 1-layer1.a));

				return res;
			}
			ENDCG
		}
	}
}
