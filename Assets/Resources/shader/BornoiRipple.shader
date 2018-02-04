Shader "Unlit/BornoiRipple"
{
	/* 제목은 BornoiRipple이지만 가운데에서 가장자리로 서서히 퍼져나가는 부드러운 원을
	 * 그리는 셰이더
	 * TODO : 여기에 보르노이 세그먼테이션을 잘 적용해서 멋진 후처리 효과 추가
	 */
	Properties
	{
		_Speed ("Speed", Float) = 1
		_Width ("Width", Float) = 1
		_Depth ("Depth", Float) = 1
		_ColorOffset ("Color Offset", Float) = 0
		_Distance ("Distance", Float) = 1
		_Centers ("Centers", Range(1, 40)) = 10
	}
	SubShader
	{
		Tags { "RenderType"="Opaque" }

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#define MAX_CENTERS 40
			
			#include "UnityCG.cginc"

			struct appdata
			{
				float4 vertex : POSITION;
				float2 uv : TEXCOORD0;
			};

			struct v2f
			{
				float4 vertex : SV_POSITION;
			};

			float _Speed;
			float _Width;
			float _Distance;
			float _Depth;
			float _ColorOffset;
			int _Centers;

			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				return o;
			}

			float2 centers[MAX_CENTERS];

			void initCenters() 
			{
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				
				float2 uv = 0.5 - i.vertex / float2(_ScreenParams.x, _ScreenParams.y);
				float aspectRatio = _ScreenParams.x / _ScreenParams.y;
				float2 p = uv * float2(aspectRatio, 1);
				float t = _Time.y*_Speed;
				float r = length(p);

				initCenters();

				float v = _Depth*(0.5 + 0.5*sin(t + _Width*r)) + _ColorOffset;
				fixed4 col = fixed4(v, v, v, 1);

				return col;
			}
			ENDCG
		}
	}
}
