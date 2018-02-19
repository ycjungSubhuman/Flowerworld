Shader "Unlit/BackgroundShader"
{
	Properties
	{
		_VignetteMix ("Vignette", Float) = 1
		_Layer1Mix ("Top Layer Mix", Float) = 1
		_Layer2Mix ("Second Layer Mix", Float) = 1
		_Layer3Mix ("Third Layer Mix (Base Layer Mix is Autocalculated)", Float) = 1
		_Speed ("Speed", Float) = 1

		_Layer1_RedThreashold ("Top Layer Mix", Float) = 1
	}
	SubShader
	{
		Tags { "RenderType"="Opaque" }
		LOD 100

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			
			#include "UnityCG.cginc"
			#include "SDF.cginc"

			struct v2f
			{
				float4 pos : SV_POSITION;
			};

			
			v2f vert (appdata_base v)
			{
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				return o;
			}

			float _Layer1Mix;
			float _Layer2Mix;
			float _Layer3Mix;
			float _VignetteMix;
			float _Speed;

			float2 distortPosition_Lava(float2 p, float t) 
			{
				return float2(0.4*sin(20*p.x + t), p.y+0.5*sin(10*p.y + t));
			}

			float2 distortPosition_HorizontalWave(float2 p, float t) 
			{
				return float2(p.x+0.3*sin(p.x + 0.5*t), p.y);
			}

			float4 drawLayer1(float2 p, float t) 
			{
				float2 p2 = distortPosition_HorizontalWave(p, t);
				float2 p3 = distortPosition_Lava(p2, t);
				float r = length(p3);
				float ripple = (sin(10*r + t)+1)*0.5;

				float f = ripple+0.3;
				return float4(step(0.8,f), 0.2*f, 0.3*f, 1);
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				float2 uv = 0.5 - i.pos / float2(_ScreenParams.x, _ScreenParams.y);
				float aspectRatio = _ScreenParams.x / _ScreenParams.y;
				float2 p = uv * float2(aspectRatio, 1);
				float t = _Time.y*_Speed;
				
				// Vignette
				float vignetteValue = length(p);
				float4 vignette = float4(0, 0, 0, vignetteValue*_VignetteMix);

				// Top Layer (Layer 1)
				float4 layer1 = drawLayer1(p, t);


				// Mix Layers
				fixed4 result = fixed4(lerp(layer1.rgb, vignette.rgb, vignette.a), 1);
				return result;
			}
			ENDCG
		}
	}
}
