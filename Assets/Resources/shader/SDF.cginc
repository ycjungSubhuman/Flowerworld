
float sdfUnion(float da, float db)
{
	return min(da, db);
}

float sdfIntersect(float da, float db)
{
	return max(da, db);
}

float sdfDiff(float da, float db) 
{
	return max(da, -db);
}

float sdfLine(float2 p1, float2 p2, float width, float2 p) 
{
	float3 params = cross(float3(p1, 1), float3(p2, 1));
	float dist = abs(dot(float3(p,1), params)) / length(params);
	return dist - width;
}

float sdfBox(float2 leftBottom, float2 rightTop, float2 p) 
{
	float dLeft = leftBottom.x - p.x;
	float dRight = p.x - rightTop.x;
	float dTop = p.y - rightTop.y;
	float dBottom = leftBottom.y - p.y;
	float vertical = sdfIntersect(dLeft, dRight);
	float horizontal = sdfIntersect(dTop, dBottom);
	return sdfIntersect(vertical, horizontal);
}

/* Don't Overuse this function. It is heavy(uses fwidth) */
fixed4 sdfRenderFill(float4 color, float d) 
{
	// Simple anti-aliasing
	float aaf = fwidth(d);
	float4 resultColor = float4(color.rgb, 1.0 - smoothstep(-aaf, aaf, d));
	return resultColor;
}
