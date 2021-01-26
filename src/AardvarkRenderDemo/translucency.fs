namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

module translucency =
    open FShade
    open fshadeExt

    [<ReflectedDefinition>]
    let transmittance (translucency : float) (sssWidth : float) (wp : V3d) (wn : V3d) (l :V3d) (shadowMap : Sampler2d) (lightViewM : M44d) (lightProjM : M44d) (lightFarZ : float) =
        let scale = 8.25 * (1.0 - translucency) / sssWidth
        let shrinked_pos = V4d(wp - 0.005 * wn, 1.0)
        let posLightSpace = lightViewM * shrinked_pos
        let shadowPos =  lightProjM * posLightSpace
        let d1 = shadowMap.Sample(shadowPos.XY / shadowPos.W).X * lightFarZ
        let d2 = -posLightSpace.Z / posLightSpace.W
        let dist = abs(d1 - d2)
        V3d(8.25*dist)

(*vec3 transmittance(float translucency, float sss_width, vec3 world_position, vec3 world_normal, vec3 light_vector, sampler2D shadow_map, mat4 light_V, mat4 ligh_BP ,float light_far_plane)
{
	float scale = 8.25 * (1.0 - translucency) / sss_width;
	vec4 shrinked_pos = vec4(world_position - 0.005 * world_normal, 1.0);
	vec4 shadow_pos = light_V*shrinked_pos;

	highp float d2 = (shadow_pos.z/shadow_pos.w)/-light_far_plane;

	shadow_pos = ligh_BP * shadow_pos;
	highp float d1 = texture(shadow_map, shadow_pos.xy/shadow_pos.w).r;
	//return vec3(2*d1);
	d1 *= light_far_plane;
	d2 *= light_far_plane;
	//return vec3(abs(d1 - d2)*10);

	highp float dist = abs(d1 - d2)*scale;
	//float d = scale * dist;
	highp float d = dist;
	highp float dd = -d * d;
	/*
    vec3 profile =	vec3(0.233, 0.455, 0.649) * exp(dd / 0.0064) +
                    vec3(0.1,   0.336, 0.344) * exp(dd / 0.0484) +
                    vec3(0.118, 0.198, 0.0)   * exp(dd / 0.187)  +
                    vec3(0.113, 0.007, 0.007) * exp(dd / 0.567)  +
                    vec3(0.358, 0.004, 0.0)   * exp(dd / 1.99)   +
                    vec3(0.078, 0.0,   0.0)   * exp(dd / 7.41);
	*/
	vec3 profile =	vec3(1, 1, 1) * exp(dd / 0.0064) +
                    vec3(0.3,   0.424, 0.344) * exp(dd / 0.0484) +
                    vec3(0.261, 0.2, 0.0)   * exp(dd / 0.187)  +
                    vec3(0.2, 0.007, 0.007) * exp(dd / 0.567)  +
                    vec3(0.388, 0.004, 0.0)   * exp(dd / 1.99)   +
                    vec3(0.078, 0.0,   0.0)   * exp(dd / 7.41);
	return profile * clamp(0.3 + dot(normalize(light_vector), -normalize(world_normal)), 0.0, 1.0);
}*)