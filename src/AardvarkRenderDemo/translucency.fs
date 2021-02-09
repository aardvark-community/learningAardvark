namespace SLEAardvarkRenderDemo

open Aardvark.Base

(*
    Translucency or Forward scattering 

    Based on: Real Time Realistic Skin Translucency, Jorge Jimenez et.al. 2010

    The implementation at https://github.com/vcrom/SubsurfaceScattering was used as a code reference
*)
module translucency =
    open FShade
    open fshadeExt

    type UniformScope with
        member x.Light : SLEUniform.Light = x?Light
        member x.sssWidth :  Arr<N<8>, float> = x?sssWidth
        member x.sssFalloff :  Arr<N<8>, V3d> = x?sssFalloff
        member x.sssStrength :  Arr<N<8>, V3d> = x?sssStrength
        member x.TranslucencyStrength :  Arr<N<8>, float> = x?TranslucencyStrength
        member x.TranslucencyBias :  Arr<N<8>, float> = x?TranslucencyBias

    let samplerShadowMap1 =
        sampler2d {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
        }

    [<ReflectedDefinition>]
    let getShadowLinearDepth (tc :V2d) = 
        match uniform.Light.lightType  with
        | SLEUniform.LightType.DirectionalLight ->              
            samplerShadowMap1.Sample(tc).X * uniform.Light.shadowMapMaxZ //directional lights use ortho projection on shadows with already give linear depht
        | _ -> // all ohters use perspective projection and depth is non linear 
            let z = 2.0 * samplerShadowMap1.Sample(tc).X - 1.0
            let n = uniform.Light.shadowMapMinZ
            let f = uniform.Light.shadowMapMaxZ
            (2.0 * n * f) / (f + n - z * (f - n))

    // local thickness in light direction approcimated as depth in light space - blocker depth from shadow map
    [<ReflectedDefinition>]
    let getThickness (bias : float) (wp  :V3d) (wn :V3d) = 
        let shrinkedPos = V4d(wp - bias * wn, 1.0) // bias to avoid artifacts at the object edges
        let posLightSpace = uniform.Light.lightViewMatrix * V4d(wp, 1.0)//* shrinkedPos
        let shadowPos =  uniform.Light.lightViewProjMatrix * shrinkedPos
        let cc = shadowPos.XY / shadowPos.W * 0.5 + 0.5
        let d1 = getShadowLinearDepth cc 
        let d2 = -posLightSpace.Z / posLightSpace.W
        max (abs(d1 - d2)) 0.001 //  minimal depth tos avoid very small or negativ thickness because of the bias

    [<ReflectedDefinition>]
    let transm (profileIndex : int) (wp : V3d) (wn : V3d) (l :V3d)  =
        let bias =  uniform.TranslucencyBias.[profileIndex]
        let sssWidth = uniform.sssWidth.[profileIndex]
        let sssFalloff = uniform.sssFalloff.[profileIndex]
        let sssStrength = uniform.sssStrength.[profileIndex]
        let translucency = uniform.TranslucencyStrength.[profileIndex]+1.0
        let scale = 4.0 / translucency / sssWidth // scale by Subsurface Scattering Width, modified by translucency strength. The constant 4 is estimated visually at the moment 
        let dist = getThickness bias wp wn
        let d = dist * scale / (sssFalloff + 0.001) // we modifie the distance with a Falloff per channel
        let dd = -d * d
        let lDotN = Vec.dot l -wn
        let profile = 0.233 * exp(dd / 0.0064) + 
                      0.1   * exp(dd / 0.0484) +
                      0.118 * exp(dd / 0.187)  +
                      0.113 * exp(dd / 0.567)  +
                      0.358 * exp(dd / 1.99)   +
                      0.078 * exp(dd / 7.41)
        sssStrength * profile * saturate (0.3 + lDotN)   //0.3 added to lDotN to avaoid a hard transition form front- to backlit areas   
