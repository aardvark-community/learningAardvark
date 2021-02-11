namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

module deferredRendering =

    let diffuseAndSpecular 
        (runtime : IRuntime) 
        (view : aval<Trafo3d>) 
        size 
        scene 
        (lights : amap<int,AdaptiveLightCase>)
        sssProfiles
        bb 
        gBuffer
        ambientOcclusion
        ambientLightIntensity
        diffuseIrradianceMap
        prefilterdSpecColor
        bRDFLtu =
        
        let signature =
            runtime.CreateFramebufferSignature [
                (Sym.ofString "Diffuse") , RenderbufferFormat.Rgba32f
                (Sym.ofString "Specular") , RenderbufferFormat.Rgba32f
            ]

        Sg.fullScreenQuad
        |> Sg.adapter
        |> Shadow.shadowMapsUniform (Shadow.shadowMaps runtime scene bb lights)
        |> Sg.shader {
            do! GBuffer.getGBufferData
            do! PBR.lightnigDeferred        
        }
        |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))
        |> SLEUniform.uniformLightArray bb lights 
        |> Sg.uniform "AmbientIntensity" ambientLightIntensity
        |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))  
        |> subSurface.sssProfileUniforms sssProfiles
        |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
        |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
        |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
        |> Sg.texture (Sym.ofString "AmbientOcclusion") ambientOcclusion
        |> Sg.texture ( DefaultSemantic.Colors) (Map.find DefaultSemantic.Colors gBuffer)
        |> Sg.texture ( Sym.ofString "WPos") (Map.find (Sym.ofString "WorldPosition") gBuffer)
        |> Sg.texture ( DefaultSemantic.Normals) (Map.find shaderCommon.Semantic.NormalR gBuffer)
        |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
        |> Sg.texture (shaderCommon.Semantic.Emission) (Map.find shaderCommon.Semantic.Emission gBuffer)
        |> Sg.texture (shaderCommon.Semantic.ClearCoat) (Map.find shaderCommon.Semantic.ClearCoat gBuffer)
        |> Sg.texture (shaderCommon.Semantic.Sheen) (Map.find shaderCommon.Semantic.Sheen gBuffer)
        |> Sg.compile runtime signature
        |> RenderTask.renderSemantics(
                    Set.ofList [
                        (Sym.ofString"Diffuse")
                        (Sym.ofString"Specular")
                    ]
               ) size 