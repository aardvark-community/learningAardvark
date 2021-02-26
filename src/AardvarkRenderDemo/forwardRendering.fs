namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Rendering
open SLEAardvarkRenderDemo.Model

module forwardRendering =

    let diffuseAndSpecular 
        (runtime : IRuntime) 
        (view : aval<Trafo3d>) 
        projection 
        size 
        (skyBoxTexture   : IAdaptiveResource<IBackendTexture>) 
        scene 
        (skyMapIntensity   : aval<float>)
        (lights : amap<int,AdaptiveLightCase>)
        bb 
        (ambientLightIntensity  : aval<float>)
        (diffuseIrradianceMap  : IAdaptiveResource<IBackendTexture>) 
        (prefilterdSpecColor   : IAdaptiveResource<IBackendTexture>) 
        (bRDFLtu   : IAdaptiveResource<IBackendTexture>) 
        sssProfiles =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Depth, RenderbufferFormat.DepthComponent24
                (Sym.ofString "Diffuse") , RenderbufferFormat.Rgba32f
                (Sym.ofString "Specular") , RenderbufferFormat.Rgba32f
            ]

        let skyBox =
            Sg.box (AVal.constant C4b.White) (AVal.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.cullMode (AVal.constant CullMode.None)
                |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
                |> Sg.uniform "SkyMapIntensity" skyMapIntensity
                |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))
                |> Sg.shader {
                    do! shaderCommon.skyBoxTrafo
                    do! shaderCommon.skyGetMatrialValues
                    do! PBR.lightnigForward
                }
        
        scene
        |> Shadow.shadowMapsUniform (Shadow.shadowMaps runtime scene bb lights)
        |> Sg.cullMode (AVal.constant CullMode.None)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! displacemntMap.displacementMap
            do! DefaultSurfaces.vertexColor
            do! AlbedoColor.albedoColor
            do! shaderCommon.normalMap 
            do! shaderCommon.getMatrialValues
            do! PBR.lightnigForward        
        }
        |> (Sg.andAlso <| skyBox )
        |> Sg.viewTrafo (view)
        |> Sg.projTrafo (projection)
        |> SLEUniform.uniformLightArray bb lights 
        |> subSurface.sssProfileUniforms sssProfiles
        |> Sg.uniform "AmbientIntensity" ambientLightIntensity
        |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))        
        |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
        |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
        |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
        |> Sg.compile runtime signature
        |> RenderTask.renderSemantics(
                Set.ofList [
                    (Sym.ofString"Diffuse")
                    (Sym.ofString"Specular")
                ]
           ) size 

