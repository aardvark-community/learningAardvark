namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

module forwardRendering =

    let diffuseAndSpecular (runtime : IRuntime) (view : aval<Trafo3d>) projection size skyBoxTexture scene skyMapIntensity (light  : aval<AdaptiveLightCase>)bb ambientLightIntensity=

                
        let lightViewMatrix light = 
            AVal.bind (Shadow.lightViewPoject bb) light

        //adaptive function to calcualte the shadow map for one light
        let shadowMapTex light = 
            AVal.bind (Shadow.shadowMap runtime scene bb) light 

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
                    do! PBR.abientDeferred
                }
        
        scene
        |> Sg.cullMode (AVal.constant CullMode.None)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! displacemntMap.displacementMap
            do! DefaultSurfaces.vertexColor
            do! AlbedoColor.albedoColor
            do! shaderCommon.normalMap 
            do! shaderCommon.getMatrialValues
            do! PBR.lightingDeferred
            do! PBR.shadowDeferred          
        }
        |> (Sg.andAlso <| skyBox )
        |> Sg.viewTrafo (view)
        |> Sg.projTrafo (projection)
        |> Sg.uniform "Light" (AVal.bind SLEUniform.uniformLight light)
        |> Sg.texture (Sym.ofString "ShadowMap") (shadowMapTex light)
        |> Sg.uniform "LightViewProjMatrix" (lightViewMatrix  light |> AVal.map(fun (v,p,_,_)  -> v * p))
        |> Sg.uniform "AmbientIntensity" ambientLightIntensity
        |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))        
        |> Sg.compile runtime signature
        |> RenderTask.renderSemantics(
                    Set.ofList [
                        (Sym.ofString"Diffuse")
                        (Sym.ofString"Specular")
                    ]
               ) size 

