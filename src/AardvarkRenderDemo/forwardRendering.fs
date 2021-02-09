namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

module forwardRendering =

    let diffuseAndSpecular 
        (runtime : IRuntime) 
        (view : aval<Trafo3d>) 
        projection 
        size 
        skyBoxTexture 
        scene 
        skyMapIntensity 
        (lights : amap<int,AdaptiveLightCase>)
        bb 
        ambientLightIntensity
        diffuseIrradianceMap
        prefilterdSpecColor
        bRDFLtu =

        //adaptive function to calcualte the shadow map for one light
        let shadowMapTex light = 
            AVal.bind (Shadow.shadowMap runtime scene bb) light 

        let shadowMaps (lights : amap<int,AdaptiveLightCase>) = 
            let mapper (l' : AdaptiveLightCase) = 
                aval{
                    let! castsShadow =
                        match l' with
                        | AdaptivePointLight l -> AVal.constant false
                        | AdaptiveSphereLight l -> AVal.constant false
                        | AdaptiveSpotLight l -> AVal.map (fun (x : SpotLightData)-> x.castsShadow) l
                        | AdaptiveDirectionalLight l -> AVal.map (fun (x : DirectionalLightData)-> x.castsShadow) l
                        | AdaptiveDiskLight l -> AVal.map (fun (x : DiskLightData)-> x.castsShadow) l
                        | AdaptiveRectangleLight l ->  AVal.map (fun (x : RectangleLightData) -> x.castsShadow) l
                    let tex = if castsShadow then Shadow.shadowMap runtime scene bb l' else AVal.constant (NullTexture() :> ITexture)
                    return! tex               
                }
            let m =
                lights
                |> AMap.mapA (fun _ l -> mapper l)
            m

        let shadowMapUniform2 (shadowMaps : amap<int,ITexture>) (inp :ISg<'m>)=
            shadowMaps
            |> AMap.toAVal
            |> AVal.map (fun (m : HashMap<int,ITexture>) -> 
                                m 
                                |> HashMap.toList 
                                |> List.sortBy fst 
                                |> List.mapi (fun i (_, t) -> i, t)
                                |> List.fold (fun s (i, t) ->  s |> (Sg.uniform ("ShadowMapArray"+i.ToString()) (AVal.constant t))) inp
                        ) 

            

        let shadowMapUniform (shadowMaps : amap<int,ITexture>) (inp :ISg<'m>)=
            let mutable ii = -1
            shadowMaps
            |> AMap.toASet
            |> ASet.sortBy fst
            |> AList.fold (fun s (_, m) ->  
                                ii <- ii + 1
                                s |> (Sg.uniform ("ShadowMapArray"+ii.ToString()) (AVal.constant m))
                                ) inp

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
        
        let shadows = shadowMapUniform2 (shadowMaps lights)

        scene
        |> shadows
        |> Sg.dynamic
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
        |> Sg.uniform "Light" (AVal.constant SLEUniform.noLight)
        |> SLEUniform.uniformLightArray bb lights 
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

