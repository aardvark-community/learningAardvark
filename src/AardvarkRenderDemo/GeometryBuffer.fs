namespace SLEAardvarkRenderDemo
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

module GeometryBuffer  =

    //Render task for the Geometry-buffer pass
    let makeGBuffer (runtime : IRuntime) (view : aval<Trafo3d>) projection size skyBoxTexture scene skyMapIntensity=

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
                Sym.ofString "WorldPosition", RenderbufferFormat.Rgba32f
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
                DefaultSemantic.Normals, RenderbufferFormat.Rgba32f
                GBufferRendering.Semantic.MaterialProperties, RenderbufferFormat.Rg32f
                GBufferRendering.Semantic.Emission, RenderbufferFormat.Rgb32f
             ]

        let skyBox =
            Sg.box (AVal.constant C4b.White) (AVal.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.cullMode (AVal.constant CullMode.None)
                |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
                |> Sg.uniform "SkyMapIntensity" skyMapIntensity
                |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))
                |> Sg.shader {
                    do! GBufferRendering.skyBoxTrafo
                    do! GBufferRendering.skyGBuffer
                }

        scene
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! displacemntMap.displacementMap
            do! DefaultSurfaces.vertexColor
            do! AlbedoColor.albedoColor
            do! NormalMap.normalMap 
            do! GBufferRendering.gBufferShader
            }
        |> (Sg.andAlso <| skyBox )
        |> Sg.viewTrafo (view)
        |> Sg.projTrafo (projection)
        |> Sg.compile runtime signature
        |> RenderTask.renderSemantics(
                    Set.ofList [
                        DefaultSemantic.Depth
                        DefaultSemantic.Colors
                        Sym.ofString "WorldPosition"
                        DefaultSemantic.Normals
                        GBufferRendering.Semantic.MaterialProperties
                        GBufferRendering.Semantic.Emission
                        ]
               ) size 