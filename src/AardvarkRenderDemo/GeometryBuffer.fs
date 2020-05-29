namespace SLEAardvarkRenderDemo
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects

 module GBufferRendering =
    open fshadeExt
    //shaders for rendering to a g-buffer

    module Semantic =
        let MaterialProperties = Symbol.Create "MaterialProperties"
        let Emission = Symbol.Create "Emission"

    type UniformScope with
        member x.Roughness : float = x?Roughness

        member x.Metallic : float = x?Metallic

        member x.AlbedoFactor : float = x?AlbedoFactor

        member x.EmissionFactor : float = x?EmissionFactor

        member x.EmissionColor : V3d =  x?EmissionColor

        member x.Discard : bool =  x?Discard

        member x.SkyMapIntensity : float =  x?SkyMapIntensity

    let internal skyBoxTrafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            //let rotView  = m33d uniform.ViewTrafo |> m44d 
            //let  clipPos =  uniform.ProjTraSfo * rotView * wp
            let cameraPos = uniform.CameraLocation
            let clipPos = (uniform.ViewProjTrafo * (wp + V4d(cameraPos,0.0))) //remove translation
            return {
                pos = V4d(clipPos.X,clipPos.Y,clipPos.W,clipPos.W)
                wp = wp
                n =  v.n
                b =  v.b
                t =  v.t
                c = v.c
                tc = v.tc
            }
        }

    type Vertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<BiNormal>]        b       : V3d
        [<Tangent>]         t       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<Semantic("MaterialProperties")>] m    : V2d
        [<Semantic("Emission")>] em : V3d
    }

    let private skySampler =
        samplerCube {
            texture uniform?SkyCubeMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private metallicSampler =
        sampler2d {
            texture uniform?MetallicMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private roughnessSampler =
        sampler2d {
            texture uniform?RoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private emissionSampler =
        sampler2d {
            texture uniform?EmissionTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let gBufferShader (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            if uniform.Discard then
                discard()
            let albedo = pow (vert.c.XYZ * uniform.AlbedoFactor) (V3d(gamma))
            let metallic = uniform.Metallic * metallicSampler.Sample(vert.tc).X
            let roughness = uniform.Roughness * roughnessSampler.Sample(vert.tc).X
            let emission = uniform.EmissionColor * uniform.EmissionFactor * emissionSampler.Sample(vert.tc).XYZ
            return {vert with c = V4d(albedo,vert.c.W); m = V2d(metallic,roughness); em = emission}
        }

    let skyGBuffer (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            let lPos  = vert.wp.XYZ |> Vec.normalize
            let texColor = skySampler.Sample(lPos).XYZ
  
            let col = texColor * uniform.SkyMapIntensity

            return {vert with c = V4d(col,vert.c.W); m = V2d(-1.0,-1.0); em = V3d.OOO}
        }
  
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