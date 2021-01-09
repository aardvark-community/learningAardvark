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
        let Emission = Symbol.Create "Emission"
        let NormalR = Symbol.Create "NormalR"
        let ClearCoat = Symbol.Create "ClearCoat"
        let Sheen = Symbol.Create "Sheen"

    type UniformScope with
        member x.Roughness : float = x?Roughness

        member x.Metallic : float = x?Metallic

        member x.AlbedoFactor : float = x?AlbedoFactor

        member x.EmissionFactor : float = x?EmissionFactor

        member x.EmissionColor : V3d =  x?EmissionColor

        member x.Discard : bool =  x?Discard

        member x.SkyMapIntensity : float =  x?SkyMapIntensity

        member  x.ClearCoat :  float =  x?ClearCoat

        member  x.ClearCoatRoughness :  float =  x?ClearCoatRoughness

        member  x.ClearCoatNormalStrength :  float =  x?ClearCoatNormalStrength
       
        member  x.UseNormalsForClearCoat :  bool =  x?UseNormalsForClearCoat

        member x.NormalMapStrength : float =  x?NormalMapStrength

        member x.SheenColorFactor : float = x?SheenColorFactor

        member x.SheenColor : V3d =  x?SheenColor

        member  x.SheenRoughness :  float =  x?SheenRoughness

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
        [<Semantic("NormalR")>] nr       : V4d
        [<BiNormal>]        b       : V3d
        [<Tangent>]         t       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<Semantic("Emission")>] em : V4d
        [<Semantic("ClearCoat")>] cc : V4d
        [<Semantic("Sheen")>] sheen : V4d
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

    let private clearCoatRoughnessSampler =
        sampler2d {
            texture uniform?ClearCoatRoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private clearCoatSampler =
        sampler2d {
            texture uniform?ClearCoatMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private sheenRoughnessSampler =
        sampler2d {
            texture uniform?SheenRoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private sheenColorSampler =
        sampler2d {
            texture uniform?SheenColorTexture
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
            let clearCoat = uniform.ClearCoat * clearCoatSampler.Sample(vert.tc).X
            let clearCoatRoughness = uniform.ClearCoatRoughness * clearCoatRoughnessSampler.Sample(vert.tc).X
            let sheenColor =  pow ( uniform.SheenColor * uniform.SheenColorFactor * sheenColorSampler.Sample(vert.tc).XYZ) (V3d(gamma))
            let sheenRoughness = uniform.SheenRoughness * sheenRoughnessSampler.Sample(vert.tc).X
  
            return {vert with 
                        c = V4d(albedo,metallic)
                        nr = V4d(vert.n.XYZ,  roughness)
                        em = V4d(emission,clearCoatRoughness)
                        cc = V4d(vert.cc.XYZ,clearCoat)
                        sheen = V4d(sheenColor.XYZ,sheenRoughness)
                        }
        }

    let skyGBuffer (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            let lPos  = vert.wp.XYZ |> Vec.normalize
            let texColor = skySampler.Sample(lPos).XYZ
  
            let col = texColor * uniform.SkyMapIntensity

            return {vert with c = V4d(col,-1.0)
                              nr = V4d(vert.n.XYZ,  -1.0) 
                              em = V4d.OOOO
                              cc = V4d.OOOO
                              sheen = V4d.OOOO
            }
        }

    let private normalSampler =
        sampler2d {
            texture uniform?NormalMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private clearCoatNormalSampler =
        sampler2d {
            texture uniform?ClearCoatNormalMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let internal normalMap (v : Vertex) =
        fragment {
            let texColor = normalSampler.Sample(v.tc).XYZ
            let texNormal = (2.0 * texColor - V3d.III) |> Vec.normalize

            let n = v.n.Normalized * texNormal.Z + v.b.Normalized * texNormal.X + v.t.Normalized * texNormal.Y |> Vec.normalize

            let strength = uniform.NormalMapStrength
            let n2 = Lerp v.n n strength

            let ccN =
                if uniform.UseNormalsForClearCoat then
                    n
                else
                    let cctexColor =  clearCoatNormalSampler.Sample(v.tc).XYZ
                    let cctexNormal = (2.0 * cctexColor - V3d.III) |> Vec.normalize

                    v.n.Normalized * cctexNormal.Z + v.b.Normalized * cctexNormal.X + v.t.Normalized * cctexNormal.Y |> Vec.normalize  

            let ccstrength = uniform.ClearCoatNormalStrength
            let ccn2 = Lerp v.n ccN ccstrength

            return { v with n = n2; cc = V4d(ccn2, 0.0) }
        }


module GeometryBuffer  =

    //Render task for the Geometry-buffer pass
    let makeGBuffer (runtime : IRuntime) (view : aval<Trafo3d>) projection size skyBoxTexture scene skyMapIntensity=

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba16f
                Sym.ofString "WorldPosition", RenderbufferFormat.Rgba32f
                DefaultSemantic.Depth, RenderbufferFormat.DepthComponent24
                GBufferRendering.Semantic.NormalR, RenderbufferFormat.Rgba32f
                GBufferRendering.Semantic.Emission, RenderbufferFormat.Rgba16f
                GBufferRendering.Semantic.ClearCoat, RenderbufferFormat.Rgba16f
                GBufferRendering.Semantic.Sheen, RenderbufferFormat.Rgba16f
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
            do! GBufferRendering.normalMap 
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
                        GBufferRendering.Semantic.NormalR
                        GBufferRendering.Semantic.Emission
                        GBufferRendering.Semantic.ClearCoat
                        GBufferRendering.Semantic.Sheen
                        ]
               ) size 

module GBuffer = 
    open FShade
    
    let wPos =
        sampler2d {
            texture uniform?WPos
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let normal =
        sampler2d {
            texture uniform?Normals
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }
        
    let color =
        sampler2d {
            texture uniform?Colors
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let depth =
        sampler2d {
            texture uniform?Depth
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let emission =
        sampler2d {
            texture uniform?Emission
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let clearCoat =
        sampler2d {
            texture uniform?ClearCoat
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let sheen =
        sampler2d {
            texture uniform?Sheen
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    type Fragment = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<Semantic("Metallic")>] metallic    : float
        [<Semantic("Roughness")>] roughness    : float
        [<Semantic("Emission")>] emission    : V3d
        [<Semantic("ClearCoat")>] clearCoat    : float
        [<Semantic("ClearCoatRoughness")>] clearCoatRoughness    : float
        [<Semantic("ClearCoatNormal")>] clearCoatNormal    : V3d
        [<Semantic("sheenRoughness")>] sheenRoughness    : float
        [<Semantic("sheenColor")>] sheenColor : V3d
    }

    let getGBufferData (vert : Vertex) =
        fragment {
            let albedo = color.Sample(vert.tc) 
            let wPos = wPos.Sample(vert.tc)
            let nr  = normal.Sample(vert.tc)
            let n = nr.XYZ |> Vec.normalize
            let em = emission.Sample(vert.tc)
            let cc  = clearCoat.Sample(vert.tc)
            let clearCoatNormal = cc.XYZ |> Vec.normalize
            let sheen = sheen.Sample(vert.tc) 
            return {wp =  wPos; n = n; c = V4d(albedo.XYZ,1.0);  tc = vert.tc; metallic = albedo.W; roughness = nr.W; emission =  em.XYZ; clearCoat =  cc.W; clearCoatRoughness = em.W; clearCoatNormal = clearCoatNormal; sheenColor = sheen.XYZ; sheenRoughness = sheen.W}
        }
