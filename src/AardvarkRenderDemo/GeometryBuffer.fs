namespace SLEAardvarkRenderDemo
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Rendering
open FShade
open Aardvark.Rendering.Effects

 module GBufferRendering =
    open fshadeExt
    //shaders for rendering to a g-buffer
   
     type Frag = {
        [<WorldPosition>]   wp      : V4d
        [<Semantic("NormalR")>] nr       : V4d
        [<Color>]           c       : V4d
        [<Semantic("Emission")>] em : V4d
        [<Semantic("ClearCoat")>] cc : V4d
        [<Semantic("Sheen")>] sheen : V4d
         }

    let gBufferShader (vert : shaderCommon.Fragment) =
        fragment {
            if min3 (vert.c.W * (V3d.III - vert.transmission)) < 1.0 then discard()//render only fully opaque fragments to gBuffer
            let m = 10.0 * float vert.sssProfile + vert.metallic

            return {wp = vert.wp
                    c = V4d(vert.c.XYZ,m)
                    nr = V4d(vert.n, vert.roughness)
                    em = V4d(vert.emission,vert.clearCoatRoughness)
                    cc = V4d(vert.clearCoatNormal,vert.clearCoat)
                    sheen = V4d(vert.sheenColor,vert.sheenRoughness)
                    }
        }
        
module GeometryBuffer  =

    //Render task for the Geometry-buffer pass
    let makeGBuffer 
        (runtime : IRuntime) 
        (view : aval<Trafo3d>) 
        projection 
        size 
        (skyBoxTexture : IAdaptiveResource<IBackendTexture>) 
        scene 
        (skyMapIntensity : aval<float>)
        (depthBuffer : aval<IFramebufferOutput> option) =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba16f
                Sym.ofString "WorldPosition", RenderbufferFormat.Rgba32f
                DefaultSemantic.Depth, RenderbufferFormat.DepthComponent24
                shaderCommon.Semantic.NormalR, RenderbufferFormat.Rgba32f
                shaderCommon.Semantic.Emission, RenderbufferFormat.Rgba16f
                shaderCommon.Semantic.ClearCoat, RenderbufferFormat.Rgba16f
                shaderCommon.Semantic.Sheen, RenderbufferFormat.Rgba16f
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
                    do! GBufferRendering.gBufferShader
                }

        let attachments =
            match depthBuffer with
            |Some a -> 
                (Map.ofList [
                    DefaultSemantic.Depth, a
                ]) 
            |None -> Map.empty

        scene
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! displacemntMap.displacementMap
            do! DefaultSurfaces.vertexColor
            do! AlbedoColor.albedoColor
            do! shaderCommon.normalMap 
            do! shaderCommon.getMatrialValues
            do! GBufferRendering.gBufferShader
            }
        |> (Sg.andAlso <| skyBox )
        |> Sg.viewTrafo (view)
        |> Sg.projTrafo (projection)
        |> Sg.compile runtime signature
        |> RenderTaskExtensions.renderSemanticsCustom'(
                    Set.ofList [
                        DefaultSemantic.Depth
                        DefaultSemantic.Colors
                        Sym.ofString "WorldPosition"
                        shaderCommon.Semantic.NormalR
                        shaderCommon.Semantic.Emission
                        shaderCommon.Semantic.ClearCoat
                        shaderCommon.Semantic.Sheen
                        ]
               ) 
               size 
               attachments
     

module GBuffer = 
    open FShade
    open shaderCommon
    
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

    //the profile index is packed together with metallic in the W comaponent of the albedo gBuffer texture
    [<ReflectedDefinition>]
    let extractProfileIndex (w : float) =
        let i = truncate (w / 10.0)
        if (i >= 0.0) && (i <= 7.0) && ( w >= 0.0) then  int i else -1

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
            let metallic = if albedo.W < 0.0 then albedo.W else (albedo.W / 10.0 - truncate (albedo.W / 10.0 )) * 10.0
            let sssProfile = extractProfileIndex albedo.W 
            return {wp =  wPos
                    n = n
                    c = V4d(albedo.XYZ,1.0)
                    tc = vert.tc
                    metallic = metallic
                    roughness = nr.W
                    emission =  em.XYZ
                    clearCoat =  cc.W
                    clearCoatRoughness = em.W
                    clearCoatNormal = clearCoatNormal
                    sheenColor = sheen.XYZ
                    sheenRoughness = sheen.W
                    sssProfile = sssProfile 
                    transmission = V3d.OOO
                   }
        }

