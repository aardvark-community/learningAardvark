namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Rendering.Effects
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open SLEAardvarkRenderDemo.Model
(*
    Colored Weighed Blended Order Independent Trransparency
    http://casual-effects.blogspot.com/2015/03/colored-blended-order-independent.html

    with additional features from  
    http://casual-effects.com/research/McGuire2016Transparency/index.html
*)

module WBOTI =
    open fshadeExt

    type Fragment = {
        [<Color>]                    Color        : V4d
        [<FragCoord>]                CsPos        : V4d
        [<Semantic("Transmission")>] Transmission : V3d
       }

    type FragmentOut = {
        [<Semantic("ModulateColor")>] Modulate     : V3d
        [<Semantic("Accum")>]         Accum     : V4d
       }

    let dummySampler =
        sampler2d {
            texture uniform?Dummy
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    //calculates 
    let accumulateShader (frag : Fragment) =
        fragment {
            let coverage = frag.Color.W
            //Perform this operation before modifying the coverage to account for transmission.
            //modulation of background color by transmission color
            let modulate = 
                coverage * (V3d.III - frag.Transmission) 
                //wild hack to convince the adaptive system that this shader depends on the gBuffer to insure that this render tasks runs after the gBuffer because 
                //it needs to use the deep attachment from the gBuffer task 
                + if frag.Color.W > 1000.0 then (dummySampler.Sample(V2d.II).W * 0.0) else 0.0
            
            (* Modulate the net coverage for composition by the transmission. This does not affect the color channels of the
               transparent surface because the caller's BSDF model should have already taken into account if transmission modulates
               reflection. See 

               McGuire and Enderton, Colored Stochastic Shadow Maps, ACM I3D, February 2011
               http://graphics.cs.williams.edu/papers/CSSM/

               for a full explanation and derivation.*)           
            let netCoverage = coverage * (1.0 - Vec.dot frag.Transmission (V3d(1.0/3.0)))

            //calcualte weight. See reference implementation on http://casual-effects.com/research/McGuire2016Transparency/index.html for alternate weight functions 
            let tmp = (1.0 - frag.CsPos.Z * 0.99) 
            let w  = netCoverage * tmp * tmp * tmp * 1000.0  |>  clamp 0.01 30.0

            let accum = V4d(frag.Color.XYZ, netCoverage) * w
            return { Modulate = modulate; Accum = accum}
        } 

    let backgroundSampler =
        sampler2d {
            texture uniform?Colors
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let accumSampler =
        sampler2d {
            texture uniform?Accum
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let modulateSampler =
        sampler2d {
            texture uniform?Modulate
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let composeShader (frag : Vertex) =
        fragment {
            let modulation = modulateSampler.Sample(frag.tc).XYZ
            let background = backgroundSampler.Sample(frag.tc).XYZ
            let color = 
                if min3 modulation > 0.99999 then //no transparency at this pixel
                    background
                else
                    let accum = accumSampler.Sample(frag.tc)
                    
                    // In the case where the denominator overflowed, at least preserve some color
                    // instead of writing zero by dividing through by infinity
                    let accumDenom = (if accum.W.IsInfinity() then max3 accum.XYZ else accum.W) |> max 0.00001

                    // Suppress overflow of the numerator by outputting white
                    let accumColor0 = if (max3 accum.XYZ).IsInfinity() then V3d.III else accum.XYZ
 
                     // Attempt to fake transmission on the additive term by blending in a little bit of the 
                    // background modulation
                    //let accum = accumColor0 * (V3d(0.5) + modulation / max 0.01 (2.0 * max3 modulation));

                    background * modulation + 
                    (V3d.III - modulation) * 
                    accumColor0 / accumDenom

            return V4d(color, 1.0)
        }

    let acummulate 
        (runtime : IRuntime) 
        (view : aval<Trafo3d>) 
        projection 
        size 
        objects
        shadowMaps
        (lights : amap<int,AdaptiveLightCase>)
        bb 
        (ambientLightIntensity : aval<float>)
        (diffuseIrradianceMap : aval<IBackendTexture>)
        (prefilterdSpecColor : aval<IBackendTexture>)
        (bRDFLtu : aval<IBackendTexture>)
        sssProfiles
        (depthBuffer : aval<IFramebufferOutput>)
        (gBuffer : Map<Symbol,IAdaptiveResource<IBackendTexture>>) //only to force dependency on the gBuffer
        =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Depth, RenderbufferFormat.DepthComponent24
                (Sym.ofString "Accum") , RenderbufferFormat.Rgba32f
                (Sym.ofString "ModulateColor") , RenderbufferFormat.Rgb32f
            ]

        let ModulateBlend =
            { BlendMode.Blend with
                SourceColorFactor = BlendFactor.Zero
                SourceAlphaFactor = BlendFactor.Zero
                DestinationColorFactor = BlendFactor.InvSourceColor
                DestinationAlphaFactor = BlendFactor.SourceAlpha
            }
        
        objects
        |> sceneObject.objectsTrimByMaterial (fun _  (a : AdaptivePBRMaterial) -> AVal.map2 (fun c t -> c > 0.99999 && t < 0.00001) a.coverage.factor a.transmission.factor)
        //|>sceneObject.objects 
        |> Sg.depthWrite' false
        //|> Sg.cullMode' CullMode.Back
        |> Sg.blendModes' (Map.ofList [
            (Sym.ofString "Accum"), BlendMode.Add
            (Sym.ofString "ModulateColor"), ModulateBlend
        ])        
        |> Shadow.shadowMapsUniform shadowMaps
        |> Sg.cullMode (AVal.constant CullMode.None)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! displacemntMap.displacementMap
            do! DefaultSurfaces.vertexColor
            do! AlbedoColor.albedoColor
            do! shaderCommon.normalMap 
            do! shaderCommon.getMatrialValues
            do! PBR.lightnigTransparent
            do! accumulateShader
        }
        |> Sg.viewTrafo (view)
        |> Sg.projTrafo (projection)
        |> SLEUniform.uniformLightArray bb lights 
        |> subSurface.sssProfileUniforms sssProfiles
        |> Sg.uniform "AmbientIntensity" ambientLightIntensity
        |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))        
        |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
        |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
        |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
        |> Sg.texture (Sym.ofString "Dummy") (Map.find DefaultSemantic.Colors gBuffer)//only to force dependency on the gBuffer
        |> Sg.compile runtime signature
        |> RenderTaskExtensions.renderSemanticsCustom
            (
                Set.ofList [
                    (Sym.ofString "ModulateColor")
                    (Sym.ofString "Accum")
                ]
           ) 
           size
           (Map.ofList [
                (Sym.ofString "Accum"), C4f.Zero
                (Sym.ofString "ModulateColor"),C4f.White
            ])
           (Map.ofList [
                DefaultSemantic.Depth, depthBuffer
            ])      

    let compose runtime outputSignature size (backgroundTexture : aval<IBackendTexture>) (accumTexture : aval<IBackendTexture>) (modulateTexture : aval<IBackendTexture>)=
        Sg.fullScreenQuad
        |> Sg.adapter
        |> Sg.shader {
           do! composeShader 
            }    
        |> Sg.texture DefaultSemantic.Colors backgroundTexture 
        |> Sg.texture (Sym.ofString "Accum") accumTexture 
        |> Sg.texture (Sym.ofString "Modulate") modulateTexture 
        |> Sg.compile runtime outputSignature
        |> RenderTask.renderToColor size  
