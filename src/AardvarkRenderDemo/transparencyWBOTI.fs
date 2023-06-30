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

    type UniformScope with
        member x.tanFoVxHalf : float = x?tanFoVxHalf
        member x.aspectRatio  : float = x?aspectRatio 
        member x.indexOfRefraction : float = x?IndexOfRefraction
        member x.refractionDistance  : float = x?RefractionDistance

    type Fragment = {
        [<Color>]                    Color        : V4d
        [<Normal>]                   N            : V3d
        [<WorldPosition>]            wsPos        : V4d
        [<TexCoord>]                 tc           : V2d
        [<Semantic("Transmission")>] transmission : V3d
        [<FrontFacing>]              frontFacing  : bool
       }

    type FragmentOut = {
        [<Semantic("ModulateColor")>] Modulate     : V3d
        [<Semantic("Accum")>]         Accum     : V4d
        [<Semantic("Delta")>]         Delta     : V2d
       }

    let dummySampler =
        sampler2d {
            texture uniform?Dummy
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let refractionDistanceSampler =
        sampler2d {
            texture uniform?RefractionDistanceMap
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    [<ReflectedDefinition>]
    let backgroundSizeInMeter z = 
        let x = 2.0 * z  + uniform.tanFoVxHalf
        V2d(x, x * uniform.aspectRatio)

    [<ReflectedDefinition>]
    let computeRefactionOffset (backgroundZ : float) (vsN : V3d) (vsPosition : V3d) (etaRatio : float) =
        // Incoming ray direction from eye, pointing away from csPosition 
        let csw_i = Vec.normalize vsPosition

        // Refracted ray direction, pointing away from wsPos 
        let csw_o = Vec.refract etaRatio vsN csw_i |> Vec.normalize

        let totalInternalRefraction = Vec.dot csw_o csw_o < 0.01


        if totalInternalRefraction then
            V2d(0.0)
        else
            //point on the backraound xy plane
            let p = V3d(vsPosition.XY, backgroundZ) 

            //intersection of the refracted ray with background plane
            let hit = p.XY - csw_o.XY * p.Z / csw_o.Z
            let start = p.XY - csw_i.XY * p.Z / csw_i.Z

            // hit is in meter from the center of the screen, scale to fraction of the background size
            let backgroundSize = backgroundSizeInMeter (backgroundZ)
            let hitPos = hit / backgroundSize + V2d(0.5)
            let startPos = start / backgroundSize + V2d(0.5)

            hitPos - startPos


    //calculates 
    let accumulateShader (frag : Fragment) =
        fragment {
            let vsPos = //normal in view space
                uniform.ViewTrafo * frag.wsPos 
                |> Vec.xyz 
                
            let coverage = frag.Color.W
            //Perform this operation before modifying the coverage to account for transmission.
            //modulation of background color by transmission color
            let modulate = 
                coverage * (V3d.III - frag.transmission) 
                //wild hack to convince the adaptive system that this shader depends on the gBuffer to insure that this render tasks runs after the gBuffer because 
                //it needs to use the deep attachment from the gBuffer task 
                + if frag.Color.W > 1000.0 then (dummySampler.Sample(V2d.II).W * 0.0) else 0.0
            
            (* Modulate the net coverage for composition by the transmission. This does not affect the color channels of the
               transparent surface because the caller's BSDF model should have already taken into account if transmission modulates
               reflection. See 

               McGuire and Enderton, Colored Stochastic Shadow Maps, ACM I3D, February 2011
               http://graphics.cs.williams.edu/papers/CSSM/

               for a full explanation and derivation.*)           
            let netCoverage = coverage * (1.0 - Vec.dot frag.transmission (V3d(1.0/3.0)))

            //calcualte weight. See reference implementation on http://casual-effects.com/research/McGuire2016Transparency/index.html for alternate weight functions 
            let tmp = (1.0 - vsPos.Z * 0.99) 
            let w  = netCoverage * tmp * tmp * tmp * 1000.0  |>  clamp 0.01 30.0

            let accum = V4d(frag.Color.XYZ, netCoverage) * w

            let n = if frag.frontFacing then frag.N else frag.N * -1.0
            let vsN = //normal in view space
                uniform.ViewTrafo * V4d(n, 0.0) 
                |> Vec.xyz 
                |> Vec.normalize
            
            let etaRatio = 1.0/uniform.indexOfRefraction
            let delta =  
                if etaRatio = 1.0 then
                    V2d(0.0)
                else
                    let refractionDistance = uniform.refractionDistance * refractionDistanceSampler.Sample(frag.tc).X //|> max  0.001
                    coverage * 8.0 * computeRefactionOffset refractionDistance vsN vsPos etaRatio

            return { Modulate = modulate; Accum = accum; Delta = delta}
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

    let deltaSampler =
        sampler2d {
            texture uniform?Delta
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let composeShader (frag : Vertex) =
        fragment {
            let modulation = modulateSampler.Sample(frag.tc).XYZ
            let color = 
                if min3 modulation > 0.99999 then //no transparency at this pixel
                    backgroundSampler.Sample(frag.tc).XYZ
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

                    let delta = deltaSampler.Sample(frag.tc).XY * 2.0 / 8.0 //the original algorithem had 3.0/8.0 but I dont know why and it looks better this way
                    let background =  backgroundSampler.Sample(frag.tc + delta).XYZ
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
        (ambientOcclusion : aval<IBackendTexture>)
        (diffuseIrradianceMap : aval<IBackendTexture>)
        (prefilterdSpecColor : aval<IBackendTexture>)
        (bRDFLtu : aval<IBackendTexture>)
        sssProfiles
        (tanFoVxHalf : aval<float>)
        (aspectRatio : aval<float>)
        (depthBuffer : aval<IFramebufferOutput>)
        (gBuffer : Map<Symbol,IAdaptiveResource<IBackendTexture>>) //only to force dependency on the gBuffer
        =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.DepthStencil, TextureFormat.DepthComponent24
                (Sym.ofString "Accum") , TextureFormat.Rgba32f
                (Sym.ofString "ModulateColor") , TextureFormat.Rgba32f
                (Sym.ofString "Delta") , TextureFormat.Rg32f
            ]

        let ModulateBlend =
            { BlendMode.Blend with
                SourceColorFactor = BlendFactor.Zero
                SourceAlphaFactor = BlendFactor.Zero
                DestinationColorFactor = BlendFactor.InvSourceColor
                DestinationAlphaFactor = BlendFactor.SourceAlpha
            }
        
        let clearValues =
            clear {
                colors [//clear colors
                (Sym.ofString "Accum"), C4f.Zero
                (Sym.ofString "ModulateColor"),C4f.White
                (Sym.ofString "Delta"), C4f.Zero
                ]
            }

        objects
        |> sceneObject.objectsTrimByMaterial (fun _  (a : AdaptivePBRMaterial) -> AVal.map2 (fun c t -> c > 0.99999 && t < 0.00001) a.coverage.factor a.transmission.factor)
        |> Sg.depthWrite' false
        //|> Sg.cullMode' CullMode.Front
        |> Sg.blendModes' (Map.ofList [
            (Sym.ofString "Accum"), BlendMode.Add
            (Sym.ofString "ModulateColor"), ModulateBlend
            (Sym.ofString "Delta"), BlendMode.Add
        ])        
        |> Shadow.shadowMapsUniform shadowMaps
        |> Sg.cullMode (AVal.constant CullMode.None)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            //do! displacemntMap.displacementMap
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
        |> Sg.uniform "tanFoVxHalf" tanFoVxHalf        
        |> Sg.uniform "aspectRatio" aspectRatio        
        |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))        
        |> Sg.texture (Sym.ofString "AmbientOcclusion") ambientOcclusion
        |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
        |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
        |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
        |> Sg.texture (Sym.ofString "Dummy") (Map.find DefaultSemantic.Colors gBuffer)//only to force dependency on the gBuffer
        |> Sg.compile runtime signature
        |> RenderTaskExtensions.renderSemanticsCustom
            (   //output
                Set.ofList [
                    (Sym.ofString "ModulateColor")
                    (Sym.ofString "Accum")
                    (Sym.ofString "Delta")
                ]
           ) 
           size
           clearValues
           (Map.ofList [// use preexisting depth attachment
               DefaultSemantic.DepthStencil, depthBuffer
           ])      

    let compose runtime outputSignature size (backgroundTexture : aval<IBackendTexture>) (accumTexture : aval<IBackendTexture>) (modulateTexture : aval<IBackendTexture>)  (deltaTexture : aval<IBackendTexture>)=
        Sg.fullScreenQuad
        |> Sg.adapter
        |> Sg.shader {
           do! composeShader 
            }    
        |> Sg.texture DefaultSemantic.Colors backgroundTexture 
        |> Sg.texture (Sym.ofString "Accum") accumTexture 
        |> Sg.texture (Sym.ofString "Modulate") modulateTexture 
        |> Sg.texture (Sym.ofString "Delta") deltaTexture 
        |> Sg.compile runtime outputSignature
        |> RenderTask.renderToColor size  
