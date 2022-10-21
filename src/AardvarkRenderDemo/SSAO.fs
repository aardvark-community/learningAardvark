namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Rendering
open SLEAardvarkRenderDemo.Model
open FShade
open Aardvark.Rendering.Effects

 module SSAO =
    //Screen Space Ambinet Occlusion

    [<GLSLIntrinsic("smoothstep({0}, {1}, {2})")>]
    let smoothStep (edge0 : float) (edge1 : float) (x : float) : 'a =
        onlyInShaderCode "smooth"

    let normal =
        sampler2d {
            texture uniform?Normals
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let depth =
        sampler2d {
            texture uniform?DepthStencil
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let depthCmp =
        sampler2dShadow {
            texture uniform?DepthStencil
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            comparison ComparisonFunction.Greater
            filter Filter.MinMagMipLinear
        }

    let random =
        sampler2d {
            texture uniform?Random
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagPoint
        }
    
    let ambientOcc =
        sampler2d {
            texture uniform?AmbientOcclusion
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    type UniformScope with
        member x.Radius : float = uniform?Radius
        member x.Threshold : float = uniform?Threshold
        member x.Sigma : float = uniform?Sigma
        member x.Sharpness : float = uniform?Sharpness
        member x.Samples : int = uniform?Samples
        member x.OcclusionStrength : float = uniform?OcclusionStrength
        member x.SampleDirections : Arr<N<128>, V3d> = uniform?SampleDirections

    [<ReflectedDefinition>]
    let getLinearDepth ndc = linearDepth.getLinearDepth  uniform.ProjTrafoInv depth ndc

    [<ReflectedDefinition>]
    let project (vp : V3d) =
        let mutable vp = vp
        vp.Z <- min -0.01 vp.Z
        let pp = uniform.ProjTrafo * V4d(vp, 1.0)
        pp.XYZ / pp.W
        
    let ambientOcclusion (v : Vertex) =
        fragment {
            let ndc = v.pos.XY / v.pos.W
            let wn = normal.Sample(v.tc).XYZ.Normalized
            let z0 = depth.Sample(v.tc).X
            let z = 2.0 * z0 - 1.0
            let pp0 = V4d(ndc.X, ndc.Y, z, 1.0) //point in screen space

            let vp =  // point in view space
                let temp = uniform.ProjTrafoInv * pp0
                temp.XYZ / temp.W

            let vn = //normal in view space
                uniform.ViewTrafo * V4d(wn, 0.0) 
                |> Vec.xyz 
                |> Vec.normalize
           
            //randomized sample direction
            let x = random.Sample(pp0.XY).XYZ |> Vec.normalize
            let z = vn
            let y = Vec.cross z x |> Vec.normalize
            let x = Vec.cross y z |> Vec.normalize  
                
            let mutable occlusion = 0.0
            let samples = clamp 1 64 uniform.Samples
            for si in 0 .. samples - 1 do

                let dir = uniform.SampleDirections.[si] * uniform.Radius
                //bias sampling towards point near the center of the hemispehere
                let temp  = (float (si)) / (float (samples-1))
                let scale = Fun.Lerp(0.1, 1.0, (temp * temp))
                let dirscaled = dir * scale
                let p = vp + x * dirscaled.X + y * dirscaled.Y + z * dirscaled.Z
          
                let f = 1.0 - uniform.Threshold / -p.Z
                let ppo = 0.5 * (project (p * f) + V3d.III)
                let pp = 0.5 * (project p + V3d.III)
                let sampleDepth = depth.Sample(pp.XY).X
                let occ = if sampleDepth > pp.Z then 0.0  else 1.0
                if depthCmp.Sample(pp.XY, ppo.Z) < 0.5 then
                    occlusion <- occlusion + occ 
                
            let occlusion = occlusion / float samples * uniform.OcclusionStrength |> min 1.0
            let ambient = 1.0 - occlusion
            
            return V4d(ambient, ambient, ambient, 1.0)
        }

    [<ReflectedDefinition>]
    let getAmbient (ndc : V2d) =
        let tc = 0.5 * (ndc + V2d.II)
        ambientOcc.SampleLevel(tc, 0.0)

    let blur (v : Vertex) =
        fragment {
            let s = 2.0 / V2d ambientOcc.Size
            let ndc = v.pos.XY / v.pos.W
            

            let sigmaPos = uniform.Sigma
            if sigmaPos <= 0.0 then
                return getAmbient ndc
            else
                let sigmaPos2 = sigmaPos * sigmaPos
                let sharpness = uniform.Sharpness
                let sharpness2 = sharpness * sharpness
                let r = 4
                let d0 = getLinearDepth ndc
                let mutable sum = V4d.Zero
                let mutable wsum = 0.0
                for x in -r .. r do
                    for y in -r .. r do
                        let deltaPos = V2d(x,y) * s
                        let pos = ndc + deltaPos

                        let deltaDepth = getLinearDepth pos - d0
                        let value = getAmbient pos

                        let wp = exp (-V2d(x,y).LengthSquared / sigmaPos2)
                        let wd = exp (-deltaDepth*deltaDepth * sharpness2)

                        let w = wp * wd

                        sum <- sum + w * value
                        wsum <- wsum + w



                return sum / wsum
        }

    //texture  with random values used in the AO shaders
    let randomTex ( runtime : IRuntime) = 
        let img = PixImage<float32>(Col.Format.RGB, V2i.II * 512)

        let rand = RandomSystem()
        img.GetMatrix<C3f>().SetByCoord (fun _ ->
            rand.UniformV3dDirection().ToC3d().ToC3f()
        ) |> ignore

        runtime.PrepareTexture(PixTexture2d(PixImageMipMap [| img :> PixImage |], TextureParams.empty))

    //Render-Task for the screen-space Abient Occlusion pass
    let makeAmbientOcclusion ( runtime : IRuntime) (size : aval<V2i>) view proj (gBuffer : Map<Symbol,IAdaptiveResource<IBackendTexture>>) (settings : AdaptiveAmbientOcclusionSettings)=

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
            ]

        let aoSize = 
             AVal.custom (fun t ->
                let s = size.GetValue t
                let d = settings.scale.GetValue t
                V2i(
                    max 1 (int (float s.X * d)),
                    max 1 (int (float s.Y * d))
                )
            )

        let sampleDirections =
            let rand = RandomSystem()
            let arr = 
                Array.init 128 (fun _ ->
                    let phi = rand.UniformDouble() * Constant.PiTimesTwo
                    let theta = rand.UniformDouble() * (Constant.PiHalf - 20.0 * Constant.RadiansPerDegree)
                    V3d(
                        cos phi * sin theta,
                        sin phi * sin theta,
                        cos theta
                    )
                )
            arr |> Array.map (fun v -> v * rand.UniformDouble())
            |> AVal.constant

        let ambientOc = 
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.shader {
                do! ambientOcclusion
            }
            |> Sg.texture ( DefaultSemantic.Normals) (Map.find shaderCommon.Semantic.NormalR gBuffer)
            |> Sg.texture ( DefaultSemantic.DepthStencil) (Map.find DefaultSemantic.DepthStencil gBuffer)
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj
            |> Sg.uniform "Random" (AVal.constant (randomTex runtime :> ITexture))
            |> Sg.uniform "SampleDirections" sampleDirections
            |> Sg.uniform "Radius" settings.radius
            |> Sg.uniform "Threshold" settings.threshold
            |> Sg.uniform "Samples" settings.samples
            |> Sg.uniform "OcclusionStrength" settings.occlusionStrength
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor aoSize

        let blurredAmbientOc =
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.shader {
                do! blur
            }
            |> Sg.texture ( DefaultSemantic.DepthStencil) (Map.find DefaultSemantic.DepthStencil gBuffer)
            |> Sg.texture (Sym.ofString "AmbientOcclusion") ambientOc
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj
            |> Sg.uniform "Radius" settings.radius
            |> Sg.uniform "Threshold" settings.threshold
            |> Sg.uniform "Sigma" settings.sigma
            |> Sg.uniform "Sharpness" settings.sharpness
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor aoSize

        blurredAmbientOc