namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering.Effects
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module subSurfaceShader =
   open fshadeExt


    type UniformScope with
        member x.Samples : int = 25
        member x.horizontal : bool = x?horizontal
        member x.sssWidth : float = x?sssWidth
        member x.camFoVy : float = x?camFoVy
        member x.kernelRange : float = 3.0
        member x.kernel : Arr<N<25>, V4d> = uniform?kernel


    let inputImage =
        sampler2d {
            texture uniform?inputImage
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

    [<ReflectedDefinition>]
    let getLinearDepth (ndc : V2d) =
        let tc = 0.5 * (ndc + V2d.II)
        let z = 2.0 * depth.Sample(tc, 0.0).X - 1.0

        let pp = V4d(ndc.X, ndc.Y, z, 1.0) 
        let temp = uniform.ProjTrafoInv * pp
        temp.Z / temp.W

    let  ssssBlur (v : Vertex) =   
        fragment {
            let ndc = v.pos.XY / v.pos.W
            let sampleM = inputImage.Sample(v.tc).XYZ
            let deepM = getLinearDepth ndc

            // Calculate the sssWidth scale (1.0 for a unit plane sitting on the
            // projection window):
            let distanceToProjectionWindow = 1.0 / tan (0.5 * uniform.camFoVy)
            let scale = distanceToProjectionWindow / deepM

            let dir = if uniform.horizontal then V2d.OI else  V2d.IO
            let step = uniform.sssWidth * scale  * dir * 0.7 / uniform.kernelRange

            let mutable blurred = V3d.OOO
            let mutable sumWeights = V3d.OOO

            let samples = clamp 1 64 uniform.Samples
            for si in 0 .. samples - 1 do
                let offset = uniform.kernel.[si].W * step
                let samplePos  = v.tc  + offset
                let sampleColor = inputImage.Sample(samplePos).XYZ

                let weight = uniform.kernel.[si].XYZ

                blurred <- blurred + sampleColor * weight
                sumWeights <- sumWeights + weight

            blurred <- blurred / sumWeights

            return V4d(blurred,1.0)
        } 

module subSurface =

    let gaussian (variance : float) (r : float) (falloff : V3d) =
        let rr = r / (falloff + 0.001)
        exp -(rr*rr) / (2.0  * variance) / (2.0 * Constant.Pi * variance)
    
    let profile (r : float) (falloff : V3d)= 
        0.100 * gaussian 0.0484 r falloff +
        0.118 * gaussian 0.187  r falloff +
        0.113 * gaussian 0.567  r falloff +
        0.358 * gaussian 1.99   r falloff +
        0.078 * gaussian 7.41   r falloff   

    let kernel (samples : int) (strength : V3d) (falloff : V3d) =
        let range  = 2.0
        let exponent = 2.0

        let step = range / (float(samples) - 1.0)


        let calcOffset i = 
            let o  = -range + float(i) * step
            let s = if o <  0.0 then -1.0 else 1.0
            range * s * (abs (pow o exponent)) / (pow range exponent)

        let offsets = Array.init samples calcOffset

        let calcWeights i =
            let w0 = if i < 0 then abs (offsets.[i] - offsets.[i-1]) else  0.0
            let w1 = if i < samples - 1 then abs (offsets.[i] - offsets.[i+1]) else  0.0
            let area = (w0+w1) / 2.0
            let weight = area * profile (offsets.[i]) falloff
            V4d(weight, offsets.[i])

        let weights = Array.init samples calcWeights

        //reorder so that the element with offset 0 comes first
        let reorder i =
            let i' = if  i = 0 then samples/2 else if  i <= samples/2 then  i-1 else i
            weights.[i']

        let weights' = Array.init samples reorder

        let sumWeights = Array.sumBy (fun (v : V4d) -> v.XYZ) weights'
        let normalized = Array.map (fun (v : V4d) -> V4d(v.XYZ / sumWeights,v.W)) weights'

        let applayStrength i (v : V4d) =
            let weight =
                if i = 0 then 
                    (V3d.III - strength) + strength * v.XYZ
                else
                    strength * v.XYZ
            V4d(weight,v.W)

        Array.mapi applayStrength normalized

    //Render-Task for the screen-space Abient Occlusion pass
    let makeSubSurfaceScatttering (runtime : IRuntime) (size : aval<V2i>) camFoVy view proj gBuffer input=

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
            ]

        let strength = V3d(0.48, 0.41, 0.28)
        let falloff = V3d(1.0, 0.37, 0.3)
        let kernel = kernel 25 strength falloff
        
        let blurr h i = 
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.blendMode (AVal.constant (BlendMode(false)))
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj
            |> Sg.texture ( Sym.ofString "inputImage")  i
            |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
            |> Sg.uniform "horizontal" (AVal.constant h)
            |> Sg.uniform "sssWidth"  (AVal.constant 0.005)
            |> Sg.uniform "camFoVy" camFoVy
            |> Sg.uniform "kernel" (AVal.constant kernel)
            |> Sg.shader {
                do! subSurfaceShader.ssssBlur
                }    
            |> Sg.compile runtime signature

        let r1 = blurr true input |> RenderTask.renderToColor  size
        let r2 = blurr false r1 |> RenderTask.renderToColor  size

        r2