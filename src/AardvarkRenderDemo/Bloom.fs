namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module bloomShader =
    open fshadeExt

    type Fragment = {
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
    }

    type UniformScope with
        member x.threshold  : float = x?threshold
        member x.horizontal : bool = x?horizontal
        member x.blurSize : int = x?blurSize
        member x.sigma : float = x?sigma

    let extractBrightColor (frag : Fragment) =
        fragment {
            let  threshold = uniform.threshold
            let luminanceVector = V3d(0.2126, 0.7152, 0.0722)
            let brightness = Vec.dot frag.c.XYZ luminanceVector
            let excesse = max 0.0 ( brightness - threshold)
            let bc = if excesse > 0.0 then frag.c * excesse else V4d.OOOI
            return bc
        }

    let inputImage =
        sampler2d {
            texture uniform?inputImage
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let  gaussianBlur (frag : Fragment) =
        fragment {
            // Incremental Gaussian Coefficent Calculation (See GPU Gems 3 pp. 877 - 889)
            let sigma = uniform.sigma
            let x = 1.0 / (sqrt(2.0 * Math.PI) * sigma)
            let y = Math.Exp(-0.5 / (sigma * sigma))
            let mutable incrementalGaussian =  V3d(x, y, y*y)

            let size = inputImage.Size
            let texoffset =  V2d(1.0/(float)size.X, 1.0/(float)size.Y)
            let mutable result = inputImage.Sample(frag.tc) * incrementalGaussian.X
            let mutable coefficientSum = incrementalGaussian.X
            incrementalGaussian <- V3d(incrementalGaussian.XY * incrementalGaussian.YZ, incrementalGaussian.Z);
            for i in 1 .. uniform.blurSize do
                if uniform.horizontal then
                    result <- result + inputImage.Sample(frag.tc + V2d(texoffset.X * (float  i),0.0) ) * incrementalGaussian.X
                    result <- result + inputImage.Sample(frag.tc - V2d(texoffset.X * (float  i),0.0) ) * incrementalGaussian.X
                else
                    result <- result + inputImage.Sample(frag.tc + V2d(0.0 ,texoffset.Y * (float  i)) ) * incrementalGaussian.X
                    result <- result + inputImage.Sample(frag.tc - V2d(0.0, texoffset.Y * (float  i)) ) * incrementalGaussian.X
                coefficientSum <- coefficientSum + 2.0 * incrementalGaussian.X
                incrementalGaussian <- V3d(incrementalGaussian.XY * incrementalGaussian.YZ, incrementalGaussian.Z);       
            return result/coefficientSum
        }

    let blend  (frag : Fragment) =
        fragment {
            let b = inputImage.Sample(frag.tc)
            return frag.c + b
        }

module bloom =

    let defaultBloom = {threshold = 1.0;  blurSize  = 3; sigma = 3.0}

    let bloom (runtime : IRuntime)  (size : aval<V2i>)  (inputTexture : aval<ITexture>)  (model : AdaptiveBloom)=

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
            ]

        let brightColor =  
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture DefaultSemantic.DiffuseColorTexture inputTexture
            |> Sg.uniform "threshold"  model.threshold
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
                do! bloomShader.extractBrightColor
                }    
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor size  


        let task h i = 
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture ( Sym.ofString "inputImage")  i
            |> Sg.uniform "horizontal" (AVal.constant h)
            |> Sg.uniform "blurSize"  model.blurSize
            |> Sg.uniform "sigma"  model.sigma
            |> Sg.shader {
                do! bloomShader.gaussianBlur
                }    
            |> Sg.compile runtime signature

        let r1 = task true brightColor |> RenderTask.renderToColor  size
        let r2 = task false r1 |> RenderTask.renderToColor  size

          
        Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture DefaultSemantic.DiffuseColorTexture inputTexture
            |> Sg.texture ( Sym.ofString "inputImage")  r2
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
                do! bloomShader.blend
                }    
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor size  

module BloomControl =
    open Aardvark.UI
    open Aardvark.UI.Primitives

    type Message =
        | SetThreshold of float
        | SetBlurSize of int
        | SetSigma of float

    let update (m : Bloom)  (msg : Message) = 
        match msg  with
        | SetThreshold t-> {m  with threshold = t}       
        | SetBlurSize s -> {m with blurSize = s}
        | SetSigma s -> {m with sigma = s}

    let view (m : AdaptiveBloom) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        Html.table [                        
            tr [] [ td [attribute "colspan" "2"] [text "Bloom"]]
            tr [] [ td [] [text "Threshold"]; 
                    td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.threshold SetThreshold]]
            tr [] [ td [] [text "Blur Size"]; 
                    td [style "width: 70%;"] [integerInput ""  0 15  SetBlurSize m.blurSize]]
            tr [] [ td [] [text "Sigma"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.1;  max = 10.0; step = 0.0001} [] m.sigma SetSigma ]]
        ]   