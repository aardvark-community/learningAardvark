namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Semantics

module bloomShader =
    open fshadeExt

    type Fragment = {
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
    }

    let extractBrightColor (frag : Fragment) =
        fragment {
            let  treshold = 1.0
            let luminanceVector = V3d(0.2126, 0.7152, 0.0722)
            let brightness = Vec.dot frag.c.XYZ luminanceVector
            let excesse = max 0.0 ( brightness - treshold)
            let bc = if excesse > 0.0 then frag.c * excesse else V4d.OOOI
            return bc
        }

    type UniformScope with
        member x.horizontal : bool = x?horizontal
        member x.weights  : Arr<N<5>, float> = Arr<N<5>, float>([|0.227027; 0.1945946; 0.1216216; 0.054054; 0.016216|])

    let inputImage =
        sampler2d {
            texture uniform?inputImage
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let  gaussianBlur (frag : Fragment) =
        fragment {
            let size = inputImage.Size
            let texoffset =  V2d(1.0/(float)size.X, 1.0/(float)size.Y)
            let mutable result = inputImage.Sample(frag.tc) * uniform.weights.[0]
            if uniform.horizontal then
                for i in 1 .. 4 do
                    result <- result + inputImage.Sample(frag.tc + V2d(texoffset.X * (float  i),0.0) ) * uniform.weights.[i]
                    result <- result + inputImage.Sample(frag.tc - V2d(texoffset.X * (float  i),0.0) ) * uniform.weights.[i]
            else
                for i in 1 .. 4 do
                    result <- result + inputImage.Sample(frag.tc + V2d(0.0 ,texoffset.Y * (float  i)) ) * uniform.weights.[i]
                    result <- result + inputImage.Sample(frag.tc - V2d(0.0, texoffset.Y * (float  i)) ) * uniform.weights.[i]       
            return result
        }

    let blend  (frag : Fragment) =
        fragment {
            let b = inputImage.Sample(frag.tc)
            return frag.c + b
        }

module bloom =
    let bloom (runtime : IRuntime)  (size : aval<V2i>)  (inputTexture : aval<ITexture>) =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
            ]

        let brightColor =  
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture DefaultSemantic.DiffuseColorTexture inputTexture
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
            |> Sg.shader {
                do! bloomShader.gaussianBlur
                }    
            |> Sg.compile runtime signature

        let r1 = task true brightColor |> RenderTask.renderToColor  size
        let r2 = task false r1 |> RenderTask.renderToColor  size
        let r3 = task true r2 |> RenderTask.renderToColor  size
        let r4 = task false r3 |> RenderTask.renderToColor  size
        (*let r5 = task true r4 |> RenderTask.renderToColor  size
        let r6 = task false r5 |> RenderTask.renderToColor  size
        let r7 = task true r6 |> RenderTask.renderToColor  size
        let r8 = task false r7 |> RenderTask.renderToColor  size
        let r9 = task true r8 |> RenderTask.renderToColor  size
        let r10 = task false r9 |> RenderTask.renderToColor  size*)

          
        Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture DefaultSemantic.DiffuseColorTexture inputTexture
            |> Sg.texture ( Sym.ofString "inputImage")  r4
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
                do! bloomShader.blend
                }    
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor size  

 

