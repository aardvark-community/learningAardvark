namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module fxaaShader =
    open fshadeExt

    type Fragment = {
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<Semantic("TexCoordShifted")>] ts : V2d
    }

    type UniformScope with
        member x.fxaa_subpix_shift  : float = 1.0/4.0
        member x.fxaa_span_max : float = 8.0
        member x.fxaa_reduce_mul : float = 1.0/8.0
        member x.fxaa_reduce_min : float = 1.0/128.0
        member x.on : bool  = true//x?on
        member x.fxaa_threshold : float  = 0.5
    
    let inputImage =
        sampler2d {
            texture uniform?inputImage
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }
    
    let fxAAVert ( vert : Fragment)=
        vertex{
            let size = inputImage.Size
            let pxSize =  V2d(1.0/(float)size.X, 1.0/(float)size.Y)
            //tc shifted 0.5 * one pixel + subpix_shift
            let ts = vert.tc - pxSize  * (0.5 + uniform.fxaa_subpix_shift)
            return {vert with ts = ts}
        }

    let fxAA (frag : Fragment) =
        fragment {
            let c  =
                if uniform.on then
                    let size = inputImage.Size
                    let pxSize =  V2d(1.0/(float)size.X, 1.0/(float)size.Y)

                    //sample center and offsets
                    let rgbM = inputImage.Sample(frag.tc).XYZ
                    let rgbNW = inputImage.Sample(frag.ts).XYZ
                    let rgbNE = inputImage.SampleOffset(frag.ts,V2i(1,0)).XYZ
                    let rgbSW = inputImage.SampleOffset(frag.ts,V2i(0,1)).XYZ
                    let rgbSE = inputImage.SampleOffset(frag.ts,V2i(1,1)).XYZ

                    //transform to luminace
                    let luminanceVector = V3d(0.2126, 0.7152, 0.0722)
                    let lumM = Vec.dot rgbM luminanceVector
                    let lumNW = Vec.dot rgbNW luminanceVector
                    let lumNE = Vec.dot rgbNE luminanceVector
                    let lumSW = Vec.dot rgbSW luminanceVector
                    let lumSE = Vec.dot rgbSE luminanceVector

                    //get minimum and maximum luminace
                    let lumMin = lumM |> min lumNW |> min lumNE |> min lumSW |> min lumSE
                    let lumMax = lumM |> max lumNW |> max lumNE |> max lumSW |> max lumSE

                    //if contrast is lower than threshold, no AA
                    if lumMax - lumMin <= lumMax * uniform.fxaa_threshold then
                        rgbM
                    else

                        // Sampling is done along the gradient.
                        let dir0 = V2d(-((lumNW + lumNE) - (lumSW + lumSE)), ((lumNW + lumSW) - (lumNE + lumSE)))
                        //reduce smapling direction in bright areas, but keep minimum
                        let dirReduce = (lumNW + lumNE + lumSW + lumSE) * 0.25 * uniform.fxaa_reduce_mul |> max uniform.fxaa_reduce_min
                        //norm distance ot 1 and apply reduction for bright areas
                        let dirMin = 1.0 / ((min (abs dir0.X) (abs dir0.Y)) + dirReduce)
                        //clamp to maximum span and norm  to pixel size
                        let dir = dir0 * dirMin |> max (V2d(-uniform.fxaa_span_max, -uniform.fxaa_span_max)) |> min (V2d(uniform.fxaa_span_max, uniform.fxaa_span_max)) |> (*) pxSize

                        //inner samples
                        let rgb2Tab  = (inputImage.Sample(frag.tc + dir * (1.0/3.0 - 0.5)).XYZ 
                                      + inputImage.Sample(frag.tc + dir * (2.0/3.0 - 0.5)).XYZ) / 2.0
                        //outer samples
                        let rgb4Tab  = rgb2Tab / 2.0 + (inputImage.Sample(frag.tc + dir * (0.0/3.0 - 0.5)).XYZ 
                                                      + inputImage.Sample(frag.tc + dir * (3.0/3.0 - 0.5)).XYZ) / 4.0
                        let lumB = Vec.dot rgb4Tab luminanceVector  
                        //if outer sample are  out  of range use only inner samples
                        if (lumB < lumMin) || (lumB > lumMax) then rgb2Tab else rgb4Tab
                else
                    inputImage.Sample(frag.tc).XYZ
            return {frag with c = V4d(c,1.0)}
        }

module fxAA =

    let fxAA (runtime : IRuntime)  (size : aval<V2i>) (outputSignature : IFramebufferSignature) (inputTexture : aval<ITexture>) (on :aval<bool>) =
         
        Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture ( Sym.ofString "inputImage") inputTexture
            |> Sg.uniform "on" on
            |> Sg.shader {
                do! fxaaShader.fxAAVert
                do! fxaaShader.fxAA
                }    
            |> Sg.compile runtime outputSignature
            |> RenderTask.renderToColor size 