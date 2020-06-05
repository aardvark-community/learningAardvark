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
        member x.fxaa_subpix_shift  : float = x?fxaa_subpix_shift
        member x.fxaa_span_max : float = x?fxaa_span_max
        member x.fxaa_reduce_mul : float = x?fxaa_reduce_mul
        member x.fxaa_reduce_min : float = x?fxaa_reduce_min
        member x.fxaa_threshold : float = x?fxaa_threshold
    

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
                    //reduce smapling direction in bright areas
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
            return {frag with c = V4d(c,1.0)}
        }

module fxAA =
    open Aardvark.UI
    open Aardvark.UI.Primitives

    let defaultfxAA =
        {   
            on = true
            threshold = 0.5
            subpix_shift = 0.25
            span_max = 8.0
            reduce_mul = 0.125
            reduce_min = 1.0/128.0
        }

    let fxAA (runtime : IRuntime)  (size : aval<V2i>) (outputSignature : IFramebufferSignature) (inputTexture : aval<ITexture>) (m : AdaptivefxAA) =
         
        Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture ( Sym.ofString "inputImage") inputTexture
            |> Sg.uniform "fxaa_subpix_shift" m.subpix_shift
            |> Sg.uniform "fxaa_span_max" m.span_max
            |> Sg.uniform "fxaa_reduce_mul" m.reduce_mul
            |> Sg.uniform "fxaa_reduce_min" m.reduce_min
            |> Sg.uniform "fxaa_threshold" m.threshold
            |> Sg.shader {
                do! fxaaShader.fxAAVert
                do! fxaaShader.fxAA
                }    
            |> Sg.compile runtime outputSignature
            |> RenderTask.renderToColor size 

    type Message =
        | SetThreshold of float
        | ToggleOn
        | SetSubpixShift of float
        | SetSpanMax of float
        | SetReduceMult of float
        | SetReduceMin of float

    let update (m : fxAA)  (msg : Message) = 
        match msg  with
        | SetThreshold t-> {m  with threshold = t}       
        | ToggleOn -> {m with on = not m.on}
        | SetSubpixShift s -> {m with subpix_shift = s}
        | SetSpanMax s -> {m with span_max = s}
        | SetReduceMult s -> {m with reduce_mul = s}
        | SetReduceMin s -> {m with reduce_min = s}

    let view (m : AdaptivefxAA) =
        Html.table [                        
            tr [] [ td [attribute "colspan" "2"] [text "fxAA Antialiasing"]]
            tr [] [ td [] [text "Active"]; 
                    td [style "width: 70%;"] [Html.SemUi.toggleBox m.on ToggleOn]]
            tr [] [ td [] [text "Contrast Threshold"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.01;  max = 1.0; step = 0.01} [] m.threshold SetThreshold]]
            tr [] [ td [] [text "Subpixel Shift"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.subpix_shift SetSubpixShift]]
            tr [] [ td [] [text "Maximum Sample Distance"]; 
                    td [style "width: 70%;"] [inputSlider {min = 1.0;  max = 20.0; step = 1.0} [] m.span_max SetSpanMax ]]
            tr [] [ td [] [text "Reduction on bright areas"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.reduce_mul SetReduceMult]]
            tr [] [ td [] [text "Minimum Reduction"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.reduce_min SetReduceMin]]
         ]   
    