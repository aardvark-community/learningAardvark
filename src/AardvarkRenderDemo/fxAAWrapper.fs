namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model
open Aardvark.SceneGraph
open FShade
open HiliteShaders.FXAA
(*

*)
module fxAA = 

    type UniformScope with
        // Only used on FXAA Quality.
        // This used to be the FXAA_QUALITY__EDGE_THRESHOLD define.
        // It is here now to allow easier tuning.
        // The minimum amount of local contrast required to apply algorithm.
        //   0.333 - too little (faster)
        //   0.250 - low quality
        //   0.166 - default
        //   0.125 - high quality 
        //   0.063 - overkill (slower)
        member x.EdgeThreshold     : float = x?EdgeThreshold

        // Only used on FXAA Quality.
        // This used to be the FXAA_QUALITY__EDGE_THRESHOLD_MIN define.
        // It is here now to allow easier tuning.
        // Trims the algorithm from processing darks.
        //   0.0833 - upper limit (default, the start of visible unfiltered edges)
        //   0.0625 - high quality (faster)
        //   0.0312 - visible limit (slower)
        // Special notes when using FXAA_GREEN_AS_LUMA,
        //   Likely want to set this to zero.
        //   As colors that are mostly not-green
        //   will appear very dark in the green channel!
        //   Tune by looking at mostly non-green content,
        //   then start at zero and increase until aliasing is a problem.
        member x.EdgeThresholdMin  : float = x?EdgeThresholdMin         

        // Only used on FXAA Quality.
        // This used to be the FXAA_QUALITY__SUBPIX define.
        // It is here now to allow easier tuning.
        // Choose the amount of sub-pixel aliasing removal.
        // This can effect sharpness.
        //   1.00 - upper limit (softer)
        //   0.75 - default amount of filtering
        //   0.50 - lower limit (sharper, less sub-pixel aliasing removal)
        //   0.25 - almost off
        //   0.00 - completely off
        member x.Subpix            : float = x?Subpix 
        member x.FxAAPreset : int =  x?FxAAPreset

    let fxAAShader (v : Vertex) =
            fragment {
                let i = uniform.FxAAPreset
                let c = //workaround because for some reason we can not have an expression or uniform with the PRESET enum as return type. Its ugly but works 
                    match i with
                    | 0 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither10, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 1 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither11, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)  
                    | 2 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither12, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 3 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither13, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 4 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither14, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 5 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither15, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 6 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality20, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix) 
                    | 7 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality21, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 8 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality22, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 9 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality23, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 10 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality24, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 11 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality25, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 12 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality26, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 13 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality27, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 14 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality28, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 15 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Quality29, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    | 16 -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Extreme39, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                    |_ -> diffuseSampler.SampleLevelFXAA(v.tc, 0.0, PRESET.Dither10, uniform.EdgeThreshold, uniform.EdgeThresholdMin, uniform.Subpix)
                return c
            }
    
    let fxAA (runtime : IRuntime) outputSignature (inputTexture : aval<ITexture>) (m : AdaptivefxAA)=
        Sg.fullScreenQuad
        |> Sg.adapter
        |> Sg.texture DefaultSemantic.DiffuseColorTexture inputTexture
        |> Sg.uniform "EdgeThreshold" m.threshold
        |> Sg.uniform "EdgeThresholdMin" m.thresholdMin
        |> Sg.uniform "Subpix" m.subpix
        |> Sg.uniform "FxAAPreset" (AVal.map (int) m.preset)
        |> Sg.shader {
            do! DefaultSurfaces.diffuseTexture
            do! fxAAShader 
            }    
        |> Sg.compile runtime outputSignature   

    let defaultFxAA = {threshold = 0.166; thresholdMin  = 0.0833; subpix = 0.75; preset  = PRESET.Quality25}

    type Message =
        | SetThreshold of float
        | SetThresholdMin of float
        | SetSubpix of float
        | SetPreset of PRESET

    let update (m : fxAA)  (msg : Message) = 
        match msg  with
        | SetThreshold t-> {m  with threshold = t}   
        | SetThresholdMin s -> {m  with thresholdMin = s} 
        | SetSubpix s -> {m  with subpix = s} 
        | SetPreset s -> {m  with preset = s} 

    let view (m : AdaptivefxAA) =
        Html.table [                        
            tr [] [ td [attribute "colspan" "2"] [text "fxAA Antialiasing"]]
            tr [] [ td [] [text "Edge Threshold"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.001;  max = 0.5; step = 0.01} [] m.threshold SetThreshold]]
            tr [] [ td [] [text "Edge Threshold Min"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 0.1; step = 0.001} [] m.thresholdMin SetThresholdMin]]
            tr [] [ td [] [text "Subpixel"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.subpix SetSubpix ]]
            tr [] [ td [] [text "Quality Preset"]; 
                    td [style "width: 70%;"] [Html.SemUi.dropDown m.preset SetPreset]]
         ]   