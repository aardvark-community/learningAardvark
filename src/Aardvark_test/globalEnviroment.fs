namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark_test.Model
open System.IO
open Aardvark.SceneGraph
(*
    UI View to set up the global enviroment
    Functions tu render the sky box and the pre renderd maps for the global ambient light
*)
module globalEnviroment = 

    open Aardvark.UI
    open Aardvark.UI.Primitives

    type Message =
        | SetSkyMap of String
        | SetSkyMapRotation of float
        | SetAbientLightIntensity of float
        | SetSkyMapIntensity of float
        | SetAbientOcclusionStrength of float
        | SetAbientOcclusionRadius of float
        | SetAbientOcclusionSamples of int
        | SetAbientOcclusionScale of float
        | SetAbientOcclusionThreshold of float
        | SetAbientOcclusionSigma of float
        | SetAbientOcclusionSharpness of float

    let update (m : GlobalEnviorment)  (msg : Message) = 
        match msg  with
        | SetSkyMap s -> {m  with skyMap = s}
        | SetSkyMapRotation r -> {m  with skyMapRotation = r}
        | SetAbientLightIntensity i-> {m  with ambientLightIntensity = i}       
        | SetSkyMapIntensity i-> {m  with skyMapIntensity = i}       
        | SetAbientOcclusionStrength s -> {m with occlusionSettings = {m.occlusionSettings with occlusionStrength = s}}
        | SetAbientOcclusionRadius s -> {m with occlusionSettings = {m.occlusionSettings with radius = s}}
        | SetAbientOcclusionSamples s -> {m with occlusionSettings = {m.occlusionSettings with samples = s}}
        | SetAbientOcclusionScale s -> {m with occlusionSettings = {m.occlusionSettings with scale = s}}
        | SetAbientOcclusionThreshold s -> {m with occlusionSettings = {m.occlusionSettings with threshold = s}}
        | SetAbientOcclusionSigma s -> {m with occlusionSettings = {m.occlusionSettings with sigma = s}}
        | SetAbientOcclusionSharpness s -> {m with occlusionSettings = {m.occlusionSettings with sharpness = s}}

    let view (m : MGlobalEnviorment) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        let path = Path.GetDirectoryName(Mod.force m.skyMap)
        Html.table [                        
            tr [] [ td [] [text "sky map"]; td [] [openDialogButton 
                    { OpenDialogConfig.file with allowMultiple = false; title = "Open sky map hdr"; filters  = [|"*.hdr"|];  startPath = path}
                    [ clazz "ui green button"; onChooseFile SetSkyMap ] 
                    [ text "Open hdr File" ]]]
            tr [] [ td [] [text "Sky Map Intensity"]; 
                    td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.skyMapIntensity SetSkyMapIntensity]]
            tr [] [ td [] [text "Sky Map Rotation"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] (Mod.map (fun r -> r/(2.0*Math.PI)) m.skyMapRotation)  (fun r -> SetSkyMapRotation (r*2.0*Math.PI)) ]]
            tr [] [ td [] [text "Ambient Light Intensity"]; 
                    td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.ambientLightIntensity SetAbientLightIntensity]]
            tr [] [ td [attribute "colspan" "2";style "text-align: center;"] [text "Abient Occlusion"]]
            tr [] [ td [] [text "Strength"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 2.0; step = 0.01} [] m.occlusionSettings.occlusionStrength SetAbientOcclusionStrength ]]
            tr [] [ td [] [text "Radius"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.01;  max = 0.5; step = 0.01} [] m.occlusionSettings.radius SetAbientOcclusionRadius ]]
            tr [] [ td [] [text "Scale"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.1;  max = 1.0; step = 0.01} [] m.occlusionSettings.scale SetAbientOcclusionScale ]]
            tr [] [ td [] [text "Samples"]; 
                    td [style "width: 70%;"] [integerInput ""  8 512  SetAbientOcclusionSamples m.occlusionSettings.samples]]
            tr [] [ td [] [text "Threshold"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.01;  max = 0.5; step = 0.0001} [] m.occlusionSettings.threshold SetAbientOcclusionThreshold ]]
            tr [] [ td [] [text "Sigma"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.1;  max = 15.0; step = 0.01} [] m.occlusionSettings.sigma SetAbientOcclusionSigma]]
            tr [] [ td [] [text "Sharpness"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.1;  max = 2.0; step = 0.01} [] m.occlusionSettings.sharpness SetAbientOcclusionSharpness]]
        ]   

// helper module to render to an texture cube
module CubeRenderTask =

    let renderToCubeTask runtime size signature source level face =
        let lookTo = 
            match face with
            |CubeSide.PositiveY  -> V3d.OIO
            |CubeSide.PositiveZ -> V3d.OOI
            |CubeSide.PositiveX -> V3d.IOO
            |CubeSide.NegativeZ-> V3d.OOI * -1.0
            |CubeSide.NegativeX -> V3d.IOO * -1.0
            |CubeSide.NegativeY -> V3d.OIO * -1.0
            |_ -> failwith "unexpected enum"
        let lookSky = 
            match face with
            |CubeSide.PositiveY  -> V3d.OOI * -1.0
            |CubeSide.PositiveZ -> V3d.OIO
            |CubeSide.PositiveX -> V3d.OIO
            |CubeSide.NegativeZ-> V3d.OIO
            |CubeSide.NegativeX -> V3d.OIO
            |CubeSide.NegativeY -> V3d.OOI
            |_ -> failwith "unexpected enum"

        source level
        |> Sg.viewTrafo (
            CameraView.lookAt V3d.OOO lookTo (lookSky * -1.0)
             |> CameraView.viewTrafo 
             |> Mod.constant
        )
        |> Sg.projTrafo (size |> Mod.map (fun actualSize -> 
                Frustum.perspective 90.0 0.01 1.0 1.0 |> Frustum.projTrafo
              )
           )
        |> Sg.compile runtime (signature runtime)

    let renderToCubeMip (runtime : IRuntime) size levels signature source=
        RenderTask.renderToColorCubeMip size levels (renderToCubeTask runtime size signature source)

    let renderToCube (runtime : IRuntime) size signature source=
        RenderTask.renderToColorCubeMip size 1 (renderToCubeTask  runtime size signature (fun _ ->  source))

//convert equirectenggular map to cube map for the sky box
module SkyBox =
    open CubeRenderTask
    open Aardvark.UI

    let signature (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgb16f; samples = 1 }
        ]

    let skyMapSize = 1024 |> Mod.init 

    let skyMapequirectengular  skyMap : ISg<'msg> -> ISg<'msg> = 
        let texture =  
            Mod.map (fun s -> FileTexture(s, { wantCompressed = false; wantMipMaps = false; wantSrgb = false }) :> ITexture) skyMap
        Sg.texture (Sym.ofString "SkyMapEquirec") texture 

    let skyBoxEquirec skyMap rotation=
        Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.uniform "SkyMapRotation" rotation
            |> skyMapequirectengular skyMap
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! PBR.skyTextureEquirec
            }

    let getTexture (runtime : IRuntime) skyMap rotation = 
        renderToCube runtime skyMapSize signature (skyBoxEquirec skyMap rotation)

//render the prefilterd maps for ambient diffuse and  specular lighning
module GlobalAmbientLight =
    open CubeRenderTask
    open Aardvark.UI

    let signature (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgb16f; samples = 1 }
        ]

    let diffuseIrradianceSize = 32 |> Mod.init 

    let diffuseIrradianceBox (runtime : IRuntime) skyBoxTexture =
        Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! PBR.convoluteDiffuseIrradiance
            }       

    let diffuseIrradianceMap (runtime : IRuntime) skyBoxTexture = 
        renderToCube runtime diffuseIrradianceSize signature (diffuseIrradianceBox runtime skyBoxTexture) 

    let prefilterSpecSize = 128 |> Mod.init 

    let prefilterSpecBox (runtime : IRuntime) skyBoxTexture (level : int) =
        Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
            |> Sg.uniform "Roughness" (Mod.constant (float level / 4.0) )
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! PBR.prefilterSpec
            } 

    let prefilterdSpecColor (runtime : IRuntime) skyBoxTexture = 
        renderToCubeMip runtime prefilterSpecSize 5 signature (prefilterSpecBox runtime skyBoxTexture) 

    let signatureBRDFLtu (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rg16f; samples = 1 }
        ]

    let LtuSize = V2i(512,512) |> Mod.init 

    let BRDFLtuTask (runtime : IRuntime)= 
        Sg.fullScreenQuad
        |>Sg.adapter
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! PBR.integrateBRDFLtu
        }
        |> Sg.viewTrafo (Trafo3d.Identity |> Mod.constant )
        |> Sg.projTrafo (Trafo3d.Identity |> Mod.constant )
        |> Sg.compile runtime (signatureBRDFLtu runtime)
    
    let BRDFLtu (runtime : IRuntime) =
        RenderTask.renderToColor LtuSize (BRDFLtuTask runtime)