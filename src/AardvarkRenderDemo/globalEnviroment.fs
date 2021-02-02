namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model
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
        | ToggleLightProbe 
        | SetLightProbePosition of V3dInput.Message

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
        | ToggleLightProbe -> {m with lightProbePosition = match m.lightProbePosition with Some _ -> None |None -> Some V3d.OOO}
        | SetLightProbePosition vMsg -> 
            match m.lightProbePosition with
            |Some p ->
                let n =  V3dInput.update (p) vMsg
                {m with lightProbePosition = Some  n}
            |None -> m

    let lightProbeView (p'' : aval<V3d option>) =
        alist{
           let! p' = p''
           match p' with
           |Some p ->                
                yield Html.table [ 
                    tr [] [ td [] [text "Light Probe"] 
                            td [style "width: 70%;"]  [button [clazz "ui button"; onClick (fun _ -> ToggleLightProbe); style "margin-bottom: 5px; width: 100%;" ]  [text "Disable"]]                      
                          ]
                ]
                yield p'' |> AVal.map (fun v -> v.Value) |> V3dInput.view "Light Probe Position"   |> UI.map SetLightProbePosition
            |None ->
                 yield Html.table [ 
                    tr [] [ td [] [text "Light Probe"] 
                            td [style "width: 70%;"]  [button [clazz "ui button"; onClick (fun _ -> ToggleLightProbe); style "margin-bottom: 5px; width: 100%;" ]  [text "Enable"]]                      
                          ]
                ]
        }
        |> Incremental.div AttributeMap.empty

    let view (m : AdaptiveGlobalEnviorment) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        let path = Path.GetDirectoryName(AVal.force m.skyMap)
        let x = m.lightProbePosition
        Html.table [                        
            tr [] [ td [] [text "sky map"]; td [] [openDialogButton 
                    { OpenDialogConfig.file with allowMultiple = false; title = "Open sky map hdr"; filters  = [|"*.hdr"|];  startPath = path}
                    [ clazz "ui green button"; onChooseFile SetSkyMap ] 
                    [ text "Open hdr File" ]]]
            tr [] [ td [] [text "Sky Map Intensity"]; 
                    td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.skyMapIntensity SetSkyMapIntensity]]
            tr [] [ td [] [text "Sky Map Rotation"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] (AVal.map (fun r -> r/(2.0*Math.PI)) m.skyMapRotation)  (fun r -> SetSkyMapRotation (r*2.0*Math.PI)) ]]
            tr [] [ td [] [text "Ambient Light Intensity"]; 
                    td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.ambientLightIntensity SetAbientLightIntensity]]
            tr [] [ td [attribute "colspan" "2"] [lightProbeView m.lightProbePosition]]
            tr [] [ td [attribute "colspan" "2";style "text-align: center;"] [text "Abient Occlusion"]]
            tr [] [ td [] [text "Strength"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 2.0; step = 0.01} [] m.occlusionSettings.occlusionStrength SetAbientOcclusionStrength ]]
            tr [] [ td [] [text "Radius"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.01;  max = 0.5; step = 0.01} [] m.occlusionSettings.radius SetAbientOcclusionRadius ]]
            tr [] [ td [] [text "Scale"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.1;  max = 1.0; step = 0.01} [] m.occlusionSettings.scale SetAbientOcclusionScale ]]
            tr [] [ td [] [text "Samples"]; 
                    td [style "width: 70%;"] [integerInput ""  8 128  SetAbientOcclusionSamples m.occlusionSettings.samples]]
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
             |> AVal.constant
        )
        |> Sg.projTrafo (size |> AVal.map (fun actualSize -> 
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

    let skyMapSize = 1024 |> AVal.init 

    let skyMapequirectengular  skyMap : ISg<'msg> -> ISg<'msg> = 
        let texture =  
            AVal.map (fun s -> FileTexture(s, { wantCompressed = false; wantMipMaps = false; wantSrgb = false }) :> ITexture) skyMap
        Sg.texture (Sym.ofString "SkyMapEquirec") texture 

    let skyBoxEquirec skyMap rotation=
        Sg.box (AVal.constant C4b.White) (AVal.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.uniform "SkyMapRotation" rotation
            |> skyMapequirectengular skyMap
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! IBL.skyTextureEquirec
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

    let diffuseIrradianceSize = 32 |> AVal.init 

    let diffuseIrradianceBox (runtime : IRuntime) skyBoxTexture =
        Sg.box (AVal.constant C4b.White) (AVal.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! IBL.convoluteDiffuseIrradiance
            }       

    let diffuseIrradianceMap (runtime : IRuntime) skyBoxTexture = 
        renderToCube runtime diffuseIrradianceSize signature (diffuseIrradianceBox runtime skyBoxTexture) 

    let prefilterSpecSize = 128 |> AVal.init 

    let prefilterSpecBox (runtime : IRuntime) skyBoxTexture (level : int) =
        Sg.box (AVal.constant C4b.White) (AVal.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
            |> Sg.uniform "Roughness" (AVal.constant (float level / 4.0) )
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! IBL.prefilterSpec
            } 

    let prefilterdSpecColor (runtime : IRuntime) skyBoxTexture = 
        renderToCubeMip runtime prefilterSpecSize 5 signature (prefilterSpecBox runtime skyBoxTexture) 

    let signatureBRDFLtu (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgb16f; samples = 1 }
        ]

    let LtuSize = V2i(512,512) |> AVal.init 

    let BRDFLtuTask (runtime : IRuntime)= 
        Sg.fullScreenQuad
        |>Sg.adapter
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! IBL.integrateBRDFLtu
        }
        |> Sg.viewTrafo (Trafo3d.Identity |> AVal.constant )
        |> Sg.projTrafo (Trafo3d.Identity |> AVal.constant )
        |> Sg.compile runtime (signatureBRDFLtu runtime)
    
    let BRDFLtu (runtime : IRuntime) =
        RenderTask.renderToColor LtuSize (BRDFLtuTask runtime)

module LightProbe =
    open CubeRenderTask
    open Aardvark.UI

    let signature (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgb16f; samples = 1 }
        ]

    let probeSize = 512 |> AVal.init 
    
    let renderLightProbeSide runtime size signature scene (lightSgs0 : aset<ISg<obj>>) skyBoxTexture skyMapIntensity ambientLightIntensity position face =
        let lookTo = 
            match face with
            |CubeSide.PositiveY  -> position+V3d.OIO
            |CubeSide.PositiveZ -> position+V3d.OOI
            |CubeSide.PositiveX -> position+V3d.IOO
            |CubeSide.NegativeZ-> position-V3d.OOI
            |CubeSide.NegativeX -> position-V3d.IOO
            |CubeSide.NegativeY -> position-V3d.OIO
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

        let view =
             CameraView.lookAt position lookTo (lookSky * -1.0)
             |> CameraView.viewTrafo 
             |> AVal.constant
        
        let proj = size |> AVal.map (fun actualSize -> 
                Frustum.perspective 90.0 0.1 100.0 1.0 |> Frustum.projTrafo
              )

        let gBuffer = GeometryBuffer.makeGBuffer runtime view proj size skyBoxTexture scene skyMapIntensity

        let lightSgs =
            aset {
                yield! lightSgs0
                let pass0 = //global  abient  lightnig
                    Sg.fullScreenQuad
                    |> Sg.adapter
                    |> Sg.shader {
                        do! GBuffer.getGBufferData
                        do! PBR.abientDeferredSimple
                        }
                yield pass0
           } 
            
        //additive blending
        let mutable blendMode = BlendMode(true)
        blendMode.AlphaOperation <- BlendOperation.Add
        blendMode.Operation <- BlendOperation.Add
        blendMode.SourceFactor <- BlendFactor.One
        blendMode.SourceAlphaFactor <- BlendFactor.One
        blendMode.DestinationFactor <- BlendFactor.One
        blendMode.DestinationAlphaFactor <- BlendFactor.One

        //render linear HDR output
        let  output = 
            Sg.set lightSgs
            |> Sg.blendMode (blendMode |> AVal.constant)
            |> Sg.uniform "AmbientIntensity" ambientLightIntensity
            |> Sg.uniform "CameraLocation" (view |> AVal.map (fun t -> t.Backward.C3.XYZ))
            |> Sg.texture ( DefaultSemantic.Colors) (Map.find DefaultSemantic.Colors gBuffer)
            |> Sg.texture ( Sym.ofString "WPos") (Map.find (Sym.ofString "WorldPosition") gBuffer)
            |> Sg.texture ( DefaultSemantic.Normals) (Map.find shaderCommon.Semantic.NormalR gBuffer)
            |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
            |> Sg.texture (shaderCommon.Semantic.Emission) (Map.find shaderCommon.Semantic.Emission gBuffer)
            |> Sg.compile runtime signature   
        
        output

    let lightProbe runtime scene lightSgs0 skyBoxTexture skyMapIntensity ambientLightIntensity position = 
       let size = AVal.map (fun (s : int) -> V2i(s,s)) probeSize
       let sign = signature runtime
       let task = renderLightProbeSide runtime size sign scene lightSgs0 skyBoxTexture skyMapIntensity ambientLightIntensity position
       RenderTask.renderToColorCubeMip probeSize 1 (fun _ -> task)