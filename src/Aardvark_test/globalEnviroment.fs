namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark_test.Model
open System.IO
open Aardvark.SceneGraph

module globalEnviroment = 

    open Aardvark.UI
    open Aardvark.UI.Primitives

    type Message =
        | SetSkyMap of String
        | SetSkyMapRotation of float
        | SetAbientLightIntensity of float
        | SetSkyMapIntensity of float

    let update (m : GlobalEnviorment)  (msg : Message) = 
        match msg  with
        | SetSkyMap s -> {m  with skyMap = s}
        | SetSkyMapRotation r -> {m  with skyMapRotation = r}
        | SetAbientLightIntensity i-> {m  with ambientLightIntensity = i}       
        | SetSkyMapIntensity i-> {m  with skyMapIntensity = i}       

    let view (m : MGlobalEnviorment) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        let path = Path.GetDirectoryName(Mod.force m.skyMap)
        Html.table [                        
            tr [] [ td [] [text "sky map"]; td [] [openDialogButton 
                    { OpenDialogConfig.file with allowMultiple = false; title = "Open sky map hdr"; filters  = [|"*.hdr"|];  startPath = path}
                    [ clazz "ui green button"; onChooseFile SetSkyMap ] 
                    [ text "Open hdr File" ]]]
            tr [] [ td [] [text "Sky Map Intensity"]; td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.skyMapIntensity SetSkyMapIntensity]]
            tr [] [ td [] [text "Sky Map Rotation"]; td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] (Mod.map (fun r -> r/(2.0*Math.PI)) m.skyMapRotation)  (fun r -> SetSkyMapRotation (r*2.0*Math.PI)) ]]
            tr [] [ td [] [text "Ambient Light Intensity"]; td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.ambientLightIntensity SetAbientLightIntensity]]
        ]   

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
                do! SLESurfaces.skyTextureEquirec
            }

    let getTexture (runtime : IRuntime) skyMap rotation = 
        renderToCube runtime skyMapSize signature (skyBoxEquirec skyMap rotation)

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
                do! SLESurfaces.convoluteDiffuseIrradiance
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
                do! SLESurfaces.prefilterSpec
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
            do! SLESurfaces.integrateBRDFLtu
        }
        |> Sg.viewTrafo (Trafo3d.Identity |> Mod.constant )
        |> Sg.projTrafo (Trafo3d.Identity |> Mod.constant )
        |> Sg.compile runtime (signatureBRDFLtu runtime)
    
    let BRDFLtu (runtime : IRuntime) =
        RenderTask.renderToColor LtuSize (BRDFLtuTask runtime)