namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model
open System.IO

module materialControl = 

    type Message =
        | SetMetallic of float
        | SetRoughness of float
        | SetAlbedoFactor of float
        | SetNormalMapStrength of float
        | SetDiscard 
        | SetDisplacmentMap of string
        | SetDisplacmentStrength of float

    let update  (m : PBRMaterial) (msg : Message)  =
        match msg with
        | SetMetallic t -> { m with  metallic = t}
        | SetRoughness r -> { m with  roughness = r}
        | SetAlbedoFactor a -> { m with  albedoFactor = a}
        | SetNormalMapStrength s -> { m with  normalMapStrenght = s}
        | SetDiscard -> { m with  discard = not m.discard}
        | SetDisplacmentMap d -> {m with displacmentMap = FileTexture (d, TextureParams.empty) :>  ITexture }
        | SetDisplacmentStrength s -> { m with  displacmentStrength = s}

    let view (m : MPBRMaterial) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        Html.table [                        
            tr [] [ td [] [text "Metallic"]; td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.metallic SetMetallic]]
            tr [] [ td [] [text "Roughness"]; td [style "width: 70%;"] [inputSlider {min = 0.01;  max = 1.0; step = 0.01} [] m.roughness SetRoughness]]
            tr [] [ td [] [text "Albedo Factor"]; td [style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.albedoFactor SetAlbedoFactor]]
            tr [] [ td [] [text "Normal Map Strength"]; td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.normalMapStrenght SetNormalMapStrength]]
            tr [] [ td [] [text "Discard"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  m.discard SetDiscard ]]
            tr [] [ td [] [text "Displacement Map"]; td [] [openDialogButton 
                    { OpenDialogConfig.file with allowMultiple = false; title = "Open displacement map"; filters  = [|"*.*"|];  startPath = ""; mode  = OpenDialogMode.File}
                    [ clazz "ui green button"; onChooseFile SetDisplacmentMap ] 
                    [ text "Open displacement map File" ]]]
            tr [] [ td [] [text "Displacement Strength"]; td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] m.displacmentStrength SetDisplacmentStrength]]
        ]         