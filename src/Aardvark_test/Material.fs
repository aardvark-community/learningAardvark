namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model

module materialControl = 

    type Message =
        | SetMetallic of float
        | SetRoughness of float
        | SetAlbedoFactor of float

    let update  (m : PBRMaterial) (msg : Message)  =
        match msg with
        | SetMetallic t -> { m with  metallic = t}
        | SetRoughness r -> { m with  roughness = r}
        | SetAlbedoFactor a -> { m with  albedoFactor = a}

    let view (m : MPBRMaterial) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        Html.table [                        
            tr [] [ td [] [text "Metallic"]; td [] [slider {min = 0.0;  max = 1.0; step = 0.01} AttributeMap.empty m.metallic SetMetallic]]
            tr [] [ td [] [text "Roughness"]; td [] [slider {min = 0.01;  max = 1.0; step = 0.01} AttributeMap.empty m.roughness SetRoughness]]
            tr [] [ td [] [text "Albedo Factor"]; td [] [numeric {min = 0.0;  max = 10.0; smallStep = 0.1; largeStep = 1.0} AttributeMap.empty m.albedoFactor SetAlbedoFactor]]
        ]         