namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model

module V3dInput =

    type Message = 
        | SetX of float
        | SetY of float
        | SetZ of float

    let update  (m : V3d) (msg : Message) =
        match msg with
        | SetX x -> V3d(x, m.Y, m.Z)
        | SetY y -> V3d(m.X, y, m.Z)
        | SetZ z -> V3d(m.X, m.Y, z)

    let numInput name changed state  = labeledFloatInput name Double.MinValue Double.MaxValue 1.0 changed state
    let view header (m : IMod<V3d>) =
        Html.table [ 
            tr [] [ td [attribute "colspan" "3"] [text header] ]                          
            tr [] [ td [] [numInput "X" SetX (Mod.map(fun (v :  V3d)-> v.X) m)]
                    td [] [numInput "Y" SetY (Mod.map(fun (v :  V3d)-> v.Y) m)]
                    td [] [numInput "Z" SetZ (Mod.map(fun (v :  V3d)-> v.Z) m)]
                  ]
        ]     

module lightControl = 

    type Message =
        | DefaultDirectionalLight
        | DefaultPointLight
        | SetLightDirection of V3dInput.Message
        | SetLightPosition of V3dInput.Message
        | SetAttenuationQad of float
        | SetAttenuationLinear of float
        | SetIntensity of float
        | SetColor of C3d

    let update (m : Light) (msg : Message) =
        match msg with
        | DefaultDirectionalLight -> light.defaultDirectionalLight
        | DefaultPointLight -> light.defaultPointLight
        | SetLightDirection vMsg  ->  
            match m with
            | DirectionalLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                DirectionalLight {r with lightDirection = V4d(n, 0.0)} 
            | x -> x
        | SetLightPosition vMsg ->  
            match m with
            | PointLight r -> 
                let n = V3dInput.update (r.lightPosition.XYZ) vMsg
                PointLight {r with lightPosition = V4d(n, 1.0)} 
            | x -> x
        | SetAttenuationQad v ->  
            match m with
            | PointLight r -> 
                PointLight {r with attenuationQad = v} 
            | x -> x
        | SetAttenuationLinear v ->  
            match m with
            | PointLight r -> 
                PointLight {r with attenuationLinear = v} 
            | x -> x
        | SetIntensity i ->  
            match m with
            | PointLight r -> 
                PointLight {r with intensity = i} 
            | DirectionalLight r -> 
                DirectionalLight {r with intensity = i} 
        | SetColor c ->  
            match m with
            | PointLight r -> 
                PointLight {r with color = c} 
            | DirectionalLight r -> 
                DirectionalLight {r with color = c} 

    let attenuationView (l : IMod<float>) (q : IMod<float>)=
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        Html.table [ 
            tr [] [ td [] [text "Attenuation"] ]                          
            tr [] [ td [] [numInput "Linear" SetAttenuationLinear l]]
            tr [] [ td [] [numInput "Quatratic" SetAttenuationQad q]]
        ] 

    let intensityView (i : IMod<float>) (c : IMod<C4b>)=
        let numInput name changed state  = labeledFloatInput name 0.0 Double.MaxValue 1.0 changed state
        Html.table [ 
            tr [] [ td [] [text "Light"] ]                          
           //Important: to make  the colorpicker work, the  assemblyWebpart for Aardvark.UI.Primitives needs to be registert in Program.fs
            tr [] [ td [] [text "Color"]; td [] [ColorPicker.viewSimple c (fun (c : C4b) -> (C3d.FromC4b).Invoke(c) |> SetColor)]]
            tr [] [ td [] [numInput "Intensity" SetIntensity i]]
         ] 

    let view (m : MLight) =
        match m with
        |MDirectionalLight l' -> 
            let i = Mod.map (fun (l : DirectionalLightData) -> l.intensity) l'
            let c = Mod.map (fun (l : DirectionalLightData) -> l.color.ToC4b()) l'
            div [] [
                Mod.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"] 
                    ] 
                    |> PList.ofList) l'
                |> AList.ofMod
                |> Incremental.div AttributeMap.empty 
                
                Mod.map (fun l -> l.lightDirection.XYZ) l'
                |> V3dInput.view "Direction"
                |> UI.map SetLightDirection

                intensityView i c
            ]                  
        |MPointLight l' -> 
            let al = Mod.map (fun l -> l.attenuationLinear) l'
            let aq = Mod.map (fun l -> l.attenuationQad) l'
            let i = Mod.map (fun l -> l.intensity) l'
            let c = Mod.map (fun l -> l.color.ToC4b()) l'
            div [] [

                Mod.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                    ] 
                    |> PList.ofList) l'
                |> AList.ofMod
                |> Incremental.div AttributeMap.empty 
                
                Mod.map (fun l -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                intensityView i c

                attenuationView al aq
            ]