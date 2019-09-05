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

    let numInput  = labeledFloatInput "" Double.MinValue Double.MaxValue 1.0 
    let view (m : IMod<V3d>) =
        Html.table [                            
            Html.row "X" [numInput SetX (Mod.map(fun (v :  V3d)-> v.X) m)]
            Html.row "Y" [numInput SetY (Mod.map(fun (v :  V3d)-> v.Y) m)]
            Html.row "Z" [numInput SetZ (Mod.map(fun (v :  V3d)-> v.Z) m)]
        ]     

module lightControl = 

    type Message =
        | DefaultDirectionalLight
        | DefaultPointLight
        | SetLightDirection of V3dInput.Message
        | SetLightPosition of V3dInput.Message

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

    let view (m : MLight) =
        match m with
        |MDirectionalLight l' -> 
            div [] [
                Mod.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight)]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight)]  [text "Change to Point Light"] 
                    ] 
                    |> PList.ofList) l'
                |> AList.ofMod
                |> Incremental.div AttributeMap.empty 
                
                Mod.map (fun l -> l.lightDirection.XYZ) l'
                |> V3dInput.view
                |> UI.map SetLightDirection

            ]                  
        |MPointLight l' -> 
             div [] [

                Mod.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight)]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight)]  [text "Change to Directional Light"]
                    ] 
                    |> PList.ofList) l'
                |> AList.ofMod
                |> Incremental.div AttributeMap.empty 
                
                Mod.map (fun l -> l.lightPosition.XYZ) l'
                |> V3dInput.view
                |> UI.map SetLightPosition

            ]