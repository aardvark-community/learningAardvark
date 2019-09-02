namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model

module lightControl = 

    type Message =
        | DefaultDirectionalLight
        | DefaultPointLight
        | SetLightDirection of Vector3d.Action
        | SetLightPosition of Vector3d.Action

    let update (m : Light) (msg : Message) =
        match msg with
        | DefaultDirectionalLight -> light.defaultDirectionalLight
        | DefaultPointLight -> light.defaultPointLight
        | SetLightDirection vMsg  ->  
            match m with
            | DirectionalLight r -> 
                let i =  Vector3d.initV3d(r.lightDirection.XYZ)
                let n = (Vector3d.update i vMsg).value
                DirectionalLight {r with lightDirection = V4d(n, 0.0)} 
            | x -> x
        | SetLightPosition vMsg ->  
            match m with
            | PointLight r -> 
                let i =  Vector3d.initV3d(r.lightPosition.XYZ)
                let n = (Vector3d.update i vMsg).value
                PointLight {r with lightPosition = V4d(n, 1.0)} 
            | x -> x

    let view (m : MLight) =
        match m with
        |MDirectionalLight l -> 
             div [] [
                button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight)]  [text "Reset"]
                button [clazz "ui button"; onClick (fun _ -> DefaultPointLight)]  [text "Change to Point Light"]
            ]
        |MPointLight l -> 
             div [] [
                button [clazz "ui button"; onClick (fun _ -> DefaultPointLight)]  [text "Reset"]
                button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight)]  [text "Change to Directional Light"]
            ]