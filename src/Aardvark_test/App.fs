namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model

type Message =
    | CameraMessage of FreeFlyController.Message
    | SetLight of Light //remove when  LightMessage  works
    | LightMessage of lightControl.Message


module App =   


    let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
    let initial = { light =  light.defaultLight; cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig}}

    let update (m : Model) (msg : Message) =
        match msg with
        | CameraMessage msg ->
            { m with cameraState = FreeFlyController.update m.cameraState msg }
        | SetLight light -> //remove when  LightMessage  works
            { m with light = light }   
        | LightMessage lms -> { m with light = lightControl.update m.light lms }
    
    let figureMesh = //Sg.box (Mod.constant C4b.Red) (Mod.constant (Box3d(-V3d.III, V3d.III)))
        Aardvark.SceneGraph.IO.Loader.Assimp.load @"..\..\..\data\SLE_Gnom3.obj"//@"..\..\..\data\aardvark\aardvark.obj" //
        |> Sg.adapter
        |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
        |> Sg.transform (Trafo3d.Scale(1.0,1.0,1.0))

    let uniformLight (l : IMod<MLight> ) (m :ISg<Message>) =
        let r = adaptive {
            let! d = l
            match d with
            | MDirectionalLight  x ->
                let! lightDirection = x.lightDirection
                let! color = x.color
                return m |> (Sg.uniform "Light" <| Mod.constant (SLEUniform.DirectionalLight {lightDirection = lightDirection; color = color.ToV3d()}) )
            | MPointLight  x ->
                let! lightPosition = x.lightPosition
                let! color = x.color
                let! attenuationQad = x.attenuationQad
                let! attenuationLinear = x.attenuationLinear
                return m |> (Sg.uniform "Light" <| Mod.constant (SLEUniform.PointLight {lightPosition = lightPosition; color = color.ToV3d(); attenuationQad = attenuationQad; attenuationLinear = attenuationLinear}) )
        } 
         r
        |> Sg.dynamic

    let lightSourceModel (l : IMod<MLight> ) =
        adaptive {
            let! l' = l
            let  m = 
                match l' with
                | MDirectionalLight ld -> Sg.empty
                | MPointLight lp -> 
                    Sg.sphere 6 (Mod.constant C4b.White) (Mod.constant 0.03) 
                    |> Sg.translate' (Mod.map ( fun (v : V4d) -> v.XYZ) lp.lightPosition)
            return m
        } 
        |> Sg.dynamic
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor 
            }       

    let view3D (m : MModel) =

        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> Mod.constant

        let sg =
            figureMesh
            |> uniformLight m.light
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.diffuseTexture 
                do! SLESurfaces.lighting false
                }
            |> Sg.andAlso <| lightSourceModel m.light

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
        
    let view (m : MModel) =
        require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
            body [] (        // explit html body for our app (adorner menus need to be immediate children of body). if there is no explicit body the we would automatically generate a body for you.
                Html.SemUi.adornerMenu [ 
                "Change Light", [ 
                    button [clazz "ui button"; onClick (fun _ -> SetLight light.defaultDirectionalLight)]  [text "Directional Light"]
                    button [clazz "ui button"; onClick (fun _ -> SetLight light.defaultPointLight)]  [text "Point Light"]
   
                    //I need to figure out how to use this instead:
                    //lightControl.view  (m.light) |> UI.map LightMessage
                    ] 
                ] [view3D m]
            )
        )

    let app =
        {
            initial = initial
            update = update
            view = view
            threads = Model.Lens.cameraState.Get >> FreeFlyController.threads >> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }