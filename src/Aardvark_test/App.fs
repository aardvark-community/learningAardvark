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
    | LightMessage of lightControl.Message

module App =   


    let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
    let initialView = CameraView.lookAt (V3d(2.0, 2.0, -3.0)) (V3d(0.0, 1.0, 0.0)) (V3d.OIO * 1.0)
    let initial = { light =  light.defaultLight; cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}}

    let update (m : Model) (msg : Message) =
        //compose the update functions from the updates of the sub-model
        match msg with
        | CameraMessage msg ->
            { m with cameraState = FreeFlyController.update m.cameraState msg }
        | LightMessage lms -> 
            { m with light = lightControl.update m.light lms }
    
    let figureMesh =
        Aardvark.SceneGraph.IO.Loader.Assimp.load @"..\..\..\data\SLE_Gnom3.obj"
        |> Sg.adapter
        //|> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
        |> Sg.transform (Trafo3d.Scale(1.0,1.0,1.0))

    //Sg Node to set the light information as an uniform
    let uniformLight (l : IMod<MLight> ) (m :ISg<Message>) =
        //needs to be adaptive because the  Light can change and is an IMod
        //we go from IMod<MLight> to IMod<ISg<Message>>
        let r = adaptive {
            let! d = l
            match d with
            | MDirectionalLight  x' ->
                let! x  = x'
                //Map to a type more convinient in the shaders
                return m |> (Sg.uniform "Light" <| Mod.constant (SLEUniform.DirectionalLight {lightDirection = x.lightDirection; color = x.color.ToV3d() * x.intensity}) )
            | MPointLight  x' ->
                let! x  = x'
                return m |> (Sg.uniform "Light" <| Mod.constant (SLEUniform.PointLight {lightPosition = x.lightPosition; color = x.color.ToV3d() * x.intensity; attenuationQad = x.attenuationQad; attenuationLinear = x.attenuationLinear}) )
        } 
        r //  Wrap the IMod<ISg<Message>> in a dynamic node
        |> Sg.dynamic

    //Sg Node to draw the light source
    let lightSourceModel (l : IMod<MLight> ) =
        adaptive {
            let! l' = l
            let  m = 
                match l' with
                | MDirectionalLight ld -> Sg.empty
                | MPointLight lp -> 
                    Sg.sphere 6 (Mod.constant C4b.White) (Mod.constant 0.03) 
                    |> Sg.translate' (Mod.map ( fun v -> v.lightPosition.XYZ) lp)
            return m
        } 
        |> Sg.dynamic
        //simpel shader indepenndend of light 
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor 
            }       

    //the 3D scene and control
    let view3D (m : MModel) =

        let frustum = 
            Frustum.perspective 30.0 0.1 100.0 1.0 
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
        
    // main view for UI and  
    let view (m : MModel) =
        require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
            body [] (        // explit html body for our app (adorner menus need to be immediate children of body). if there is no explicit body the we would automatically generate a body for you.
                Html.SemUi.adornerMenu [ 
                "Change Light", [ 
                    //because  m.Light is an IMod<MLight> I need  to wrap the light view (taking a MLight) an Incremental.div
                    m.light
                    |> Mod.map (fun x -> lightControl.view  (x) |> UI.map LightMessage) //map to an IMod<DomNode<Message>>
                    |> AList.ofModSingle // convert to Alist
                    |> Incremental.div AttributeMap.empty
                    
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