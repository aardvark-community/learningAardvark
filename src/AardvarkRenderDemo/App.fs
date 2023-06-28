namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Semantics
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open SLEAardvarkRenderDemo.Model

(*
   The main addaptive elm-style App and the main setup of the render pipeline
*)
//the main app message types
type Message =
    | CameraMessage of FreeFlyController.Message
    | LightMessage of int * lightControl.Message //from light.fs
    | RemoveLight  of int
    | AddLight of Light
    | SetExpousure of float
    | BloomMessage of BloomControl.Message
    | GlobalEnviormentMessage of globalEnviroment.Message // from globalEnviroment.fs
    | SceneObjectMessage of sceneObjectControl.Message //from  SceneObject.fs
    | AddSceneObject of string
    | RemoveSceneObject of string
    | SelectObject of string
    | SaveProject of string
    | LoadProject of string
    | FxAAMessage  of fxAA.Message
    | ToneMappingMessage of filmicToneMappingControl.Message
    | SssProfileMessage of sssProfiles.Message

module App =   

    //setup the initial state
    let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
    let initialView = CameraView.lookAt (V3d(0.0, 2.0, -6.0)) (V3d(0.0, 2.0, 0.0)) (V3d.OIO * 1.0)

    let (!!) inner = Path.combine ([__SOURCE_DIRECTORY__;"..";"..";"data";] @ inner)

    let obj , selected = sceneObject.loadObject HashMap.empty !!["SLE_Gnom4.obj"]

    let defaultModel = { 
        cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
        lights = HashMap.ofList [(0, light.defaultDirectionalLight)]
        enviorment = {skyMap = !!["GrandCanyon_C_YumaPoint";"GCanyon_C_YumaPoint_3k.hdr"]
                      skyMapRotation = Math.PI; 
                      skyMapIntensity = 1.0;
                      ambientLightIntensity = 1.0
                      occlusionSettings = light.defaultAbientOcclusion
                      lightProbePosition = None
                      }
        expousure  = 1.0
        toneMapping = filmicToneMapping.defaultToneMapping
        fxAA = fxAA.defaultFxAA
        bloom = bloom.defaultBloom
        objects = obj
        selectedObject = selected
        sssProfiles = HashMap.empty
    }

    let initial = 
        let file = !!["initial_scene.json"]
        if (System.IO.File.Exists file)
        then 
           projetIO.load file
        else defaultModel

    //the main update function
    let update (m : Model) (msg : Message) =
        //compose the update function from the updates of the sub-model
        match msg with
        | CameraMessage msg ->
            { m with cameraState = FreeFlyController.update m.cameraState msg }
        | LightMessage (i, lms) -> 
            let li' = HashMap.tryFind i  m.lights //from light.fs
            match  li' with 
            |Some li ->
                let l = lightControl.update li lms
                { m with lights = HashMap.update i (fun _ -> l ) m.lights }
            |None ->  m
        | RemoveLight i ->  
            let lights = HashMap.remove i m.lights
            { m with lights = lights}
        | AddLight l -> 
            let i = 
                if HashMap.isEmpty m.lights then
                    1
                else
                    HashMap.keys m.lights
                    |> Seq.max
                    |> max 0
                    |> (+) 1
            let lights = HashMap.add i l m.lights
            { m with lights = lights}
        | SceneObjectMessage (msg) ->
            let o' = HashMap.tryFind m.selectedObject m.objects
            match o' with
            |Some o  ->
                let newo = sceneObjectControl.update o msg //from  SceneObject.fs
                let objects = HashMap.update m.selectedObject (fun _ -> newo ) m.objects
                { m with objects = objects }
            |None -> m
        | RemoveSceneObject name ->
            let obj =  HashMap.remove name m.objects  
            let selected =  obj |> HashMap.keys |> Seq.first |> Option.defaultValue "None"
            { m with objects = obj; selectedObject = selected }           
        | AddSceneObject file ->
            let objects, selected = sceneObject.loadObject m.objects file
            { m with objects = objects; selectedObject = selected}
        | SelectObject name -> { m with selectedObject = name }
        | SetExpousure e ->
            { m with expousure = e }
        | BloomMessage msg ->  
             { m with bloom = BloomControl.update m.bloom msg }
        | GlobalEnviormentMessage msg ->
            { m with enviorment = globalEnviroment.update m.enviorment msg } // from globalEnviroment.fs
        | SaveProject f -> 
            Log.warn "Save %s" f
            do projetIO.save m f
            m
        | LoadProject f -> projetIO.load f
        | FxAAMessage msg -> 
            { m with fxAA = fxAA.update m.fxAA msg }
        | ToneMappingMessage msg -> 
            { m with toneMapping = filmicToneMappingControl.update m.toneMapping msg }
        | SssProfileMessage msg -> 
            { m with sssProfiles = sssProfiles.update m.sssProfiles msg }

     //main render task: put all passes together for deferred rendering
    let compileDeffered (m : AdaptiveModel) (values : Aardvark.Service.ClientValues)=
        let outputSignature = values.signature
        let view = values.viewTrafo
        let proj = values.projTrafo
        let f = AVal.map Frustum.ofTrafo values.projTrafo
        let camFoVx = AVal.map (Frustum.horizontalFieldOfViewInDegrees >> radians) f
        let tanFoVxHalf =  AVal.map (fun  b -> b * 0.5  |> tan) camFoVx
        let size = values.size |> AVal.map (fun s -> V2i(max 1 s.X, max 1 s.Y))
        let aspectRatio = AVal.map (fun (s : V2i) -> float s.Y / float s.X) size 
        let runtime = values.runtime

        let scene = m.objects |> sceneObject.objects 

        //let scene = m.objects |> sceneObject.objectsTrimByMaterial (fun _ mat -> AVal.map (fun v -> v > 0.0) mat.metallic.factor)

        //render the speical sky map to a texture cube
        //Ardvark will only rexecute this if the skyMap or rotation changes
        let skyBoxTexture = SkyBox.getTexture runtime m.enviorment.skyMap m.enviorment.skyMapRotation

        //calculate the  global bounding box
        let bb = //scene?GlobalBoundingBox() //not defined for Sg.set Todo: define semantics
            let seed = Box3d(V3d.OOO, V3d.OOO) |> AVal.constant
            let bounds (o : AdaptiveSceneObject) =
                adaptive{
                    let! object  = sceneObject.object o
                    let! trans = sceneObject.trafo o
                    let bounds = object.bounds 
                    return bounds.Transformed(trans)
                }    
            m.objects
            |> AMap.toASet       
            |> ASet.fold (fun s (_,(o : AdaptiveSceneObject)) -> AVal.map2( fun (s : Box3d) (b : Box3d)-> Box.Union(s,b)) s (bounds o)) seed 
            |> AVal.bind id


        let enviromentTexture = 
            m.enviorment.lightProbePosition
            |> AdaptiveResource.bind 
                (fun (p' : V3d option) -> 
                    match p' with
                    |Some p -> LightProbe.lightProbe runtime scene skyBoxTexture m.enviorment.skyMapIntensity m.enviorment.ambientLightIntensity bb m.lights p 
                    |None -> skyBoxTexture 
                )
   

        //render the precalculated  aps for PBR Global Ambient Light
        //Ardvark will only rexecute this if the global enviroment changes
        let diffuseIrradianceMap = GlobalAmbientLight.diffuseIrradianceMap runtime enviromentTexture

        let prefilterdSpecColor = GlobalAmbientLight.prefilterdSpecColor runtime enviromentTexture

        let bRDFLtu = GlobalAmbientLight.BRDFLtu runtime

        //create a depth frameBuffer attachment that can be shared between the gBuffer and the transparency task
        let depthTex = runtime.CreateTexture2D(size, TextureFormat.DepthComponent24)
        let depthAttachment = runtime.CreateTextureAttachment(depthTex) :> aval<_>
        
        //get shadow maps for all lights
        let shadowMaps = Shadow.shadowMaps runtime scene bb m.lights

        //render the geometry to the gBuffer
        let gBuffer = GeometryBuffer.makeGBuffer runtime view proj size skyBoxTexture scene m.enviorment.skyMapIntensity (Some depthAttachment)
        //let wpt = gBuffer.[DefaultSemantic.Colors]
        //render the abient occlousion map    
        let ambientOcclusion = 
            //material.onPixTex C3f.White |> AVal.constant
            SSAO.makeAmbientOcclusion runtime size view proj gBuffer m.enviorment.occlusionSettings

        let diffuseAndSpecular =
            deferredRendering.diffuseAndSpecular
                runtime
                view
                size 
                shadowMaps 
                m.lights
                m.sssProfiles
                bb 
                gBuffer
                ambientOcclusion
                m.enviorment.ambientLightIntensity
                diffuseIrradianceMap
                prefilterdSpecColor
                bRDFLtu
          (*
        let diffuseAndSpecular = 
           forwardRendering.diffuseAndSpecular 
            runtime 
            view 
            proj 
            size 
            skyBoxTexture 
            scene 
            m.enviorment.skyMapIntensity 
            m.lights
            bb 
            m.enviorment.ambientLightIntensity 
            diffuseIrradianceMap
            prefilterdSpecColor
            bRDFLtu
            m.sssProfiles
            *)
        let diffuse =  Map.find  (Sym.ofString"Diffuse")  diffuseAndSpecular
        let specular = Map.find  (Sym.ofString"Specular") diffuseAndSpecular

        let ssss = subSurface.makeSubSurfaceScatttering (runtime : IRuntime) (size : aval<V2i>) tanFoVxHalf view proj gBuffer diffuse m.sssProfiles

        let diffuseAndSpecular' = Map.ofList [((Sym.ofString"Diffuse"),ssss); ((Sym.ofString"Specular"), specular)]
        let combined = combine.combine runtime size diffuseAndSpecular'

        let transparent = 
            WBOTI.acummulate 
                runtime 
                view 
                proj 
                size 
                m.objects
                shadowMaps
                m.lights
                bb 
                m.enviorment.ambientLightIntensity
                diffuseIrradianceMap
                prefilterdSpecColor
                bRDFLtu
                m.sssProfiles
                tanFoVxHalf
                aspectRatio
                depthAttachment//depth attachment shared with the gBuffer creation
                gBuffer//only to force dependency on the gBuffer, so that this is always executed after the gBuffer creation

        let combined' =
            WBOTI.compose
                runtime
                outputSignature 
                size 
                combined 
                (Map.find  (Sym.ofString "Accum") transparent)
                (Map.find  (Sym.ofString "ModulateColor") transparent)
                (Map.find  (Sym.ofString "Delta") transparent)

        let postprocessed = 
            AdaptiveResource.bind (fun doBloom  ->  
                if doBloom then
                    bloom.bloom runtime size combined' m.bloom :> aval<IBackendTexture>
                else 
                    combined' :> aval<IBackendTexture>) 
                m.bloom.on
       

        //tone mapping and gamma correction
        let toneMapped =  //(Map.find  (Sym.ofString "Accum") transparent)
            filmicToneMapping.toneMapping runtime outputSignature postprocessed m        
            |> RenderTask.renderToColor size
        
        //let debug= SSAO.visualizeDepth runtime size gBuffer

        fxAA.fxAA runtime outputSignature toneMapped m.fxAA

        
    (*
        For deferred Rendering and some other techniques it is nessesary to know the size of the render window.
        That is not straitforward in Aardvark.media because there could be multiple clients with different screen sizes.
        The solution is to create a custem scene that takes a function from client values to a IRenderTask and feed that into a RenderControl.
        If I understand it correctly, at runtime a render Task per client will be created with the respectice client values.
        Thanks to Georg Haaser for the tip. 
        Note that the ClientValues contain the Output Framebuffer Signature. That is very usefull because you can obtain a reference to the runtime from the signatur. 
    *)
    let RenderControl (att : list<string * AttributeValue<Message>>) (s : AdaptiveCameraControllerState) (frustum : Frustum) (task : Aardvark.Service.ClientValues -> IRenderTask) =
        let scene =  Aardvark.Service.Scene.custom task
        let cam : aval<Camera> = AVal.map (fun v -> { cameraView = v; frustum = frustum }) s.view 
        DomNode.RenderControl(AttributeMap.ofList att, cam, scene, None)
        |> FreeFlyController.withControls s CameraMessage (AVal.constant frustum)

    //the 3D scene and control
    let view3D (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 30.0 0.1 100.0 1.0 

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
                attribute "showFPS" "true"
                //attribute "data-renderalways" "1"
            ]

        let t = compileDeffered m
        RenderControl att m.cameraState frustum t

    // main view for UI and  
    let view (m : AdaptiveModel) =
        let saveButton = 
            openDialogButton 
                { OpenDialogConfig.file with allowMultiple = false; title = "Save"; filters  = [|"*.*"|];  startPath = ""; mode  = OpenDialogMode.File}
                [ clazz "ui green button"; onChooseFile SaveProject ] 
                [ text "Save" ]
        let loadButton = 
            openDialogButton 
                { OpenDialogConfig.file with allowMultiple = false; title = "load"; filters  = [|"*.*"|];  startPath = ""; mode  = OpenDialogMode.File}
                [ clazz "ui green button"; onChooseFile LoadProject ] 
                [ text "Load" ]
        require Html.semui ( 
            body [] (        
                Html.SemUi.adornerAccordeonMenu [ 
                "Edit Objects",
                    [
                        Html.table [                        
                            tr [] [ td [attribute "colspan" "2"] [
                                openDialogButton 
                                 { OpenDialogConfig.file with allowMultiple = false; title = "Open Model file"; filters  = [|"*.obj"|];  startPath = ""; mode  = OpenDialogMode.File}
                                 [ clazz "ui green button"; onChooseFile AddSceneObject ] 
                                 [ text "Add Object" ]
                                ]]
                            tr [] [ td [] [text "Object"]; td [style "width: 70%;"] [Html.SemUi.dropDown' (m.objects |> AMap.keys |> ASet.toAList) m.selectedObject SelectObject id]]
                            tr [] [ td [attribute "colspan" "2"] [
                                button [clazz "ui button"; onClick (fun _ -> RemoveSceneObject (AVal.force m.selectedObject))]  [text "Remove selected Object"]
                                ]]
                        ]
                        m.selectedObject
                        |> AVal.bind (fun n -> AMap.tryFind n m.objects)
                        |> AVal.map (Option.map  (sceneObjectControl.view m.sssProfiles>> UI.map SceneObjectMessage  ) >> IndexList.single)
                        |> AList.ofAVal
                        |> AList.choose id
                        |> Incremental.div AttributeMap.empty
                    ]    
                "Add Light",
                    [
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Point Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Spot Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Sphere Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Disk Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultRectangleLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Rectangle Light"]
                    ]    
                "Change Light",
                    [   //build a list of light views from the set of lights
                        m.lights
                        |> AMap.fold 
                            ( fun items i l -> 
                                let name = 
                                    match l with
                                    |AdaptiveDirectionalLight _ -> sprintf "Directional Light %i" i
                                    |AdaptivePointLight _ -> sprintf "Point Light %i" i
                                    |AdaptiveSpotLight _ -> sprintf "Spot Light %i" i
                                    |AdaptiveSphereLight _ -> sprintf "Sphere Light %i" i
                                    |AdaptiveDiskLight _ -> sprintf "Disk Light %i" i
                                    |AdaptiveRectangleLight _ -> sprintf "Rectangle Light %i" i
                                let d = 
                                        lightControl.view  l |> UI.map (fun msg -> LightMessage (i, msg)) 
                                        |> AList.single
                                        |> Incremental.div AttributeMap.empty
                                
                                let item = 
                                    name, [
                                        d
                                        button [clazz "ui button"; onClick (fun _ -> RemoveLight i); style "margin-top: 5px;width: 100%;" ]  [text "Remove"]
                                        ]
                                item::items
                            ) []
                         //feed that into a accordeon
                        |> AVal.map (Html.SemUi.accordionMenu true "ui vertical inverted fluid accordion menu" >> IndexList.single)
                        // and that  into  a incremantal div to handel the case that the numbers of lights change
                        |> AList.ofAVal
                        |> Incremental.div AttributeMap.empty
                    ]
                "Global Enviorment",
                    [
                        globalEnviroment.view m.enviorment |> UI.map GlobalEnviormentMessage
                    ]  
                "Subsurface scattering profiles",
                    [
                        sssProfiles.view m.sssProfiles |> UI.map SssProfileMessage
                    ]
                "Render Settings",
                    [
                        Html.table [                        
                            tr [] [ td [] [text "Exposure"]; td [ style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.expousure SetExpousure]]
                        ]  
                        filmicToneMappingControl.view m.toneMapping |> UI.map ToneMappingMessage
                        BloomControl.view m.bloom |>  UI.map BloomMessage 
                        fxAA.view m.fxAA |>  UI.map FxAAMessage 
                    ]    
                "Project",
                    [
                        saveButton
                        loadButton
                    ]  
                ] [view3D  m]
            )
        )

    //assmble the map
    let app =
        {
            initial = initial
            update = update
            view = view
            threads = fun _ -> ThreadPool.empty
            unpersist = Unpersist.instance
        }