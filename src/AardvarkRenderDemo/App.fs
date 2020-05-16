namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Semantics
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model
open Aardvark.Base.Ag
open material
open Aardvark.SceneGraph.IO.Loader.SgSems

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
    | GlobalEnviormentMessage of globalEnviroment.Message // from globalEnviroment.fs
    | SceneObjectMessage of sceneObjectControl.Message //from  SceneObject.fs
    | AddSceneObject of string
    | RemoveSceneObject of string
    | SelectObject of string
    | SaveProject of string
    | LoadProject of string

module App =   

    //setup the initial state
    let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
    let initialView = CameraView.lookAt (V3d(0.0, 2.0, -6.0)) (V3d(0.0, 2.0, 0.0)) (V3d.OIO * 1.0)

    let obj , selected = sceneObject.loadObject HMap.empty @"..\..\..\data\SLE_Gnom4.obj" 

    let defaultModel = { 
        cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
        lights = HMap.ofList [(0, light.defaultDirectionalLight)]
        enviorment = {skyMap = @"..\..\..\data\GrandCanyon_C_YumaPoint\GCanyon_C_YumaPoint_3k.hdr"; 
                      skyMapRotation = Math.PI; 
                      skyMapIntensity = 1.0;
                      ambientLightIntensity = 1.0
                      occlusionSettings = light.defaultAbientOcclusion}
        expousure  = 1.0
        objects = obj
        selectedObject = selected
    }

    let initial = 
        let file = @"..\..\..\data\initial_scene.json"
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
            let li' = HMap.tryFind i  m.lights //from light.fs
            match  li' with 
            |Some li ->
                let l = lightControl.update li lms
                { m with lights = HMap.update i (fun _ -> l ) m.lights }
            |None ->  m
        | RemoveLight i ->  { m with lights = HMap.remove i m.lights }
        | AddLight l -> 
            let i = 
                if HMap.isEmpty m.lights then
                    1
                else
                    HMap.keys m.lights
                    |> Seq.max
                    |> max 0
                    |> (+) 1
            { m with lights = HMap.add i l m.lights }
        | SceneObjectMessage (msg) ->
            let o' = HMap.tryFind m.selectedObject m.objects
            match o' with
            |Some o  ->
                let newo = sceneObjectControl.update o msg //from  SceneObject.fs
                let objects = HMap.update m.selectedObject (fun _ -> newo ) m.objects
                { m with objects = objects }
            |None -> m
        | RemoveSceneObject name ->
            let obj =  HMap.remove name m.objects  
            let selected =  obj |> HMap.keys |> Seq.first |> Option.defaultValue "None"
            { m with objects = obj; selectedObject = selected }           
        | AddSceneObject file ->
            let objects, selected = sceneObject.loadObject m.objects file
            { m with objects = objects; selectedObject = selected}
        | SelectObject name -> { m with selectedObject = name }
        | SetExpousure e ->
            { m with expousure = e }
        | GlobalEnviormentMessage msg ->
            { m with enviorment = globalEnviroment.update m.enviorment msg } // from globalEnviroment.fs
        | SaveProject f -> 
            Log.warn "Save %s" f
            do projetIO.save m f
            m
        | LoadProject f -> projetIO.load f

    //Render task for the Geometry-buffer pass
    let makeGBuffer (runtime : IRuntime) (view : IMod<Trafo3d>) projection size skyBoxTexture scene (m : MModel) =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
                Sym.ofString "WorldPosition", RenderbufferFormat.Rgba32f
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
                DefaultSemantic.Normals, RenderbufferFormat.Rgba32f
                GBufferRendering.Semantic.MaterialProperties, RenderbufferFormat.Rg32f
            ]

        let skyBox =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.cullMode (Mod.constant CullMode.None)
                |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
                |> Sg.uniform "SkyMapIntensity" m.enviorment.skyMapIntensity
                |> Sg.uniform "CameraLocation" (view |> Mod.map (fun t -> t.Backward.C3.XYZ))
                |> Sg.shader {
                    do! GBufferRendering.skyBoxTrafo
                    do! GBufferRendering.skyGBuffer
                }

        scene
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! displacemntMap.displacementMap
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.diffuseTexture 
            do! NormalMap.normalMap 
            do! GBufferRendering.gBufferShader
            }
        |> (Sg.andAlso <| skyBox )
        |> Sg.viewTrafo (view)
        |> Sg.projTrafo (projection)
        |> Sg.compile runtime signature
        |> RenderTask.renderSemantics(
                    Set.ofList [
                        DefaultSemantic.Depth
                        DefaultSemantic.Colors
                        Sym.ofString "WorldPosition"
                        DefaultSemantic.Normals
                        GBufferRendering.Semantic.MaterialProperties]
               ) size 

    //texture  with random values used in the AO shaders
    let randomTex ( runtime : IRuntime) = 
        let img = PixImage<float32>(Col.Format.RGB, V2i.II * 512)

        let rand = RandomSystem()
        img.GetMatrix<C3f>().SetByCoord (fun _ ->
            rand.UniformV3dDirection().ToC3d().ToC3f()
        ) |> ignore

        runtime.PrepareTexture(PixTexture2d(PixImageMipMap [| img :> PixImage |], TextureParams.empty))

    //Render-Task for the screen-space Abient Occlusion pass
    let makeAmbientOcclusion ( runtime : IRuntime) (size : IMod<V2i>) view proj gBuffer (settings : MAmbientOcclusionSettings)=

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba8
            ]

        let aoSize = 
             Mod.custom (fun t ->
                let s = size.GetValue t
                let d = settings.scale.GetValue t
                V2i(
                    max 1 (int (float s.X * d)),
                    max 1 (int (float s.Y * d))
                )
            )

        let ambientOc = 
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.shader {
                do! SSAO.ambientOcclusion
            }
            |> Sg.texture ( DefaultSemantic.Normals) (Map.find DefaultSemantic.Normals gBuffer)
            |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj
            |> Sg.uniform "Random" (Mod.constant (randomTex runtime :> ITexture))
           
            |> Sg.uniform "Radius" settings.radius
            |> Sg.uniform "Threshold" settings.threshold
            |> Sg.uniform "Samples" settings.samples
            |> Sg.uniform "OcclusionStrength" settings.occlusionStrength
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor aoSize

        let blurredAmbientOc =
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.shader {
                do! SSAO.blur
            }
            |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
            |> Sg.texture (Sym.ofString "AmbientOcclusion") ambientOc
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj
            |> Sg.uniform "Radius" settings.radius
            |> Sg.uniform "Threshold" settings.threshold
            |> Sg.uniform "Sigma" settings.sigma
            |> Sg.uniform "Sharpness" settings.sharpness
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor aoSize

        blurredAmbientOc

    //main render task: put all passes together for deferred rendering
    let compileDeffered (scene : ISg<'msg>) (m : MModel) (values : Aardvark.Service.ClientValues)=
        let outputSignature = values.signature
        let view = values.viewTrafo
        let proj = values.projTrafo
        let size = values.size |> Mod.map (fun s -> V2i(max 1 s.X, max 1 s.Y))
        let runtime = outputSignature.Runtime

        //render the speical sky map to a texture cube
        //Ardvark will only rexecute this if the skyMap or rotation changes
        let skyBoxTexture = SkyBox.getTexture runtime m.enviorment.skyMap m.enviorment.skyMapRotation

        //render the geometry to the gBuffer
        let gBuffer = makeGBuffer runtime view proj size skyBoxTexture scene m

        //render the precalculated  Maps for PBR Global Ambient Light
        //Ardvark will only rexecute this if the global enviroment changes
        let diffuseIrradianceMap = GlobalAmbientLight.diffuseIrradianceMap runtime skyBoxTexture

        let prefilterdSpecColor = GlobalAmbientLight.prefilterdSpecColor runtime skyBoxTexture

        let bRDFLtu = GlobalAmbientLight.BRDFLtu runtime

        //render the abient occlousion map    
        let ambientOcclusion = makeAmbientOcclusion runtime size view proj gBuffer m.enviorment.occlusionSettings

        //calculate the  global bounding box
        let bb = //scene?GlobalBoundingBox() //not defined for Sg.set Todo: define semantics
            let seed = Box3d(V3d.OOO, V3d.OOO) |> Mod.constant
            let bounds (o : MSceneObject) =
                adaptive{
                    let! object  = sceneObject.object o
                    let! trans = sceneObject.trafo o
                    let bounds = object.bounds
                    return bounds.Transformed(trans)
                }    
            m.objects
            |> AMap.toASet       
            |> ASet.fold (fun s (_,(o : MSceneObject)) -> Mod.map2( fun s b-> Box3d.Union(s,b)) s (bounds o)) seed 
            |> Mod.bind id

        //adaptive function to calcualte the light view matrix for one light
        let lightViewMatrix i = 
            let light = AMap.find i m.lights
            Mod.bind (Mod.bind (Shadow.lightViewPoject bb)) light

        //adaptive function to calcualte the shadow map for one light
        let shadowMapTex i = 
            let light = AMap.find i m.lights
            Mod.bind (Mod.bind (Shadow.shadowMap runtime scene bb)) light 

        // lightning pass per light
        let lightSgs = 
            let lightSet =
                m.lights
                |> AMap.toASet
            aset  {
                for  (i,l) in lightSet do
                    let! l' = l
                    let pass = 
                        match l' with 
                        |MDirectionalLight dl ->
                            Mod.map (fun (d : DirectionalLightData)->
                                if d.castsShadow then
                                    Sg.fullScreenQuad
                                    |> Sg.adapter
                                    |> Sg.uniform "Light" (SLEUniform.uniformLight l)
                                    |> Sg.texture (Sym.ofString "ShadowMap") (shadowMapTex i)
                                    |> Sg.uniform "LightViewMatrix" (lightViewMatrix  i |> Mod.map(fun (v,p)  -> v * p))
                                    |> Sg.shader {
                                        do! PBR.getGBufferData
                                        do! PBR.lightingDeferred
                                        do! PBR.shadowDeferred
                                        }
                                else
                                     Sg.fullScreenQuad
                                    |> Sg.adapter
                                    |> Sg.uniform "Light" (SLEUniform.uniformLight l)
                                    |> Sg.shader {
                                        do! PBR.getGBufferData
                                        do! PBR.lightingDeferred
                                        } ) dl
                            |> Sg.dynamic                                
                        |MPointLight _ ->
                            Sg.fullScreenQuad
                            |> Sg.adapter
                            |> Sg.uniform "Light" (SLEUniform.uniformLight l)
                            |> Sg.shader {
                                do! PBR.getGBufferData
                                do! PBR.lightingDeferred
                                }
                    yield  pass
                let pass0 = //global  abient  lightnig
                    Sg.fullScreenQuad
                    |> Sg.adapter
                    |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
                    |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
                    |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
                    |> Sg.texture (Sym.ofString "AmbientOcclusion") ambientOcclusion
                    |> Sg.shader {
                        do! PBR.getGBufferData
                        do! PBR.abientDeferred
                        do! PBR.nonLightedDeferred
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

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
            ]

        //render linear HDR output
        let  output = 
            Sg.set lightSgs
            |> Sg.blendMode (blendMode |> Mod.constant)
            |> Sg.uniform "AmbientIntensity" m.enviorment.ambientLightIntensity
            |> Sg.uniform "CameraLocation" (view |> Mod.map (fun t -> t.Backward.C3.XYZ))
            |> Sg.texture ( DefaultSemantic.Colors) (Map.find DefaultSemantic.Colors gBuffer)
            |> Sg.texture ( Sym.ofString "WPos") (Map.find (Sym.ofString "WorldPosition") gBuffer)
            |> Sg.texture ( DefaultSemantic.Normals) (Map.find DefaultSemantic.Normals gBuffer)
            |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
            |> Sg.texture ( GBufferRendering.Semantic.MaterialProperties) (Map.find GBufferRendering.Semantic.MaterialProperties gBuffer)
            |> Sg.compile runtime signature
            |> RenderTask.renderToColor size    

        //tone mapping and gamma correction
        Sg.fullScreenQuad
        |> Sg.adapter
        |> Sg.texture DefaultSemantic.DiffuseColorTexture output
        |> Sg.uniform "Expousure" m.expousure
        |> Sg.shader {
            do! DefaultSurfaces.diffuseTexture
            do! PBR.gammaCorrection
            }    
        |> Sg.compile runtime outputSignature    

    (*
        For deferrde Rednering and some other techniques it is nessesary to know the size of the render window.
        That is not straitforward in Aardvark.media because there could be multiple clients with different screen sizes.
        The solution is to create a custem scene that takes a function from client values to a IRenderTask and feed that into a RenderControl.
        If I understand it correctly, at runtime a render Task per client will be created with the respectice client values.
        Thanks to Georg Haaser for the tip. 
        Note that the ClientValues contain the Output Framebuffer Signature. That is very usefull because you can obtain a reference to the runtime from the signatur. 
    *)
    let RenderControl (att : list<string * AttributeValue<Message>>) (s : MCameraControllerState) (frustum : Frustum) (task : Aardvark.Service.ClientValues -> IRenderTask) =
        let scene =  Aardvark.Service.Scene.custom task
        let cam : IMod<Camera> = Mod.map (fun v -> { cameraView = v; frustum = frustum }) s.view 
        DomNode.RenderControl(AttributeMap.ofList att, cam, scene, None)
        |> FreeFlyController.withControls s CameraMessage (Mod.constant frustum)

    //the 3D scene and control
    let view3D (m : MModel) =

        let frustum = 
            Frustum.perspective 30.0 0.1 100.0 1.0 

        //generate sceen graph from the objects 
        let objects = 
            aset {
                for _,o in AMap.toASet m.objects do
                    let! s = sceneObject.sg o
                    yield s |> Sg.trafo (sceneObject.trafo o ) 
            }
            |> Sg.set  

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
                attribute "showFPS" "true"
           //     attribute "data-renderalways" "1"
            ]

        let t = compileDeffered objects m
        RenderControl att m.cameraState frustum t

    // main view for UI and  
    let view (m : MModel) =
        let lights' = AMap.toASet m.lights
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
                                button [clazz "ui button"; onClick (fun _ -> RemoveSceneObject (Mod.force m.selectedObject))]  [text "Remove selected Object"]
                                ]]
                        ]
                        m.selectedObject
                        |> Mod.bind (fun n -> AMap.tryFind n m.objects)
                        |> Mod.map (Option.map  (sceneObjectControl.view >> UI.map SceneObjectMessage))
                        |> AList.ofModSingle
                        |> AList.choose id
                        |> Incremental.div AttributeMap.empty
                    ]    
                "Add Light",
                    [
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Point Light"]
                    ]    
                "Change Light",
                    [   //build a list of light views from the set of lights
                        lights'
                        |> ASet.fold 
                            ( fun items (i, l) -> 
                                let name = sprintf "Light %i" i
                                let d = 
                                        Mod.map (fun l -> lightControl.view  l |> UI.map (fun msg -> LightMessage (i, msg))) l
                                        |> AList.ofModSingle
                                        |> Incremental.div AttributeMap.empty
                                
                                let item = 
                                    name, [
                                        d
                                        button [clazz "ui button"; onClick (fun _ -> RemoveLight i); style "margin-top: 5px;width: 100%;" ]  [text "Remove"]
                                        ]
                                item::items
                            ) []
                         //feed that into a accordeon
                        |> Mod.map (fun items -> Html.SemUi.accordionMenu true "ui vertical inverted fluid accordion menu" items)
                        // and that  into  a incremantal div to handel the case that the numbers of lights change
                        |> AList.ofModSingle
                        |> Incremental.div AttributeMap.empty
                    ]
                "Global Enviorment",
                    [
                        globalEnviroment.view m.enviorment |> UI.map GlobalEnviormentMessage
                    ]  
                "Render Settings",
                    [
                         Html.table [                        
                            tr [] [ td [] [text "Exposure"]; td [ style "width: 70%;"] [inputLogSlider {min = 0.01;  max = 10.0; step = 0.01} [] m.expousure SetExpousure]]
                        ]   
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
            threads = Model.Lens.cameraState.Get >> FreeFlyController.threads >> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }