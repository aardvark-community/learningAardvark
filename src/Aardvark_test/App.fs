namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model
open material

type Message =
    | CameraMessage of FreeFlyController.Message
    | LightMessage of int * lightControl.Message
    | RemoveLight  of int
    | AddLight of Light
    | MaterialMessage of materialControl.Message * string
    | SetExpousure of float
    | GlobalEnviormentMessage of globalEnviroment.Message
    | SetCurrentMaterial of string

module App =   

    let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
    let initialView = CameraView.lookAt (V3d(3.0, 2.5, -6.0)) (V3d(0.0, 2.0, 0.0)) (V3d.OIO * 1.0)

    let import = Aardvark.SceneGraph.IO.Loader.Assimp.load @"..\..\..\data\SLE_Gnom4.obj"

    let initial = { 
        cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
        lights = HMap.ofList [(0, light.defaultDirectionalLight)]
        material = material.defaultMaterial
        enviorment = {skyMap = @"..\..\..\data\GrandCanyon_C_YumaPoint\GCanyon_C_YumaPoint_3k.hdr"; 
                      skyMapRotation = Math.PI; 
                      skyMapIntensity = 1.0;
                      ambientLightIntensity = 1.0}
        expousure  = 1.0
        materials = materials import
        currentMaterial = 
            materials  import
            |> HMap.keys 
            |> HSet.toList
            |> List.first
            |> Option.defaultValue "none"
    }

    let update (m : Model) (msg : Message) =
        //compose the update functions from the updates of the sub-model
        match msg with
        | CameraMessage msg ->
            { m with cameraState = FreeFlyController.update m.cameraState msg }
        | LightMessage (i, lms) -> 
            let li' =  m.lights |> HMap.tryFind i
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
        | MaterialMessage (msg, s) ->
            let m' = materialControl.update m.materials.[s] msg
            let materials' =  HMap.update  s (fun _ -> m' ) m.materials 
            { m with materials = materials' }
        | SetExpousure e ->
            { m with expousure = e }
        | GlobalEnviormentMessage msg ->
            { m with enviorment = globalEnviroment.update m.enviorment msg }
        | SetCurrentMaterial s ->
            { m with currentMaterial = s }    

    let makeGBuffer (runtime : IRuntime) (view : IMod<Trafo3d>) projection size skyBoxTexture scene (m : MModel) =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
                Sym.ofString "WorldPosition", RenderbufferFormat.Rgba32f
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
                DefaultSemantic.Normals, RenderbufferFormat.Rgba32f
                DeferredShading.Semantic.MaterialProperties, RenderbufferFormat.Rg32f
            ]

        let skyBox =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.cullMode (Mod.constant CullMode.None)
                |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
                |> Sg.uniform "SkyMapIntensity" m.enviorment.skyMapIntensity
                |> Sg.uniform "CameraLocation" (view |> Mod.map (fun t -> t.Backward.C3.XYZ))
                |> Sg.shader {
                    do! SLESurfaces.skyBoxTrafo
                    do! DeferredShading.skyGBuffer
                }

        scene
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! SLESurfaces.displacementMap
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.diffuseTexture 
            do! SLESurfaces.normalMap 
            do! DeferredShading.gBufferShader
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
                        DeferredShading.Semantic.MaterialProperties]
               ) size 

    let compileDeffered(outputSignature : IFramebufferSignature) (view : IMod<Trafo3d>) (proj : IMod<Trafo3d>) (size : IMod<V2i>) (scene : ISg<'msg>) (m : MModel) =
        let size = size |> Mod.map (fun s -> V2i(max 1 s.X, max 1 s.Y))

        let runtime = outputSignature.Runtime

        let skyBoxTexture = SkyBox.getTexture runtime m.enviorment.skyMap m.enviorment.skyMapRotation

        let gBuffer = makeGBuffer runtime view proj size skyBoxTexture scene m

        let diffuseIrradianceMap = GlobalAmbientLight.diffuseIrradianceMap runtime skyBoxTexture

        let prefilterdSpecColor = GlobalAmbientLight.prefilterdSpecColor runtime skyBoxTexture

        let bRDFLtu = GlobalAmbientLight.BRDFLtu runtime

        let lightViewMatrix = 
            let light = AMap.find 0 m.lights
            Mod.bind (Mod.bind (Shadow.lightViewPoject scene)) light

        let shadowMapTex = 
            let light = AMap.find 0 m.lights
            Mod.bind (Mod.bind (Shadow.shadowMap runtime scene)) light

        Sg.fullScreenQuad
        |> Sg.adapter
        |> SLEUniform.uniformLights m.lights
        |> Sg.uniform "Expousure" m.expousure
        |> Sg.uniform "AmbientIntensity" m.enviorment.ambientLightIntensity
        |> Sg.uniform "CameraLocation" (view |> Mod.map (fun t -> t.Backward.C3.XYZ))
        |> Sg.texture ( DefaultSemantic.Colors) (Map.find DefaultSemantic.Colors gBuffer)
        |> Sg.texture ( Sym.ofString "WPos") (Map.find (Sym.ofString "WorldPosition") gBuffer)
        |> Sg.texture ( DefaultSemantic.Normals) (Map.find DefaultSemantic.Normals gBuffer)
        |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
        |> Sg.texture ( DeferredShading.Semantic.MaterialProperties) (Map.find DeferredShading.Semantic.MaterialProperties gBuffer)
        |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
        |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
        |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
        |> Sg.texture (Sym.ofString "ShadowMap") shadowMapTex
        |> Sg.uniform "LightViewMatrix" (lightViewMatrix |> Mod.map(fun (v,p)  -> v * p))
        |> Sg.shader {
            do! PBR.lightingDeferred
            }
        |> Sg.compile runtime outputSignature

    let getScene (m : MModel) (sg : ISg<'msg>) =
        Aardvark.Service.Scene.custom (fun values ->
            compileDeffered values.signature values.viewTrafo values.projTrafo values.size sg m
        )

    let deferrdRenderControl (att : list<string * AttributeValue<Message>>) (s : MCameraControllerState) (frustum : Frustum) (sg : ISg<'msg>) (m : MModel) =

        let scene = getScene m sg
        let cam : IMod<Camera> = Mod.map (fun v -> { cameraView = v; frustum = frustum }) s.view 
        DomNode.RenderControl(AttributeMap.ofList att, cam, scene, None)
        |> FreeFlyController.withControls s CameraMessage (Mod.constant frustum)

    //the 3D scene and control
    let view3D runtime (m : MModel) =

        let frustum = 
            Frustum.perspective 30.0 0.1 100.0 1.0 

        let figureMesh =            
            import.SubstituteMaterial (fun mat -> Some ({importedMaterial = mat; material = (AMap.find ( removeDigits mat.name) m.materials)} :> IO.Loader.IMaterial))
            |> Sg.adapter

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
                attribute "showFPS" "true"
               // attribute "data-renderalways" "1"
            ]

        deferrdRenderControl att m.cameraState frustum figureMesh m
     (*   let skyBoxTexture = SkyBox.getTexture runtime m.enviorment.skyMap m.enviorment.skyMapRotation

        let skyBox  runtime =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.cullMode (Mod.constant CullMode.None)
                |> Sg.texture (Sym.ofString "SkyCubeMap") skyBoxTexture
                |> Sg.uniform "SkyMapIntensity" m.enviorment.skyMapIntensity
                |> Sg.shader {
                    do! SLESurfaces.skyBoxTrafo
                    do! SLESurfaces.skyTexture
                }
        let diffuseIrradianceMap = GlobalAmbientLight.diffuseIrradianceMap runtime skyBoxTexture

        let prefilterdSpecColor = GlobalAmbientLight.prefilterdSpecColor runtime skyBoxTexture

        let bRDFLtu = GlobalAmbientLight.BRDFLtu runtime

        let lightViewMatrix = 
            let light = AMap.find 0 m.lights
            Mod.bind (Mod.bind (Shadow.lightViewPoject figureMesh)) light

        let shadowMapTex = 
            let light = AMap.find 0 m.lights
            Mod.bind (Mod.bind (Shadow.shadowMap runtime figureMesh)) light

        let sg = 
            figureMesh
            |> SLEUniform.uniformLights m.lights
            |> Sg.uniform "Expousure" m.expousure
            |> Sg.uniform "AmbientIntensity" m.enviorment.ambientLightIntensity
            |> Sg.texture (Sym.ofString "DiffuseIrradiance") diffuseIrradianceMap
            |> Sg.texture (Sym.ofString "PrefilteredSpecColor") prefilterdSpecColor
            |> Sg.texture (Sym.ofString "BRDFLtu") bRDFLtu
            |> Sg.texture (Sym.ofString "ShadowMap") shadowMapTex
            |> Sg.uniform "LightViewMatrix" (lightViewMatrix |> Mod.map(fun (v,p)  -> v * p))
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! SLESurfaces.displacementMap
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.diffuseTexture 
                do! SLESurfaces.normalMap 
                //do! SLESurfaces.lighting false
                do! SLESurfaces.lightingPBR
                }
            |> Sg.andAlso <| light.lightSourceModels m.lights
            |> Sg.andAlso <| (skyBox runtime |> Sg.uniform "Expousure" m.expousure)
            

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
                attribute "showFPS" "true"
               // attribute "data-renderalways" "1"
            ]

        let f = FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
        
        f*)
 
    // main view for UI and  
    let view runtime (m : MModel) =
        let lights' = AMap.toASet m.lights
        require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
            body [] (        // explit html body for our app (adorner menus need to be immediate children of body). if there is no explicit body the we would automatically generate a body for you.

                Html.SemUi.adornerAccordeonMenu [ 
                "Edit Material",
                    [
                        Html.SemUi.dropDown' (m.materials |> AMap.keys |> ASet.toAList) m.currentMaterial SetCurrentMaterial id
                        m.currentMaterial
                        |> Mod.bind (fun c ->
                            m.materials
                            |> AMap.find c
                            |> Mod.map (fun m -> materialControl.view m |> UI.map (fun msg -> MaterialMessage (msg,c)))
                        )
                        |> AList.ofModSingle
                        |> Incremental.div AttributeMap.empty
                    ]    
                "Add Light",
                    [
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> AddLight light.defaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Point Light"]
                    ]    
                "Change Light",
                    [
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
                        |> Mod.map (fun items -> Html.SemUi.accordionMenu true "ui vertical inverted fluid accordion menu" items)
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
                ] [view3D runtime m]
            )
        )

    let app runtime=
        {
            initial = initial
            update = update
            view = view runtime
            threads = Model.Lens.cameraState.Get >> FreeFlyController.threads >> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }