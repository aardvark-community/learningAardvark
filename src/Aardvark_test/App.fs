namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model

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

    let sceneBB = import.bounds

    let getMaterials (s : IO.Loader.Scene) =
            let rec traverse (state : IO.Loader.IMaterial list) (n : IO.Loader.Node) =
                match n with
                    | IO.Loader.Node.Empty -> 
                        state
                    | IO.Loader.Node.Group es ->
                        List.fold traverse state es 
                    | IO.Loader.Node.Leaf m ->
                        state
                    | IO.Loader.Node.Material(m, n) ->
                        m::state
                    | IO.Loader.Node.Trafo(t, n) ->
                        traverse state n

            traverse [] s.root 

    let removeDigits = String.filter (Char.IsDigit >> not)

    let materials = 
        getMaterials import
        |> List.map (fun m -> removeDigits m.name)
        |> List.distinct
        |> List.map (fun n -> n, material.defaultMaterial)
        |> HMap.ofList

    let initial = { 
        cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
        lights = HMap.ofList [(0, light.defaultDirectionalLight)]
        material = material.defaultMaterial
        enviorment = {skyMap = @"..\..\..\data\GrandCanyon_C_YumaPoint\GCanyon_C_YumaPoint_3k.hdr"; 
                      skyMapRotation = Math.PI; 
                      skyMapIntensity = 1.0;
                      ambientLightIntensity = 1.0}
        expousure  = 1.0
        materials = materials
        currentMaterial = 
            HMap.keys materials
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
    
    let uniformLight (l : IMod<MLight>) : IMod<SLEUniform.Light>  =
        //needs to be adaptive because the  Light can change and is an IMod
        //we go from IMod<MLight> to IMod<ISg<Message>>
        adaptive {
            let! d = l
            match d with
            | MDirectionalLight  x' ->
               let! x  = x'
                //Map to a type more convinient in the shaders
               let r : SLEUniform.Light = {lightType = SLEUniform.LightType.DirectionalLight; lightPosition = x.lightDirection; color = x.color.ToV3d() * x.intensity; attenuationQad = 0.0; attenuationLinear = 0.0}
               return  r
            | MPointLight  x' ->
               let! x  = x'
               let r : SLEUniform.Light = {lightType = SLEUniform.LightType.PointLight; lightPosition = x.lightPosition; color = x.color.ToV3d() * x.intensity; attenuationQad = x.attenuationQad; attenuationLinear = x.attenuationLinear}
               return r
        } 

    let uniformLights (lights : amap<int,IMod<MLight>>)   =
        let lights' = AMap.toASet lights
        let numLights = ASet.count lights'
        let a =  Array.init 10 (fun _ -> SLEUniform.noLight )
        let u = aset{
                for l  in  lights' do
                    let! l =  snd l |> uniformLight
                    yield l
                }
                |> ASet.fold (fun ((i : int), (a : SLEUniform.Light [])) l -> a.[i] <- l; (i+1, a)) (0,a)
                |> Mod.map (fun (i, a) -> a)
                |> Sg.uniform "Lights" 
        
        u >> Sg.uniform "NumLights" numLights

    let lightSourceModels (lights : amap<int,IMod<MLight>> ) =
        let lights' = AMap.toASet lights
        aset {
            for l in  lights' do
                let! l' = snd l
                let  m = 
                    match l' with
                    | MDirectionalLight ld -> Sg.empty
                    | MPointLight lp -> 
                        Sg.sphere 6 (Mod.map ( fun v -> v.color.ToC4b()) lp ) (Mod.constant 0.03) 
                        |> Sg.translate' (Mod.map ( fun v -> v.lightPosition.XYZ) lp)
                yield m 
        } 
        |> Sg.set
        //simpel shader independend of light 
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor 
            }   

    type ProxyMaterial =
        {
            importedMaterial : IO.Loader.IMaterial
            material : IMod<MPBRMaterial>
        }
        
        interface IO.Loader.IMaterial with

            member x.name = x.importedMaterial.name

            member x.TryGetUniform(s, sem) =
                match string sem with
                | "Metallic" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.metallic) x.material :> IMod)
                | "Roughness" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.roughness) x.material :> IMod)
                | "AlbedoFactor" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.albedoFactor) x.material :> IMod)
                | "NormalMapStrength" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.normalMapStrenght) x.material :> IMod)
                | "Discard" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.discard) x.material :> IMod)
                | _ -> x.importedMaterial.TryGetUniform(s, sem)

            member x.Dispose() = x.importedMaterial.Dispose()

    let renderToCubeTask runtime size signature source level face =
        let lookTo = 
            match face with
            |CubeSide.PositiveY  -> V3d.OIO
            |CubeSide.PositiveZ -> V3d.OOI
            |CubeSide.PositiveX -> V3d.IOO
            |CubeSide.NegativeZ-> V3d.OOI * -1.0
            |CubeSide.NegativeX -> V3d.IOO * -1.0
            |CubeSide.NegativeY -> V3d.OIO * -1.0
            |_ -> failwith "unexpected enum"
        let lookSky = 
            match face with
            |CubeSide.PositiveY  -> V3d.OOI * -1.0
            |CubeSide.PositiveZ -> V3d.OIO
            |CubeSide.PositiveX -> V3d.OIO
            |CubeSide.NegativeZ-> V3d.OIO
            |CubeSide.NegativeX -> V3d.OIO
            |CubeSide.NegativeY -> V3d.OOI
            |_ -> failwith "unexpected enum"

        source level
        |> Sg.viewTrafo (
            CameraView.lookAt V3d.OOO lookTo (lookSky * -1.0)
             |> CameraView.viewTrafo 
             |> Mod.constant
        )
        |> Sg.projTrafo (size |> Mod.map (fun actualSize -> 
                Frustum.perspective 90.0 0.01 1.0 1.0 |> Frustum.projTrafo
              )
           )
        |> Sg.compile runtime (signature runtime)

    let renderToCubeMip (runtime : IRuntime) size levels signature source=
        RenderTask.renderToColorCubeMip size levels (renderToCubeTask runtime size signature source)

    let renderToCube (runtime : IRuntime) size signature source=
        RenderTask.renderToColorCubeMip size 1 (renderToCubeTask  runtime size signature (fun _ ->  source))
    
    let signature (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgb16f; samples = 1 }
        ]

    let signatureBRDFLtu (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rg16f; samples = 1 }
        ]

    let LtuSize = V2i(512,512) |> Mod.init 

    // create a scenegraph for the offscreen render passt
    let BRDFLtuTask (runtime : IRuntime)= 
        Sg.fullScreenQuad
        |>Sg.adapter
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! SLESurfaces.integrateBRDFLtu
        }
        |> Sg.viewTrafo (Trafo3d.Identity |> Mod.constant )
        |> Sg.projTrafo (Trafo3d.Identity |> Mod.constant )
        |> Sg.compile runtime (signatureBRDFLtu runtime)
    
    let BRDFLtu (runtime : IRuntime) =
        RenderTask.renderToColor LtuSize (BRDFLtuTask runtime)

    let signatureShadowMap (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]

    let shadowMapSize = V2i(1024) |> Mod.constant

    let lightViewPoject (light : MLight) =
        match light with
        | MPointLight l -> failwith "not implemented"
        | MDirectionalLight l -> 
            adaptive {
                let! light = l 
                let distance = max sceneBB.Min.Length sceneBB.Max.Length
                let size = (sceneBB.Max - sceneBB.Min).Length
                let lightPos = -light.lightDirection.XYZ |> Vec.normalize |> (*) distance //make sure the light position is outside the sceneBB
                let up = if abs(lightPos.Z) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt lightPos V3d.OOO up
                    |> CameraView.viewTrafo 
                let b = sceneBB.Transformed(lightView)
                let bb = Box3d(V3d(b.Min.XY,0.0001),V3d(b.Max.XY,size*2.0))//set Z Size so that all the scene fits in all cases (size*2.0 ist the upper bound, could be optimized)
                let proj = 
                    Frustum.ortho bb
                    |> Frustum.projTrafo
                return lightView , proj
            }

    let shadowMap (runtime : IRuntime) (scene :ISg<'msg>) (light : MLight) =
            let v = lightViewPoject light |> Mod.map fst
            let p = lightViewPoject light |> Mod.map snd
            scene
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                }
            |> Sg.viewTrafo (v)
            |> Sg.projTrafo (p)
            |> Sg.compile runtime (signatureShadowMap runtime)
            |> RenderTask.renderToDepth shadowMapSize

    //the 3D scene and control
    let view3D runtime (m : MModel) =

        let figureMesh =            
            import.SubstituteMaterial (fun mat -> Some ({importedMaterial = mat; material = (AMap.find ( removeDigits mat.name) m.materials)} :> IO.Loader.IMaterial))
            |> Sg.adapter

        let skyMapSize = 1024 |> Mod.init 

        let diffuseIrradianceSize = 32 |> Mod.init 

        let skyMapequirectengular : ISg<Message> -> ISg<Message> = 
            let texture =  
                Mod.map (fun s -> FileTexture(s, { wantCompressed = false; wantMipMaps = false; wantSrgb = false }) :> ITexture) m.enviorment.skyMap
            Sg.texture (Sym.ofString "SkyMapEquirec") texture 

        let skyBoxEquirec =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.uniform "SkyMapRotation" m.enviorment.skyMapRotation
                |> skyMapequirectengular
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! SLESurfaces.skyTextureEquirec
                }

        let skyCubeMap (runtime : IRuntime) = 
            renderToCube runtime skyMapSize signature (skyBoxEquirec)

        let diffuseIrradianceBox (runtime : IRuntime) =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.texture (Sym.ofString "SkyCubeMap") (skyCubeMap runtime)
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! SLESurfaces.convoluteDiffuseIrradiance
                }       

        let diffuseIrradianceMap (runtime : IRuntime) = 
            renderToCube runtime diffuseIrradianceSize signature (diffuseIrradianceBox runtime) 

        let prefilterSpecBox (runtime : IRuntime) (level : int) =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.texture (Sym.ofString "SkyCubeMap") (skyCubeMap runtime)
                |> Sg.uniform "Roughness" (Mod.constant (float level / 4.0) )
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! SLESurfaces.prefilterSpec
                } 

        let prefilterSpecSize = 128 |> Mod.init 
        let prefilterdSpecColor (runtime : IRuntime) = 
            renderToCubeMip runtime prefilterSpecSize 5 signature (prefilterSpecBox runtime) 

        let skyBox  runtime =
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
                |> Sg.cullMode (Mod.constant CullMode.None)
                |> Sg.texture (Sym.ofString "SkyCubeMap") (skyCubeMap runtime)
                |> Sg.uniform "SkyMapIntensity" m.enviorment.skyMapIntensity
                |> Sg.shader {
                    do! SLESurfaces.skyBoxTrafo
                    do! SLESurfaces.skyTexture
                }

        let frustum = 
            Frustum.perspective 30.0 0.1 100.0 1.0 
                |> Mod.constant

        let lightViewMatrix = 
            let light = AMap.find 0 m.lights
            Mod.bind (Mod.bind lightViewPoject) light

        let shadowMapTex = 
            let light = AMap.find 0 m.lights
            Mod.bind (Mod.bind (shadowMap runtime figureMesh)) light

        let sg = 
            figureMesh
            |> uniformLights m.lights
            |> Sg.uniform "Expousure" m.expousure
            |> Sg.uniform "AmbientIntensity" m.enviorment.ambientLightIntensity
            |> Sg.texture (Sym.ofString "Displacment") (FileTexture (@"..\..\..\data\SLE_Gnom_disp3.jpg", TextureParams.empty) :>  ITexture |> Mod.constant)
            |> Sg.texture (Sym.ofString "DiffuseIrradiance") (diffuseIrradianceMap runtime)
            |> Sg.texture (Sym.ofString "PrefilteredSpecColor") (diffuseIrradianceMap runtime)
            |> Sg.texture (Sym.ofString "BRDFLtu") (BRDFLtu runtime)
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
            |> Sg.andAlso <| lightSourceModels m.lights
            (*|> Sg.andAlso 
                <|(Sg.fullScreenQuad
                    |> Sg.adapter
                    |> Sg.texture (DefaultSemantic.DiffuseColorTexture)  shadowMapTex
                    |> Sg.shader {
                        do! DefaultSurfaces.vertexColor
                        do! SLESurfaces.testShadowMap 
                    })*)
            |> Sg.andAlso <| (skyBox runtime |> Sg.uniform "Expousure" m.expousure)
            

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
                attribute "showFPS" "true"
               // attribute "data-renderalways" "1"
            ]

        let f = FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
        
        let x = Frustum.top
        
        f
 
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