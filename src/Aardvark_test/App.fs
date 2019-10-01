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
    | MaterialMessage of materialControl.Message

module App =   


    let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
    let initialView = CameraView.lookAt (V3d(2.0, 2.0, -3.0)) (V3d(0.0, 1.0, 0.0)) (V3d.OIO * 1.0)
    let initial = { 
        cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
        lights = HMap.ofList [(0, light.defaultDirectionalLight)]
        currentLightIndex  = 0
        material = material.defaultMaterial
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
        | MaterialMessage msg ->
            { m with material = materialControl.update m.material msg }

    let figureMesh =
        Aardvark.SceneGraph.IO.Loader.Assimp.load @"..\..\..\data\SLE_Gnom4.obj"
        |> Sg.adapter
        //|> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
        |> Sg.transform (Trafo3d.Scale(1.0,1.0,1.0))
    
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
    
    let materialUniforms (m : MPBRMaterial) =
        Sg.uniform "Metallic" m.metallic
        >> Sg.uniform "Roughness" m.roughness
        >> Sg.uniform "AlbedoFactor" m.albedoFactor


    let skyMapequirectengular : ISg<Message> -> ISg<Message> = 
        let texture =  FileTexture(@"..\..\..\data\GrandCanyon_C_YumaPoint\GCanyon_C_YumaPoint_3k.hdr", { wantCompressed = false; wantMipMaps = true; wantSrgb = true }) :> ITexture
        Sg.texture (Sym.ofString "SkyMapEquirec")(texture |> Mod.constant)

    let skyBoxEquirec  =
        Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
            |> skyMapequirectengular
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! SLESurfaces.skyTextureEquirec
            }

    let signature (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgb16f; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

    let size = 1024 |> Mod.init 

    let equirectengularToCubeMapTask  runtime face =
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

        skyBoxEquirec
        |> Sg.viewTrafo (
                CameraView.lookAt V3d.OOO lookTo (lookSky * -1.0)
                 |> CameraView.viewTrafo 
                 |> Mod.constant
           )
        |> Sg.projTrafo (size |> Mod.map (fun actualSize -> 
                Frustum.perspective 90.0 0.01 1.0 1.0 |> Frustum.projTrafo
              )
           )
        // next, we use Sg.compile in order to turn a scene graph into a render task (a nice composable alias for runtime.CompileRender)
        |> Sg.compile runtime (signature runtime)

    let skyCubeMap (runtime : IRuntime) =
        Log.warn "skyCubeMap"
        RenderTask.renderToColorCube size (equirectengularToCubeMapTask  runtime)

    let sizeD = 512 |> Mod.init 

    let convoluteDiffuseIrradianceTask  runtime face =
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

        Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.texture (Sym.ofString "SkyCubeMap") (skyCubeMap runtime)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! SLESurfaces.convoluteDiffuseIrradiance
            }
            |> Sg.viewTrafo (
                CameraView.lookAt V3d.OOO lookTo (lookSky * -1.0)
                 |> CameraView.viewTrafo 
                 |> Mod.constant
           )
        |> Sg.projTrafo (sizeD |> Mod.map (fun actualSize -> 
                Frustum.perspective 90.0 0.01 1.0 1.0 |> Frustum.projTrafo
              )
           )
        // next, we use Sg.compile in order to turn a scene graph into a render task (a nice composable alias for runtime.CompileRender)
        |> Sg.compile runtime (signature runtime)

    let diffuseIrradianceMap (runtime : IRuntime) =
        RenderTask.renderToColorCube sizeD (convoluteDiffuseIrradianceTask  runtime)

    let skyBox  runtime =
        Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d(-V3d.III,V3d.III)))
            |> Sg.cullMode (Mod.constant CullMode.None)
            //|> Sg.depthTest (Mod.constant DepthTestMode.None)
            //|> Sg.translate 3.0 0.0 0.0
            |> Sg.texture (Sym.ofString "SkyCubeMap") (skyCubeMap runtime)
            |> Sg.shader {
                do! SLESurfaces.skyBoxTrafo
                do! SLESurfaces.skyTexture
            }

    //the 3D scene and control
    let view3D runtime (m : MModel) =
        let frustum = 
            Frustum.perspective 30.0 0.1 100.0 1.0 
                |> Mod.constant

        let sg = 
            figureMesh
            |> uniformLights m.lights
            |> materialUniforms m.material
            |> Sg.texture (Sym.ofString "DiffuseIrradiance") (diffuseIrradianceMap runtime)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.diffuseTexture 
                //do! SLESurfaces.lighting false
                do! SLESurfaces.lightingPBR
                }
            |> Sg.andAlso <| lightSourceModels m.lights
            |> Sg.andAlso <| skyBox runtime

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
                attribute "showFPS" "true"
               // attribute "data-renderalways" "1"
            ]

        FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
 
    // main view for UI and  
    let view runtime (m : MModel) =
        let lights' = AMap.toASet m.lights
        require Html.semui ( // we use semantic ui for our gui. the require function loads semui stuff such as stylesheets and scripts
            body [] (        // explit html body for our app (adorner menus need to be immediate children of body). if there is no explicit body the we would automatically generate a body for you.

                Html.SemUi.adornerAccordeonMenu [ 
                "Edit Material",
                    [
                        materialControl.view m.material |> UI.map MaterialMessage
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