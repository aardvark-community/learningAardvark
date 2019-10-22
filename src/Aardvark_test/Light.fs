namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark_test.Model

module light = 
    
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
            tr [] [ td [attribute "colspan" "2"] [text "Attenuation"] ]                          
            tr [] [ td [] [numInput "Linear" SetAttenuationLinear l]
                    td [] [numInput "Quatratic" SetAttenuationQad q]]
        ] 

    let intensityView (i : IMod<float>) (c : IMod<C4b>)=
        let numInput name changed state  = labeledFloatInput name 0.0 Double.MaxValue 1.0 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "3"] [text "Light"] ]                          
           //Important: to make  the colorpicker work, the  assemblyWebpart for Aardvark.UI.Primitives needs to be registert in Program.fs
            tr [] [ td [] [text "Color"]; td [] [ColorPicker.viewSimple c (fun (c : C4b) -> (C3d.FromC4b).Invoke(c) |> SetColor)]
                    td [] [numInput "Intensity" SetIntensity i]]
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
module Shadow =
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics
    open Aardvark.UI

    
    let signatureShadowMap (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]

    let shadowMapSize = V2i(1024) |> Mod.constant

    let lightViewPoject (scene :ISg<'msg>) (light : MLight) =
        let s = scene :>  ISg
        let bb = s.GlobalBoundingBox()
        match light with
        | MPointLight l -> failwith "not implemented"
        | MDirectionalLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let distance = max BB.Min.Length BB.Max.Length
                let size = (BB.Max - BB.Min).Length
                let lightPos = -light.lightDirection.XYZ |> Vec.normalize |> (*) distance //make sure the light position is outside the sceneBB
                let up = if abs(lightPos.Z) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt lightPos V3d.OOO up
                    |> CameraView.viewTrafo 
                let b = BB.Transformed(lightView)
                let bb = Box3d(V3d(b.Min.XY,0.0001),V3d(b.Max.XY,size*2.0))//set Z Size so that all the scene fits in all cases (size*2.0 ist the upper bound, could be optimized)
                let proj = 
                    Frustum.ortho bb
                    |> Frustum.projTrafo
                return lightView , proj
            }

    let shadowMap (runtime : IRuntime) (scene :ISg<'msg>) (light : MLight) =
            let v = lightViewPoject scene light |> Mod.map fst
            let p = lightViewPoject scene light |> Mod.map snd
            scene
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                }
            |> Sg.viewTrafo (v)
            |> Sg.projTrafo (p)
            |> Sg.compile runtime (signatureShadowMap runtime)
            |> RenderTask.renderToDepth shadowMapSize

module SLEUniform =

    type LightType =
        | NoLight = 0
        | DirectionalLight = 1
        | PointLight = 2

    type Light = {
        lightType : LightType
        lightPosition : V4d
        color : V3d
        attenuationQad :float
        attenuationLinear :float
        castsShadow: bool
    }
     
    let noLight = {lightType = LightType.NoLight; lightPosition = V4d.Zero; color = V3d.Zero; attenuationQad = 0.0; attenuationLinear = 0.0; castsShadow = false}

    let uniformLight (l : IMod<MLight>) : IMod<Light>  =
        //needs to be adaptive because the  Light can change and is an IMod
        //we go from IMod<MLight> to IMod<ISg<Message>>
        adaptive {
            let! d = l
            match d with
            | MDirectionalLight  x' ->
               let! x  = x'
                //Map to a type more convinient in the shaders
               let r : Light = {lightType = LightType.DirectionalLight; lightPosition = x.lightDirection; color = x.color.ToV3d() * x.intensity; attenuationQad = 0.0; attenuationLinear = 0.0; castsShadow = true}
               return  r
            | MPointLight  x' ->
               let! x  = x'
               let r : Light = {lightType = LightType.PointLight; lightPosition = x.lightPosition; color = x.color.ToV3d() * x.intensity; attenuationQad = x.attenuationQad; attenuationLinear = x.attenuationLinear; castsShadow = false}
               return r
        } 

    let uniformLights (lights : amap<int,IMod<MLight>>)   =
        let lights' = AMap.toASet lights
        let numLights = ASet.count lights'
        let a =  Array.init 10 (fun _ -> noLight )
        let u = aset{
                for l  in  lights' do
                    let! l =  snd l |> uniformLight
                    yield l
                }
                |> ASet.fold (fun ((i : int), (a : Light [])) l -> a.[i] <- l; (i+1, a)) (0,a)
                |> Mod.map (fun (i, a) -> a)
                |> Sg.uniform "Lights" 
        
        u >> Sg.uniform "NumLights" numLights
