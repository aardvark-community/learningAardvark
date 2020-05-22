namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model
(*
    Light UI control
    and light uniforms for shaders 
    and some light defaults
*)
module light = 

    let defaultDirectionalLight = DirectionalLight  {lightDirection = V4d(0.0,-1.0,1.0,1.0); color = C3d.White; intensity = 1.0; castsShadow = true}

    let defaultPointLight = PointLight  {lightPosition = V4d(0.0,1.5,-0.5,1.0); color = C3d.White; attenuationQad = 1.0; attenuationLinear = 0.0; intensity = 1.0}

    let defaultLight = defaultPointLight

    let defaultAbientOcclusion = {occlusionStrength = 1.0; scale = 1.0; radius = 0.2; samples = 32; threshold = 0.2; sigma = 2.0; sharpness = 1.0}
   
    //simple geometry to indicate point light positions
    let lightSourceModels (lights : amap<int,aval<AdaptiveLightCase>> ) =
        let lights' = AMap.toASet lights
        aset {
            for l in  lights' do
                let! l' = snd l
                let  m = 
                    match l' with
                    | AdaptiveDirectionalLight ld -> Sg.empty
                    | AdaptivePointLight lp -> 
                        Sg.sphere 6 (AVal.map ( fun (v : PointLightData) -> v.color.ToC4b()) lp ) (AVal.constant 0.03) 
                        |> Sg.translate' (AVal.map ( fun v -> v.lightPosition.XYZ) lp)
                yield m 
        } 
        |> Sg.set
        //simpel shader independend of light 
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor 
            } 

// a control to set the properties of a single light
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
        | ToggleCastShadow

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
        | ToggleCastShadow   ->  
            match m with
            | DirectionalLight r -> 
                DirectionalLight {r with castsShadow = not r.castsShadow} 
            | x -> x

    let attenuationView (l : aval<float>) (q : aval<float>)=
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "2"] [text "Attenuation"] ]                          
            tr [] [ td [] [numInput "Linear" SetAttenuationLinear l]
                    td [] [numInput "Quatratic" SetAttenuationQad q]]
        ] 

    let intensityView (i : aval<float>) (c : aval<C4b>)=
        let numInput name changed state  = labeledFloatInput name 0.0 Double.MaxValue 1.0 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "3"] [text "Light"] ]                          
           //Important: to make  the colorpicker work, the  assemblyWebpart for Aardvark.UI.Primitives needs to be registert in Program.fs
            tr [] [ td [] [text "Color"]; td [] [ColorPicker.viewSimple c (C3d.FromC4b >> SetColor)]
                    td [] [numInput "Intensity" SetIntensity i]]
         ] 

    let view (m : AdaptiveLightCase) =
        match m with
        |AdaptiveDirectionalLight l' -> 
            let i = AVal.map (fun (l : DirectionalLightData) -> l.intensity) l'
            let c = AVal.map (fun (l : DirectionalLightData) -> l.color.ToC4b()) l'
            div [] [
                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"] 
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun l -> l.lightDirection.XYZ) l'
                |> V3dInput.view "Direction"
                |> UI.map SetLightDirection

                intensityView i c
                Html.table [
                    tr [] [ td [] [text "Cast Shadow"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  (AVal.map (fun l -> l.castsShadow) l') ToggleCastShadow ]]
                ]
            ]                  
        |AdaptivePointLight l' -> 
            let al = AVal.map (fun l -> l.attenuationLinear) l'
            let aq = AVal.map (fun l -> l.attenuationQad) l'
            let i = AVal.map (fun l -> l.intensity) l'
            let c = AVal.map (fun (l : PointLightData) -> l.color.ToC4b()) l'
            div [] [

                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> DefaultDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun l -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                intensityView i c

                attenuationView al aq
            ]

module Shadow =
    open Aardvark.SceneGraph
    open Aardvark.UI //nessary to avoid confusion between SceneGraph.SG and UI.SG 

    
    let signatureShadowMap (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]

    let shadowMapSize = V2i(1024) |> AVal.constant

    //calculate light view and prjection
    let lightViewPoject (bb : aval<Box3d>) (light : AdaptiveLightCase) =
        match light with
        | AdaptivePointLight l -> failwith "not implemented"
        | AdaptiveDirectionalLight l -> 
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

    let shadowMap (runtime : IRuntime) (scene :ISg<'msg>) (bb : aval<Box3d>) (light : AdaptiveLightCase) =
            let pv = lightViewPoject bb light
            let v = pv |> AVal.map fst
            let p = pv |> AVal.map snd
            scene
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                }
            |> Sg.viewTrafo (v)
            |> Sg.projTrafo (p)
            |> Sg.compile runtime (signatureShadowMap runtime)
            |> RenderTask.renderToDepth shadowMapSize
            :> aval<_>

module SLEUniform =
    //light information as shader uniforms

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

    let uniformLight (l : AdaptiveLightCase) : aval<Light>  =
        //needs to be adaptive because the  Light can change and is an IMod
        //we go from aval<MLight> to aval<ISg<Message>>
        adaptive {
            let d = l
            match d with
            | AdaptiveDirectionalLight  x' ->
               let! x  = x'
                //Map to a type more convinient in the shaders
               let r : Light = {lightType = LightType.DirectionalLight; lightPosition = x.lightDirection; color = x.color.ToV3d() * x.intensity; attenuationQad = 0.0; attenuationLinear = 0.0; castsShadow = x.castsShadow}
               return  r
            | AdaptivePointLight  x' ->
               let! x  = x'
               let r : Light = {lightType = LightType.PointLight; lightPosition = x.lightPosition; color = x.color.ToV3d() * x.intensity; attenuationQad = x.attenuationQad; attenuationLinear = x.attenuationLinear; castsShadow = false}
               return r
        } 

    let uniformLights (lights : amap<int,AdaptiveLightCase>)   =
        let lights' = AMap.toASet lights
        let numLights = ASet.count lights'
        let a =  Array.init 10 (fun _ -> noLight )
        let u = aset{
                for l  in  lights' do
                    let! l =  snd l |> uniformLight
                    yield l
                }
                |> ASet.fold (fun ((i : int), (a : Light [])) l -> a.[i] <- l; (i+1, a)) (0,a)
                |> AVal.map (fun (i, a) -> a)
                |> Sg.uniform "Lights" 
        
        u >> Sg.uniform "NumLights" numLights
