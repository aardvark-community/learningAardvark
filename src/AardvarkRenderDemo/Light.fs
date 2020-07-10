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

    let defaultDirectionalLight = DirectionalLight  {
        lightDirection = V4d(0.0,-1.0,1.0,1.0); 
        color = C3d.White; 
        intensity = 1.0; 
        castsShadow = true
    }

    let defaultPointLight = PointLight  {
        lightPosition = V4d(0.0,1.5,-0.5,1.0); 
        color = C3d.White; 
        attenuationQad = 1.0; 
        attenuationLinear = 0.0; 
        intensity = 1.0
    }

    let defaultSphereLight = SphereLight  {
        lightPosition = V4d(0.0,1.5,-0.5,1.0); 
        color = C3d.White; 
        radius = 0.2
        intensity = 1.0
    }

    let defaultSpotLight = SpotLight  {
        lightPosition = V4d(0.0,1.5,-0.5,1.0); 
        lightDirection = V4d(0.0,-1.0,1.0,1.0); 
        color = C3d.White; 
        attenuationQad = 1.0; 
        attenuationLinear = 0.0; 
        intensity = 1.0; 
        cutOffInner = 30.0; 
        fallOff = 10.0; 
        castsShadow = true
    }

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
                    | AdaptiveSpotLight ls -> Sg.empty
                    | AdaptivePointLight lp -> 
                        Sg.sphere 6 (AVal.map ( fun (v : PointLightData) -> v.color.ToC4b()) lp ) (AVal.constant 0.03) 
                        |> Sg.translate' (AVal.map ( fun (v : PointLightData) -> v.lightPosition.XYZ) lp)
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
        | DefaultSpotLight
        | DefaultSphereLight
        | ToDirectionalLight
        | ToPointLight
        | ToSpotLight
        | ToSphereLight
        | SetLightDirection of V3dInput.Message
        | SetLightPosition of V3dInput.Message
        | SetAttenuationQad of float
        | SetAttenuationLinear of float
        | SetIntensity of float
        | SetColor of C3d
        | SetCutOffInner of float
        | SetFallOff of float
        | SetRadius of float
        | ToggleCastShadow

    let update (m : Light) (msg : Message) =
        match msg with
        | ToDirectionalLight ->
            match m with
            | PointLight r -> 
                DirectionalLight  {
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    castsShadow = true
                }
            | SpotLight r -> 
                DirectionalLight  {
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity
                    castsShadow = true
                }
            | SphereLight r -> 
                DirectionalLight  {
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    castsShadow = true
                }
            | x -> x
        | ToPointLight ->
            match m with
            | DirectionalLight r -> 
                PointLight  {
                    lightPosition = -r.lightDirection
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity
                }
            | SpotLight r -> 
                PointLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    attenuationQad = r.attenuationQad 
                    attenuationLinear = r.attenuationLinear
                    intensity = r.intensity
                }
            | SphereLight r -> 
                PointLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity
                }
            | x -> x
        | ToSphereLight ->
            match m with
            | DirectionalLight r -> 
                SphereLight  {
                    lightPosition = -r.lightDirection
                    color = r.color
                    intensity = r.intensity
                    radius = 0.2
                }
            | SpotLight r -> 
                SphereLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    radius = 0.2
                }
            | PointLight r -> 
                SphereLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    radius = 0.2
                }
            | x -> x
        | ToSpotLight ->
            match m with
            | DirectionalLight r -> 
                SpotLight  {
                    lightPosition = -r.lightDirection
                    lightDirection = r.lightDirection
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = r.castsShadow
                }
            | PointLight r -> 
                SpotLight  {
                    lightPosition = r.lightPosition
                    lightDirection = -r.lightPosition
                    color = r.color
                    attenuationQad = r.attenuationQad
                    attenuationLinear = r.attenuationLinear
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = true
                }
            | SphereLight r -> 
                SpotLight  {
                    lightPosition = r.lightPosition
                    lightDirection = -r.lightPosition
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = true
                }
            | x -> x
        | DefaultDirectionalLight -> light.defaultDirectionalLight
        | DefaultPointLight -> light.defaultPointLight
        | DefaultSpotLight -> light.defaultSpotLight
        | DefaultSphereLight -> light.defaultSphereLight
        | SetLightDirection vMsg  ->  
            match m with
            | DirectionalLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                DirectionalLight {r with lightDirection = V4d(n, 0.0)} 
            | SpotLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                SpotLight {r with lightDirection = V4d(n, 0.0)} 
            | x -> x
        | SetLightPosition vMsg ->  
            match m with
            | PointLight r -> 
                let n = V3dInput.update (r.lightPosition.XYZ) vMsg
                PointLight {r with lightPosition = V4d(n, 1.0)} 
            | SphereLight r -> 
                let n = V3dInput.update (r.lightPosition.XYZ) vMsg
                SphereLight {r with lightPosition = V4d(n, 1.0)} 
            | SpotLight r -> 
                let n = V3dInput.update (r.lightPosition.XYZ) vMsg
                SpotLight {r with lightPosition = V4d(n, 1.0)} 
            | x -> x
        | SetAttenuationQad v ->  
            match m with
            | PointLight r -> 
                PointLight {r with attenuationQad = v} 
            | SpotLight r -> 
                SpotLight {r with attenuationQad = v} 
            | x -> x
        | SetAttenuationLinear v ->  
            match m with
            | PointLight r -> 
                PointLight {r with attenuationLinear = v} 
            | SpotLight r -> 
                SpotLight {r with attenuationLinear = v} 
            | x -> x
        | SetIntensity i ->  
            match m with
            | PointLight r -> 
                PointLight {r with intensity = i} 
            | DirectionalLight r -> 
                DirectionalLight {r with intensity = i} 
            | SpotLight r -> 
                SpotLight {r with intensity = i} 
            | SphereLight r -> 
                SphereLight {r with intensity = i} 
        | SetColor c ->  
            match m with
            | PointLight r -> 
                PointLight {r with color = c} 
            | DirectionalLight r -> 
                DirectionalLight {r with color = c} 
            | SpotLight r -> 
                SpotLight {r with color = c} 
            | SphereLight r -> 
                SphereLight {r with color = c} 
         | SetCutOffInner w ->  
            match m with
            | SpotLight r -> 
                SpotLight {r with cutOffInner = w} 
            | x -> x
         | SetFallOff w ->  
            match m with
            | SpotLight r -> 
                SpotLight {r with fallOff = w} 
            | x -> x
         | SetRadius w ->  
            match m with
            | SphereLight r -> 
                SphereLight {r with radius = w} 
            | x -> x
         | ToggleCastShadow   ->  
            match m with
            | DirectionalLight r -> 
                DirectionalLight {r with castsShadow = not r.castsShadow} 
            | SpotLight r -> 
                SpotLight {r with castsShadow = not r.castsShadow} 
            | x -> x

    let attenuationView (l : aval<float>) (q : aval<float>)=
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "2"] [text "Attenuation"] ]                          
            tr [] [ td [] [numInput "Linear" SetAttenuationLinear l]
                    td [] [numInput "Quatratic" SetAttenuationQad q]]
        ] 

    let cutOffView (c : aval<float>) (f : aval<float>) =
        let numInput name changed state  = labeledFloatInput name 0.0 360.0 1.0 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "4"] [text "Cutoff"] ]                          
            tr [] [ td [] [numInput "Cutoff inner" SetCutOffInner c]
                    td [] [numInput "Falloff" SetFallOff f]
                   ]
        ]

    let radiusView (r : aval<float>) =
        let numInput name changed state  = labeledFloatInput name 0.0 10.0 0.1 changed state
        Html.table [ 
           tr [] [ td [] [numInput "Radius" SetRadius r]
                   ]
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
                        button [clazz "ui button"; onClick (fun _ -> ToPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"] 
                        button [clazz "ui button"; onClick (fun _ -> ToSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Spot Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Sphere Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : DirectionalLightData) -> l.lightDirection.XYZ) l'
                |> V3dInput.view "Direction"
                |> UI.map SetLightDirection

                intensityView i c
                Html.table [
                    tr [] [ td [] [text "Cast Shadow"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  (AVal.map (fun (l : DirectionalLightData) -> l.castsShadow) l') ToggleCastShadow ]]
                ]
            ]                  
        |AdaptivePointLight l' -> 
            let al = AVal.map (fun (l : PointLightData) -> l.attenuationLinear) l'
            let aq = AVal.map (fun (l : PointLightData) -> l.attenuationQad) l'
            let i = AVal.map (fun (l : PointLightData) -> l.intensity) l'
            let c = AVal.map (fun (l : PointLightData) -> l.color.ToC4b()) l'
            div [] [

                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> ToSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Spot Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Sphere Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : PointLightData) -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                intensityView i c
                attenuationView al aq
            ]
        |AdaptiveSpotLight l' -> 
            let al = AVal.map (fun (l : SpotLightData) -> l.attenuationLinear) l'
            let aq = AVal.map (fun (l : SpotLightData) -> l.attenuationQad) l'
            let i = AVal.map (fun (l : SpotLightData) -> l.intensity) l'
            let c = AVal.map (fun (l : SpotLightData) -> l.color.ToC4b()) l'
            let ci = AVal.map (fun (l : SpotLightData) -> l.cutOffInner) l'
            let fo = AVal.map (fun (l : SpotLightData) -> l.fallOff) l'
            div [] [

                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> ToPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Sphere Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : SpotLightData) -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                AVal.map (fun (l : SpotLightData) -> l.lightDirection.XYZ) l'
                |> V3dInput.view "Direction"
                |> UI.map SetLightDirection

                intensityView i c

                attenuationView al aq

                cutOffView ci fo
                Html.table [
                    tr [] [ td [] [text "Cast Shadow"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  (AVal.map (fun (l : SpotLightData) -> l.castsShadow) l') ToggleCastShadow ]]
                ]
            ]
        |AdaptiveSphereLight l' -> 
            let i = AVal.map (fun (l : SphereLightData) -> l.intensity) l'
            let c = AVal.map (fun (l : SphereLightData) -> l.color.ToC4b()) l'
            let r = AVal.map (fun (l : SphereLightData) -> l.radius) l'
            div [] [

                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> ToSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Spot Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : SphereLightData) -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                radiusView r
                intensityView i c
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
        | AdaptiveSpotLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ) target.XYZ up
                    |> CameraView.viewTrafo 
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) 1.0 size 1.0
                    |> Frustum.projTrafo
                return lightView , proj
            }
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
        | SpotLight = 3
        | SphereLight = 4

    type Light = {
        lightType : LightType
        lightPosition : V4d
        lightDirection : V4d
        color : V3d
        attenuationQad :float
        attenuationLinear :float
        castsShadow: bool
        cutOffInner  : float
        cutOffOuter : float
        radius : float
    }
     
    let noLight = {
        lightType = LightType.NoLight
        lightPosition = V4d.Zero
        lightDirection = V4d.Zero
        color = V3d.Zero
        attenuationQad = 0.0
        attenuationLinear = 0.0
        castsShadow = false
        cutOffInner = 0.0
        cutOffOuter = 0.0
        radius = 0.0
        }

    let spotLightAxis (lightDirection : V4d) rotation =
        let d = Vec.normalize lightDirection.YXZ
        let s = if d = (Vec.normalize V3d.IOO) then V3d.OOI else V3d.IOO
        let a0 = Vec.cross d s
        let r = M44d.RotationInDegrees(d,rotation)
        V4d(a0,1.0) * r

    let uniformLight (l : AdaptiveLightCase) : aval<Light>  =
        //needs to be adaptive because the  Light can change and is an IMod
        //we go from aval<MLight> to aval<ISg<Message>>
        adaptive {
            let d = l
            match d with
            | AdaptiveDirectionalLight  x' ->
               let! x  = x'
                //Map to a type more convinient in the shaders
               let r : Light = {
                   lightType = LightType.DirectionalLight
                   lightPosition = V4d.Zero
                   lightDirection = x.lightDirection
                   color = x.color.ToV3d() * x.intensity
                   attenuationQad = 0.0
                   attenuationLinear = 0.0
                   castsShadow = x.castsShadow
                   cutOffInner = 0.0
                   cutOffOuter = 0.0
                   radius = 0.0
                }
               return  r
            | AdaptivePointLight  x' ->
               let! x  = x'
               let r : Light = {
                   lightType = LightType.PointLight
                   lightPosition = x.lightPosition
                   lightDirection = V4d.Zero 
                   color = x.color.ToV3d() * x.intensity
                   attenuationQad = x.attenuationQad
                   attenuationLinear = x.attenuationLinear
                   castsShadow = false
                   cutOffInner = 0.0
                   cutOffOuter = 0.0
                   radius = 0.0
                }
               return r
            | AdaptiveSpotLight  x' ->
               let! x  = x'
               let r : Light = {
                   lightType = LightType.SpotLight
                   lightPosition = x.lightPosition
                   lightDirection = x.lightDirection
                   color = x.color.ToV3d() * x.intensity
                   attenuationQad = x.attenuationQad
                   attenuationLinear = x.attenuationLinear
                   castsShadow = x.castsShadow
                   cutOffInner = x.cutOffInner |> radians |> cos 
                   cutOffOuter = x.fallOff+x.cutOffInner |> radians |> cos
                   radius = 0.0 
                 }
               return r
            | AdaptiveSphereLight  x' ->
               let! x  = x'
               let r : Light = {
                   lightType = LightType.SphereLight
                   lightPosition = x.lightPosition
                   lightDirection = V4d.Zero 
                   color = x.color.ToV3d() * x.intensity
                   attenuationQad = 1.0
                   attenuationLinear = 0.0
                   castsShadow = false
                   cutOffInner = 0.0
                   cutOffOuter = 0.0
                   radius = x.radius
                }
               return r
        } 
