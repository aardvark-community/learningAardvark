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
        lightDirection = V4d(0.0,-1.0,1.0,0.0); 
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
        lightDirection = V4d(0.0,-1.0,1.0,0.0); 
        color = C3d.White; 
        attenuationQad = 1.0; 
        attenuationLinear = 0.0; 
        intensity = 1.0; 
        cutOffInner = 30.0; 
        fallOff = 10.0; 
        castsShadow = true
    }

    let defaultDiskLight = DiskLight  {
        lightPosition = V4d(0.0,1.5,-0.5,1.0); 
        lightDirection = V4d(0.0,-1.0,1.0,0.0); 
        color = C3d.White; 
        intensity = 1.0; 
        cutOffInner = 30.0; 
        fallOff = 10.0; 
        castsShadow = true
        radius = 0.2
    }

    let defaultRectangleLight = RectangleLight  {
        lightPosition = V4d(0.0,1.5,-0.5,1.0); 
        lightDirection = V4d(0.0,-1.0,1.0,0.0); 
        color = C3d.White; 
        intensity = 1.0; 
        cutOffInner = 30.0; 
        fallOff = 10.0; 
        castsShadow = true
        rotation = 0.0
        width = 0.5
        height = 0.5
    }
    
    let defaultLight = defaultPointLight

    let defaultAbientOcclusion = {occlusionStrength = 1.0; scale = 1.0; radius = 0.2; samples = 32; threshold = 0.2; sigma = 2.0; sharpness = 1.0}
   
    let lightIndexMap (lights : HashMap<int, Light>) =
        lights
        |> HashMap.keys
        |> HashSet.toList
        |> List.sort
        |> List.mapi (fun i k -> i , k)
        |> Map.ofList

    let calcVirtualPositionOffset  (l : AdaptiveLightCase) =
        match l with 
        | AdaptiveDirectionalLight ld -> AVal.constant V2d.OO
        | AdaptiveSpotLight ls -> AVal.constant V2d.OO
        | AdaptiveSphereLight ls -> AVal.map (fun (ls : SphereLightData) ->  V2d(ls.radius * sqrt 2.0,0.0)) ls
        | AdaptiveDiskLight ls -> AVal.map  ( fun (ls : DiskLightData) -> V2d(ls.radius/tan(radians(ls.cutOffInner + ls.fallOff)),0.0)) ls
        | AdaptivePointLight lp -> AVal.constant V2d.OO
        | AdaptiveRectangleLight lp -> AVal.map ( fun (ls : RectangleLightData) -> V2d(ls.height/tan(radians(ls.cutOffInner + ls.fallOff)),ls.width/tan(radians(ls.cutOffInner + ls.fallOff)))) lp

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
                    | AdaptiveSphereLight ls -> Sg.empty
                    | AdaptiveDiskLight ls -> Sg.empty
                    | AdaptiveRectangleLight ls -> Sg.empty
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

    //calculate light view and prjection
    let lightViewPoject (bb : aval<Box3d>) (alight : AdaptiveLightCase) =
        match alight with
        | AdaptivePointLight l -> failwith "not implemented"
        | AdaptiveSphereLight l -> failwith "not implemented"
        | AdaptiveSpotLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ ) target.XYZ up
                    |> CameraView.viewTrafo 
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) 0.1 size 1.0
                    |> Frustum.projTrafo
                return lightView , proj, 0.1, size
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
                return lightView , proj, 0.1, size*2.0
            }
        | AdaptiveDiskLight l -> 
           adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let! offset = (calcVirtualPositionOffset alight) //offset position so that the disk fits into the Camera Frustum
                let n = light.lightDirection.XYZ |> Vec.normalize
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ - (offset.X * n)) target.XYZ up
                    |> CameraView.viewTrafo 
                //set the near plane so that the offset is compensated
                let zNear = light.radius+offset.X |> max 0.1
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) zNear size 1.0
                    |> Frustum.projTrafo
                return lightView , proj, zNear, size
            }
        | AdaptiveRectangleLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let n = light.lightDirection.XYZ |> Vec.normalize
                let target = light.lightPosition + light.lightDirection
                //claculat sky direction according to the rotation of the rectangle
                let rotate = M44d.RotationInDegrees(n,light.rotation) * M44d.RotateInto(V3d.OIO, n) 
                let up = (rotate * V4d.OOIO).XYZ
                //offset the position sot thet the  camera fits into the camera fustrum, use the bigger offset of the twoc directions 
                let! o = (calcVirtualPositionOffset alight)
                let offset = max o.X o.Y
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ - (offset * n)) target.XYZ up
                    |> CameraView.viewTrafo 
                //set the near plane so that the offset is compensated, minimum near plane 0.1 to avoid artefacts
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) (max offset 0.1) size 1.0
                    |> Frustum.projTrafo
                return lightView , proj, max offset 0.1, size
            }

// a control to set the properties of a single light
module lightControl = 

    type Message =
        | DefaultDirectionalLight
        | DefaultPointLight
        | DefaultSpotLight
        | DefaultSphereLight
        | DefaultDiskLight
        | DefaultRectangleLight
        | ToDirectionalLight
        | ToPointLight
        | ToSpotLight
        | ToSphereLight
        | ToDiskLight
        | ToRectangleLight
        | SetLightDirection of V3dInput.Message
        | SetLightPosition of V3dInput.Message
        | SetIntensity of float
        | SetColor of C3d
        | SetCutOffInner of float
        | SetFallOff of float
        | SetRadius of float
        | SetRotation of float
        | SetHeight of float
        | SetWidth of float
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
                    castsShadow = r.castsShadow
                }
            | SphereLight r -> 
                DirectionalLight  {
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    castsShadow = true
                }
            | DiskLight r -> 
                DirectionalLight  {
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity
                    castsShadow = r.castsShadow
                }
            | RectangleLight r -> 
                DirectionalLight  {
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity
                    castsShadow = r.castsShadow
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
            | DiskLight r -> 
                PointLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity
                }
            | RectangleLight r -> 
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
            | DiskLight r -> 
                SphereLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    radius = r.radius
                }            
            | RectangleLight r -> 
                SphereLight  {
                    lightPosition = r.lightPosition
                    color = r.color
                    intensity = r.intensity
                    radius = r.height * 0.5
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
            | DiskLight r -> 
                SpotLight  {
                    lightPosition = r.lightPosition
                    lightDirection = r.lightDirection
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity 
                    cutOffInner = r.cutOffInner
                    fallOff = r.fallOff
                    castsShadow = r.castsShadow
                }
            | RectangleLight r -> 
                SpotLight  {
                    lightPosition = r.lightPosition
                    lightDirection = r.lightDirection
                    color = r.color
                    attenuationQad = 1.0
                    attenuationLinear = 0.0
                    intensity = r.intensity 
                    cutOffInner = r.cutOffInner
                    fallOff = r.fallOff
                    castsShadow = r.castsShadow
                }            | x -> x
        | ToDiskLight ->
            match m with
            | DirectionalLight r -> 
                DiskLight  {
                    lightPosition = -r.lightDirection
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = r.castsShadow
                    radius = 0.01
                }
            | PointLight r -> 
                DiskLight  {
                    lightPosition = r.lightPosition
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = true
                    radius = 0.01
                }
            | SpotLight r -> 
                DiskLight  {
                    lightPosition = r.lightPosition
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = r.cutOffInner
                    fallOff = r.fallOff
                    castsShadow = true
                    radius = 0.01
                }
            | SphereLight r -> 
                DiskLight  {
                    lightPosition = r.lightPosition
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = true
                    radius =r.radius
                }
            | RectangleLight r -> 
                DiskLight  {
                    lightPosition = r.lightPosition
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = r.cutOffInner
                    fallOff = r.fallOff 
                    castsShadow = r.castsShadow
                    radius =r.height * 0.5
                }
            | x -> x
        | ToRectangleLight ->
            match m with
            | DirectionalLight r -> 
                RectangleLight  {
                    lightPosition = -r.lightDirection
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = r.castsShadow
                    rotation = 0.0
                    height = 0.01
                    width = 0.01
                }
            | PointLight r -> 
                RectangleLight  {
                    lightPosition = r.lightPosition
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = true
                    rotation = 0.0
                    height = 0.01
                    width = 0.01
                }
            | SpotLight r -> 
                RectangleLight  {
                    lightPosition = r.lightPosition
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = r.cutOffInner
                    fallOff = r.fallOff
                    castsShadow = true
                    rotation = 0.0
                    height = 0.01
                    width = 0.01
                }
            | SphereLight r -> 
                RectangleLight  {
                    lightPosition = r.lightPosition
                    lightDirection = -r.lightPosition
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = 30.0
                    fallOff = 10.0 
                    castsShadow = true
                    rotation = 0.0
                    height = r.radius * 2.0
                    width = r.radius * 2.0
                }
            | DiskLight r -> 
                RectangleLight  {
                    lightPosition = r.lightPosition
                    lightDirection = r.lightDirection
                    color = r.color
                    intensity = r.intensity 
                    cutOffInner = r.cutOffInner
                    fallOff = r.fallOff 
                    castsShadow = r.castsShadow
                    rotation = 0.0
                    height = r.radius * 2.0
                    width = r.radius * 2.0
                }
            | x -> x
        | DefaultDirectionalLight -> light.defaultDirectionalLight
        | DefaultPointLight -> light.defaultPointLight
        | DefaultSpotLight -> light.defaultSpotLight
        | DefaultSphereLight -> light.defaultSphereLight
        | DefaultDiskLight -> light.defaultDiskLight
        | DefaultRectangleLight -> light.defaultRectangleLight
        | SetLightDirection vMsg  ->  
            match m with
            | DirectionalLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                DirectionalLight {r with lightDirection = V4d(n, 0.0)} 
            | SpotLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                SpotLight {r with lightDirection = V4d(n, 0.0)} 
            | DiskLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                DiskLight {r with lightDirection = V4d(n, 0.0)} 
            | RectangleLight r -> 
                let n = V3dInput.update (r.lightDirection.XYZ) vMsg
                RectangleLight {r with lightDirection = V4d(n, 0.0)} 
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
            | DiskLight r -> 
                let n = V3dInput.update (r.lightPosition.XYZ) vMsg
                DiskLight {r with lightPosition = V4d(n, 1.0)} 
            | RectangleLight r -> 
                let n = V3dInput.update (r.lightPosition.XYZ) vMsg
                RectangleLight {r with lightPosition = V4d(n, 1.0)} 
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
            | DiskLight r -> 
                DiskLight {r with intensity = i} 
            | RectangleLight r -> 
                RectangleLight {r with intensity = i} 
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
            | DiskLight r -> 
                DiskLight {r with color = c} 
            | RectangleLight r -> 
                RectangleLight {r with color = c} 
         | SetCutOffInner w ->  
            match m with
            | SpotLight r -> 
                SpotLight {r with cutOffInner = w} 
            | DiskLight r -> 
                DiskLight {r with cutOffInner = w} 
            | RectangleLight r -> 
                RectangleLight {r with cutOffInner = w} 
            | x -> x
         | SetFallOff w ->  
            match m with
            | SpotLight r -> 
                SpotLight {r with fallOff = w} 
            | DiskLight r -> 
                DiskLight {r with fallOff = w} 
            | RectangleLight r -> 
                RectangleLight {r with fallOff = w} 
            | x -> x
         | SetRadius w ->  
            match m with
            | SphereLight r -> 
                SphereLight {r with radius = w} 
            | DiskLight r -> 
                DiskLight {r with radius = w} 
            | x -> x
         | ToggleCastShadow   ->  
            match m with
            | DirectionalLight r -> 
                DirectionalLight {r with castsShadow = not r.castsShadow} 
            | SpotLight r -> 
                SpotLight {r with castsShadow = not r.castsShadow} 
            | DiskLight r -> 
                DiskLight {r with castsShadow = not r.castsShadow} 
            | RectangleLight r -> 
                RectangleLight {r with castsShadow = not r.castsShadow} 
            | x -> x
        | SetRotation r  ->  
            match m with
            | RectangleLight l -> 
                 RectangleLight {l with rotation = r} 
            | x -> x
        | SetHeight h  ->  
            match m with
            | RectangleLight r -> 
                RectangleLight {r with height = h} 
            | x -> x
        | SetWidth w  ->  
            match m with
            | RectangleLight r -> 
                RectangleLight {r with width = w} 
            | x -> x

    let cutOffView (c : aval<float>) (f : aval<float>) =
        let numInput name changed state  = labeledFloatInput name 0.0 360.0 1.0 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "4"] [text "Cutoff"] ]                          
            tr [] [ td [] [numInput "Cutoff inner" SetCutOffInner c]
                    td [] [numInput "Falloff" SetFallOff f]]
        ]

    let radiusView (r : aval<float>) =
        let numInput name changed state  = labeledFloatInput name 0.0 10.0 0.1 changed state
        Html.table [ 
           tr [] [ td [] [numInput "Radius" SetRadius r]]
        ]

    let rotationView (r : aval<float>) =
        let numInput name changed state  = labeledFloatInput name 0.0 10.0 0.1 changed state
        Html.table [ 
            tr [] [ td [] [text "Rotation"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 360.0; step = 1.0} [] r SetRotation ]]
        ]

    let rectView (h : aval<float>) (w : aval<float>)=
        let numInput name changed state  = labeledFloatInput name 0.0 10.0 0.1 changed state
        Html.table [ 
           tr [] [ td [] [numInput "Height" SetHeight h]]
           tr [] [ td [] [numInput "Width" SetWidth w]]
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
                        button [clazz "ui button"; onClick (fun _ -> ToDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Disk Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToRectangleLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Rectangle Light"]
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
                        button [clazz "ui button"; onClick (fun _ -> ToDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Disk Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToRectangleLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Rectangle Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : PointLightData) -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                intensityView i c
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
                        button [clazz "ui button"; onClick (fun _ -> ToDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Disk Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToRectangleLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Rectangle Light"]
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
                        button [clazz "ui button"; onClick (fun _ -> ToDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Disk Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToRectangleLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Rectangle Light"]
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
        |AdaptiveDiskLight l' -> 
            let i = AVal.map (fun (l : DiskLightData) -> l.intensity) l'
            let c = AVal.map (fun (l : DiskLightData) -> l.color.ToC4b()) l'
            let ci = AVal.map (fun (l : DiskLightData) -> l.cutOffInner) l'
            let fo = AVal.map (fun (l : DiskLightData) -> l.fallOff) l'
            let r = AVal.map (fun (l : DiskLightData) -> l.radius) l'
            div [] [

                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> ToPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Sphere Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Spot Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToRectangleLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Rectangle Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : DiskLightData) -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                AVal.map (fun (l : DiskLightData) -> l.lightDirection.XYZ) l'
                |> V3dInput.view "Direction"
                |> UI.map SetLightDirection

                radiusView r
                intensityView i c
                cutOffView ci fo
                Html.table [
                    tr [] [ td [] [text "Cast Shadow"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  (AVal.map (fun (l : DiskLightData) -> l.castsShadow) l') ToggleCastShadow ]]
                ]
            ]
        |AdaptiveRectangleLight l' -> 
            let i = AVal.map (fun (l : RectangleLightData) -> l.intensity) l'
            let c = AVal.map (fun (l : RectangleLightData) -> l.color.ToC4b()) l'
            let ci = AVal.map (fun (l : RectangleLightData) -> l.cutOffInner) l'
            let fo = AVal.map (fun (l : RectangleLightData) -> l.fallOff) l'
            let h = AVal.map (fun (l : RectangleLightData) -> l.height) l'
            let w = AVal.map (fun (l : RectangleLightData) -> l.width) l'
            div [] [

                AVal.map (fun l -> 
                    [
                        button [clazz "ui button"; onClick (fun _ -> DefaultDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Reset"]
                        button [clazz "ui button"; onClick (fun _ -> ToPointLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Point Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToDirectionalLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Directional Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSphereLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Sphere Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToSpotLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Spot Light"]
                        button [clazz "ui button"; onClick (fun _ -> ToDiskLight); style "margin-bottom: 5px; width: 100%;" ]  [text "Change to Disk Light"]
                    ] 
                    |> IndexList.ofList) l'
                |> AList.ofAVal
                |> Incremental.div AttributeMap.empty 
                
                AVal.map (fun (l : RectangleLightData) -> l.lightPosition.XYZ) l'
                |> V3dInput.view "Position"
                |> UI.map SetLightPosition

                AVal.map (fun (l : RectangleLightData) -> l.lightDirection.XYZ) l'
                |> V3dInput.view "Direction"
                |> UI.map SetLightDirection

                AVal.map (fun (l : RectangleLightData) -> l.rotation) l'
                |> rotationView

                rectView h w
                intensityView i c
                cutOffView ci fo
                Html.table [
                    tr [] [ td [] [text "Cast Shadow"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  (AVal.map (fun (l : RectangleLightData) -> l.castsShadow) l') ToggleCastShadow ]]
                ]
            ]

module SLEUniform =
    //light information as shader uniforms

    type LightType =
        | NoLight = 0
        | DirectionalLight = 1
        | PointLight = 2
        | SpotLight = 3
        | SphereLight = 4
        | DiskLight = 5
        | RectangleLight = 6
    
    type Light = {
        lightType : LightType
        lightPosition : V4d
        lightDirection : V4d
        color : V3d
        castsShadow: bool
        cutOffInner  : float
        cutOffOuter : float
        radius : float
        virtualPos  : V3d
        virtualPos2  : V3d
        p1 : V3d
        p2 : V3d
        p3 : V3d
        p4 : V3d
        toWorld : M44d
        fromWorld : M44d
        lightViewProjMatrix : M44d
        lightViewMatrix : M44d
        shadowMapMinZ : float
        shadowMapMaxZ : float
    }

    let noLight = {
        lightType = LightType.NoLight
        lightPosition = V4d.Zero
        lightDirection = V4d.Zero
        color = V3d.Zero
        castsShadow = false
        cutOffInner = 0.0
        cutOffOuter = 0.0
        radius = 0.0
        virtualPos = V3d.Zero
        virtualPos2 = V3d.Zero
        p1 = V3d.Zero
        p2 = V3d.Zero
        p3 = V3d.Zero
        p4 = V3d.Zero
        toWorld = M44d.Identity
        fromWorld = M44d.Identity
        lightViewProjMatrix = M44d.Identity
        lightViewMatrix = M44d.Identity
        shadowMapMinZ = 0.0
        shadowMapMaxZ = 0.0
        }


    let spotLightAxis (lightDirection : V4d) rotation =
        let d = Vec.normalize lightDirection.YXZ
        let s = if d = (Vec.normalize V3d.IOO) then V3d.OOI else V3d.IOO
        let a0 = Vec.cross d s
        let r = M44d.RotationInDegrees(d,rotation)
        V4d(a0,1.0) * r

    let uniformLight bb (l : AdaptiveLightCase) : aval<Light>  =
        //needs to be adaptive because the  Light can change and is an IMod
        //we go from aval<MLight> to aval<ISg<Message>>
        adaptive {
            let d = l
            let! offset = light.calcVirtualPositionOffset d
            match d with
            | AdaptiveDirectionalLight  x' ->
               let! x  = x'
               let! viewM, projM, minZ, maxZ = light.lightViewPoject bb l
                //Map to a type more convinient in the shaders
               let r : Light = {
                   lightType = LightType.DirectionalLight
                   lightPosition = V4d.Zero
                   lightDirection = x.lightDirection
                   color = x.color.ToV3d() * x.intensity
                   castsShadow = x.castsShadow
                   cutOffInner = 0.0
                   cutOffOuter = 0.0
                   radius = 0.0
                   virtualPos = V3d.Zero
                   virtualPos2 = V3d.Zero
                   p1 = V3d.Zero
                   p2 = V3d.Zero
                   p3 = V3d.Zero
                   p4 = V3d.Zero
                   toWorld = M44d.Identity
                   fromWorld = M44d.Identity
                   lightViewProjMatrix =  (viewM * projM).Forward
                   lightViewMatrix = viewM.Forward
                   shadowMapMinZ = minZ
                   shadowMapMaxZ = maxZ
                }
               return  r
            | AdaptivePointLight  x' ->
               let! x  = x'
               let r : Light = {
                   lightType = LightType.PointLight
                   lightPosition = x.lightPosition
                   lightDirection = V4d.Zero 
                   color = x.color.ToV3d() * x.intensity
                   castsShadow = false
                   cutOffInner = 0.0
                   cutOffOuter = 0.0
                   radius = 0.0
                   virtualPos = V3d.Zero
                   virtualPos2 = V3d.Zero
                   p1 = V3d.Zero
                   p2 = V3d.Zero
                   p3 = V3d.Zero
                   p4 = V3d.Zero
                   toWorld = M44d.Identity
                   fromWorld = M44d.Identity
                   lightViewProjMatrix = M44d.Identity
                   lightViewMatrix = M44d.Identity
                   shadowMapMinZ = 0.0
                   shadowMapMaxZ = 0.0
                }
               return r
            | AdaptiveSpotLight  x' ->
               let! x  = x'
               let! viewM, projM, minZ, maxZ = light.lightViewPoject bb l
               let r : Light = {
                   lightType = LightType.SpotLight
                   lightPosition = x.lightPosition
                   lightDirection = x.lightDirection
                   color = x.color.ToV3d() * x.intensity
                   castsShadow = x.castsShadow
                   cutOffInner = x.cutOffInner |> radians |> cos 
                   cutOffOuter = x.fallOff+x.cutOffInner |> radians |> cos
                   radius = 0.0
                   virtualPos = V3d.Zero
                   virtualPos2 = V3d.Zero
                   p1 = V3d.Zero
                   p2 = V3d.Zero
                   p3 = V3d.Zero
                   p4 = V3d.Zero
                   toWorld = M44d.Identity
                   fromWorld = M44d.Identity
                   lightViewProjMatrix = M44d.Identity
                   lightViewMatrix = M44d.Identity
                   shadowMapMinZ = 0.0
                   shadowMapMaxZ = 0.0
                 }
               return r
            | AdaptiveSphereLight  x' ->
               let! x  = x'
               let r : Light = {
                   lightType = LightType.SphereLight
                   lightPosition = x.lightPosition
                   lightDirection = V4d.Zero 
                   color = x.color.ToV3d() * x.intensity
                   castsShadow = false
                   cutOffInner = 0.0
                   cutOffOuter = 0.0
                   radius = x.radius
                   virtualPos = V3d.Zero
                   virtualPos2 = V3d.Zero
                   p1 = V3d.Zero
                   p2 = V3d.Zero
                   p3 = V3d.Zero
                   p4 = V3d.Zero
                   toWorld = M44d.Identity
                   fromWorld = M44d.Identity
                   lightViewProjMatrix = M44d.Identity
                   lightViewMatrix = M44d.Identity
                   shadowMapMinZ = 0.0
                   shadowMapMaxZ = 0.0
                }
               return r
            | AdaptiveDiskLight  x' ->
               let! x  = x'
               let! viewM, projM, minZ, maxZ = light.lightViewPoject bb l
               let r : Light = {
                   lightType = LightType.DiskLight
                   lightPosition = x.lightPosition
                   lightDirection = x.lightDirection
                   color = x.color.ToV3d() * x.intensity
                   castsShadow = x.castsShadow
                   cutOffInner = x.cutOffInner |> radians |> cos 
                   cutOffOuter = x.fallOff+x.cutOffInner |> radians |> cos
                   radius = x.radius
                   virtualPos = x.lightPosition.XYZ - (Vec.normalize x.lightDirection.XYZ)  * offset.X
                   virtualPos2 = V3d.Zero
                   p1 = V3d.Zero
                   p2 = V3d.Zero
                   p3 = V3d.Zero
                   p4 = V3d.Zero
                   toWorld = M44d.Identity
                   fromWorld = M44d.Identity
                   lightViewProjMatrix = M44d.Identity
                   lightViewMatrix = M44d.Identity
                   shadowMapMinZ = 0.0
                   shadowMapMaxZ = 0.0
                 }
               return r
            | AdaptiveRectangleLight  x' ->
               let! x  = x'
               let! viewM, projM, minZ, maxZ = light.lightViewPoject bb l
               let n = x.lightDirection.XYZ |> Vec.normalize
               let toWorld = M44d.Translation(x.lightPosition.XYZ) * M44d.RotationInDegrees(n,x.rotation) * M44d.RotateInto(V3d.OIO, n) 
               let r : Light = {
                   lightType = LightType.RectangleLight
                   lightPosition = x.lightPosition
                   lightDirection = x.lightDirection
                   color = x.color.ToV3d() * x.intensity
                   castsShadow = x.castsShadow
                   cutOffInner = x.cutOffInner |> radians |> cos 
                   cutOffOuter = x.fallOff+x.cutOffInner |> radians |> cos
                   radius = x.height
                   virtualPos = x.lightPosition.XYZ - (Vec.normalize x.lightDirection.XYZ)  * offset.Y
                   virtualPos2 = x.lightPosition.XYZ - (Vec.normalize x.lightDirection.XYZ)  * offset.X
                   p1 = (toWorld *  V4d(-0.5*x.width,0.0,0.5*x.height, 1.0)).XYZ
                   p2 = (toWorld *  V4d(-0.5*x.width,0.0,-0.5*x.height, 1.0)).XYZ
                   p3 = (toWorld *  V4d(+0.5*x.width,0.0,-0.5*x.height, 1.0)).XYZ
                   p4 = (toWorld *  V4d(+0.5*x.width,0.0,0.5*x.height, 1.0)).XYZ
                   toWorld = toWorld
                   fromWorld = toWorld.Inverse
                   lightViewProjMatrix = M44d.Identity
                   lightViewMatrix = M44d.Identity
                   shadowMapMinZ = 0.0
                   shadowMapMaxZ = 0.0
                 }
               Log.debug "p1 %A p2 %A p3 %A p4 %A" r.p1 r.p2 r.p3 r.p4
               return r        } 
 
    let uniformLightArray bb (lights : amap<int,AdaptiveLightCase>) = 
        let uarr =
            lights
            |> AMap.mapA (fun _ l -> uniformLight bb l)
            |> AMap.toAVal
            |> AVal.map (fun (m : HashMap<int,Light>) -> m |> HashMap.toArray |> Array.sortBy fst |> Array.map snd ) 
        let c'  = uarr |> AVal.map (Array.length >> min 80) 
        let arr  =
            aval{
                let out = Array.replicate 80 noLight
                let! c = c'
                let! a = uarr
                let out = Array.init 80 (fun i -> if i < c then a.[i]  else noLight)  
                return out
            }
        Sg.uniform "LightArray" arr
        >> Sg.uniform "LightCount" c'