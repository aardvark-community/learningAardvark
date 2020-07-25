namespace SLEAardvarkRenderDemo.Model

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open Aardvark.SceneGraph
open Adaptify

(*
    The Domain Types forming the Model of the Elm-style App
    They need to be in a sepparat file at the start of the projekt to let the preprocessor generate the addaptive Model 
*)

type DirectionalLightData = {
    lightDirection : V4d
    color : C3d
    intensity : float
    castsShadow : bool
}

type PointLightData = {
    lightPosition : V4d
    color : C3d
    attenuationQad : float
    attenuationLinear : float
    intensity : float
}

type SpotLightData = {
    lightPosition : V4d
    lightDirection : V4d
    color : C3d
    attenuationQad : float
    attenuationLinear : float
    intensity : float
    cutOffInner : float
    fallOff : float
    castsShadow : bool
}

type SphereLightData = {
    lightPosition : V4d
    radius : float
    color : C3d
    intensity : float
}

type DiskLightData = {
    lightPosition : V4d
    lightDirection : V4d
    color : C3d
    intensity : float
    cutOffInner : float
    fallOff : float
    castsShadow : bool
    radius : float
}

type RectangleLightData = {
    lightPosition : V4d
    lightDirection : V4d
    color : C3d
    intensity : float
    cutOffInner : float
    fallOff : float
    castsShadow : bool
    rotation : float
    height : float
    width : float  
}

[<ModelType>]
type TextureMappedValue = {
    fileName : string Option
    factor : float 
}

[<ModelType>]
type TextureMappedColor = {
    fileName : string Option
    color : C3d
    factor : float 
}

[<ModelType>]
type PBRMaterial = {
    metallic  : TextureMappedValue
    roughness : TextureMappedValue
    albedo : TextureMappedColor
    emission : TextureMappedColor
    normal : TextureMappedValue
    discard : bool
    displacment : TextureMappedValue
}

[<ModelType>]
type SceneObject = {
    //[<PrimaryKey>] 
    name : string
    file : string
    scale : float
    translation : V3d
    rotation : V3d
    materials : HashMap<string, PBRMaterial>
    currentMaterial : string
    materialLinks : HashMap<string, string> 
}

[<ModelType>]
type Light =
    | DirectionalLight of DirectionalLightData
    | PointLight of PointLightData
    | SpotLight of SpotLightData
    | SphereLight of  SphereLightData
    | DiskLight of  DiskLightData
    | RectangleLight of RectangleLightData

[<ModelType>]
type AmbientOcclusionSettings =
    {
        occlusionStrength : float
        scale : float
        radius : float
        samples : int
        threshold : float
        sigma : float
        sharpness : float
    }

[<ModelType>]
type Bloom =
    {   
        on : bool
        threshold : float
        blurSize : int
        sigma : float
    }

[<ModelType>]
type fxAA =
    {   
        threshold : float
        thresholdMin : float
        subpix : float
        preset : HiliteShaders.FXAA.PRESET
    }

[<ModelType>]
type GlobalEnviorment =
    {
        skyMap : string
        skyMapRotation : float
        skyMapIntensity : float
        ambientLightIntensity : float
        occlusionSettings : AmbientOcclusionSettings
        lightProbePosition :V3d option
    }

type ToneMapping =  
    {
        ToeLength : float
        ToeStrength : float
        ShoulderLength : float
        ShoulderStrength : float
        ShoulderAngle  : float
        Gamma : float
    }

[<ModelType>]
type Model =
    {
        cameraState : CameraControllerState
        lights : HashMap<int, Light>
        enviorment : GlobalEnviorment
        expousure  : float
        toneMapping : ToneMapping
        fxAA : fxAA
        bloom : Bloom
        objects : HashMap<string, SceneObject>
        selectedObject : string
    }


