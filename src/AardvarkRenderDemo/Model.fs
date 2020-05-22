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
    attenuationQad :float
    attenuationLinear :float
    intensity : float
}

//todo: Optionally use a single color value instead a texture 
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
    materialLinks : hmap<string, string>
}

[<ModelType>]
type Light =
    | DirectionalLight of DirectionalLightData
    | PointLight of PointLightData

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
type GlobalEnviorment =
    {
        skyMap : string
        skyMapRotation : float
        skyMapIntensity : float
        ambientLightIntensity : float
        occlusionSettings : AmbientOcclusionSettings
    }

[<ModelType>]
type Model =
    {
        cameraState : CameraControllerState
        lights : HashMap<int, Light>
        enviorment : GlobalEnviorment
        expousure  : float
        objects : HashMap<string, SceneObject>
        selectedObject : string
    }


