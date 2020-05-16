namespace SLEAardvarkRenderDemo.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Aardvark.SceneGraph

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
[<DomainType>]
type TextureMappedValue = {
    fileName : string Option
    factor : float 
}

[<DomainType>]
type PBRMaterial = {
    metallic  : TextureMappedValue
    roughness : TextureMappedValue
    albedo : TextureMappedValue
    normal : TextureMappedValue
    //albedoFactor : float
    //normalMapStrenght : float
    discard : bool
    displacment : TextureMappedValue
}

[<DomainType>]
type SceneObject = {
    [<PrimaryKey>] 
    name : string
    file : string
    scale : float
    translation : V3d
    rotation : V3d
    materials : hmap<string, PBRMaterial>
    currentMaterial : string
}

[<DomainType>]
type Light =
    | DirectionalLight of DirectionalLightData
    | PointLight of PointLightData

[<DomainType>]
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

[<DomainType>]
type GlobalEnviorment =
    {
        skyMap : string
        skyMapRotation : float
        skyMapIntensity : float
        ambientLightIntensity : float
        occlusionSettings : AmbientOcclusionSettings
    }

[<DomainType>]
type Model =
    {
        cameraState : CameraControllerState
        lights : hmap<int, Light>
        enviorment : GlobalEnviorment
        expousure  : float
        objects : hmap<string, SceneObject>
        selectedObject : string
    }


