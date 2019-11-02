namespace Aardvark_test.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Aardvark.SceneGraph


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
    albedoFactor : float
    normalMapStrenght : float
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
    object  : IO.Loader.Scene 
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

module light =

    let defaultDirectionalLight = DirectionalLight  {lightDirection = V4d(0.0,-1.0,1.0,1.0); color = C3d.White; intensity = 1.0; castsShadow = true}

    let defaultPointLight = PointLight  {lightPosition = V4d(0.0,1.5,-0.5,1.0); color = C3d.White; attenuationQad = 1.0; attenuationLinear = 0.0; intensity = 1.0}

    let defaultLight = defaultPointLight

    let defaultAbientOcclusion = {occlusionStrength = 1.0; scale = 1.0; radius = 0.2; samples = 32; threshold = 0.2; sigma = 2.0; sharpness = 1.0}

