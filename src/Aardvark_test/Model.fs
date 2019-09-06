namespace Aardvark_test.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives


type DirectionalLightData = {
    lightDirection : V4d
    color : C3d
}

type PointLightData = {
    lightPosition : V4d
    color : C3d
    attenuationQad :float
    attenuationLinear :float
}

[<DomainType>]
type Light =
    | DirectionalLight of DirectionalLightData
    | PointLight of PointLightData


[<DomainType>]
type Model =
    {
        cameraState     : CameraControllerState
        light :  Light
    }

module light =

    let  defaultDirectionalLight = DirectionalLight  {lightDirection = V4d(0.0,-1.0,1.0,1.0); color = C3d.White}

    let  defaultPointLight = PointLight  {lightPosition = V4d(0.0,1.5,-0.5,1.0); color = C3d.White; attenuationQad = 1.0; attenuationLinear = 0.0}

    let defaultLight = defaultPointLight