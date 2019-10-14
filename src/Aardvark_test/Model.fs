namespace Aardvark_test.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives


type DirectionalLightData = {
    lightDirection : V4d
    color : C3d
    intensity : float
}

type PointLightData = {
    lightPosition : V4d
    color : C3d
    attenuationQad :float
    attenuationLinear :float
    intensity : float
}

[<DomainType>]
type Light =
    | DirectionalLight of DirectionalLightData
    | PointLight of PointLightData

[<DomainType>]
type PBRMaterial = 
    {
        metallic  : float
        roughness : float
        albedoFactor : float
        normalMapStrenght : float
        discard : bool
        displacmentMap : ITexture
        displacmentStrength : float
    }

[<DomainType>]
type GlobalEnviorment =
    {
        skyMap : string
        skyMapRotation : float
        skyMapIntensity : float
        ambientLightIntensity : float
    }

[<DomainType>]
type Model =
    {
        cameraState : CameraControllerState
        lights : hmap<int, Light>
        material : PBRMaterial
        enviorment : GlobalEnviorment
        expousure  : float
        materials : hmap<string, PBRMaterial>
        currentMaterial : string
    }

module light =

    let defaultDirectionalLight = DirectionalLight  {lightDirection = V4d(0.0,-1.0,1.0,1.0); color = C3d.White; intensity = 1.0}

    let defaultPointLight = PointLight  {lightPosition = V4d(0.0,1.5,-0.5,1.0); color = C3d.White; attenuationQad = 1.0; attenuationLinear = 0.0; intensity = 1.0}

    let defaultLight = defaultPointLight

module material =

    let grayPix = 
        let pi = PixImage<byte>(Col.Format.RGB, V2i.II)
        pi.GetMatrix<C3f>().SetByCoord(fun (c : V2l) -> C3f.Gray50) |> ignore
        pi

    let grayTex = 
        PixTexture2d(PixImageMipMap [| grayPix :> PixImage |], false) :> ITexture

    let defaultMaterial = {metallic = 0.0; roughness = 0.8; albedoFactor = 1.0; normalMapStrenght = 1.0; discard = false; displacmentMap = grayTex; displacmentStrength = 0.0}