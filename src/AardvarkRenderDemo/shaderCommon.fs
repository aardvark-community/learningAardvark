namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Rendering.Effects
open System

module shaderCommon = 
    open fshadeExt

    module Semantic =
        let Emission = Symbol.Create "Emission"
        let NormalR = Symbol.Create "NormalR"
        let ClearCoat = Symbol.Create "ClearCoat"
        let Sheen = Symbol.Create "Sheen"

    type UniformScope with
        member x.Roughness : float = x?Roughness

        member x.Metallic : float = x?Metallic

        member x.EmissionColor : V3d =  x?EmissionColor

        member x.Discard : bool =  x?Discard

        member x.SkyMapIntensity : float =  x?SkyMapIntensity

        member  x.ClearCoat :  float =  x?ClearCoat

        member  x.ClearCoatRoughness :  float =  x?ClearCoatRoughness

        member  x.ClearCoatNormalStrength :  float =  x?ClearCoatNormalStrength
       
        member  x.UseNormalsForClearCoat :  bool =  x?UseNormalsForClearCoat

        member x.NormalMapStrength : float =  x?NormalMapStrength

        member x.SheenColor : V3d =  x?SheenColor

        member  x.SheenRoughness :  float =  x?SheenRoughness

        member  x.SssProfileIndex :  int =  x?SssProfileIndex

        member  x.Coverage :  float =  x?Coverage

        member  x.Transmission :  V3d =  x?Transmission

    let internal skyBoxTrafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            //let rotView  = m33d uniform.ViewTrafo |> m44d 
            //let  clipPos =  uniform.ProjTraSfo * rotView * wp
            let cameraPos = uniform.CameraLocation
            let clipPos = (uniform.ViewProjTrafo * (wp + V4d(cameraPos,0.0))) //remove translation
            return {
                pos = V4d(clipPos.X,clipPos.Y,clipPos.W,clipPos.W)
                wp = wp
                n =  v.n
                b =  v.b
                t =  v.t
                c = v.c
                tc = v.tc
            }
        }

    let private skySampler =
        samplerCube {
            texture uniform?SkyCubeMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private metallicSampler =
        sampler2d {
            texture uniform?MetallicMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private roughnessSampler =
        sampler2d {
            texture uniform?RoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private emissionSampler =
        sampler2d {
            texture uniform?EmissionTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private clearCoatRoughnessSampler =
        sampler2d {
            texture uniform?ClearCoatRoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private clearCoatSampler =
        sampler2d {
            texture uniform?ClearCoatMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private sheenRoughnessSampler =
        sampler2d {
            texture uniform?SheenRoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private sheenColorSampler =
        sampler2d {
            texture uniform?SheenColorTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private coverageSampler =
        sampler2d {
            texture uniform?CoverageTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private transmissionSampler =
        sampler2d {
            texture uniform?TransmissionColorTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type Fragment = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<Semantic("Metallic")>] metallic    : float
        [<Semantic("Roughness")>] roughness    : float
        [<Semantic("Emission")>] emission    : V3d
        [<Semantic("ClearCoat")>] clearCoat    : float
        [<Semantic("ClearCoatRoughness")>] clearCoatRoughness    : float
        [<Semantic("ClearCoatNormal")>] clearCoatNormal    : V3d
        [<Semantic("sheenRoughness")>] sheenRoughness    : float
        [<Semantic("sheenColor")>] sheenColor : V3d
        [<Semantic("sssProfile")>] sssProfile : int
        [<Semantic("Transmission")>] transmission : V3d
    }


    let getMatrialValues (frag : Fragment) =
        fragment {
            if uniform.Discard then
                discard()
            let gamma  = 2.2
            let albedo = pow frag.c.XYZ (V3d(gamma))
            let metallic = uniform.Metallic * metallicSampler.Sample(frag.tc).X
            let roughness = uniform.Roughness * roughnessSampler.Sample(frag.tc).X
            let emission = uniform.EmissionColor * emissionSampler.Sample(frag.tc).XYZ
            let clearCoat = uniform.ClearCoat * clearCoatSampler.Sample(frag.tc).X
            let clearCoatRoughness = uniform.ClearCoatRoughness * clearCoatRoughnessSampler.Sample(frag.tc).X
            let sheenColor =  pow ( uniform.SheenColor * sheenColorSampler.Sample(frag.tc).XYZ) (V3d(gamma))
            let sheenRoughness = uniform.SheenRoughness * sheenRoughnessSampler.Sample(frag.tc).X
            let alpha = uniform.Coverage * coverageSampler.Sample(frag.tc).X
            let transmission = pow (uniform.Transmission * transmissionSampler.Sample(frag.tc).XYZ) (V3d(gamma))
           return { frag with   
                        c = V4d(albedo,alpha)
                        metallic = metallic
                        roughness = roughness
                        n = frag.n |> Vec.normalize
                        clearCoatNormal = frag.clearCoatNormal |> Vec.normalize
                        emission = emission
                        clearCoat = clearCoat
                        clearCoatRoughness = clearCoatRoughness
                        sheenRoughness = sheenRoughness
                        sheenColor = sheenColor
                        sssProfile = uniform.SssProfileIndex
                        transmission = transmission
                        }
        }

    let skyGetMatrialValues (frag : Fragment) =
        fragment {
           
            let lPos  = frag.wp.XYZ |> Vec.normalize
            let texColor = skySampler.Sample(lPos).XYZ
  
            let col = texColor * uniform.SkyMapIntensity

            return {    wp = frag.wp
                        n = frag.n |> Vec.normalize
                        tc = frag.tc
                        c = V4d(col,1.0)
                        metallic = -1.0
                        roughness = 0.0
                        emission = V3d.OOO
                        clearCoat = 0.0
                        clearCoatRoughness = 0.0
                        clearCoatNormal = V3d.OOO
                        sheenRoughness = 0.0
                        sheenColor = V3d.OOO
                        sssProfile = -1
                        transmission = V3d.OOO
                        }
        }

    let private normalSampler =
        sampler2d {
            texture uniform?NormalMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private clearCoatNormalSampler =
        sampler2d {
            texture uniform?ClearCoatNormalMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let internal normalMap (v : Vertex) =
        fragment {
            let texColor = normalSampler.Sample(v.tc).XYZ
            let texNormal = (2.0 * texColor - V3d.III) |> Vec.normalize

            let n = v.n.Normalized * texNormal.Z + v.b.Normalized * texNormal.X + v.t.Normalized * texNormal.Y |> Vec.normalize

            let strength = uniform.NormalMapStrength
            let n2 = Lerp v.n n strength

            let ccN =
                if uniform.UseNormalsForClearCoat then
                    n
                else
                    let cctexColor =  clearCoatNormalSampler.Sample(v.tc).XYZ
                    let cctexNormal = (2.0 * cctexColor - V3d.III) |> Vec.normalize

                    v.n.Normalized * cctexNormal.Z + v.b.Normalized * cctexNormal.X + v.t.Normalized * cctexNormal.Y |> Vec.normalize  

            let ccstrength = uniform.ClearCoatNormalStrength
            let ccn2 = Lerp v.n ccN ccstrength

            return {    wp = v.wp
                        n = n2
                        tc = v.tc
                        c = v.c
                        metallic = 0.0
                        roughness = 0.0
                        emission = V3d.OOO
                        clearCoat = 0.0
                        clearCoatRoughness = 0.0
                        clearCoatNormal = ccn2 
                        sheenRoughness = 0.0
                        sheenColor = V3d.OOO
                        sssProfile = -1
                        transmission = V3d.OOO
                        }
        }