namespace Aardvark_test

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open Aardvark_test.Model

module SLEUniform =

    type DirectionalLightData = {
        lightDirection : V4d
        color : V3d
    }

    type PointLightData = {
        lightPosition : V4d
        color : V3d
        attenuationQad :float
        attenuationLinear :float
    }

    type Light =
        | DirectionalLight of DirectionalLightData
        | PointLight of PointLightData


module Lighting = 

    type UniformScope with
        member x.Light : SLEUniform.Light = x?Light

    let internal lighting (twoSided : bool) (v : Vertex) =
        fragment {
            let light = uniform.Light
            let (ld, lc)  = 
                match  light  with
                | SLEUniform.DirectionalLight ld -> -ld.lightDirection.XYZ |> Vec.normalize, ld.color
                | SLEUniform.PointLight lp -> lp.lightPosition.XYZ - v.wp.XYZ |> Vec.normalize, lp.color
            let n = v.n |> Vec.normalize
            let h = ld

            let ambient = 0.1
            
            let diffuse = 
                Vec.dot ld n 
                |> if twoSided then abs else max 0.0

            let l = ambient + (1.0 - ambient) * diffuse

            let spec = V3d.III
            let s = Vec.dot h n 

            return V4d(v.c.XYZ * l * lc + spec * (pown s 32) * lc, v.c.W)
        }

module SLESurfaces = 

    let  lighting = Lighting.lighting