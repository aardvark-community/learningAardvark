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
            let gamma  = 2.2
            let c = pow (v.c.XYZ) (V3d(gamma))
            //let c = v.c.XYZ
            let light = uniform.Light
            let (ld, lc)  = 
                match  light  with
                | SLEUniform.DirectionalLight ld -> -ld.lightDirection.XYZ |> Vec.normalize, ld.color
                | SLEUniform.PointLight lp -> lp.lightPosition.XYZ - v.wp.XYZ |> Vec.normalize, lp.color
            let n = v.n |> Vec.normalize
            let h = ld

            let ambient = 0.2
            
            let diffuse = 
                Vec.dot ld n 
                |> if twoSided then abs else max 0.0

            let l = ambient + (1.0 - ambient) * diffuse

            let spec = V3d.III
            let s = Vec.dot h n |> if twoSided then abs else max 0.0

            return pow (V4d(c * l * lc + spec * (pown s 32) * lc, v.c.W)) (V4d(1.0/gamma))
            //return V4d(c * l * lc + spec * (pown s 32) * lc, v.c.W)
        }

module SLESurfaces = 

    let  lighting = Lighting.lighting