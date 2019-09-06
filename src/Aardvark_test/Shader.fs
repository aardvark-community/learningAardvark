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
            let light = uniform.Light
            let (ld, lc)  = 
                match  light  with
                | SLEUniform.DirectionalLight ld -> -ld.lightDirection.XYZ |> Vec.normalize, ld.color * intensity
                | SLEUniform.PointLight lp -> 
                    let ld = lp.lightPosition.XYZ - v.wp.XYZ |> Vec.normalize
                    let dist = V3d.Distance (lp.lightPosition.XYZ, v.wp.XYZ)
                    let att = 1.0 / (1.0 + lp.attenuationLinear * dist + lp.attenuationQad * dist * dist)
                    ld , lp.color * att * intensity
            let n = v.n |> Vec.normalize
            let h = ld

            let ambient = 0.05
            
            let diffuse = 
                Vec.dot ld n 
                |> if twoSided then abs else max 0.0

            let l = ambient + (1.0 - ambient) * diffuse

            let spec = V3d.III
            let s = Vec.dot h n |> if twoSided then abs else max 0.0

            // total output
            let o = c * l * lc + spec * (pown s 32) * lc
            
            //Reihnard tone mapping
            let om = o / (o+1.0)

            //gamma  correction
            let og = pow  om (V3d(1.0/gamma))

            return V4d(og, v.c.W)
        }

module SLESurfaces = 

    let  lighting = Lighting.lighting