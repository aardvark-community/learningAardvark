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
        | NoLight


module Lighting = 

    type UniformScope with
     
        member x.Lights : Arr<N<10>, SLEUniform.Light> = 
         x?Lights

        member x.NumLights : int = x?NumLights

    let internal lighting (twoSided : bool) (v : Vertex) =
        fragment {
            let gamma  = 2.2
            let c = pow (v.c.XYZ) (V3d(gamma))
            let mutable col = V3d.Zero
            let numLights = uniform.NumLights
            for i in 0 .. 9 do
                
                let light = uniform.Lights.[i]
                let (exists, lDir, lCol)  = 
                    match  light  with
                    | SLEUniform.DirectionalLight ld -> i < numLights, -ld.lightDirection.XYZ |> Vec.normalize, ld.color  
                    | SLEUniform.PointLight lp -> 
                        let lDir = lp.lightPosition.XYZ - v.wp.XYZ |> Vec.normalize
                        let dist = V3d.Distance (lp.lightPosition.XYZ, v.wp.XYZ)
                        let att = 1.0 / (1.0 + lp.attenuationLinear * dist + lp.attenuationQad * dist * dist)
                        i < numLights, lDir , lp.color * att             
                    | SLEUniform.NoLight -> false, c,c  //allways match any cases, otherwise fshade will give  a  cryptic error 
              
                let oi = 
                    if exists then
                        let n = v.n |> Vec.normalize
                        let h = lDir

                        let ambient = 0.05
                        
                        let diffuse = 
                            Vec.dot lDir n 
                            |> if twoSided then abs else max 0.0

                        let l = ambient + (1.0 - ambient) * diffuse

                        let spec = V3d.III
                        let s = Vec.dot h n |> if twoSided then abs else max 0.0

                        // total output
                        c * l * lCol + spec * (pown s 32) * lCol
                    else V3d.Zero

                col <- col + oi
            
            //Reihnard tone mapping
            let colm = col / (col+1.0)

            //gamma  correction
            let colg = pow colm (V3d(1.0/gamma))

            return V4d(colg, v.c.W)
        }

module SLESurfaces = 

    let  lighting = Lighting.lighting