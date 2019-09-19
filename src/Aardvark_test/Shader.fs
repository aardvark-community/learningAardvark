namespace Aardvark_test

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open Aardvark_test.Model
open System

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
            let cameraPos = uniform.CameraLocation
            let view = cameraPos - v.wp.XYZ |> Vec.normalize
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
                        let h = view + lDir |> Vec.normalize

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

module PBR =

    type UniformScope with
     
        member x.Lights : Arr<N<10>, SLEUniform.Light> = 
         x?Lights

        member x.NumLights : int = x?NumLights

    [<GLSLIntrinsic("mix({0}, {1}, {2})")>] // Define function as intrinsic, no implementation needed
    let Lerp (a : V3d) (b : V3d) (s : float) : V3d = failwith ""

    [<ReflectedDefinition>] //add this attribute to  make the function callable in the shader
    let  fresnelSchlick (f0 : V3d) (cosTheta : float)=
        f0 + (1.0 - f0) * pow (1.0 - cosTheta) 5.0

    [<ReflectedDefinition>]
    let DistributionGGX (n : V3d)  h roughness  =
        let a  = roughness*roughness
        let a2 = a * a
        let nDotH = Vec.dot n h |> max 0.0  
        let nDotH2 = nDotH + nDotH
        let deno = nDotH2 * (a2 - 1.0) + 1.0
        let denom =  Math.PI * deno * deno
        a2/denom
    
    [<ReflectedDefinition>]
    let GeometrySchlickGGX nDotV roughness =
        let r = roughness + 1.0
        let k = r * r / 8.0
        let denom = nDotV * (1.0 - k) + k
        nDotV / denom
    
    [<ReflectedDefinition>]
    let GeometrySmith (n : V3d) v l roughness =
        let nDotV = Vec.dot n v |> max 0.0
        let nDotL = Vec.dot n l |> max 0.0
        let ggx2 = GeometrySchlickGGX nDotV roughness
        let ggx1 = GeometrySchlickGGX nDotL roughness
        ggx1 * ggx2


    let internal lighting  (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            let albedo = pow (vert.c.XYZ) (V3d(gamma))
            let metallic = 1.0
            let roughness = 0.01

            let cameraPos = uniform.CameraLocation

            let n = vert.n |> Vec.normalize

            let v = cameraPos - vert.wp.XYZ |> Vec.normalize

            let f0 = Lerp (V3d(0.04)) albedo metallic

            let mutable lo = V3d.Zero
            let numLights = uniform.NumLights
            for i in 0 .. 9 do
                
                let light = uniform.Lights.[i]
                let (exists, lDir, radiance)  = 
                    match  light  with
                    | SLEUniform.DirectionalLight ld -> i < numLights, -ld.lightDirection.XYZ |> Vec.normalize, ld.color  
                    | SLEUniform.PointLight lp -> 
                        let lDir = lp.lightPosition.XYZ - vert.wp.XYZ |> Vec.normalize
                        let dist = V3d.Distance (lp.lightPosition.XYZ, vert.wp.XYZ)
                        let attenuation = 1.0 / (1.0 + lp.attenuationLinear * dist + lp.attenuationQad * dist * dist)
                        i < numLights, lDir , lp.color * attenuation             
                    | SLEUniform.NoLight -> false, V3d(0.0), V3d(0.0)  //allways match any cases, otherwise fshade will give  a  cryptic error 
              
                let oi = 
                    if exists then
                        let h = v + lDir |> Vec.normalize

                        // cook-torrance brdf
                        let ndf = DistributionGGX n h roughness 
                        let g = GeometrySmith n v lDir roughness 
                        let hDotV = Vec.dot h v |>  max 0.0      
                        let f = fresnelSchlick f0 hDotV   
                        
                        let kS = f
                        let kD' = V3d.III - kS
                        let kD = (1.0 - metallic) * kD'
        
                        let nDotV = Vec.dot n v |>  max 0.0
                        let nDotL = Vec.dot n lDir |>  max 0.0

                        let numerator = ndf * g * f
                        let denominator = 4.0 * nDotV * nDotL |> max 0.001
                        let specular = numerator / denominator  
            
                        // add to outgoing radiance from single light
                        (kD * albedo / Math.PI + specular) * radiance * nDotL; 

                    else V3d.Zero

                lo <- lo + oi
            
            let ambient = V3d(0.03) * albedo
            let col = lo + ambient
            //Reihnard tone mapping
            let colm = col / (col+1.0)

            //gamma  correction
            let colg = pow colm (V3d(1.0/gamma))

            return V4d(colg, vert.c.W)
        }

module SLESurfaces = 

    let  lighting = Lighting.lighting
    let  lightingPBR  = PBR.lighting

    