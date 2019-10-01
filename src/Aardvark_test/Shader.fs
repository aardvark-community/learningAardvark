namespace Aardvark_test

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open Aardvark_test.Model
open System

module SLEUniform =

    type LightType =
        | NoLight = 0
        | DirectionalLight = 1
        | PointLight = 2

    type Light = {
        lightType : LightType
        lightPosition : V4d
        color : V3d
        attenuationQad :float
        attenuationLinear :float
    }
     
    let noLight = {lightType = LightType.NoLight; lightPosition = V4d.Zero; color = V3d.Zero; attenuationQad = 0.0; attenuationLinear = 0.0}

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
                    match  light.lightType  with
                    | SLEUniform.LightType.DirectionalLight -> i < numLights, light.lightPosition.XYZ |> Vec.normalize, light.color  
                    | SLEUniform.LightType.PointLight -> 
                        let lDir = light.lightPosition.XYZ - v.wp.XYZ |> Vec.normalize
                        let dist = V3d.Distance (light.lightPosition.XYZ, v.wp.XYZ)
                        let att = 1.0 / (1.0 + light.attenuationLinear * dist + light.attenuationQad * dist * dist)
                        i < numLights, lDir , light.color * att             
                    | SLEUniform.LightType.NoLight -> false, c,c 
                    |_ ->  false, V3d(0.0), V3d(0.0)  //allways match any cases, otherwise fshade will give  a  cryptic error 
              
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

        member x.Roughness : float = x?Roughness

        member x.Metallic : float = x?Metallic

        member x.AlbedoFactor : float = x?AlbedoFactor

    //Note: Do not use ' in variabel names for shader code,  it will lead  to an error

     //------------------------------------------
     // direkt Light PBR based on this  tutorial:
     //
     //-------------------------------------------

    let private diffuseIrradianceSampler =
        samplerCube {
            texture uniform?DiffuseIrradiance
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

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
        let nDotH2 = nDotH * nDotH
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

    [<ReflectedDefinition>] //add this attribute to  make the function callable in the shader
    let  fresnelSchlickRoughness (f0 : V3d) (roughness : float) (cosTheta : float)=
        let r = V3d.Max(V3d(1.0 - roughness), f0)
        f0 + (r  - f0) * pow (1.0 - cosTheta) 5.0

    let internal lighting  (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            let albedo = pow (vert.c.XYZ * uniform.AlbedoFactor) (V3d(gamma))
            let metallic = uniform.Metallic
            let roughness = uniform.Roughness

            let cameraPos = uniform.CameraLocation

            let n = vert.n |> Vec.normalize

            let v = cameraPos - vert.wp.XYZ |> Vec.normalize

            //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
            let f0 = Lerp (V3d(0.04)) albedo metallic

            let nDotV = Vec.dot n v |>  max 0.0
            let mutable lo = V3d.Zero
            let numLights = uniform.NumLights
            for i in 0 .. 9 do
                
                let light = uniform.Lights.[i]
                let (exists, lDir, radiance)  = 
                    match  light.lightType  with
                    | SLEUniform.LightType.DirectionalLight -> i < numLights, -light.lightPosition.XYZ |> Vec.normalize, light.color  
                    | SLEUniform.LightType.PointLight -> 
                        let lDir = light.lightPosition.XYZ - vert.wp.XYZ |> Vec.normalize
                        let dist = V3d.Distance (light.lightPosition.XYZ, vert.wp.XYZ)
                        let attenuation = 1.0 / (1.0 + light.attenuationLinear * dist + light.attenuationQad * dist * dist)
                        i < numLights, lDir , light.color * attenuation             
                    | SLEUniform.LightType.NoLight -> false, V3d(0.0), V3d(0.0)
                    |_ ->  false, V3d(0.0), V3d(0.0)  //allways match any cases, otherwise fshade will give  a  cryptic error 
              
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
        
                        let nDotL = Vec.dot n lDir |>  max 0.0

                        let numerator = ndf * g * f
                        let denominator = 4.0 * nDotV * nDotL |> max 0.001
                        let specular = numerator / denominator  
            
                        // add to outgoing radiance from single light
                        (kD * albedo / Math.PI + specular) * radiance * nDotL; 

                    else V3d.Zero

                lo <- lo + oi

            let kSA = fresnelSchlickRoughness f0 roughness nDotV
            let kdA  = (1.0 - kSA) * (1.0 - metallic)
            let irradiance = diffuseIrradianceSampler.Sample(n).XYZ
            let diffuse = irradiance * albedo
            let ambient = kdA * diffuse //todo: ambient Strength, abient occlusion
            let col = lo + ambient
            //Reihnard tone mapping
            let colm = col / (col+1.0)

            //gamma  correction
            let colg = pow colm (V3d(1.0/gamma))

            return V4d(colg, vert.c.W)
        }

    let private envSampler =
        samplerCube {
            texture uniform?SkyCubeMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }


    let internal convoluteDiffuseIrradiance (vert : Vertex) =
        fragment {
            let normal = vert.wp.XYZ |> Vec.normalize
            let right = Vec.cross V3d.OIO normal
            let up = Vec.cross normal right
            let  sampleDelta = 0.025
            let mutable nrSamples = 0.0
            let mutable irradiance = V4d(0.0)

            //for phi in  0.0..sampleDelta..2.0*Math.PI do
                //for theta in 0.0..sampleDelta..0.5*Math.PI do //dosn't work:  never returns

            let mutable phi = 0.0
            let mutable theta = 0.0
            while phi <= 2.0*Math.PI do
                theta <- 0.0
                while theta <= 0.5*Math.PI do
                    nrSamples <- nrSamples + 1.0
                    let tangentSample = V3d(sin theta * cos phi, sin theta * sin phi, cos theta)
                    let sampleVec = tangentSample.X  * right + tangentSample.Y * up + tangentSample.Z * normal
                    irradiance <- irradiance + envSampler.Sample(sampleVec)
                    theta  <- theta  + sampleDelta
                phi <- phi + sampleDelta
                   
            return Math.PI * irradiance / nrSamples
        }

module Sky =

    let private skySamplerEquirec =
        sampler2d {
            texture uniform?SkyMapEquirec
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    //from https://learnopengl.com/PBR/IBL/Diffuse-irradiance
    [<ReflectedDefinition>]
    let sampleSphericalMap (vec : V3d) =
        let rotation = Math.PI //todo: get from uniform
        let invAtan = V2d (0.1591, 0.3183)
        let u = atan2 vec.Z vec.X |>  (+) rotation 
        let v = asin  vec.Y
        let uv = 
            V2d(u, v)
            |> (*) invAtan
            |> (+) 0.5
        uv

    let internal skyTextureEquirec (v : Vertex) =
        fragment {
            let lPos  = v.wp.XYZ |> Vec.normalize
            let uv  = sampleSphericalMap lPos
            let texColor = skySamplerEquirec.Sample(uv)
            return texColor
        }

    let private skySampler =
        samplerCube {
            texture uniform?SkyCubeMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let internal skyBoxTrafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            //let rotView  = m33d uniform.ViewTrafo |> m44d 
            //let  clipPos =  uniform.ProjTrafo * rotView * wp
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

    let internal skyTexture (v : Vertex) =
        fragment {
            let gamma  = 2.2
            
            let lPos  = v.wp.XYZ |> Vec.normalize
            let texColor = skySampler.Sample(lPos).XYZ

            //Reihnard tone mapping
            let colm = texColor / (texColor+1.0)

            //gamma  correction
            let colg = pow colm (V3d(1.0/gamma))

            return V4d(colg,v.c.W)
        }


module SLESurfaces = 

    let lighting = Lighting.lighting
    let lightingPBR  = PBR.lighting
    let skyTextureEquirec = Sky.skyTextureEquirec
    let skyTexture = Sky.skyTexture
    let skyBoxTrafo = Sky.skyBoxTrafo
    let convoluteDiffuseIrradiance = PBR.convoluteDiffuseIrradiance

    