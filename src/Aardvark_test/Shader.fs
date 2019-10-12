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

module fshadeExt = 
    
    [<GLSLIntrinsic("mix({0}, {1}, {2})")>] // Define function as intrinsic, no implementation needed
    let Lerp (a : V3d) (b : V3d) (s : float) : V3d = failwith ""

    [<GLSLIntrinsic("exp({0})")>] // Define function as intrinsic, no implementation needed
    let exp (a : V3d) : V3d = failwith ""

module PBR =
    open fshadeExt

    type UniformScope with
     
        member x.Lights : Arr<N<10>, SLEUniform.Light> = 
         x?Lights

        member x.NumLights : int = x?NumLights

        member x.Roughness : float = x?Roughness

        member x.Metallic : float = x?Metallic

        member x.AlbedoFactor : float = x?AlbedoFactor

        member x.Expousure : float =  x?Expousure

        member x.AmbientIntensity : float = x?AmbientIntensity

        member x.SkyMapRotation : float =  x?SkyMapRotation

        member x.SkyMapIntensity : float =  x?SkyMapIntensity

        member x.Discard : bool =  x?Discard

        member x.LightViewMatrix : M44d = x?LightViewMatrix

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

    let private prefilteredSpecColorSampler =
        samplerCube {
            texture uniform?PrefilteredSpecColor
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private samplerBRDFLtu =
        sampler2d {
            texture uniform?BRDFLtu
            filter Filter.MinMagMipLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    let private samplerShadowMap =
        sampler2dShadow {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }

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
    let GeometrySchlickGGX ilb nDotV roughness =
        let k  =
            if ilb then 
                roughness * roughness / 2.0 
            else
                let r = roughness + 1.0
                r * r / 8.0
        let denom = nDotV * (1.0 - k) + k
        nDotV / denom
    
    [<ReflectedDefinition>]
    let GeometrySmith ilb (n : V3d) v l roughness =
        let nDotV = Vec.dot n v |> max 0.0
        let nDotL = Vec.dot n l |> max 0.0
        let ggx2 = GeometrySchlickGGX ilb nDotV roughness
        let ggx1 = GeometrySchlickGGX ilb nDotL roughness
        ggx1 * ggx2

    [<ReflectedDefinition>] //add this attribute to  make the function callable in the shader
    let  fresnelSchlickRoughness (f0 : V3d) (roughness : float) (cosTheta : float)=
        let r = V3d.Max(V3d(1.0 - roughness), f0)
        f0 + (r  - f0) * pow (1.0 - cosTheta) 5.0

    [<ReflectedDefinition>]
    let poissonSampling (shadowMap :Sampler2dShadow) (samplePos : V4d) comp  =
        let poissonDisk =   
            Arr<N<4>, V2d>([|V2d( -0.94201624, -0.39906216 );V2d( 0.94558609, -0.76890725 );V2d( -0.094184101, -0.92938870 );V2d( 0.34495938, 0.29387760 )|])
        let numSamples = 4
        let mutable vis = 0.0
        let spread = 400.0
        for i in 0..numSamples-1 do
            vis <- vis + shadowMap.Sample(samplePos.XY + poissonDisk.[i]/spread, comp)/(float numSamples)
        vis

    [<ReflectedDefinition>]
    let  random (seed  : V3d) (i : int) =
        let seed4 = V4d(seed,float i)
        let dotProduct = Vec.dot seed4 (V4d(12.9898,78.233,45.164,94.673))
        Fun.Frac(sin(dotProduct) * 43758.5453)

    [<ReflectedDefinition>]
    let poissonSamplingStrat (shadowMap :Sampler2dShadow) (samplePos : V4d) (pos  : V4d) comp  =
        let poissonDisk =   
            Arr<N<16>, V2d>([|
                V2d( -0.94201624, -0.39906216 );V2d( 0.94558609, -0.76890725 );V2d( -0.094184101, -0.92938870 );V2d( 0.34495938, 0.29387760 )
                V2d( -0.91588581, 0.45771432 );V2d( -0.81544232, -0.87912464 );V2d( -0.38277543, 0.27676845 );V2d( 0.97484398, 0.75648379  )
                V2d(  0.44323325, -0.97511554 );V2d( 0.53742981, -0.47373420 );V2d( -0.26496911, -0.41893023 );V2d(  0.79197514, 0.19090188  )
                V2d( -0.24188840, 0.99706507 );V2d(  -0.81409955, 0.91437590);V2d(  0.19984126, 0.78641367 );V2d(  0.14383161, -0.14100790 )
            |])
        let numSamples = 8
        let mutable vis = 0.0
        let spread = 600.0
        for i in 0..numSamples-1 do
            let index = int (16.0*random (pos.XYZ) i )%16
            vis <- vis + shadowMap.Sample(samplePos.XY + poissonDisk.[index]/spread, comp)/(float numSamples)
        vis

    let internal lighting  (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            if uniform.Discard then
                discard()
            let albedo = pow (vert.c.XYZ * uniform.AlbedoFactor) (V3d(gamma))
            let metallic = uniform.Metallic
            let roughness = uniform.Roughness

            let cameraPos = uniform.CameraLocation

            let n = vert.n |> Vec.normalize
            let v = cameraPos - vert.wp.XYZ |> Vec.normalize
            let r = Vec.reflect -v n

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
                    |_ ->  false, V3d(0.0), V3d(0.0)  //allways match any cases, otherwise fshade will give a cryptic error 
              
                let oi = 
                    if exists then
                        let h = v + lDir |> Vec.normalize

                        // cook-torrance brdf
                        let ndf = DistributionGGX n h roughness 
                        let g = GeometrySmith false n v lDir roughness 
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

                let shadow =
                    if i = 0 then
                        let lm = uniform.LightViewMatrix
                        let lightSpacePos = lm * vert.wp
                        let samplePos = 
                            lightSpacePos/lightSpacePos.W
                            |> (*) 0.5
                            |> (+) 0.5
                        let shadowBias = 0.005
                        poissonSamplingStrat samplerShadowMap samplePos vert.wp (samplePos.Z-shadowBias)
                        //samplerShadowMap.Sample(samplePos.XY, samplePos.Z-shadowBias)
                    else
                        1.0
                lo <- lo + (oi*shadow)

            let kSA = fresnelSchlickRoughness f0 roughness nDotV
            let kdA  = (1.0 - kSA) * (1.0 - metallic)
            let irradiance = diffuseIrradianceSampler.Sample(n).XYZ
            let diffuse = irradiance * albedo

            let maxReflectLod = 4.0
            let prefilteredColor = prefilteredSpecColorSampler.SampleLevel(r, roughness * maxReflectLod).XYZ
            let brdf = samplerBRDFLtu.Sample(V2d(nDotV, roughness)).XY
            let specular = prefilteredColor * (kSA * brdf.X + brdf.Y)
   
            let ambient = kdA * diffuse + specular//todo:  abient occlusion
            let ambientIntensity = uniform.AmbientIntensity
            let col = lo + ambient * ambientIntensity

            // tone mapping
            let expousure = uniform.Expousure
            let colm = V3d(1.0) - exp (-col*expousure)//col / (col+1.0)

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

    [<ReflectedDefinition>]
    let radicalInverseVdC (bitss : uint32) =
        let mutable bits = (bitss <<< 16) ||| (bitss >>> 16)
        bits <- ((bits &&& 0x55555555u) <<< 1) ||| ((bits &&& 0xAAAAAAAAu) >>> 1)
        bits <- ((bits &&& 0x33333333u) <<< 2) ||| ((bits &&& 0xCCCCCCCCu) >>> 2)
        bits <- ((bits &&& 0x0F0F0F0Fu) <<< 4) ||| ((bits &&& 0xF0F0F0F0u) >>> 4)
        bits <- ((bits &&& 0x00FF00FFu) <<< 8) ||| ((bits &&& 0xFF00FF00u) >>> 8)
        (float bits) * 2.3283064365386963e-10

    [<ReflectedDefinition>]
    let hammersley i n = 
        V2d(float i / float n, radicalInverseVdC(i))
    
    [<ReflectedDefinition>]
    let importanceSampleGGX (xi : V2d) (n : V3d) roughness =
        let a = roughness*roughness
        let phi = 2.0 * Math.PI * xi.X
        let cosTheta = sqrt((1.0 - xi.Y) / (1.0 + (a*a - 1.0) * xi.Y))
        let sinTheta = sqrt(1.0 - cosTheta*cosTheta)

        // from spherical coordinates to cartesian coordinates
        let h = V3d( cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta)

        // from tangent-space vector to world-space sample vector
        let up = if abs(n.Z) < 0.999 then  V3d.OOI else V3d.IOO
        let tangent = Vec.cross up n |> Vec.normalize
        let bitangent = Vec.cross n  tangent
        tangent * h.X + bitangent * h.Y + n * h.Z |> Vec.normalize  

    let prefilterSpec (vert : Vertex) =
        fragment {
            let n = vert.wp.XYZ |> Vec.normalize
            let v = n

            let sampleCount = 1024u
            let mutable totalWeight = 0.0;
            let mutable prefilteredColor = V3d.OOO
            for i in 0..(int sampleCount) do
                let xi = hammersley (uint32 i) sampleCount
                let h = importanceSampleGGX xi n uniform.Roughness
                let l = 2.0 * Vec.dot v h * h |>  Vec.normalize

                let nDotL = Vec.dot n l |> max 0.0
                if nDotL > 0.0 then
                    prefilteredColor <- prefilteredColor + envSampler.Sample(l).XYZ * nDotL
                    totalWeight <- totalWeight + nDotL
                else ()

            prefilteredColor <- prefilteredColor/totalWeight
            return V4d(prefilteredColor,vert.c.W)  
        }

    [<ReflectedDefinition>]
    let integrateBRDF nDotV roughness =
        let v = V3d(sqrt (1.0 - nDotV*nDotV), 0.0, nDotV)
        let n = V3d.OOI
        let sampleCount = 1024u
        let mutable a = 0.0
        let mutable b = 0.0
        for i in 0..(int sampleCount) do
                let xi = hammersley (uint32 i) sampleCount
                let h = importanceSampleGGX xi n roughness
                let l = 2.0 * Vec.dot v h * h |> Vec.normalize         
                let nDotL = max l.Z 0.0
                let nDotH = max h.Z 0.0
                let vDotH = Vec.dot v h |> max 0.0

                if nDotL > 0.0 then
                    let g = GeometrySmith true n  v  l roughness
                    let gVis = (g * vDotH) / (nDotH * nDotV)
                    let fc = pow (1.0 - vDotH) 5.0

                    a <- a + (1.0 - fc) * gVis
                    b <- b + fc * gVis;
        V2d(a/(float sampleCount), b/(float sampleCount))

    let integrateBRDFLtu (vert : Vertex) =
        fragment {
           let r = integrateBRDF vert.tc.X vert.tc.Y
           return V4d(V3d(r,0.0),1.0)
        }

    let testBDRF (vert : Vertex)=
        fragment  {
            return samplerBRDFLtu.Sample(vert.tc)
        }

    let private skySamplerEquirec =
        sampler2d {
            texture uniform?SkyMapEquirec
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }


    let private testSampler =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let testShadowMap (vert : Vertex)=
        fragment  {
            let  deep = testSampler.Sample(vert.tc).X
            return V4d(V3d(deep),1.0)
        }

    //from https://learnopengl.com/PBR/IBL/Diffuse-irradiance
    [<ReflectedDefinition>]
    let sampleSphericalMap (vec : V3d) =
        let rotation = uniform.SkyMapRotation
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
            //let texColor = skySampler.SampleLevel(lPos,2.0).XYZ

            let col = texColor * uniform.SkyMapIntensity

            //tone mapping
            let expousure = uniform.Expousure
            let colm = V3d(1.0) - exp (-col*expousure)//col / (col+1.0)
            
            //gamma  correction
            let colg = pow colm (V3d(1.0/gamma))

            return V4d(colg,v.c.W)
        }

module NormalMap =

    type UniformScope with
        member x.NormalMapStrength : float =  x?NormalMapStrength

    [<GLSLIntrinsic("mix({0}, {1}, {2})")>] // Define function as intrinsic, no implementation needed
    let Lerp (a : V3d) (b : V3d) (s : float) : V3d = failwith ""

    let private normalSampler =
        sampler2d {
            texture uniform?NormalMapTexture
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

            return { v with n = n2 }
        }

module  displacemntMap =

    let private samplerDisp =
        sampler2d {
            texture uniform?Displacment
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.Gray50
        }

    let internal displacementMap (tri : Triangle<Vertex>) =
        tessellation {
            // calculate tessellation levels (TessControl)
            let center = (tri.P0.wp + tri.P1.wp + tri.P2.wp) / 3.0
            let level = 128.0

            // call tessellateTriangle/tessellateQuad
            let! coord = tessellateTriangle level (level, level, level)

            // interpolate the attributes (TessEval)
            let wp' = coord.X * tri.P0.wp + coord.Y * tri.P1.wp + coord.Z * tri.P2.wp
            let n = coord.X * tri.P0.n + coord.Y * tri.P1.n + coord.Z * tri.P2.n |> Vec.normalize
            let b = coord.X * tri.P0.b + coord.Y * tri.P1.b + coord.Z * tri.P2.b
            let t = coord.X * tri.P0.t + coord.Y * tri.P1.t + coord.Z * tri.P2.t
            let tc = coord.X * tri.P0.tc + coord.Y * tri.P1.tc + coord.Z * tri.P2.tc
            let c = coord.X * tri.P0.c + coord.Y * tri.P1.c + coord.Z * tri.P2.c

            let disp = (-0.5 + samplerDisp.Sample(tc).X) * 0.1
            let wp = wp' + V4d(n * disp, 0.0)
            let pos = uniform.ViewProjTrafo * wp

            return { 
                pos = pos
                wp = wp
                n = n
                t = t
                b = b
                tc = tc
                c = c
              }
        }

 module SLESurfaces = 

    let lighting = Lighting.lighting
    let lightingPBR  = PBR.lighting
    let skyTextureEquirec = PBR.skyTextureEquirec
    let skyTexture = PBR.skyTexture
    let skyBoxTrafo = PBR.skyBoxTrafo
    let convoluteDiffuseIrradiance = PBR.convoluteDiffuseIrradiance
    let prefilterSpec = PBR.prefilterSpec
    let integrateBRDFLtu = PBR.integrateBRDFLtu
    let  testBDRF = PBR.testBDRF
    let testShadowMap = PBR.testShadowMap
    let normalMap = NormalMap.normalMap
    let displacementMap  = displacemntMap.displacementMap
    