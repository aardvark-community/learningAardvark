namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open System
(*
    Shaders for pysical based rendering (PBR)

    They are mostly ports of the tutorials found at https://learnopengl.com/https://learnopengl.com/ 
*)


//physical base rendering, mostly a port of the shaders found here https://learnopengl.com/PBR/Lighting,
//here https://learnopengl.com/PBR/IBL/Diffuse-irradiance and here https://learnopengl.com/PBR/IBL/Specular-IBL.

module BRDF =

    [<ReflectedDefinition>] //add this attribute to  make the function callable in the shader
    let  fresnelSchlick (f0 : V3d) (cosTheta : float)=
        //usually  set f90 to 1.0 but fade out if f0 < 0.02
        let f90 = Vec.dot f0  (V3d(50.0  * 0.33)) |> saturate
        f0 + (f90 - f0) * pow (1.0 - cosTheta) 5.0

    [<ReflectedDefinition>]
    let DistributionGGX nDotH roughness  =
        let a  = roughness*roughness
        let a2 = a * a
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
    let GeometrySmith ilb nDotV nDotL roughness =
        let ggx2 = GeometrySchlickGGX ilb nDotV roughness
        let ggx1 = GeometrySchlickGGX ilb nDotL roughness
        ggx1 * ggx2

    [<ReflectedDefinition>] 
    let  fresnelSchlickRoughness (f0 : V3d) (roughness : float) (cosTheta : float)=
        //usually  set f90 to 1.0 but fade out if f0 < 0.02
        let f90 = Vec.dot f0  (V3d(50.0  * 0.33) ) |> saturate
        let r = V3d.Max(V3d(f90 - roughness), f0)
        f0 + (r  - f0) * pow (1.0 - cosTheta) 5.0

    [<ReflectedDefinition>] 
    let distributionCharlie roughness nDotH =
        let  rcpR = 1.0 / roughness
        let cos2H = nDotH * nDotH
        let sin2H = 1.0 - cos2H
        (2.0 + rcpR) * (pow sin2H (rcpR * 0.5)) / Constant.PiTimesTwo

    [<ReflectedDefinition>] 
    let distributionAshikhmin roughness nDotH =
        let r2= roughness * roughness
        let cos2H = nDotH * nDotH
        let sin2H = 1.0 - cos2H
        let sin4H = sin2H * sin2H
        (sin4H + 4.0 * exp (-cos2H / (sin2H * r2))) / (Constant.Pi * (1.0 + 4.0 * r2) * sin4H)

    [<ReflectedDefinition>] 
    let visibilityAshikhmin nDotV nDotL =
        let denominator = (4.0 * (nDotL  +  nDotV  - nDotL * nDotV)) |> max 0.001
        1.0 / denominator  

    [<ReflectedDefinition>] 
    let L x (r : float) =  
        let rs = saturate r
        let rr =  1.0 - (1.0 - rs) * (1.0 - rs)
        let a = lerp 25.3245  21.5473 rr
        let b = lerp  3.32435  3.82987 rr
        let c = lerp  0.16801  0.19823 rr
        let d = lerp -1.27393 -1.97760 rr
        let e = lerp -4.85967 -4.32054 rr
        a / (1.0 +  b * pow x c) + d + x + e

    let visibilityCharlie roughness  nDotV nDotL =
        let visV = if nDotV < 0.5  then L nDotV roughness |> exp else 2.0 * L 0.5 roughness - L (1.0 - nDotV) roughness |> exp
        let visL = if nDotL < 0.5  then L nDotL roughness |> exp else 2.0 * L 0.5 roughness - L (1.0 - nDotL) roughness |> exp
        1.0 / ((1.0 + visV + visL) * 4.0 * nDotV * nDotL)

module IBL =
    open BRDF

    type UniformScope with
        member x.Roughness : float = x?Roughness
        member x.AmbientIntensity : float = x?AmbientIntensity
        member x.SkyMapRotation : float =  x?SkyMapRotation
        member x.SkyMapIntensity : float =  x?SkyMapIntensity
        member x.LightViewMatrix : M44d = x?LightViewMatrix

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

    [<GLSLIntrinsic("uint({0})")>]
    let u32 (a : int) : uint32 = onlyInShaderCode "uint32"

    [<ReflectedDefinition>]
    let radicalInverseVdC (bitss : uint32) =
        let mutable bits = (bitss <<< 16) ||| (bitss >>> 16)
        bits <- ((bits &&& u32 0x55555555) <<< 1) ||| ((bits &&& u32 0xAAAAAAAA) >>> 1)
        bits <- ((bits &&& u32 0x33333333) <<< 2) ||| ((bits &&& u32 0xCCCCCCCC) >>> 2)
        bits <- ((bits &&& u32 0x0F0F0F0F) <<< 4) ||| ((bits &&& u32 0xF0F0F0F0) >>> 4)
        bits <- ((bits &&& u32 0x00FF00FF) <<< 8) ||| ((bits &&& u32 0xFF00FF00) >>> 8)
        (float bits) * 2.3283064365386963e-10

    [<ReflectedDefinition>]
    let hammersley i n = 
        V2d(float i / float n, radicalInverseVdC(i))

    [<ReflectedDefinition>]
    let sampleHemisphereUniform (xi : V2d) =
        let phi = 2.0 * Math.PI * xi.X
        let cosTheta = 1.0 - xi.Y
        let sinTheta = sqrt(1.0 - cosTheta*cosTheta);
        // from spherical coordinates to cartesian coordinates
        
        V3d( cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta)

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
                    let g = GeometrySmith true nDotV nDotL roughness
                    let gVis = (g * vDotH) / (nDotH * nDotV)
                    let fc = pow (1.0 - vDotH) 5.0

                    a <- a + (1.0 - fc) * gVis
                    b <- b + fc * gVis;
        V2d(a/(float sampleCount), b/(float sampleCount))


    [<ReflectedDefinition>]
    let integrateBRDFCharlie nDotV roughness =
        let v = V3d(sqrt (1.0 - nDotV*nDotV), 0.0, nDotV)
        let n = V3d.OOI
        let sampleCount = 1024u
        let mutable r = 0.0
        for i in 0..(int sampleCount) do
                let xi = hammersley (uint32 i) sampleCount
                let h = sampleHemisphereUniform xi
                let l = 2.0 * Vec.dot v h * h |> Vec.normalize         
                let nDotL = max l.Z 0.0
                let nDotH = max h.Z 0.0
                let vDotH = Vec.dot v h |> max 0.0

                if nDotL > 0.0 then
                    let v = visibilityAshikhmin  nDotV nDotL 
                    let d = distributionCharlie roughness nDotH

                    r <- r + v * d * nDotL * vDotH
        r + (4.0 * 2.0 * Math.PI / ( float sampleCount))

    let integrateBRDFLtu (vert : Vertex) =
        fragment {
           let r = integrateBRDF vert.tc.X vert.tc.Y
           let c = integrateBRDFCharlie vert.tc.X vert.tc.Y
           return V4d(V3d(r,c),1.0)
        }


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