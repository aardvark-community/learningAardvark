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

    [<ReflectedDefinition>] 
    let  fresnelSchlickRoughness (f0 : V3d) (roughness : float) (cosTheta : float)=
        let r = V3d.Max(V3d(1.0 - roughness), f0)
        f0 + (r  - f0) * pow (1.0 - cosTheta) 5.0

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