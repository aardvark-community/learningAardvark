namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Rendering.Effects
open System

module fshadeExt = 
    open FShade
    //some missing intrinsics for fshade  
    [<GLSLIntrinsic("mix({0}, {1}, {2})")>] // Define function as intrinsic, no implementation needed
    let Lerp (a : V3d) (b : V3d) (s : float) : V3d = failwith ""

    [<GLSLIntrinsic("exp({0})")>] // Define function as intrinsic, no implementation needed
    let exp (a : V3d) : V3d = failwith ""

    [<GLSLIntrinsic("length({0})")>] // Define function as intrinsic, no implementation needed
    let length (a : V3d) : V3d = failwith ""

    [<GLSLIntrinsic("length({0})")>] // Define function as intrinsic, no implementation needed
    let length2 (a : V2d) : float = failwith ""

    [<GLSLIntrinsic("max({0}.x,max({0}.y,{0}.z))")>] // Define function as intrinsic, no implementation needed
    let max3 (a : V3d) : float = Vec.NormMax(a)

    [<GLSLIntrinsic("min({0}.x,min({0}.y,{0}.z))")>]
    let min3 (a : V3d) : float = min a.X a.Y |> min a.Z
    
module  displacemntMap =
    //simple  displacement mapping, I am not realy happy with the results.

    let private samplerDisp =
        sampler2d {
            texture uniform?DisplacmentMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.Gray50
        }

    type UniformScope with
        member x.DisplacmentStrength : float =  x?DisplacmentStrength


    let internal displacementMap (tri : Triangle<Vertex>) =
        tessellation {
            let displacmentStrength = uniform.DisplacmentStrength
            // calculate tessellation levels (TessControl)
            let center = (tri.P0.wp + tri.P1.wp + tri.P2.wp) / 3.0
            let level = if displacmentStrength = 0.0  then 1.0 else 32.0

            // call tessellateTriangle/tessellateQuad
            let! coord = tessellateTriangle level (level, level, level)

            // interpolate the attributes (TessEval)
            let wp' = coord.X * tri.P0.wp + coord.Y * tri.P1.wp + coord.Z * tri.P2.wp
            let n = coord.X * tri.P0.n + coord.Y * tri.P1.n + coord.Z * tri.P2.n |> Vec.normalize
            let b = coord.X * tri.P0.b + coord.Y * tri.P1.b + coord.Z * tri.P2.b
            let t = coord.X * tri.P0.t + coord.Y * tri.P1.t + coord.Z * tri.P2.t
            let tc = coord.X * tri.P0.tc + coord.Y * tri.P1.tc + coord.Z * tri.P2.tc
            let c = coord.X * tri.P0.c + coord.Y * tri.P1.c + coord.Z * tri.P2.c

            let disp = (-0.5 + samplerDisp.Sample(tc).X) * uniform.DisplacmentStrength
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

module AlbedoColor = 

    let private albedoSampler =
        sampler2d {
            texture uniform?AlbedoColorTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type UniformScope with
        member x.AlbedoColor : V4d =  x?AlbedoColor

    let internal albedoColor (v : Vertex) =
        fragment {
            let texColor = albedoSampler.Sample(v.tc)
            let c = uniform.AlbedoColor
            return texColor * c
        }

module linearDepth =

    [<ReflectedDefinition>]
    let getLinearDepth (projTrafoInv : M44d) (depth : Sampler2d) (ndc : V2d) =
        let tc = 0.5 * (ndc + V2d.II)
        let z = 2.0 * depth.Sample(tc, 0.0).X - 1.0

        let pp = V4d(ndc.X, ndc.Y, z, 1.0) 
        let temp = projTrafoInv * pp
        temp.Z / temp.W