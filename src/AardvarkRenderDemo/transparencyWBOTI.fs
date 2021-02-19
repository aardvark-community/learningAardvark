namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open System
(*
    Colored Weighed Blended Order Independent Trransparency
    http://casual-effects.blogspot.com/2015/03/colored-blended-order-independent.html

    with additional features from  
    http://casual-effects.com/research/McGuire2016Transparency/index.html
*)

module WBOTI =
    open fshadeExt

    type Fragment = {
        [<Color>]                    Color        : V4d
        [<FragCoord>]                CsPos        : V4d
        [<Semantic("Transmission")>] Transmission : V3d
       }

    type FragmentOut = {
        [<Semantic("ModulateColor")>] Modulate     : V3d
        [<Semantic("Accum")>]         Accum     : V4d
       }

    //calculates 
    let accumulate (frag : Fragment) =
        fragment {
            let coverage = frag.Color.W
            //Perform this operation before modifying the coverage to account for transmission.
            //modulation of background color by transmission color
            let modulate = coverage * (V3d.III - frag.Transmission)
            
            (* Modulate the net coverage for composition by the transmission. This does not affect the color channels of the
               transparent surface because the caller's BSDF model should have already taken into account if transmission modulates
               reflection. See 

               McGuire and Enderton, Colored Stochastic Shadow Maps, ACM I3D, February 2011
               http://graphics.cs.williams.edu/papers/CSSM/

               for a full explanation and derivation.*)           
            let netCoverage = coverage * (1.0 - Vec.dot frag.Transmission (V3d(1.0/3.0)))

            //calcualte weight. See reference implementation on http://casual-effects.com/research/McGuire2016Transparency/index.html for alternate weight functions 
            let tmp = (1.0 - frag.CsPos.Z * 0.99) 
            let w  = netCoverage * tmp * tmp * tmp * 1000.0  |>  clamp 0.01 30.0

            let accum = V4d(frag.Color.XYZ * coverage, netCoverage) * w
            return { Modulate = modulate; Accum = accum}
        } 

    let backgroundSampler =
        sampler2d {
            texture uniform?Colors
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let accumSampler =
        sampler2d {
            texture uniform?Accum
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let modulateSampler =
        sampler2d {
            texture uniform?Modulate
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let revaelageSampler =
        sampler2d {
            texture uniform?Revaelage
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let compose (frag : Vertex) =
        fragment {
            let modulation = modulateSampler.Sample(frag.tc).XYZ
            let background = backgroundSampler.Sample(frag.tc).XYZ
            let color = 
                if min3 modulation = 1.0 then //no transparency at this pixel
                    background
                else
                    let accum = accumSampler.Sample(frag.tc)
                    
                    // In the case where the denominator overflowed, at least preserve some color
                    // instead of writing zero by dividing through by infinity
                    let accumDenom = (if accum.W.IsInfinity() then max3 accum.XYZ else accum.W) |> max 0.00001

                    // Suppress overflow of the numerator by outputting white
                    let accumColor0 = if (max3 accum.XYZ).IsInfinity() then V3d.III else accum.XYZ
 
                     // Attempt to fake transmission on the additive term by blending in a little bit of the 
                    // background modulation
                    let accum = accumColor0 * (V3d(0.5) + modulation / max 0.01 (2.0 * max3 modulation));

                    background * modulation + (V3d.III - modulation) * accum / accumDenom

            return V4d(color, 1.0)
        }