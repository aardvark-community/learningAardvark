namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module subSurface =

    let gaussian (variance : float) (r : float) (falloff : V3d) =
        let rr = r / (falloff + 0.001)
        exp -(rr*rr) / (2.0  * variance) / (2.0 * Constant.Pi * variance)
    
    let profile (r : float) (falloff : V3d)= 
        0.100 * gaussian 0.0484 r falloff +
        0.118 * gaussian 0.187  r falloff +
        0.113 * gaussian 0.567  r falloff +
        0.358 * gaussian 1.99   r falloff +
        0.078 * gaussian 7.41   r falloff   

    let kernel (samples : int) (strength : V3d) (falloff : V3d) =
        let range  = 2.0
        let exponent = 2.0

        let step = range / (float(samples) - 1.0)


        let calcOffset i = 
            let o  = -range + float(i) * step
            let s = if o <  0.0 then -1.0 else 1.0
            range * s * (abs (pow o exponent)) / (pow range exponent)

        let offsets = Array.init samples calcOffset

        let calcWeights i =
            let w0 = if i < 0 then abs (offsets.[i] - offsets.[i-1]) else  0.0
            let w1 = if i < samples - 1 then abs (offsets.[i] - offsets.[i+1]) else  0.0
            let area = (w0+w1) / 2.0
            let weight = area * profile (offsets.[i]) falloff
            V4d(weight, offsets.[i])

        let weights = Array.init samples calcWeights

        //reorder so that the element with offset 0 comes first
        let w0 = weights.[samples/2]

        let reorder i =
            let i' = if  i = 0 then samples/2 else if  i <= samples/2 then  i-1 else i
            weights.[i']

        let weights' = Array.init samples reorder

        let sumWeights = Array.sumBy (fun (v : V4d) -> v.XYZ) weights'
        let normalized = Array.map (fun (v : V4d) -> V4d(v.XYZ / sumWeights,v.W)) weights'

        let applayStrength i (v : V4d) =
            let weight =
                if i = 0 then 
                    (V3d.III - strength) + strength * v.XYZ
                else
                    strength * v.XYZ
            V4d(weight,v.W)

        Array.mapi applayStrength normalized

(*
   
*)