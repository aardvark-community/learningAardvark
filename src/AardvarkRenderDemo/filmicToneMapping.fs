namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Rendering.Effects
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module filmicToneMapping =
    // adapted from here : http://filmicworlds.com/blog/filmic-tonemapping-with-piecewise-power-curves/

    type CurveParams =
        {
            P0 : V2d
            P1 : V2d
            W  : float
            Overshot : V2d
            Gamma  : float
        }

    type SegmentParams =
        {
            Offset : V2d
            Scale : V2d
            LnA  : float
            B : float 
        }

    type Curve = 
        {
            X0 : float
            X1 : float
            W : float
            Toe : SegmentParams
            Linear : SegmentParams
            Shoulder : SegmentParams
        }

    [<ReflectedDefinition>]    
    let evalSegment (s : SegmentParams)  (x : float)=
        let x0 = (x - s.Offset.X) * s.Scale.X
        let y0 = if x0 > 0.0 then exp (s.LnA + s.B * log(x0)) else 0.0
        y0 * s.Scale.Y + s.Offset.Y

     // convert to y=mx+b
    let asSlopeInterceptf (p0 : V2d) (p1 :V2d) =
        let d = p1 - p0 
        let m = if d.X = 0.0 then  1.0 else d.Y/d.X
        let b =  p0.Y - p0.X * m
        (b, m)

    // find a function of the form:
    //   f(x) = e^(lnA + Bln(x))
    // where
    //   f(0)   = 0; not really a constraint
    //   f(x0)  = y0
    //   f'(x0) = m
    let solveAB (p0 : V2d) (m : float) =
        let B = m * p0.X / p0.Y
        let lnA =(log p0.Y) - (B * log p0.X)
        (B, lnA)

    // f(x) = (mx+b)^g
    // f'(x) = gm(mx+b)^(g-1)
    let derivativeLinearGamma m (b : float) g (x : float) = g * m * (pow (m * x + b) (g - 1.0))

    let CurveFromParams  (P : CurveParams) =
        //normaize parameters
        let W' = 1.0
        let p0' = V2d(P.P0.X/P.W, P.P0.Y)
        let p1' = V2d(P.P1.X/P.W, P.P1.Y)
        let overshot' = V2d(P.Overshot.X/P.W,P.Overshot.Y)

        // base function of linear section plus gamma is
        // y = (mx+b)^g

        // which we can rewrite as
        // y = exp(g*ln(m) + g*ln(x+b/m))

        // and our evaluation function is (skipping the if parts):
        (*
            x0 = (x - Offset.X)*Scale.X;
            y0 = exp(LnA + B*log(x0));
            y0*Scale.Y + Offset.Y;
        *)

        //linerar Segement, include gamma
        let  b, m = asSlopeInterceptf p0' p1'
        let linear' =  {
            Offset = V2d(-b/m, 0.0)
            Scale = V2d(1.0)
            LnA = P.Gamma * log(m)
            B = P.Gamma
        }
        
        //apply gamma to endpoints of linear section
        let p0'' = V2d(p0'.X, P.Gamma |> pow p0'.Y |> max 0.000001) 
        let p1'' = V2d(p1'.X, P.Gamma |> pow p1'.Y |> max 0.000001)
        let overshot'' = V2d(overshot'.X, (pow (1.0 + overshot'.Y) (P.Gamma)) - 1.0)

        //toe
        let toeM = derivativeLinearGamma m b P.Gamma p0''.X        
        let B, lnA = solveAB p0'' toeM 
        let toe'  =  {
            Offset = V2d(0.0)
            Scale = V2d(1.0)
            LnA = lnA
            B = B
        }

        // shoulder section
        let shoulderM = derivativeLinearGamma m b P.Gamma p1''.X

        // build the simple version that is usually too flat 
        let p0 = V2d(1.0) + overshot'' - p1''
        let b', lnA' = solveAB p0 shoulderM
        let shoulder'  =  {
            Offset =  V2d(1.0) + overshot''
            Scale = V2d(-1.0)
            LnA = lnA'
            B = b'
        }

        // Normalize so that we hit 1.0 at our white point. We wouldn't have do this if we 
        // skipped the overshoot part.
        let scale = evalSegment shoulder' 1.0

        let shoulder = 
            { shoulder' with
                Offset =  V2d(shoulder'.Offset.X, shoulder'.Offset.Y / scale)
                Scale =  V2d(shoulder'.Scale.X, shoulder'.Scale.Y / scale)
            }

        let toe = 
            { toe' with
                Offset =  V2d(toe'.Offset.X, toe'.Offset.Y / scale)
                Scale =  V2d(toe'.Scale.X, toe'.Scale.Y / scale)
            }

        let linear = 
            { linear' with
                Offset =  V2d(linear'.Offset.X, linear'.Offset.Y / scale)
                Scale =  V2d(linear'.Scale.X, linear'.Scale.Y / scale)
            }

        {X0 = p0''.X
         X1 =  p1''.X
         W = P.W
         Toe = toe 
         Linear = linear
         Shoulder = shoulder
        }

    let defaultToneMapping : ToneMapping =
        {
            ToeLength = 0.5
            ToeStrength = 0.5
            ShoulderLength = 0.5
            ShoulderStrength = 2.0
            ShoulderAngle  = 1.0
            Gamma = 1.0
        }

    let modelToParams (m :ToneMapping) =
  
        // This is not actually the display gamma. It's just a UI space to avoid having to 
        // enter small numbers for the input.
        let perceptualGamma = 2.2

        let toeLength = pow (m.ToeLength |> clamp 0.0 1.0) perceptualGamma 
        let toeStrength = m.ToeStrength |> clamp 0.0 1.0
        let shoulderLength = m.ShoulderLength |>  clamp 0.00001 1.0
        let shoulderStrength = m.ShoulderStrength |>  max 0.0
        let shoulderAngle = m.ShoulderAngle |> clamp 0.0 1.0

        // toe goes from 0 to 0.5
        let x0 = toeLength * 0.5
        let y0 = lerp x0 0.0 toeStrength // lerp from 0 to x0

        //slope of lineare section fixed to 1.0
        let remainingY = 1.0 - y0

        let initialW = x0 + remainingY

        let y1_offset = (1.0 - shoulderLength) * remainingY
        let x1 = x0 + y1_offset;
        let y1 = y0 + y1_offset;

        // filmic shoulder strength is in F stops
        let extraW = (pow 2.0 shoulderStrength) - 1.0

        let W = initialW + extraW

        let overshootX = (W * 2.0) * shoulderAngle * shoulderStrength
        let overshootY = 0.5 * shoulderAngle * shoulderStrength

        {
            P0 = V2d(x0, y0)
            P1 = V2d(x1, y1)
            W = W
            Overshot = V2d(overshootX, overshootY)
            Gamma = m.Gamma
        } 

    let  modelToCurve =
        modelToParams >> CurveFromParams

    let  defaultCurve =  
        defaultToneMapping
        |> modelToCurve

    type UniformScope with
        member x.mappingCurve : Curve = x?mappingCurve
        member x.Expousure : float =  x?Expousure

    [<ReflectedDefinition>]
    let evalCurve  (curve  : Curve) (x: float) =
        let xn = x / curve.W
        if xn < curve.X0  then evalSegment curve.Toe xn
        elif xn <  curve.X1 then evalSegment curve.Linear xn
        else evalSegment curve.Shoulder xn

    let toneMappingShader (vert : Vertex) =
        fragment {
            // tone mapping
            let expousure = uniform.Expousure
            let c = vert.c.XYZ*expousure 
            let r = evalCurve uniform.mappingCurve c.X
            let g = evalCurve uniform.mappingCurve c.Y
            let b = evalCurve uniform.mappingCurve c.Z
            let colg = pow (V3d(r, g, b)) (V3d(1.0/2.2))
            //let colg = pow c (V3d(1.0/2.2))

            return V4d(colg, 1.0)
        }

    let toneMapping (runtime : IRuntime) outputSignature (inputTexture : aval<IBackendTexture>) (m : AdaptiveModel) =             
        Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.texture DefaultSemantic.DiffuseColorTexture inputTexture
            |> Sg.uniform "Expousure" m.expousure
            |> Sg.uniform "mappingCurve" (AVal.map modelToCurve m.toneMapping)
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
                do! toneMappingShader
                }    
            |> Sg.compile runtime outputSignature 

module filmicToneMappingControl =
    open Aardvark.UI
    open Aardvark.UI.Primitives

    type Message =
        | SetToeLength of float
        | SetToeStrength of float
        | SetShoulderLength of float
        | SetShoulderStrength of float
        | SetShoulderAngle of float

    let update (m : ToneMapping)  (msg : Message) = 
        match msg  with
        | SetToeLength s -> {m with ToeLength = s}
        | SetToeStrength s -> {m with ToeStrength = s}
        | SetShoulderLength s -> {m with ShoulderLength = s}
        | SetShoulderStrength s -> {m with ShoulderStrength = s}
        | SetShoulderAngle s -> {m with ShoulderAngle = s}

    let view (m : aval<ToneMapping>) =
        Html.table [                        
            tr [] [ td [attribute "colspan" "2"] [text "Tone  Mapping"]]
            tr [] [ td [] [text "Toe Length"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] (AVal.map (fun t ->  t.ToeLength) m)  SetToeLength]]
            tr [] [ td [] [text "Toe Strength"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] (AVal.map (fun t ->  t.ToeStrength) m)  SetToeStrength]]
            tr [] [ td [] [text "Shoulder Length"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.00001;  max = 1.0; step = 0.01} [] (AVal.map (fun t ->  t.ShoulderLength) m)  SetShoulderLength]]
            tr [] [ td [] [text "Shoulder Strength"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 10.0; step = 0.01} [] (AVal.map (fun t ->  t.ShoulderStrength) m)  SetShoulderStrength]]
            tr [] [ td [] [text "Shoulder Angle"]; 
                    td [style "width: 70%;"] [inputSlider {min = 0.0;  max = 1.0; step = 0.01} [] (AVal.map (fun t ->  t.ShoulderAngle) m)  SetShoulderAngle]]
        ]       