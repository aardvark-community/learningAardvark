namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

module Shadow =
    open Aardvark.SceneGraph
    open Aardvark.UI //nessary to avoid confusion between SceneGraph.SG and UI.SG 
    open FShade
    open fshadeExt
    
    let signatureShadowMap (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]

    let shadowMapSize = V2i(1024) |> AVal.constant

    //calculate light view and prjection
    let lightViewPoject (bb : aval<Box3d>) (light : AdaptiveLightCase) =
        match light with
        | AdaptivePointLight l -> failwith "not implemented"
        | AdaptiveSphereLight l -> failwith "not implemented"
        | AdaptiveSpotLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ) target.XYZ up
                    |> CameraView.viewTrafo 
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) 1.0 size 1.0
                    |> Frustum.projTrafo
                return lightView , proj
            }
        | AdaptiveDirectionalLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let distance = max BB.Min.Length BB.Max.Length
                let size = (BB.Max - BB.Min).Length
                let lightPos = -light.lightDirection.XYZ |> Vec.normalize |> (*) distance //make sure the light position is outside the sceneBB
                let up = if abs(lightPos.Z) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt lightPos V3d.OOO up
                    |> CameraView.viewTrafo 
                let b = BB.Transformed(lightView)
                let bb = Box3d(V3d(b.Min.XY,0.0001),V3d(b.Max.XY,size*2.0))//set Z Size so that all the scene fits in all cases (size*2.0 ist the upper bound, could be optimized)
                let proj = 
                    Frustum.ortho bb
                    |> Frustum.projTrafo
                return lightView , proj
            }
        | AdaptiveDiskLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ) target.XYZ up
                    |> CameraView.viewTrafo 
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) 1.0 size 1.0
                    |> Frustum.projTrafo
                return lightView , proj
            }
        | AdaptiveRectangleLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ) target.XYZ up
                    |> CameraView.viewTrafo 
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) 1.0 size 1.0
                    |> Frustum.projTrafo
                return lightView , proj
            }
            
    let shadowMap (runtime : IRuntime) (scene :ISg<'msg>) (bb : aval<Box3d>) (light : AdaptiveLightCase) =
            let pv = lightViewPoject bb light
            let v = pv |> AVal.map fst
            let p = pv |> AVal.map snd
            scene
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                }
            |> Sg.viewTrafo (v)
            |> Sg.projTrafo (p)
            |> Sg.compile runtime (signatureShadowMap runtime)
            |> RenderTask.renderToDepth shadowMapSize
            :> aval<_>

    type UniformScope with
        member x.LightViewMatrix : M44d = x?LightViewMatrix

    let private samplerShadowMap =
        sampler2dShadow {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }

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
    let vogelDiskOffset (sampleIndex : int) (sampleCount : int)  (phi : float) =
        let goldenAngle = 2.4
        let r = sqrt (float sampleIndex + 0.5) / sqrt(float sampleCount)
        let theta = float sampleIndex *  goldenAngle + phi
        let sine = sin theta
        let cosine = cos theta
        V2d(r * cosine, r * sine)
    
    [<ReflectedDefinition>]
    let interleavedGradientNoise (pos : V2d) =
        let magic = V3d(0.06711056, 0.00583715, 52.9829189)
        magic.Z * Fun.Frac (Vec.dot pos magic.XY) 
        |> Fun.Frac

    [<ReflectedDefinition>]
    let vogelDiskSampling (shadowMap :Sampler2dShadow) (samplePos : V2d)  comp  =
        let numSamples = 16
        let mutable vis = 0.0
        let spread = 50.0
        let noise =interleavedGradientNoise samplePos
        for i in 0..numSamples-1 do
            let p = samplePos +  (vogelDiskOffset i numSamples noise)/spread
            vis <- vis + shadowMap.Sample(p, comp)/(float numSamples)
        vis

    [<ReflectedDefinition>]
    let poissonSamplingStrat (shadowMap :Sampler2dShadow) (samplePos : V4d) (pos  : V4d) comp  =
        let poissonDisk =   
            Arr<N<16>, V2d>([|
                V2d( -0.94201624, -0.39906216 );V2d( 0.94558609, -0.76890725 );V2d( -0.094184101, -0.92938870 );V2d( 0.34495938, 0.29387760 )
                V2d( -0.91588581, 0.45771432 );V2d( -0.81544232, -0.87912464 );V2d( -0.38277543, 0.27676845 );V2d( 0.97484398, 0.75648379  )
                V2d(  0.44323325, -0.97511554 );V2d( 0.53742981, -0.47373420 );V2d( -0.26496911, -0.41893023 );V2d(  0.79197514, 0.19090188  )
                V2d( -0.24188840, 0.99706507 );V2d(  -0.81409955, 0.91437590);V2d(  0.19984126, 0.78641367 );V2d(  0.14383161, -0.14100790 )
            |])
        let numSamples = 32
        let mutable vis = 0.0
        let spread = 50.0
        for i in 0..numSamples-1 do
            let index = int (16.0*random (pos.XYZ) i )%16
            vis <- vis + shadowMap.Sample(samplePos.XY + poissonDisk.[index]/spread, comp)/(float numSamples)
        vis

    [<ReflectedDefinition>]
    let getSpecularDominantDirArea n v roughness =
        // Simple linear approximation 
        let r = - Vec.reflect v n
        let  lerpFactor = (1.0 - roughness);
        Lerp n r lerpFactor |> Vec.normalize

    [<ReflectedDefinition>]
    let getShadow (wPos : V4d) = 
        let lm = uniform.LightViewMatrix
        let lightSpacePos = lm * wPos
        let samplePos = 
            lightSpacePos/lightSpacePos.W
            |> (*) 0.5
            |> (+) 0.5
        let shadowBias = 0.005
        //poissonSamplingStrat samplerShadowMap samplePos wPos (samplePos.Z-shadowBias)
        vogelDiskSampling samplerShadowMap samplePos.XY (samplePos.Z-shadowBias)