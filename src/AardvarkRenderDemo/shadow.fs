namespace SLEAardvarkRenderDemo

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.Base.Rendering
open SLEAardvarkRenderDemo.Model

(*
    Shadow map creation and evaluation

    Contact hardening soft shadows based on 
    Randima Fernando. Percentage-Closer Soft Shadows. http://developer.download.nvidia.com/shaderlibrary/docs/shadow_PCSS.pdf.
    and
    https://www.gamedev.net/articles/programming/graphics/contact-hardening-soft-shadows-made-fast-r4906/ 
*)
module Shadow =
    open Aardvark.SceneGraph
    open Aardvark.UI //nessary to avoid confusion between SceneGraph.SG and UI.SG 
    open FShade
    open fshadeExt
    open light
    
    let signatureShadowMap (runtime : IRuntime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]

    let shadowMapSize = V2i(2048) |> AVal.constant

    //calculate light view and prjection
    let lightViewPoject (bb : aval<Box3d>) (alight : AdaptiveLightCase) =
        match alight with
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
                    CameraView.lookAt (light.lightPosition.XYZ ) target.XYZ up
                    |> CameraView.viewTrafo 
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) 0.1 size 1.0
                    |> Frustum.projTrafo
                return lightView , proj, 0.1, size
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
                return lightView , proj, 0.1, size*2.0
            }
        | AdaptiveDiskLight l -> 
           adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let target = light.lightPosition + light.lightDirection
                let up = if abs(light.lightDirection.Z) < 0.0000001 && abs(light.lightDirection.X) < 0.0000001 then V3d.OOI else V3d.OIO
                let! offset = (calcVirtualPositionOffset alight) //offset position so that the disk fits into the Camera Frustum
                let n = light.lightDirection.XYZ |> Vec.normalize
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ - (offset.X * n)) target.XYZ up
                    |> CameraView.viewTrafo 
                //set the near plane so that the offset is compensated
                let zNear = light.radius+offset.X |> max 0.1
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) zNear size 1.0
                    |> Frustum.projTrafo
                return lightView , proj, zNear, size
            }
        | AdaptiveRectangleLight l -> 
            adaptive {
                let! light = l
                let! BB = bb
                let size = (BB.Max - BB.Min).Length
                let n = light.lightDirection.XYZ |> Vec.normalize
                let target = light.lightPosition + light.lightDirection
                //claculat sky direction according to the rotation of the rectangle
                let rotate = M44d.RotationInDegrees(n,light.rotation) * M44d.RotateInto(V3d.OIO, n) 
                let up = (rotate * V4d.OOIO).XYZ
                //offset the position sot thet the  camera fits into the camera fustrum, use the bigger offset of the twoc directions 
                let! o = (calcVirtualPositionOffset alight)
                let offset = max o.X o.Y
                let lightView = 
                    CameraView.lookAt (light.lightPosition.XYZ - (offset * n)) target.XYZ up
                    |> CameraView.viewTrafo 
                //set the near plane so that the offset is compensated, minimum near plane 0.1 to avoid artefacts
                let proj = 
                    Frustum.perspective ((light.fallOff+light.cutOffInner) *2.0) (max offset 0.1) size 1.0
                    |> Frustum.projTrafo
                return lightView , proj, max offset 0.1, size
            }
            
    let shadowMap (runtime : IRuntime) (scene :ISg<'msg>) (bb : aval<Box3d>) (light : AdaptiveLightCase) =
            let pv = lightViewPoject bb light
            let v = pv |> AVal.map (fun (m,_,_,_) -> m)
            let p = pv |> AVal.map (fun (_,m,_,_) -> m)
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

    //shaders for shadow evaluation

    type UniformScope with
        member x.LightViewProjMatrix : M44d = x?LightViewProjMatrix
        member x.Light : SLEUniform.Light = x?Light


    let private samplerShadowMap =
        sampler2dShadow {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }

    let private samplerShadowMapTex =
        sampler2d {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
        }

    //assume size of 1 cm for punctual lights
    [<ReflectedDefinition>]
    let lightWidth () =
        let l = uniform.Light
        match l.lightType with
        | SLEUniform.LightType.DirectionalLight -> V2d(0.01)
        | SLEUniform.LightType.PointLight -> V2d(0.01) 
        | SLEUniform.LightType.SpotLight -> V2d(0.01)   
        | SLEUniform.LightType.SphereLight -> V2d(l.radius * 2.0 |> max 0.01)
        | SLEUniform.LightType.DiskLight -> V2d(l.radius * 2.0 |> max 0.01)  
        | SLEUniform.LightType.RectangleLight -> V2d(Vec.length(l.p1 - l.p4) |> max 0.01, Vec.length(l.p1 - l.p2) |> max 0.01)    
        | SLEUniform.LightType.NoLight -> V2d(0.01)
        | _ -> V2d(0.01)

    //not used 
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

    //Pseudo Random Value from a V4d Seed
    [<ReflectedDefinition>]
    let  random (seed  : V4d)  =
        let dotProduct = Vec.dot seed (V4d(12.9898,78.233,45.164,94.673))
        Fun.Frac(sin(dotProduct) * 43758.5453)

    //not used because VogelDisk Sampling looks better 
    [<ReflectedDefinition>]
    let poissonSamplingStrat (shadowMap :Sampler2dShadow) (samplePos : V4d) (pos  : V4d) comp  =
        let poissonDisk =   
            Arr<N<16>, V2d>([|
                V2d( -0.94201624, -0.39906216 );V2d( 0.94558609, -0.76890725 );V2d( -0.094184101, -0.92938870 );V2d( 0.34495938, 0.29387760 )
                V2d( -0.91588581, 0.45771432 );V2d( -0.81544232, -0.87912464 );V2d( -0.38277543, 0.27676845 );V2d( 0.97484398, 0.75648379  )
                V2d(  0.44323325, -0.97511554 );V2d( 0.53742981, -0.47373420 );V2d( -0.26496911, -0.41893023 );V2d(  0.79197514, 0.19090188  )
                V2d( -0.24188840, 0.99706507 );V2d(  -0.81409955, 0.91437590);V2d(  0.19984126, 0.78641367 );V2d(  0.14383161, -0.14100790 )
            |])
        let numSamples = 16
        let mutable vis = 0.0
        let spread = 600.0 
        for i in 0..numSamples-1 do
            let index = int (16.0*random (V4d(pos.XYZ, float i)) )%16
            vis <- vis + shadowMap.Sample(samplePos.XY + poissonDisk.[index]/spread, comp)/(float numSamples)
        vis
        
    //Vogel Dsik coordinates
    [<ReflectedDefinition>]
    let vogelDiskOffset (sampleIndex : int) (sampleCount : int)  (phi : float) =
        let goldenAngle = 2.39996
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

    //sample form Shadow map around pos in a VogelDisk pattern rotated by noise 
    [<ReflectedDefinition>]
    let vogelDiskSampling (noise : float) (pos : V4d)  spread =
        let numSamples = 16
        let mutable vis = 0.0
        let shadowBias = 0.005
        for i in 0..numSamples-1 do
            let p = pos +  V4d((vogelDiskOffset i numSamples noise)*spread,0.0,0.0)
            //to normalized device coordinates 
            let samplePos = 
                p/p.W
                |> (*) 0.5
                |> (+) 0.5 
            vis <- vis + samplerShadowMap.Sample(samplePos.XY, samplePos.Z-shadowBias)/(float numSamples)
        vis

    //Calculate preumbra from average Blocker Depth
    [<ReflectedDefinition>]
    let avgBlockersDepthToPenumbra (lightSize : V2d) surfaceDepth avgBlockersDepth =
        lightSize * (surfaceDepth - avgBlockersDepth) / avgBlockersDepth

    //approximate average Blocker Depth
    [<ReflectedDefinition>]
    let penumbra noise (shadowMapUV : V2d) (surfaceDepth : float)  lightSize=
        let numSamples = 16
        let mutable avgBlockersDepth = 0.0
        let mutable blockersCount = 0.0
        for i in 0..numSamples-1 do
            let sampleUV = shadowMapUV + (vogelDiskOffset i numSamples noise)/100.0
            let sampleDepth = samplerShadowMapTex.Sample(sampleUV).X
            if sampleDepth < surfaceDepth then
                avgBlockersDepth <- avgBlockersDepth + sampleDepth
                blockersCount <- blockersCount + 1.0

        if blockersCount > 0.0 then  
            avgBlockersDepth <- avgBlockersDepth / blockersCount
            avgBlockersDepthToPenumbra lightSize surfaceDepth avgBlockersDepth
        else
            V2d(0.0)

    [<ReflectedDefinition>]
    let getShadow (wPos : V4d) = 
        let lm = uniform.LightViewProjMatrix
        let lightSpacePos = lm * wPos
        //to normalized device coordinates 
        let samplePos = 
            lightSpacePos/lightSpacePos.W
            |> (*) 0.5
            |> (+) 0.5
        //this noise function looks better than interleavedGradientNoise
        let noise = Constant.PiTimesTwo * random samplePos
        let spread = lightWidth () |> penumbra noise samplePos.XY samplePos.Z  
        vogelDiskSampling noise lightSpacePos spread