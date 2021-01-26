namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open System
(*
    Shaders for pysical based rendering (PBR) 

    They are mostly ports of the tutorials found at https://learnopengl.com/https://learnopengl.com/ 
    For the BRDF functions see brdf.fs

    Area light approximations for diffuse lightning are based on the Frostbite SigGraph 2014 Curse Notes
    Moving Frostbite to Physically Based Rendering by Sebastien Lagarde, Charles de Rousiers, Siggraph 2014
    http://www.frostbite.com/wp-content/uploads/2014/11/course_notes_moving_frostbite_to_pbr.pdf

    and for specular light on the Representative Point Method from Brian Karis
    http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf 

    The implemntation in the Wicked Engine was used as a code reference:
    https://github.com/turanszkij/WickedEngine/blob/master/WickedEngine/lightingHF.hlsli
*)

module PBR =
    open fshadeExt
    open BRDF
    open GBuffer

    type ShadeingType =
        | Standard = 0
        | Cloth = 1

    type UniformScope with
        member x.Light : SLEUniform.Light = x?Light
        member x.AmbientIntensity : float = x?AmbientIntensity
        member x.LightViewM : M44d = x?LightViewM
        member x.LightProjM : M44d = x?LightProjM
        member x.LightProjMInv : M44d = x?LightProjMInv
        member x.LightFarZ : float = x?LightFarZ
        member x.LightViewMatrix : M44d = x?LightViewMatrix
        member x.sssWidth :  Arr<N<8>, float> = x?sssWidth
        member x.sssFalloff :  Arr<N<8>, V3d> = x?sssFalloff
        member x.sssStrength :  Arr<N<8>, V3d> = x?sssStrength

    //Note: Do not use ' in variabel names for shader code, it will lead to an error because it is not valid for GLSL

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

    let ambientOcc =
        sampler2d {
            texture uniform?AmbientOcclusion
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let samplerShadowMap1 =
        sampler2d {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
        }

    //punctual and area light intensity is given as luminous Power, 
    //we need to convert that to luminance
    [<ReflectedDefinition>]
    let luminousPowerToLuminancePoint =
        1.0 / (4.0 * Math.PI)

    [<ReflectedDefinition>]
    let luminousPowerToLuminanceSpot =
        1.0 / Math.PI
        
    [<ReflectedDefinition>]
    let luminousPowerToLuminanceSphere radius =
        1.0 / (radius * radius * 4.0 * Math.PI * Math.PI)
 
    [<ReflectedDefinition>]
    let luminousPowerToLuminanceDisk radius =
        1.0 / (radius * radius  * Math.PI * Math.PI)

    [<ReflectedDefinition>]
    let luminousPowerToLuminanceRectangle (p1 : V3d)  (p2 : V3d)  (p3 : V3d)  (p4 : V3d) =
        let w = p1 - p4 |> Vec.length  |> abs
        let h = p1 - p2 |> Vec.length  |> abs
        1.0 / (h * w * Math.PI)

    //aproximation of main direction of the specular lobe
    [<ReflectedDefinition>]
    let getSpecularDominantDirArea n v roughness =
        // Simple linear approximation 
        let r = - Vec.reflect v n
        let  lerpFactor = 1.0 - roughness;
        Lerp n r lerpFactor |> Vec.normalize

    // some intersection functions

    // o		: ray origin
    // d		: ray direction
    // returns distance on the ray to the object if hit, 0 otherwise
    [<ReflectedDefinition>]
    let tracePlane (o : V3d) (d : V3d) (planeOrigin : V3d) (planeNormal : V3d) =
        (Vec.dot planeNormal (planeOrigin - o)) / (Vec.dot planeNormal d)

    // o		: ray origin
    // d		: ray direction
    // A,B,C	: traingle corners
    // returns distance on the ray to the object if hit, 0 otherwise
    [<ReflectedDefinition>]
    let traceTriangle (o : V3d) (d : V3d)  (A : V3d) (B : V3d) (C : V3d) =
        let planeNormal = Vec.cross (B - A) (C - B) |> Vec.normalize
        let t = tracePlane o d A planeNormal
        let p = o + d * t

        let n1 = Vec.cross (B - A) (p - B) |> Vec.normalize 
        let n2 = Vec.cross (C - B) (p - C) |> Vec.normalize 
        let n3 = Vec.cross (A - C) (p - A) |> Vec.normalize 
    
        let d0 = Vec.dot n1 n2
        let d1 = Vec.dot n2 n3

        let threshold = 1.0 - 0.001
        if  (d0 > threshold && d1 > threshold) then  1.0 else 0.0

    // o : ray origin
    // d : ray direction
    // A,B,C,D	: rectangle corners
    // returns distance on the ray to the object if hit, 0 otherwise
    [<ReflectedDefinition>]
    let traceRectangle (o : V3d) (d : V3d)  (A : V3d) (B : V3d) (C : V3d) (D : V3d) =   
       max (traceTriangle o d A B C) (traceTriangle o d C D A) 

    // Return the closest point on the segment (with limit) 
    [<ReflectedDefinition>]
    let closestPointOnSegment (a : V3d) (b : V3d) (c : V3d) =
        let ab = b - a
        let t = (Vec.dot (c - a) ab) / Vec.dot ab ab
        a + (saturate t) * ab;

    //agualar attenuation for spot, disc and rectagle lights
    [<ReflectedDefinition>]
    let attenuationAgular (lDir : V3d) (ln : V3d) cutOffInner cutOffOuter =
        let cosTheta = Vec.dot lDir -ln
        let epsilon = cutOffInner - cutOffOuter
        (cosTheta - cutOffOuter) / epsilon 
        |> saturate    

    //distance attenuation for punctual lights (inverse square law)
    [<ReflectedDefinition>]
    let attenuationPunctualLight  dist =
        let d = max dist 0.01
        1.0 / (d * d)

    //unified illuminace calcualtion for disc and sphere lights
    [<ReflectedDefinition>]
    let illuminanceSphereDisc cosTheta sinSigmaSqr =
        let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        let illuminance  =
            if cosTheta * cosTheta > sinSigmaSqr
            then  
                Math.PI * sinSigmaSqr * clamp 0.0 1.0 cosTheta
            else
                let x = sqrt(1.0 / sinSigmaSqr - 1.0) 
                let y = -x * (cosTheta / sinTheta)
                let sinThetaSqrtY = sinTheta * sqrt(1.0 - y * y)
                (cosTheta * acos(y) - x * sinThetaSqrtY) * sinSigmaSqr + atan(sinThetaSqrtY / x)
        illuminance
        |> max 0.0

    // attenuation for sphere lights
    [<ReflectedDefinition>]
    let attenuationSphere (lUnnorm : V3d) (radius : float) (n : V3d) (lDir : V3d) =
        let dist2  = Vec.dot lUnnorm lUnnorm
        let radius2 = radius * radius  
        let cosTheta = Vec.dot n lDir |> clamp -0.999 0.999
        let sinSigmaSqr = radius2/dist2 |> min 0.9999
        illuminanceSphereDisc cosTheta sinSigmaSqr
    
    // representative point calcualtion for sphere: Nearest Point on sphere to a ray reflected in dominant specualr direction
    [<ReflectedDefinition>]
    let representativePointSpehre n v roughness lUnnorm radius = 
        let r = getSpecularDominantDirArea n v roughness
        let centerToRay = (Vec.dot lUnnorm r) * r - lUnnorm
        lUnnorm + centerToRay * clamp 0.0 1.0 (radius / length centerToRay) 
        |> Vec.normalize

    //attenuation for disc lights
    [<ReflectedDefinition>]
    let attenuationDisk (lUnnorm : V3d) (radius : float) (n : V3d) (lDir : V3d) (ln : V3d) =
        let cosTheta = Vec.dot n lDir |> clamp -0.999 0.999
        let dist2  = Vec.dot lUnnorm lUnnorm
        let radius2 = radius * radius  
        let sinSigmaSqr = radius2 / (radius2 + max radius2 dist2)
        let diskDirDotL = Vec.dot ln -lDir  |> saturate
        illuminanceSphereDisc cosTheta sinSigmaSqr * diskDirDotL

    // representative point calcualtion for disk: Nearest point on disk to a ray reflected in dominant specualr direction
    [<ReflectedDefinition>]
    let representativePointDisk r wPos radius ln lightPosition lUnnorm= 
        let t = tracePlane wPos r lightPosition ln
        let p = wPos + r * t
        let centerToRay = p - lightPosition
        lUnnorm + centerToRay * saturate (radius / length centerToRay) 
        |> Vec.normalize

    [<ReflectedDefinition>]
    let rectangleSolidAngle (wPos :V3d) p1 p2 p3 p4 =
        let v1 = p1 - wPos
        let v2 = p2 - wPos
        let v3 = p3 - wPos
        let v4 = p4 - wPos

        let n1 =  Vec.cross v1 v2 |> Vec.normalize
        let n2 =  Vec.cross v2 v3 |> Vec.normalize
        let n3 =  Vec.cross v3 v4 |> Vec.normalize
        let n4 =  Vec.cross v4 v1 |> Vec.normalize

        let g0 = Vec.dot -n1 n2 |> acos
        let g1 = Vec.dot -n2 n3 |> acos
        let g2 = Vec.dot -n3 n4 |> acos
        let g3 = Vec.dot -n4 n1 |> acos

        g0 + g1 + g2 + g3 - 2.0 * Math.PI

    [<ReflectedDefinition>]
    let attenuationRectangle (wPos :V3d) p1 p2 p3 p4 (lUnnorm : V3d) ln n=
        let solidAngle = rectangleSolidAngle wPos p1 p2 p3 p4 |> max 0.0
        if Vec.dot -lUnnorm ln > 0.0 then
            let x0 = lUnnorm |> Vec.normalize |> Vec.dot n |> saturate
            let x1 = p1 - wPos |> Vec.normalize |> Vec.dot n |> saturate
            let x2 = p2 - wPos |> Vec.normalize |> Vec.dot n |> saturate
            let x3 = p3 - wPos |> Vec.normalize |> Vec.dot n |> saturate
            let x4 = p4 - wPos |> Vec.normalize |> Vec.dot n |> saturate
            solidAngle * 0.2 * (x0 + x1 + x2 + x3 + x4)
        else
            0.0

    //point on the rectangle nearest to the a ray reflected in dominant specualr direction 
    [<ReflectedDefinition>]
    let representativePointRectangle r wPos p1 p2 p3 p4 ln lightPosition = 
        let t = traceRectangle wPos r p1 p2 p3 p4
        if t > 0.0 then
            r 
        else
            // The trace didn't succeed, so we need to find the closest point to the ray on the rectangle

            // We find the intersection point on the plane of the rectangle
            let tracedPlane = wPos + r * tracePlane wPos r lightPosition ln
            // Then find the closest point along the edges of the rectangle (edge = segment)
            let pc1 = closestPointOnSegment p1 p2 tracedPlane 
            let pc2 = closestPointOnSegment p2 p3 tracedPlane 
            let pc3 = closestPointOnSegment p3 p4 tracedPlane 
            let pc4 = closestPointOnSegment p4 p1 tracedPlane 

            let d1 = Vec.distance pc1 tracedPlane
            let d2 = Vec.distance pc2 tracedPlane
            let d3 = Vec.distance pc3 tracedPlane
            let d4 = Vec.distance pc4 tracedPlane

            let mutable minDist = d1
            let mutable p = pc1
            if d2 < minDist then
                minDist <- d2
                p <- pc2
            if d3 < minDist then
                minDist <- d3
                p <- pc3
            if d4 < minDist then
                minDist <- d4
                p <- pc4

            p - wPos |> Vec.normalize

    //returns: Flag for aktive light, light direction, illuminannce, specular Attenuation
    [<ReflectedDefinition>]
    let getLightParams (light : SLEUniform.Light) (wPos : V3d) (n : V3d) v roughness= 
        match  light.lightType  with
        | SLEUniform.LightType.DirectionalLight -> true, -light.lightDirection.XYZ |> Vec.normalize, light.color, 1.0
        | SLEUniform.LightType.PointLight -> 
            let lDir = light.lightPosition.XYZ - wPos |> Vec.normalize
            let dist = Vec.Distance (light.lightPosition.XYZ, wPos)
            let luminance = luminousPowerToLuminancePoint
            let attenuation = attenuationPunctualLight  dist
            true, lDir, light.color * attenuation * luminance, 1.0  
        | SLEUniform.LightType.SpotLight -> 
            let lDir = light.lightPosition.XYZ - wPos |> Vec.normalize
            let ln = light.lightDirection.XYZ |> Vec.normalize
            let luminance = luminousPowerToLuminanceSpot
            let intensity = attenuationAgular lDir ln light.cutOffInner light.cutOffOuter
            let dist = Vec.Distance (light.lightPosition.XYZ, wPos)
            let attenuation = attenuationPunctualLight  dist 
            true, lDir, light.color * intensity * attenuation * luminance, 1.0             
        | SLEUniform.LightType.SphereLight ->
            let lUnnorm = light.lightPosition.XYZ - wPos
            let lDir = lUnnorm |> Vec.normalize
            let luminance = luminousPowerToLuminanceSphere light.radius
            let attenuation = attenuationSphere lUnnorm light.radius n lDir
            let l = representativePointSpehre n v roughness lUnnorm light.radius
            true, l , light.color * attenuation * luminance, 1.0
        | SLEUniform.LightType.DiskLight -> 
            let lUnnorm = light.lightPosition.XYZ - wPos
            let lDir = lUnnorm |> Vec.normalize
            let ln = light.lightDirection.XYZ |> Vec.normalize
            let luminance = luminousPowerToLuminanceDisk light.radius
            let attenuation = attenuationDisk lUnnorm light.radius n lDir ln
            let lDir2 = light.virtualPos.XYZ - wPos |> Vec.normalize
            let intensity = attenuationAgular lDir2 ln light.cutOffInner light.cutOffOuter

            let r = getSpecularDominantDirArea n v roughness
            let l = representativePointDisk r wPos light.radius ln light.lightPosition.XYZ lUnnorm

            let specularAttenuation = // if ray is perpendicular to light plane, it would break specular, so fade in that case
                Vec.dot ln r
                |> abs
                |> saturate 

            true, l, light.color * intensity * attenuation * luminance, specularAttenuation    
        | SLEUniform.LightType.RectangleLight -> 
            let lUnnorm = light.lightPosition.XYZ - wPos
            let ln = light.lightDirection.XYZ |> Vec.normalize
            let luminance = luminousPowerToLuminanceRectangle light.p1 light.p2 light.p3 light.p4
            let attenuation = attenuationRectangle wPos light.p1 light.p2 light.p3 light.p4 lUnnorm ln n
            let lDir2 = light.virtualPos.XYZ - wPos |> Vec.normalize
            let d1 = light.fromWorld * V4d(lDir2,0.0)
            let d2 = V3d(d1.X,d1.Y,0.0) |> Vec.normalize
            let lDir3 = light.virtualPos2.XYZ - wPos |> Vec.normalize
            let d3 = light.fromWorld * V4d(lDir3,0.0)
            let d4 = V3d(0.0,d3.Y,d3.Z) |> Vec.normalize
            let intensity = 
                (attenuationAgular d2 V3d.OIO light.cutOffInner light.cutOffOuter)
                * (attenuationAgular d4 V3d.OIO light.cutOffInner light.cutOffOuter)
            let r = getSpecularDominantDirArea n v roughness
            let l = representativePointRectangle r wPos light.p1 light.p2 light.p3 light.p4 ln light.lightPosition.XYZ

            let specularAttenuation = // if ray is perpendicular to light plane, it would break specular, so fade in that case
                Vec.dot ln r
                |> abs
                |> saturate 

            true, l, light.color * intensity * attenuation * luminance, specularAttenuation    
        | SLEUniform.LightType.NoLight -> false, V3d(0.0), V3d(0.0), 1.0
        |_ ->  false, V3d(0.0), V3d(0.0), 1.0  //allways match any cases, otherwise fshade will give a cryptic error 

    [<ReflectedDefinition>]
    let f0ClearCoatToSurface (f0Base : V3d)=
        let sqrtBase = sqrt f0Base 
        let baseIOR =  (1.0+sqrtBase) / (1.0-sqrtBase)
        ((baseIOR-1.5)/ (baseIOR+1.5))*((baseIOR-1.5)/ (baseIOR+1.5))

    [<ReflectedDefinition>]
    let diffuseLobe (F : V3d) metallic (albedo : V3d) =
        let diffuseColor = (1.0 - metallic) * albedo
        let kD = (V3d.III - F)
        kD * diffuseColor / Math.PI

    [<ReflectedDefinition>]
    let specularLobeStandard (F : V3d) roughness nDotH nDotV nDotL=
        // cook-torrance brdf
        let ndf = DistributionGGX nDotH roughness 
        let g = GeometrySmith false nDotV nDotL roughness 
        
        let numerator = ndf * g * F
        let denominator = 4.0 * nDotV * nDotL |> max 0.001
        numerator / denominator  

    [<ReflectedDefinition>]
    let specularLobeCloth (F : V3d) roughness nDotH nDotV nDotL=
        let ndf = distributionCharlie  roughness nDotH //distributionCharlie roughness nDotH 
        let g = visibilityAshikhmin nDotV nDotL  
        ndf * g * F  
         
    [<ReflectedDefinition>]
    let directSheen (sheenColor : V3d) sheenRoughness nDotH nDotV nDotL (diffuse : V3d) (specular : V3d) =
        if sheenColor = V3d.OOO then
            (diffuse, specular)
        else
            let sheen = specularLobeCloth sheenColor sheenRoughness nDotH nDotV nDotL
            let sheenDFG = samplerBRDFLtu.Sample(V2d(nDotV, sheenRoughness)).Z
            let sheenScaling = 1.0 - (max3 sheenColor) *  sheenDFG
            (diffuse  * sheenScaling, specular  * sheenScaling + sheen)
    
    [<ReflectedDefinition>]
    let directclearCoat (clearCoat : float) clearCoatRoughness (clearCoatNormal : V3d) (lDir : V3d) (v : V3d) (h : V3d) hDotV =
        let ccF0 = V3d(0.04)
        let ccNDotL = Vec.dot clearCoatNormal lDir |> saturate
        let ccNDotV = Vec.dot clearCoatNormal v |> saturate 
        let ccNDotH = Vec.dot clearCoatNormal h |> saturate

        let ccF = clearCoat * fresnelSchlick ccF0 hDotV
        
        let clearCoatResponce = specularLobeStandard ccF clearCoatRoughness ccNDotH ccNDotV ccNDotL
        (ccF, clearCoatResponce,ccNDotL)

    type FragmentOut = {
        [<Semantic("Diffuse")>]  Diffuse       : V4d
        [<Semantic("Specular")>] Specular      : V4d
        [<TexCoord>]        tc      : V2d
    }

    [<ReflectedDefinition>]
    let getShadowLinearDepth (tc :V2d) = //linearDepth.getLinearDepth samplerShadowMap1 uniform.LightProjMInv ndc
        samplerShadowMap1.Sample(tc).X * uniform.LightFarZ
        (*let z = 2.0 * samplerShadowMap1.Sample(tc).X - 1.0
        let n = 0.0001
        let f = uniform.LightFarZ
        (2.0 * n * f) / (f + n - z * (f - n))*)


    [<ReflectedDefinition>]
    let transm (translucency : float) (profileIndex : int) (wp : V3d) (wn : V3d) (l :V3d)  =
        let sssWidth = uniform.sssWidth.[profileIndex]
        let sssFalloff = uniform.sssFalloff.[profileIndex]
        let sssStrength = uniform.sssStrength.[profileIndex]
        let t = min translucency  0.999
        let scale = 8.25 * (1.0 - t) / sssWidth
        let shrinkedPos = V4d(wp - 0.0005 * wn, 1.0)
        let posLightSpace = uniform.LightViewM * shrinkedPos
        let shadowPos =  uniform.LightViewMatrix * shrinkedPos
        let cc = shadowPos.XY / shadowPos.W * 0.5 + 0.5
        let d1 = getShadowLinearDepth cc 
        let d2 = -posLightSpace.Z / posLightSpace.W
        let dist = abs(d1 - d2) 
        let d = dist * scale / (sssFalloff + 0.001)
        let dd = -d * d
        let lDotN = Vec.dot l -wn
        (*let profile = V3d(0.233, 0.455, 0.649) * Math.Exp(dd / 0.0064) +
                      V3d(0.1,   0.336, 0.344) * Math.Exp(dd / 0.0484) +
                      V3d(0.118, 0.198, 0.0)   * Math.Exp(dd / 0.187)  +
                      V3d(0.113, 0.007, 0.007) * Math.Exp(dd / 0.567)  +
                      V3d(0.358, 0.004, 0.0)   * Math.Exp(dd / 1.99)   +
                      V3d(0.078, 0.0,   0.0)   * Math.Exp(dd / 7.41)*)
        let profile = 0.233 * exp(dd / 0.0064) +
                      0.1   * exp(dd / 0.0484) +
                      0.118 * exp(dd / 0.187)  +
                      0.113 * exp(dd / 0.567)  +
                      0.358 * exp(dd / 1.99)   +
                      0.078 * exp(dd / 7.41)
        sssStrength *profile * saturate (0.3 + lDotN) / Constant.Pi
        //if dist < 0.005 then V3d.IOO else  if dist > 0.01 then V3d.OIO else V3d.OOO
       

    let lightingDeferred (frag : Fragment)  =
        fragment {
            let diffuse, specular = 
                if frag.metallic < 0.0 then //no lighting, ignore      
                    V3d.Zero, V3d.Zero
                else //PBR lightning
                    let wPos = frag.wp.XYZ
                    let albedo = frag.c.XYZ
                    let v = uniform.CameraLocation - wPos |> Vec.normalize

                    //calculat light direction for area lights with the clear coat normal if clear coat is applied   
                    let nforl = Lerp frag.n frag.clearCoatNormal frag.clearCoat
                    let (_, lDir, illuminannce, specularAttenuation)  = getLightParams uniform.Light wPos nforl v frag.roughness

                    let h = v + lDir |> Vec.normalize
                    let nDotL = Vec.dot frag.n lDir |> saturate
                    let hDotV = Vec.dot h v |> saturate      
                    let nDotH = Vec.dot frag.n h |> saturate
                    let nDotV = Vec.dot frag.n v |> saturate

                    //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
                    let f0Base = Lerp (V3d(0.04)) albedo frag.metallic
                    //correct for clear coat
                    let f0 = Lerp f0Base (f0ClearCoatToSurface f0Base) frag.clearCoat

                    let F = fresnelSchlick f0 hDotV 

                    let spec = specularLobeStandard F frag.roughness nDotH nDotV nDotL
                    let diff = diffuseLobe F frag.metallic albedo

                   
                    let diff1', spec1 = directSheen frag.sheenColor frag.sheenRoughness nDotH nDotV nDotL diff (spec * specularAttenuation)

                    let transmission = 
                        if frag.sssProfile >= 0 then
                             (1.0 - frag.metallic) * transm 0.5 frag.sssProfile wPos frag.n lDir
                        else
                            V3d.OOO

                    let diff1 = (diff1' * nDotL + transmission) *  illuminannce

                    if frag.clearCoat = 0.0 then
                        (diff1 , spec1 * illuminannce * nDotL )
                    else
                        let ccF, clearCoatResponce, ccNDotL = directclearCoat frag.clearCoat frag.clearCoatRoughness frag.clearCoatNormal lDir v h hDotV
                        (diff1 * (V3d.One-ccF), (spec1 * (V3d.One-ccF) * nDotL + clearCoatResponce * ccNDotL)* illuminannce )

                     
            return {Diffuse = V4d(diffuse, 1.0); Specular = V4d(specular, 1.0); tc = frag.tc}
        }


    let maxReflectLod = 4.0

    [<ReflectedDefinition>]
    let ambientDiffuse (kdA : V3d) (albedo : V3d) (n : V3d) =                  
        let irradiance = diffuseIrradianceSampler.Sample(n).XYZ
        kdA * irradiance * albedo

    [<ReflectedDefinition>]
    let ambientSpecular (kSA : V3d) (roughness : float) nDotV r =                  
        let prefilteredColor = prefilteredSpecColorSampler.SampleLevel(r, roughness * maxReflectLod).XYZ
        let brdf = samplerBRDFLtu.Sample(V2d(nDotV, roughness)).XYZ
        prefilteredColor * (kSA * brdf.X + brdf.Y)

    [<ReflectedDefinition>]
    let ambientSheen (sheenColor : V3d) (sheenRoughness : float) nDotV r (diffuse : V3d) (specular : V3d)=                  
        if sheenColor = V3d.OOO then
            (diffuse, specular) 
        else
            let prefilteredColorSheen = prefilteredSpecColorSampler.SampleLevel(r, sheenRoughness * maxReflectLod).XYZ
            let sheenDFG = samplerBRDFLtu.Sample(V2d(nDotV, sheenRoughness)).Z
            let sheen = prefilteredColorSheen * sheenColor * sheenDFG
            let sheenScaling = 1.0 - (max3 sheenColor) * sheenDFG
            (diffuse * sheenScaling , specular * sheenScaling + sheen) 

    [<ReflectedDefinition>]
    let ambienClearCoat (clearCoat : float) (clearCoatRoughness : float) (clearCoatNormal : V3d) v r (diffuse : V3d) (specular : V3d)=
        if clearCoat = 0.0 then
            (diffuse, specular)
        else
            let ccNDotV = Vec.dot clearCoatNormal v |>  max 0.0
            let ccf0 = V3d(0.04)
            let cckSA = clearCoat * fresnelSchlickRoughness ccf0 clearCoatRoughness ccNDotV
            let ccprefilteredColor = prefilteredSpecColorSampler.SampleLevel(r, clearCoatRoughness * maxReflectLod).XYZ
            let ccbrdf = samplerBRDFLtu.Sample(V2d(ccNDotV, clearCoatRoughness)).XY
            let ccspecular = ccprefilteredColor * (cckSA * ccbrdf.X + ccbrdf.Y)
            (diffuse * (1.0 - cckSA), specular * (1.0 - cckSA)  + ccspecular)   

    let abientDeferred (frag : Fragment) =
        fragment {
            let diffuse, specular = 
                if frag.metallic < 0.0 then //no lighting, just put out the color      
                    (frag.c.XYZ, V3d.OOO)
                else //PBR lightning
                    let occlusion = ambientOcc.Sample(frag.tc).X
                    let albedo = frag.c.XYZ
                    let cameraPos = uniform.CameraLocation
                    let v = cameraPos - frag.wp.XYZ |> Vec.normalize
                    let r = Vec.reflect -v frag.n

                    //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
                    let f0Base = Lerp (V3d(0.04)) albedo frag.metallic
                    //corret for clean coat
                    let f0 = Lerp f0Base (f0ClearCoatToSurface f0Base) frag.clearCoat

                    let nDotV = Vec.dot frag.n v |>  max 0.0
                    let kSA = fresnelSchlickRoughness f0 frag.roughness nDotV
                    let kdA  = (1.0 - kSA) * (1.0 - frag.metallic)

                    let diff = ambientDiffuse kdA albedo frag.n

                    let spec = ambientSpecular kSA frag.roughness nDotV r

                    let diff1, spec1 = ambientSheen frag.sheenColor frag.sheenRoughness nDotV r diff spec
                    let diff2, spec2 = ambienClearCoat frag.clearCoat frag.clearCoatRoughness frag.clearCoatNormal v r diff1 spec1

                    (diff2 * uniform.AmbientIntensity * occlusion, spec2 * uniform.AmbientIntensity * occlusion)

            let em = frag.emission
            return {Diffuse = V4d(diffuse, 1.0); Specular = V4d(specular+em, 1.0); tc = frag.tc}
        }

    let abientDeferredSimple (frag : Fragment) =
        fragment {
            let col = 
                if frag.metallic < 0.0 then //no lighting, just put out the color      
                    frag.c.XYZ
                else //ambient
                    let albedo = frag.c.XYZ
                    let ambientIntensity = uniform.AmbientIntensity
                    albedo * ambientIntensity
            let em = frag.emission
            return V4d(em + col, 1.0)
        }

    let shadowDeferred  (vert : FragmentOut) =
        fragment {
            let wPos = wPos.Sample(vert.tc)
            let shadow = Shadow.getShadow wPos
            return {Diffuse = vert.Diffuse * shadow; Specular = vert.Specular * shadow; tc = vert.tc}
        }




