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
    
    type UniformScope with
        member x.Light : SLEUniform.Light = x?Light
        member x.AmbientIntensity : float = x?AmbientIntensity

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
    let pbrDirect f0 roughness metallic (albedo : V3d) (wPos : V4d) v n nDotV light (clearCoat : float)  clearCoatRoughness = 
           
        let (exists, lDir, illuminannce, specularAttenuation)  = getLightParams light wPos.XYZ n v roughness
      
        let oi = 
            if exists then
                let h = v + lDir |> Vec.normalize

                // cook-torrance brdf
                let ndf = DistributionGGX n h roughness 
                let g = GeometrySmith false n v lDir roughness 
                let hDotV = Vec.dot h v |>  max 0.0      
                let kS = fresnelSchlick f0 hDotV   
                let nDotL = Vec.dot n lDir |>  max 0.0
            
                let kD = (1.0 - metallic) * (V3d.III - kS)
                let diffuse = kD * albedo / Math.PI

                let numerator = ndf * g * kS
                let denominator = 4.0 * nDotV * nDotL |> max 0.001
                let specular = numerator / denominator  

                if clearCoat = 0.0 then
                    // add to outgoing radiance from single light
                    (diffuse + specular * specularAttenuation) * illuminannce * nDotL
                else
                    let ccF0 = V3d(0.04)
                    let ccndf = DistributionGGX n h clearCoatRoughness 
                    let ccg = GeometrySmith false n v lDir clearCoatRoughness 
                    let clearCoatKS = clearCoat * fresnelSchlick ccF0 hDotV  
                    let clearCoatResponce = ccndf * ccg * clearCoatKS / denominator

                    // add to outgoing radiance from single light
                    ((diffuse + specular * specularAttenuation) * (V3d.One-clearCoatKS) + clearCoatResponce) * illuminannce * nDotL


            else V3d.Zero
        oi

    [<ReflectedDefinition>]
    let pBRLightning metallic roughness albedo n (wPos : V4d) (clearCoat : float) clearCoatRoughness =
        
        let cameraPos = uniform.CameraLocation

        let v = cameraPos - wPos.XYZ |> Vec.normalize

        //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
        let f0Base = Lerp (V3d(0.04)) albedo metallic

        let f0 = Lerp f0Base (f0ClearCoatToSurface f0Base) clearCoat
        let nDotV = Vec.dot n v |>  max 0.0
        let light = uniform.Light
        let directLight = pbrDirect f0 roughness metallic albedo wPos v n nDotV light clearCoat clearCoatRoughness
        directLight

    let lightingDeferred (frag : Fragment)  =
        fragment {
            let col = 
                if frag.metallic < 0.0 then //no lighting, just put out the color      
                    V3d.Zero
                else //PBR lightning
                    let metallic = frag.metallic
                    let roughness = frag.roughness
                    let albedo = frag.c.XYZ
                    let n = frag.n
                    let wPos = frag.wp
                    let clearCoat = frag.clearCoat
                    pBRLightning metallic roughness albedo n wPos clearCoat frag.clearCoatRoughness

            return V4d(col, 1.0)
        }

    [<ReflectedDefinition>]
    let pBRAbientLight f0 roughness metallic (albedo : V3d) n r nDotV (clearCoat : float) clearCoatRoughness=
        let kSA = fresnelSchlickRoughness f0 roughness nDotV
        let kdA  = (1.0 - kSA) * (1.0 - metallic)
        let irradiance = diffuseIrradianceSampler.Sample(n).XYZ
        let diffuse = kdA * irradiance * albedo

        let maxReflectLod = 4.0
        let prefilteredColor = prefilteredSpecColorSampler.SampleLevel(r, roughness * maxReflectLod).XYZ
        let brdf = samplerBRDFLtu.Sample(V2d(nDotV, roughness)).XY
        let specular = prefilteredColor * (kSA * brdf.X + brdf.Y)

        let ambient = 
            if clearCoat = 0.0 then
                diffuse + specular
            else
                let ccf0 = V3d(0.04)
                let cckSA = clearCoat* fresnelSchlickRoughness ccf0 clearCoatRoughness nDotV
                let ccprefilteredColor = prefilteredSpecColorSampler.SampleLevel(r, clearCoatRoughness * maxReflectLod).XYZ
                let ccbrdf = samplerBRDFLtu.Sample(V2d(nDotV, clearCoatRoughness)).XY
                let ccspecular = ccprefilteredColor * (cckSA * ccbrdf.X + ccbrdf.Y)
                (diffuse + specular) * (1.0 - cckSA) + ccspecular

        let ambientIntensity = uniform.AmbientIntensity
        ambient * ambientIntensity

    [<ReflectedDefinition>]
    let pBRAbient metallic roughness albedo n (wPos : V4d) (clearCoat : float) clearCoatRoughness =

        let cameraPos = uniform.CameraLocation
        let v = cameraPos - wPos.XYZ |> Vec.normalize
        let r = Vec.reflect -v n

        //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
        let f0Base = Lerp (V3d(0.04)) albedo metallic
        let f0 = Lerp f0Base (f0ClearCoatToSurface f0Base) clearCoat

        let nDotV = Vec.dot n v |>  max 0.0
        let ambient = pBRAbientLight f0 roughness metallic albedo n r nDotV clearCoat clearCoatRoughness
        ambient   


    let abientDeferred (frag : Fragment) =
        fragment {
            let occlusion = ambientOcc.Sample(frag.tc).X
            let col = 
                if frag.metallic < 0.0 then //no lighting, just put out the color      
                    frag.c.XYZ
                else //PBR lightning
                    let metallic = frag.metallic
                    let roughness = frag.roughness
                    let albedo = frag.c.XYZ
                    let n = frag.n
                    let wPos = frag.wp
                    let c = pBRAbient metallic roughness albedo n wPos frag.clearCoat frag.clearCoatRoughness
                    c * occlusion

            let em = frag.emission
            return V4d(em + col, 1.0)
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





