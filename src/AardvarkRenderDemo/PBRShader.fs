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
*)

module PBR =
    open fshadeExt
    open BRDF
    open GBuffer

    type ShadeingType =
        | Standard = 0
        | Cloth = 1

    type UniformScope with
        member x.LightArray : Arr<N<80>,SLEUniform.Light> = x?LightArray
        member x.LightCount : int = x?LightCount
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
       }

    [<ReflectedDefinition>]
    let directLighting metallic (wp :V4d) (c: V4d) (n: V3d) (clearCoatNormal: V3d)  clearCoat  roughness (sheenColor: V3d) sheenRoughness clearCoatRoughness (light : SLEUniform.Light) =
        if metallic < 0.0 then //no lighting, ignore      
            V3d.Zero, V3d.Zero, V3d.Zero, V3d.Zero
        else //PBR lightning
            let wPos = wp.XYZ
            let albedo = c.XYZ
            let v = uniform.CameraLocation - wPos |> Vec.normalize

            //calculat light direction for area lights with the clear coat normal if clear coat is applied   
            let nforl = Lerp n clearCoatNormal clearCoat
            let (_, lDir, illuminannce, specularAttenuation, illuminannceSimple)  = LightShader.getLightParams light wPos nforl v roughness
            //illuminaceSimple gives punctual illuminace for area lights and is used for translucency to approximate back lightning

            let h = v + lDir |> Vec.normalize
            let nDotL = Vec.dot n lDir |> saturate
            let hDotV = Vec.dot h v |> saturate      
            let nDotH = Vec.dot n h |> saturate
            let nDotV = Vec.dot n v |> saturate

            //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
            let f0Base = Lerp (V3d(0.04)) albedo metallic
            //correct for clear coat
            let f0 = Lerp f0Base (f0ClearCoatToSurface f0Base) clearCoat

            let F = fresnelSchlick f0 hDotV 

            let spec = specularLobeStandard F roughness nDotH nDotV nDotL
            let diff = diffuseLobe F metallic albedo

            let diff1', spec1 = directSheen sheenColor sheenRoughness nDotH nDotV nDotL diff (spec * specularAttenuation)

            let diff1 = diff1' * nDotL * illuminannce

            if clearCoat = 0.0 then
                (diff1 , spec1 * illuminannce * nDotL, lDir, illuminannceSimple )
            else
                let ccF, clearCoatResponce, ccNDotL = directclearCoat clearCoat clearCoatRoughness clearCoatNormal lDir v h hDotV
                (diff1 * (V3d.One-ccF), (spec1 * (V3d.One-ccF) * nDotL + clearCoatResponce * ccNDotL)* illuminannce, lDir, illuminannceSimple )


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

    [<ReflectedDefinition>]
    let ambientLight metallic (c : V4d) (wp : V4d) (n : V3d) (clearCoat) roughness (sheenColor : V3d) sheenRoughness clearCoatRoughness clearCoatNormal=   
        if metallic < 0.0 then //no lighting, just put out the color      
            (c.XYZ, V3d.OOO)
        else //PBR lightning
            let albedo = c.XYZ
            let cameraPos = uniform.CameraLocation
            let v = cameraPos - wp.XYZ |> Vec.normalize
            let r = Vec.reflect -v n

            //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
            let f0Base = Lerp (V3d(0.04)) albedo metallic
            //corret for clean coat
            let f0 = Lerp f0Base (f0ClearCoatToSurface f0Base) clearCoat

            let nDotV = Vec.dot n v |>  max 0.0
            let kSA = fresnelSchlickRoughness f0 roughness nDotV
            let kdA  = (1.0 - kSA) * (1.0 - metallic)

            let diff = ambientDiffuse kdA albedo n

            let spec = ambientSpecular kSA roughness nDotV r

            let diff1, spec1 = ambientSheen sheenColor sheenRoughness nDotV r diff spec
            let diff2, spec2 = ambienClearCoat clearCoat clearCoatRoughness clearCoatNormal v r diff1 spec1

            (diff2 * uniform.AmbientIntensity, spec2 * uniform.AmbientIntensity)

    let lightnigForward (frag : Fragment) =
        fragment {
            let mutable diffuseD = V3d.Zero 
            let mutable specularD = V3d.Zero
            for i in 0..uniform.LightCount-1 do
                let diffuseDi, specularDi, lDir, simpleIllumn = 
                    directLighting frag.metallic frag.wp frag.c frag.n frag.clearCoatNormal frag.clearCoat frag.roughness frag.sheenColor frag.sheenRoughness frag.clearCoatRoughness uniform.LightArray.[i]
                let shadow = if uniform.LightArray.[i].castsShadow then Shadow.getShadow i frag.wp else  1.0
                let transmission =  simpleIllumn * translucency.transm frag.sssProfile i frag.wp.XYZ frag.n lDir
                diffuseD  <- diffuseD  + diffuseDi * shadow + transmission
                specularD <- specularD + specularDi * shadow
            let diffuseO, specularO = 
                ambientLight frag.metallic frag.c frag.wp frag.n frag.clearCoat frag.roughness frag.sheenColor frag.sheenRoughness frag.clearCoatRoughness frag.clearCoatNormal
            let em = frag.emission
            let diffuse = diffuseD + diffuseO         
            let specular = specularD  + specularO + em        
            return {Diffuse = V4d(diffuse, 1.0)
                    Specular = V4d(specular, 1.0)
                    }        
        }

    [<ReflectedDefinition>]
    let lightnigDeferred (frag : Fragment) =
        fragment {
            let mutable diffuseD = V3d.Zero 
            let mutable specularD = V3d.Zero
            for i in 0..uniform.LightCount-1 do
                let diffuseDi, specularDi, lDir, simpleIllumn = 
                    directLighting frag.metallic frag.wp frag.c frag.n frag.clearCoatNormal frag.clearCoat frag.roughness frag.sheenColor frag.sheenRoughness frag.clearCoatRoughness uniform.LightArray.[i]
                let shadow = if uniform.LightArray.[i].castsShadow then Shadow.getShadow i frag.wp else  1.0
                let transmission =  simpleIllumn * translucency.transm frag.sssProfile i frag.wp.XYZ frag.n lDir    
                diffuseD  <- diffuseD  + diffuseDi * shadow + transmission
                specularD <- specularD + specularDi * shadow
            let diffuseO, specularO = 
                ambientLight frag.metallic frag.c frag.wp frag.n frag.clearCoat frag.roughness frag.sheenColor frag.sheenRoughness frag.clearCoatRoughness frag.clearCoatNormal
            let em = frag.emission
            let diffuse = diffuseD + diffuseO         
            let specular = specularD  + specularO + em    
            return {Diffuse = V4d(diffuse, 1.0)
                    Specular = V4d(specular, 1.0)
                    }        
        }

    [<ReflectedDefinition>]
    let abientLightSimple metallic (c : V3d)  =
        let col = 
            if metallic < 0.0 then //no lighting, just put out the color      
                c
            else //ambient
                let albedo = c
                let ambientIntensity = uniform.AmbientIntensity
                albedo * ambientIntensity
        col


    let lightnigDeferredProbe (frag : Fragment) =
        fragment {
            let mutable diffuseD = V3d.Zero 
            let mutable specularD = V3d.Zero
            for i in 0..uniform.LightCount-1 do
                let diffuseDi, specularDi, _, _ = 
                    directLighting frag.metallic frag.wp frag.c frag.n frag.clearCoatNormal frag.clearCoat frag.roughness frag.sheenColor frag.sheenRoughness frag.clearCoatRoughness uniform.LightArray.[i]
                let shadow = if uniform.LightArray.[i].castsShadow then Shadow.getShadow i frag.wp else  1.0
                diffuseD  <- diffuseD  + diffuseDi * shadow
                specularD <- specularD + specularDi * shadow
            let ambient = abientLightSimple frag.metallic frag.c.XYZ
            let em = frag.emission
            let col = diffuseD + specularD + ambient + em        
            return V4d(col, 1.0)
        }

 

