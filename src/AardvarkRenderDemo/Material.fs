namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model
open Aardvark.Rendering
open System.IO

(*
    A proxy Material to subtitute a PBR material into an imported  Object
    Some UI Views for material values
*)
module material = 

    let defaultMaterial = {
        metallic = {
            fileName = None
            factor = 0.0
        }
        roughness = {
            fileName = None
            factor = 0.8
        }
        albedo ={
            fileName = None
            color = C3d.White
            factor = 1.0
        }
        emission = {
            fileName = None
            color = C3d.Black
            factor = 0.0
        }
        normal = {
            fileName = None
            factor = 0.0
        }
        discard = false
        displacment = {
            fileName = None
            factor = 0.0
        }
        clearCoat = {
            fileName = None
            factor = 0.0
        }
        clearCoatRoughness = {
            fileName = None
            factor = 0.2
        }
        clearCoatNormal = {
            fileName = None
            factor = 0.0
        }
        sheenColor ={
            fileName = None
            color = C3d.White
            factor = 0.0
        }
        sheenRoughness = {
            fileName = None
            factor = 0.8
        }
        SssProfileIndex = None
        coverage = {
            fileName = None
            factor = 1.0
        }
        transmission ={
            fileName = None
            color = C3d.White
            factor = 0.0
        }
        indexOfRefraction = 1.0
        refractionDistance = {
            fileName = None
            factor = 2.0
        }
    }

    let onePxPix (color :C3f)= 
        let pi = PixImage<byte>(Col.Format.RGB, V2i.II)
        pi.GetMatrix<C3f>().SetByCoord(fun (c : V2l) -> color) |> ignore
        pi

    let onPixTex (color :C3f) = 
        PixTexture2d(PixImageMipMap [| onePxPix color :> PixImage |], false) :> ITexture

    //PBR material type to replace the imported ASSIMP materials in the imported models
    type ProxyMaterial =
        {
            Name : string
            Material : aval<AdaptivePBRMaterial>
        }
        
        member x.DisplacemntMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.Gray50)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.displacment.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.MetallicMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.metallic.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.RoughnessMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.roughness.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.AlbedoMap =
            adaptive {
                let! m = x.Material 
                let! f = m.albedo.fileName
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  onPixTex C3f.White
            }

        member x.EmissionMap =
            adaptive {
                let! m = x.Material 
                let! f = m.emission.fileName
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  onPixTex C3f.White
            }

        member x.NormalMap =
            adaptive {
                let! m = x.Material 
                let! f = m.normal.fileName
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  onPixTex C3f.White
            }

        member x.ClearCoatMap =              
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.clearCoat.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.ClearCoatRoughnessMap =               
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.clearCoatRoughness.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.SheenRoughnessMap =               
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.sheenRoughness.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.SheenColorMap =
            adaptive {
                let! m = x.Material 
                let! f = m.sheenColor.fileName
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  onPixTex C3f.White
            }

        member x.TransmissionColorMap =
            adaptive {
                let! m = x.Material 
                let! f = m.transmission.fileName
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  onPixTex C3f.White
            }

        member x.ClearCoatNormalMap =
            adaptive {
                let! m = x.Material 
                let! f = m.clearCoatNormal.fileName
                let! nf = m.normal.fileName
                let! nt = x.NormalMap 
                return match f with
                        | Some file -> 
                            match nf with
                            |Some f  when f = file -> nt
                            |_ -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  onPixTex C3f.White
            } :> IAdaptiveValue

        member x.UseNormalsForClearCoat =
            adaptive {
                let! m = x.Material 
                let! f = m.clearCoatNormal.fileName
                let! nf = m.normal.fileName
                return match f with
                        | Some file -> 
                            match nf with
                            |Some f  when f = file -> true
                            |_ -> false
                        | None ->  false
            } :> IAdaptiveValue

        member x.CoverageMap =               
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.coverage.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        member x.RefractionDistanceMap =               
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            AVal.bind (fun (m : AdaptivePBRMaterial)-> m.refractionDistance.fileName |> AVal.map loadTex)  x.Material :> IAdaptiveValue

        interface IO.Loader.IMaterial with

            member x.name = x.Name

            member x.TryGetUniform(s, sem) =
                match string sem with
                | "Metallic" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.metallic.factor) x.Material :> IAdaptiveValue)
                | "MetallicMap" -> Some x.MetallicMap 
                | "Roughness" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.roughness.factor) x.Material :> IAdaptiveValue)
                | "RoughnessMap" -> Some x.RoughnessMap 
                | "AlbedoColorTexture" -> Some (x.AlbedoMap :> IAdaptiveValue)
                | "AlbedoColor" -> Some  (AVal.bind (fun (m : AdaptivePBRMaterial)-> AVal.map2 (*) m.albedo.color  m.albedo.factor ) x.Material :> IAdaptiveValue)
                | "NormalMapStrength" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.normal.factor) x.Material :> IAdaptiveValue)
                | "NormalMapTexture" -> Some (x.NormalMap :> IAdaptiveValue)
                | "Discard" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.discard) x.Material :> IAdaptiveValue)
                | "DisplacmentStrength" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.displacment.factor) x.Material :> IAdaptiveValue)
                | "DisplacmentMap" -> Some x.DisplacemntMap 
                | "EmissionTexture" -> Some (x.EmissionMap :> IAdaptiveValue)
                | "EmissionColor" -> Some  (AVal.bind (fun (m : AdaptivePBRMaterial)->  AVal.map2 (*) m.emission.color  m.emission.factor) x.Material :> IAdaptiveValue)
                | "ClearCoat" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.clearCoat.factor) x.Material :> IAdaptiveValue)
                | "ClearCoatMap" -> Some x.ClearCoatMap 
                | "ClearCoatRoughness" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.clearCoatRoughness.factor) x.Material :> IAdaptiveValue)
                | "ClearCoatRoughnessMap" -> Some x.ClearCoatRoughnessMap 
                | "ClearCoatNormalStrength" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.clearCoatNormal.factor) x.Material :> IAdaptiveValue)
                | "ClearCoatNormalMapTexture" -> Some x.ClearCoatNormalMap
                | "UseNormalsForClearCoat" -> Some x.UseNormalsForClearCoat
                | "SheenColorTexture" -> Some (x.SheenColorMap :> IAdaptiveValue)
                | "SheenColor" -> Some  (AVal.bind (fun (m : AdaptivePBRMaterial)-> AVal.map2 (*) m.sheenColor.color  m.sheenColor.factor) x.Material :> IAdaptiveValue)
                | "SheenRoughness" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.sheenRoughness.factor) x.Material :> IAdaptiveValue)
                | "SheenRoughnessMap" -> Some x.SheenRoughnessMap 
                | "Coverage" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.coverage.factor) x.Material :> IAdaptiveValue)
                | "CoverageTexture" -> Some x.CoverageMap 
                | "TransmissionColorTexture" -> Some (x.TransmissionColorMap :> IAdaptiveValue)
                | "Transmission" ->  Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> AVal.map2 (*) m.transmission.color  m.transmission.factor) x.Material :> IAdaptiveValue)
                | "SssProfileIndex" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> AVal.map (Option.defaultValue -1) m.SssProfileIndex) x.Material :> IAdaptiveValue)
                | "IndexOfRefraction" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.indexOfRefraction) x.Material :> IAdaptiveValue) 
                | "RefractionDistance" -> Some (AVal.bind (fun (m : AdaptivePBRMaterial)-> m.refractionDistance.factor) x.Material :> IAdaptiveValue)
                | "RefractionDistanceMap" -> Some x.RefractionDistanceMap
                | _ -> None
            
            member x.Dispose() = ()

    //find all material definitions in a IO.Loader.Scene
    let getMaterials (s : IO.Loader.Scene) =
            let rec traverse (state : IO.Loader.IMaterial list) (n : IO.Loader.Node) =
                match n with
                    | IO.Loader.Node.Empty -> 
                        state
                    | IO.Loader.Node.Group es ->
                        List.fold traverse state es 
                    | IO.Loader.Node.Leaf m ->
                        state
                    | IO.Loader.Node.Material(m, n) ->
                        m::state
                    | IO.Loader.Node.Trafo(t, n) ->
                        traverse state n

            traverse [] s.root 
    
    let getTextureFileName (kind : Symbol) (material : IO.Loader.Material) =
        match material.textures.TryFind kind with
            |Some t -> 
                match t.texture with
                    | :? FileTexture as f -> Some f.FileName
                    | _ -> None
            | None -> None

    let  toPBRMaterial (material : IO.Loader.Material) =
        let albedoMap = getTextureFileName DefaultSemantic.DiffuseColorTexture material
        let normalMap = getTextureFileName DefaultSemantic.NormalMapTexture material
        let emissionMap = getTextureFileName DefaultSemantic.EmissiveColorTexture material
        {defaultMaterial with 
            albedo = {defaultMaterial.albedo with fileName = albedoMap; color = C3d.FromC4f(material.diffuse)}
            normal = {defaultMaterial.normal with fileName = normalMap; factor = match normalMap with |Some _ -> 1.0 |None -> 0.0}
            emission = {defaultMaterial.emission with fileName = emissionMap; factor = 1.0; color = C3d.FromC4f(material.emissive)}
        }
    //get a list of all material names from an  imported  object
    //and use that as Keys for a Map initialized with  the default PBR material
    //This is used to initialize the materials for an imported object
    let materials model = 
        getMaterials model
        |> List.map (fun m -> m.name, toPBRMaterial (m :?>  IO.Loader.Material))
        |> HashMap.ofList
    
//UI Control for a optionally texture mapped value with linear or logaritmic strength control
module textureMappedValueControl =

    type Message =
        | SetMap of string
        | RemoveMap
        | SetFactor of float

    let update (m :TextureMappedValue) (msg : Message)  =
        match msg with
        | SetMap file -> {m with fileName = Some file}
        | RemoveMap -> {m with fileName = None}
        | SetFactor f -> {m with  factor = f}

    type Kind =
    |Linear
    |Log

    let view kind titel min max step (m : AdaptiveTextureMappedValue) =
        let slider =
            match kind with
            |Linear -> inputSlider {min =min;  max = max; step = step} [] m.factor SetFactor
            |Log -> inputLogSlider {min =min;  max = max; step = step} [] m.factor SetFactor
        let openButton = 
            openDialogButton 
                { OpenDialogConfig.file with allowMultiple = false; title = sprintf "Open %s" titel; filters  = [|"*.*"|];  startPath = ""; mode  = OpenDialogMode.File}
                [ clazz "ui green button"; onChooseFile SetMap ] 
                [ text "Choose File" ]
        let removeButton = 
            m.fileName
            |> AVal.map (fun f -> match f with |Some fn -> IndexList.single(button [clazz "ui button"; onClick (fun _ -> RemoveMap)]  [text "Remove"]) |None -> IndexList.empty)
            |> AList.ofAVal
            |> Incremental.div AttributeMap.empty
        let name = m.fileName |> AVal.map (Option.map (fun f -> IO.Path.GetFileNameWithoutExtension(f)) >> Option.defaultValue "none") |> Incremental.text
        Html.table [                        
            tr [] [ td [] [text titel]; td [style "width: 70%;"; attribute "colspan" "2"] [slider]]
            tr [] [ td [] [openButton]; td [] [name]; td [] [removeButton]]
        ]        

module textureMappedColorControl =

    type Message =
        | SetMap of string
        | RemoveMap
        | SetFactor of float
        | SetColor of C3d

    let update (m :TextureMappedColor) (msg : Message)  =
        match msg with
        | SetMap file -> {m with fileName = Some file}
        | RemoveMap -> {m with fileName = None}
        | SetFactor f -> {m with  factor = f}
        | SetColor c -> {m with color = c}

    type Kind =
    |Linear
    |Log

    let view kind titel min max step (m : AdaptiveTextureMappedColor) =
        let slider =
            match kind with
            |Linear -> inputSlider {min =min;  max = max; step = step} [] m.factor SetFactor
            |Log -> inputLogSlider {min =min;  max = max; step = step} [] m.factor SetFactor
        let openButton = 
            openDialogButton 
                { OpenDialogConfig.file with allowMultiple = false; title = sprintf "Open %s" titel; filters  = [|"*.*"|];  startPath = ""; mode  = OpenDialogMode.File}
                [ clazz "ui green button"; onChooseFile SetMap ] 
                [ text "Choose File" ]
        let removeButton = 
            m.fileName
            |> AVal.map (fun f -> match f with |Some fn -> IndexList.single(button [clazz "ui button"; onClick (fun _ -> RemoveMap)]  [text "Remove"]) |None -> IndexList.empty)
            |> AList.ofAVal
            |> Incremental.div AttributeMap.empty
        let name = m.fileName |> AVal.map (Option.map (fun f -> IO.Path.GetFileNameWithoutExtension(f)) >> Option.defaultValue "none") |> Incremental.text
        let c = AVal.map (fun (col : C3d) -> col.ToC4b()) m.color
        Html.table [                        
            tr [] [ td [] [text titel]; td [style "width: 70%;"; attribute "colspan" "2"] [slider]]
            tr [] [ td [] [text "Color"]; td [] [ColorPicker.viewSimple c (fun (c : C4b) -> C3d.FromC4b c |> SetColor)]; td [] []]
            tr [] [ td [] [openButton]; td [] [name]; td [] [removeButton]]
        ]   

//UI control for a single material
module materialControl = 

    type Message =
        | SetMetallic of textureMappedValueControl.Message
        | SetRoughness of textureMappedValueControl.Message
        | SetNormal of textureMappedValueControl.Message
        | SetAlbedo of textureMappedColorControl.Message
        | SetDiscard 
        | SetDisplacment of textureMappedValueControl.Message
        | SetEmission of textureMappedColorControl.Message
        | SetClearCoat of textureMappedValueControl.Message
        | SetClearCoatRoughness of textureMappedValueControl.Message
        | SetClearCoatNormal of textureMappedValueControl.Message
        | SetSheenColor of textureMappedColorControl.Message
        | SetSheenRoughness of textureMappedValueControl.Message
        | SetCoverage of textureMappedValueControl.Message
        | SetTransmission of  textureMappedColorControl.Message
        | SetSssProfile of int option
        | SetIndexOfRefraction of float
        | SetRefractionDistance of textureMappedValueControl.Message

    let update  (m : PBRMaterial) (msg : Message)  =
        match msg with
        | SetMetallic msg' -> { m with  metallic = textureMappedValueControl.update m.metallic msg'}
        | SetRoughness msg' -> { m with  roughness = textureMappedValueControl.update m.roughness msg'}
        | SetAlbedo msg' -> { m with  albedo = textureMappedColorControl.update m.albedo msg'}
        | SetNormal msg' -> { m with  normal = textureMappedValueControl.update m.normal msg'}
        | SetDiscard -> { m with  discard = not m.discard}
        | SetDisplacment msg' -> { m with  displacment = textureMappedValueControl.update m.displacment msg'}
        | SetEmission msg' -> { m with  emission = textureMappedColorControl.update m.emission msg'}
        | SetClearCoat msg' -> { m with  clearCoat = textureMappedValueControl.update m.clearCoat msg'}
        | SetClearCoatRoughness msg' -> { m with  clearCoatRoughness = textureMappedValueControl.update m.clearCoatRoughness msg'}
        | SetClearCoatNormal msg' -> { m with  clearCoatNormal = textureMappedValueControl.update m.clearCoatNormal msg'}
        | SetSheenRoughness msg' -> { m with  sheenRoughness = textureMappedValueControl.update m.sheenRoughness msg'}
        | SetSheenColor msg' -> { m with  sheenColor = textureMappedColorControl.update m.sheenColor msg'}
        | SetSssProfile p -> {m with SssProfileIndex = p}
        | SetCoverage msg' -> { m with  coverage = textureMappedValueControl.update m.coverage msg'}
        | SetTransmission msg' -> { m with transmission = textureMappedColorControl.update m.transmission msg'}
        | SetIndexOfRefraction i -> {m with indexOfRefraction = i}
        | SetRefractionDistance msg' -> { m with  refractionDistance = textureMappedValueControl.update m.refractionDistance msg'}

    let view (m : AdaptivePBRMaterial) (profiles : amap<int,AdaptiveSssProfile>)=
        let ProfilList = 
            profiles 
            |> AMap.keys 
            |> ASet.toAList 
            |> AList.sort
            |> AList.map (Some)
            |> AList.append (AList.single None)

        let projection  i = 
            let inner i' =
                let name = 
                    AMap.find i' profiles
                    |> AVal.bind (fun p -> p.Name)
                    |> AVal.force 
                sprintf "%s (%i)" name i'
            Option.map inner i
            |>Option.defaultValue "-"


        div [] [
            textureMappedValueControl.view textureMappedValueControl.Linear "Metallic" 0.0 1.0 0.01 m.metallic  |> UI.map SetMetallic
            textureMappedValueControl.view textureMappedValueControl.Linear "Roughness" 0.0 1.0 0.01 m.roughness  |> UI.map SetRoughness
            textureMappedColorControl.view textureMappedColorControl.Linear "Albedo" 0.0 1.0 0.01 m.albedo  |> UI.map SetAlbedo
            textureMappedValueControl.view textureMappedValueControl.Linear "Normal Map" 0.0 1.0 0.01 m.normal  |> UI.map SetNormal
            textureMappedValueControl.view textureMappedValueControl.Linear "Coverage" 0.0 1.0 0.01 m.coverage  |> UI.map SetCoverage
            textureMappedColorControl.view textureMappedColorControl.Linear "Transmission" 0.0 1.0 0.01 m.transmission  |> UI.map SetTransmission
            Html.table [   
                 tr [] [ td [] [text "Index of Refraction"]; td [style "width: 70%;"] [inputSlider {min =1.0;  max = 3.0; step = 0.01} [] m.indexOfRefraction SetIndexOfRefraction]]
            ]   
            textureMappedValueControl.view textureMappedValueControl.Log "Refraction Distance" 0.001 100.0 0.01 m.refractionDistance  |> UI.map SetRefractionDistance
            textureMappedValueControl.view textureMappedValueControl.Linear "Clear Coat" 0.0 1.0 0.01 m.clearCoat  |> UI.map SetClearCoat
            textureMappedValueControl.view textureMappedValueControl.Linear "Clear Coat Roughness" 0.0 1.0 0.01 m.clearCoatRoughness  |> UI.map SetClearCoatRoughness
            textureMappedValueControl.view textureMappedValueControl.Linear "Clear Coat Normal" 0.0 1.0 0.01 m.clearCoatNormal  |> UI.map SetClearCoatNormal
            textureMappedColorControl.view textureMappedColorControl.Linear "Sheen Color" 0.0 1.0 0.01 m.sheenColor  |> UI.map SetSheenColor
            textureMappedValueControl.view textureMappedValueControl.Linear "Sheen Roughness" 0.0 1.0 0.01 m.sheenRoughness  |> UI.map SetSheenRoughness
            textureMappedColorControl.view textureMappedColorControl.Linear "Emission" 0.0 10.0 0.01 m.emission  |> UI.map SetEmission
            //textureMappedValueControl.view textureMappedValueControl.Linear "Displacement" 0.0 1.0 0.01 m.displacment  |> UI.map SetDisplacment
            Html.table [   
                 tr [] [ td [] [text "Subsufrace Scattering Profile"]; td [style "width: 70%;"] [Html.SemUi.dropDown' (ProfilList) m.SssProfileIndex  SetSssProfile  projection]]                     
                 tr [] [ td [] [text "Discard"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  m.discard SetDiscard ]]
            ]   
        ]