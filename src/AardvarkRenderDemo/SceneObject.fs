namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model
open Aardvark.SceneGraph.IO

(*
    functions to load an imported Object into a Scene Model and to build a sceneGraph node from it
*)
module sceneObject = 
    open Loader
    open material
    open Aardvark.UI

    //let emptyObject = {Scene.meshes = [||]; animantions = Map.empty; bounds = Box3d.Invalid; root = Empty; rootTrafo = Trafo3d.Identity}

    let defaultObject = {name = "Default"; file  = ""; scale = 1.0; translation = V3d.Zero; rotation = V3d.Zero; materials = HMap.empty;  currentMaterial = ""; materialLinks = HMap.empty}

    //add an external object to the scene model
    //note that we have to import it to read the materiaal list but discart the import
    //for rendering we import it again
    //This is not very efficient, but happens only when adding a new object to a scene
    let loadObject (objects : hmap<string,SceneObject>) file = 

        let rec uniqueName name =
            if HMap.containsKey name objects then
                let name' = sprintf "%s_1" name
                uniqueName name'
            else name
       
        let name' = IO.Path.GetFileNameWithoutExtension(file)
        let name = uniqueName name'
        let import = Assimp.load file
        let materials = materials import
        let currentMaterial = 
            materials
            |> HMap.keys 
            |> HSet.toList
            |> List.first
            |> Option.defaultValue "none"
        let obj = {name = name; file = file; scale = 1.0; translation = V3d.Zero; rotation = V3d.Zero; materials = materials;  currentMaterial = currentMaterial; materialLinks = HMap.empty}
        HMap.add name obj objects , name

    //load an external object into an mod and substitute the material definitions with PBR materials
    let object (m : MSceneObject) = 
        Mod.custom (fun toc ->
            let f = m.file.GetValue toc
            let o = Assimp.load f
            o.SubstituteMaterial (fun mat -> Some ({Name = mat.name; Material = (AMap.find  mat.name m.materials)} :> IO.Loader.IMaterial))
        )    

    // build a scene graph node for a object
    let sg (m : MSceneObject) =
        m
        |> object
        |> Mod.map (fun o ->
        
            o
            |> Sg.adapter
        )   

    let trafo (m : MSceneObject) =
        Mod.custom (fun toc ->
            let s = m.scale.GetValue toc
            let t = m.translation.GetValue toc
            let r = m.rotation.GetValue toc / Constant.DegreesPerRadian

            Trafo3d.FromComponents(V3d(s),r,t)
        )  

//UI control for a scene object 
module sceneObjectControl = 
    open Aardvark.UI
    open Aardvark.UI.Primitives
    
    type Message =
    |SetTranslation of V3dInput.Message
    |SetRotation of V3dInput.Message
    |SetScale of float
    |MaterialMessage of materialControl.Message * string
    |SetCurrentMaterial of string
    |SetMaterialLink of string option

    let update (m : SceneObject) (msg : Message) =
        match msg with
        |SetTranslation vMsg -> 
            let t = V3dInput.update (m.translation.XYZ) vMsg
            {m with translation = t} 
        |SetRotation vMsg -> 
            let r = V3dInput.update (m.rotation.XYZ) vMsg
            {m with rotation = r} 
        |SetScale s -> 
            {m with  scale = s}
        | MaterialMessage (msg, s) ->
            let m' = materialControl.update m.materials.[s] msg
            let materials' =  
                m.materials
                |> HMap.update  s (fun _ -> m' ) 
                |>  HMap.map (fun k m'' -> if HMap.tryFind k m.materialLinks = Some s then m' else m'')
            { m with materials = materials'}
        | SetCurrentMaterial s ->
            { m with currentMaterial = s }   
        | SetMaterialLink  t -> 
            let newLinks =
                m.materialLinks
                |> HMap.alter m.currentMaterial (fun _ -> t)
                |> HMap.map (fun k t1 -> match m.currentMaterial, t with
                                            |t1, Some t' -> t' 
                                            |_ ->  t1)
            let newMaterials = 
                match t with
                |Some t' -> m.materials 
                            |> HMap.map (fun k m' -> if HMap.tryFind k newLinks = t then m.materials.[t'] else m')
                |None -> m.materials
            { m with materialLinks =  newLinks; materials = newMaterials }

    let view (m : MSceneObject) =
        let linkedMaterial = Mod.bind (fun c -> AMap.tryFind c m.materialLinks) m.currentMaterial
        let updatedMaterial = Mod.map2 (Option.defaultValue)  m.currentMaterial  linkedMaterial
        let materialsList = m.materials |> AMap.keys |> ASet.toAList |> AList.sort
        let materialsToLink = 
            materialsList
            |> AList.filteriM (fun _ mat -> 
                Mod.map (fun c -> c != mat) m.currentMaterial
                |> Mod.map2 (fun a b -> not a && b) (m.materialLinks |> AMap.keys |> ASet.contains mat)  )
            |> AList.map (Some)
            |> AList.append (AList.single None)  
        div [] [
            Incremental.text m.name
            V3dInput.view "Transaltion" m.translation |> UI.map SetTranslation
            V3dInput.view "Rotation" m.rotation |> UI.map SetRotation
            Html.table [                        
            tr [] [ td [] [text "Scale"]; td [style "width: 70%;"] [inputLogSlider {min = 0.0001;  max = 100.0; step = 0.01} [] m.scale SetScale]]
            ]
            Html.table [                        
            tr [] [ td [] [text "Material"]; td [style "width: 70%;"] [Html.SemUi.dropDown' (materialsList) m.currentMaterial SetCurrentMaterial id]]
            tr [] [ td [] [text "Linked to"]; td [style "width: 70%;"] [Html.SemUi.dropDown' (materialsToLink) linkedMaterial  SetMaterialLink  (Option.defaultValue "-")]]
            ]
            updatedMaterial
            |> Mod.bind (fun c ->
                m.materials
                |> AMap.find c
                |> Mod.map (fun m -> materialControl.view m |> UI.map (fun msg -> MaterialMessage (msg,c)))
            )
            |> AList.ofModSingle
            |> Incremental.div AttributeMap.empty
       ]