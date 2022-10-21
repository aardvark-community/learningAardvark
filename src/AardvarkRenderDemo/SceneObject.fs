namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
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

    let defaultObject = {name = "Default"; file  = ""; scale = 1.0; translation = V3d.Zero; rotation = V3d.Zero; materials = HashMap.empty;  currentMaterial = ""; materialLinks = HashMap.empty; object = emptyScene}

    //add an external object to the scene model
    let loadObject (objects : HashMap<string,SceneObject>) (file : string) = 

        let rec uniqueName name =
            if HashMap.containsKey name objects then
                let name' = sprintf "%s_1" name
                uniqueName name'
            else name
       
        let name' = IO.Path.GetFileNameWithoutExtension(file)
        let name = uniqueName name'
        let import = Assimp.load file
        let materials = materials import
        let currentMaterial = 
            materials
            |> HashMap.keys 
            |> HashSet.toList
            |> List.first
            |> Option.defaultValue "none"
        let obj = {
            name = name
            file = file
            scale = 1.0
            translation = V3d.Zero
            rotation = V3d.Zero
            materials = materials
            currentMaterial = currentMaterial
            materialLinks = HashMap.empty
            object = import
            }
        HashMap.add name obj objects , name

    //load an external object into an mod and substitute the material definitions with PBR materials
    let object (m : AdaptiveSceneObject) = 
        m.object
        |> AVal.map (fun (o :Scene) -> o.SubstituteMaterial (fun mat -> Some ({Name = mat.name; Material = (AMap.find  mat.name m.materials)} :> IO.Loader.IMaterial))) 

    // build a scene graph node for a object
    let sg (m : AdaptiveSceneObject) =
        m
        |> object
        |> AVal.map (fun o ->        
            o
            |> Sg.adapter
        )   

    let trafo (m : AdaptiveSceneObject) =
        AVal.custom (fun toc ->
            let s = m.scale.GetValue toc
            let t = m.translation.GetValue toc
            let r = m.rotation.GetValue toc / Constant.DegreesPerRadian

            Trafo3d.FromComponents(V3d(s),r,t)
        )  

    //generate sceen graph from the objects 
    let objects (os :amap<string,  AdaptiveSceneObject>) = 
        aset {
            for o in AMap.toASetValues os do
                let! s = sg o
                yield s |> Sg.trafo (trafo o) 
        }
        |> Sg.set  

    let trimByMaterials (trimBy : HashSet<string>) (s : Scene) =
        let rec traverse (n : Node) =
                match n with
                    | Node.Empty -> 
                        Node.Empty
                    | Node.Group es ->
                        Node.Group (List.map traverse es)
                    | Node.Leaf m ->
                        Node.Leaf m
                    | Node.Material(m, ni) ->
                        if trimBy |>  HashSet.contains m.name then
                            Node.Empty
                        else
                            Node.Material(m, traverse ni)
                    | Node.Trafo(t, ni) ->
                        Node.Trafo(t, traverse ni)
        {s with root = traverse s.root}

    let trimByMatrial (filter : string -> AdaptivePBRMaterial -> aval<bool>) (m : AdaptiveSceneObject)  =
        let trimBy =
            m.materials
            |> AMap.filterA filter
            |> AMap.keys
            |> ASet.toAVal
        let o  = object m
        let o' = 
            aval {
                let! t = trimBy
                let! obj = o
                return trimByMaterials t obj |> Sg.adapter
            }
        o'

    let objectsTrimByMaterial (filter : string -> AdaptivePBRMaterial -> aval<bool>) (os :amap<string,  AdaptiveSceneObject>) = 
        aset {
            for _,o in AMap.toASet os do
                let! s = trimByMatrial filter o
                yield s |> Sg.trafo (trafo o ) 
        }
        |> Sg.set  

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
                |> HashMap.update  s (fun _ -> m' ) 
                |>  HashMap.map (fun k m'' -> if HashMap.tryFind k m.materialLinks = Some s then m' else m'')
            { m with materials = materials'}
        | SetCurrentMaterial s ->
            { m with currentMaterial = s }   
        | SetMaterialLink  t -> 
            let newLinks =
                m.materialLinks
                |> HashMap.alter m.currentMaterial (fun _ -> t)
                |> HashMap.map (fun k t1 -> if t1 =  m.currentMaterial then
                                                match t with
                                                |Some t' -> t' 
                                                |_ ->  t1
                                            else
                                                t1
                               )
            let newMaterials = 
                match t with
                |Some t' -> m.materials 
                            |> HashMap.map (fun k m' -> if HashMap.tryFind k newLinks = t then m.materials.[t'] else m')
                |None -> m.materials
            { m with materialLinks =  newLinks; materials = newMaterials }

    let view  (sssProfiles : amap<int,AdaptiveSssProfile>) (m : AdaptiveSceneObject) =
        let linkedMaterial = AVal.bind (fun c -> AMap.tryFind c m.materialLinks)  m.currentMaterial
        let updatedMaterial = AVal.map2 (Option.defaultValue)  m.currentMaterial  linkedMaterial
        let materialsList = m.materials |> AMap.keys |> ASet.toAList |> AList.sort
        let materialsToLink = 
            materialsList
            |> AList.filterA (fun mat -> 
                AVal.map (fun c -> c != mat) m.currentMaterial
                |> AVal.map2 (fun a b -> not a && b) (m.materialLinks |> AMap.keys |> ASet.contains mat)  )
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
            |> AVal.bind (fun c ->
                m.materials
                |> AMap.find c
                |> AVal.map (fun m -> materialControl.view m sssProfiles |> UI.map (fun msg -> MaterialMessage (msg,c)) |> IndexList.single)
            )
            |> AList.ofAVal
            |> Incremental.div AttributeMap.empty
       ]