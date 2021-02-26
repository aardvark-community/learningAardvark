namespace SLEAardvarkRenderDemo

open MBrace.FsPickler.Json
open SLEAardvarkRenderDemo.Model
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Aardvark.SceneGraph.IO

(*
    Scene save and load
*)
module projetIO =

    type ExportSceneObject = {
        //[<PrimaryKey>] 
        name : string
        file : string
        scale : float
        translation : V3d
        rotation : V3d
        materials : HashMap<string, PBRMaterial>
        currentMaterial : string   
        materialLinks : HashMap<string, string> 
    }

    type ExportModel  = {
        cameraViewParams : V3d * V3d * V3d
        lights : HashMap<int, Light>
        enviorment : GlobalEnviorment
        expousure  : float
        objects : HashMap<string, ExportSceneObject>
        selectedObject : string
        toneMapping : ToneMapping
        bloom : Bloom
        fxAA : fxAA
        sssProfiles : HashMap<int, SssProfile>
    }

    let toExportSceneObject (o : SceneObject) =
        let o' : ExportSceneObject =
            {
                name = o.name
                file = o.file
                scale = o.scale
                translation = o.translation
                rotation = o.rotation
                materials = o.materials
                currentMaterial = o.currentMaterial  
                materialLinks = o.materialLinks
            }
        o'

    let toSceneObject (o : ExportSceneObject) (p :string)=
        let absoluteF =  System.IO.Path.Combine(p, o.file)
        let o' : SceneObject =
            {
                name = o.name
                file = o.file
                scale = o.scale
                translation = o.translation
                rotation = o.rotation
                materials = o.materials
                currentMaterial = o.currentMaterial  
                materialLinks = o.materialLinks
                object =  Loader.Assimp.load absoluteF
            }
        o'
    //store all file paths relative to the dir of the scene file
    let mapFileNames (mapper : string -> string) (m : Model) (f :string) =
        {m with 
            enviorment = {m.enviorment with skyMap = mapper m.enviorment.skyMap}
            objects = HashMap.map (fun _ o -> {
                 o with 
                    file = mapper o.file
                    materials = HashMap.map ( fun _ m -> {
                        m with
                            metallic = {m.metallic with fileName = Option.map mapper m.metallic.fileName}
                            roughness = {m.roughness with fileName = Option.map mapper m.roughness.fileName}
                            albedo = {m.albedo with fileName = Option.map mapper m.albedo.fileName}
                            normal = {m.normal with fileName = Option.map mapper m.normal.fileName}
                            displacment = {m.displacment with fileName = Option.map mapper m.displacment.fileName}
                     }) o.materials
            }) m.objects
        }

    let toRelativePaths (m : Model) (f :string) =
        let relative p =  System.IO.Path.GetRelativePath(System.IO.Path.GetDirectoryName(f), System.IO.Path.GetFullPath(p))
        mapFileNames relative m f

    let toAbsolutePaths (m : Model) (f :string) =
        let absolute p =  System.IO.Path.Combine(System.IO.Path.GetFullPath(System.IO.Path.GetDirectoryName(f)), p)
        mapFileNames absolute m f
        
    let toExportModel (m : Model) f =
        let m' =  toRelativePaths m f
        let v = m'.cameraState.view
        let vp = v.Location, v.Forward, v.Sky
        {   cameraViewParams = vp
            lights  = m'.lights
            enviorment = m'.enviorment
            expousure = m'.expousure
            bloom = m'.bloom
            fxAA  =m'.fxAA
            toneMapping = m'.toneMapping
            objects = m'.objects |> HashMap.map (fun _ o -> toExportSceneObject o)
            selectedObject = m'.selectedObject
            sssProfiles = m'.sssProfiles
        }

    let fromExportModel (em : ExportModel) (f : string) =
        let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
        let l,c,s = em.cameraViewParams
        let initialView = CameraView.look l c s
        let p = System.IO.Path.GetFullPath(System.IO.Path.GetDirectoryName(f))
        let m = {
            cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
            lights  = em.lights
            enviorment = em.enviorment
            expousure = em.expousure
            toneMapping = em.toneMapping
            fxAA = em.fxAA
            bloom = em.bloom
            objects = em.objects |> HashMap.map (fun _ o -> toSceneObject o p)
            selectedObject = em.selectedObject
            sssProfiles = em.sssProfiles
        }
        toAbsolutePaths m f

    let pickler = FsPickler.CreateJsonSerializer(indent = true)

    //toDo: create file  if it dosn't exist
    let save (m : Model) f =
        let em = toExportModel m f
        let text = pickler.PickleToString em
        do System.IO.File.WriteAllText(f, text)

    //toDo: errorhandling
    let load f = 
        let text = System.IO.File.ReadAllText(f)
        let em = pickler.UnPickleOfString text
        fromExportModel em f