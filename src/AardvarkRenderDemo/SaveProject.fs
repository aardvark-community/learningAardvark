namespace SLEAardvarkRenderDemo

open MBrace.FsPickler.Json
open  SLEAardvarkRenderDemo.Model
open Aardvark.Base
open Aardvark.UI.Primitives

(*
    Scene save and load
*)
module projetIO =

    type ExportModel  = {
        cameraViewParams : V3d * V3d * V3d
        lights : hmap<int, Light>
        enviorment : GlobalEnviorment
        expousure  : float
        objects : hmap<string, SceneObject>
        selectedObject : string
    }

    let toExportModel (m : Model) =
        let v = m.cameraState.view
        let vp = v.Location, v.Forward, v.Sky
        {   cameraViewParams = vp
            lights  = m.lights
            enviorment = m.enviorment
            expousure = m.expousure
            objects = m.objects
            selectedObject = m.selectedObject
        }

    let fromExportModel (em : ExportModel) =
        let cameraConfig  =  {FreeFlyController.initial.freeFlyConfig with zoomMouseWheelSensitivity = 0.5} 
        let l,c,s = em.cameraViewParams
        let initialView = CameraView.look l c s
        {   cameraState = {FreeFlyController.initial  with freeFlyConfig = cameraConfig; view = initialView}
            lights  = em.lights
            enviorment = em.enviorment
            expousure = em.expousure
            objects = em.objects
            selectedObject = em.selectedObject
        }

    let pickler = FsPickler.CreateJsonSerializer(indent = true)

    //toDo: create file  if it dosn't exist
    let save (m : Model) f =
        let em = toExportModel m
        let text = pickler.PickleToString em
        do System.IO.File.WriteAllText(f, text)

    //toDo: errorhandling
    let load f = 
        let text = System.IO.File.ReadAllText(f)
        let em = pickler.UnPickleOfString text
        fromExportModel em