open Aardvark_test

open Aardium
open Aardvark.Service
open Aardvark.UI
open Suave
open Suave.WebPart
open Aardvark.Rendering.Vulkan
open Aardvark.Base
open System

[<EntryPoint>]
let main args =
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    let app = new HeadlessVulkanApplication()

    WebPart.startServer 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start App.app)
        Reflection.assemblyWebPart (System.Reflection.Assembly.GetEntryAssembly())
        //requiered to load the spectrum.js for the colorpicker (EmbeddedResources is the marker type to find the correct assembly)
        Reflection.assemblyWebPart typeof<Aardvark.UI.Primitives.EmbeddedResources>.Assembly
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1600
        height 1000
        url "http://localhost:4321/"
    }

    0
