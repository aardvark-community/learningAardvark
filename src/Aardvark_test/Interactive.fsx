#if INTERACTIVE
#load @"..\..\.paket\load\netcoreapp2.0\main.group.fsx"
#else
#endif

#load @"..\..\paket-files\aardvark-platform\aardvark.rendering\src\Application\Aardvark.Application.Utilities\FsiHelper.fsx"
FsiHelper.InteractiveHelper.init()

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering

let aardvarkMesh : ISg<obj>= 
        Aardvark.SceneGraph.IO.Loader.Assimp.load @"..\..\data\SLE_Gnom.obj" 
        |> Sg.adapter
        |> Sg.transform (Trafo3d.Scale(1.0,1.0,-1.0))