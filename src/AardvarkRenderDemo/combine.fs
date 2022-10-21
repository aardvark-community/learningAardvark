namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module combine =

    let combine (runtime : IRuntime)  (size : aval<V2i>)  (textures : Map<Symbol,IAdaptiveResource<IBackendTexture>>)=
      
        let ts = 
             textures
            |> Map.toList
            |> ASet.ofList 

        let signatureC =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba32f
            ] 
       
        let s = aset {
           for  (_,t) in ts do
                let pass = 
                     Sg.fullScreenQuad
                    |> Sg.adapter
                    |> Sg.texture DefaultSemantic.DiffuseColorTexture  t
                    |> Sg.shader {
                        do! DefaultSurfaces.diffuseTexture
                        } 
                yield  pass
            } 

        Sg.set s
        |> Sg.blendMode' BlendMode.Add
        |> Sg.compile runtime signatureC
        |> RenderTask.renderToColor  size     
