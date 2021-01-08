namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model

module combine =

    let combine (runtime : IRuntime)  (size : aval<V2i>)  (textures : Map<Symbol,IOutputMod<ITexture>>)=

        //additive blending
        let mutable blendMode = BlendMode(true)
        blendMode.AlphaOperation <- BlendOperation.Add
        blendMode.Operation <- BlendOperation.Add
        blendMode.SourceFactor <- BlendFactor.One
        blendMode.SourceAlphaFactor <- BlendFactor.One
        blendMode.DestinationFactor <- BlendFactor.One
        blendMode.DestinationAlphaFactor <- BlendFactor.One
        
        let ts = 
             textures
            |> Map.toList
            |> ASet.ofList 

        let signatureC =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
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
        |> Sg.blendMode (blendMode |> AVal.constant)
        |> Sg.compile runtime signatureC
        |> RenderTask.renderToColor  size     
