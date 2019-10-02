namespace Aardvark.Base

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open System.Collections.Generic
open Aardvark.Base.Rendering
open System.Runtime.CompilerServices
open System.Threading
open OpenTK
open OpenTK.Platform
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL4
open Aardvark.Rendering.GL

[<AutoOpen>]
module private RefCountedResources = 

    type IMod<'a> with
        member x.GetValue(c : AdaptiveToken, t : RenderToken) =
            match x with
                | :? IOutputMod<'a> as x -> x.GetValue(c, t)
                | _ -> x.GetValue(c)

    type AdaptiveTexture(runtime : IRuntime, format : TextureFormat, samples : int, size : IMod<V2i>) =
        inherit AbstractOutputMod<ITexture>()

        let mutable handle : Option<IBackendTexture> = None

        override x.Create() = ()
        override x.Destroy() =
            match handle with
                | Some h ->
                    runtime.DeleteTexture(h)
                    handle <- None
                | None ->
                    ()

        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let size = size.GetValue(token)

            match handle with
                | Some h when h.Size.XY = size -> 
                    h :> ITexture

                | Some h -> 
                    t.ReplacedResource(ResourceKind.Texture)
                    runtime.DeleteTexture(h)
                    let tex = runtime.CreateTexture(size, format, 1, samples)
                    handle <- Some tex
                    tex :> ITexture

                | None ->
                    t.CreatedResource(ResourceKind.Texture)
                    let tex = runtime.CreateTexture(size, format, 1, samples)
                    handle <- Some tex
                    tex :> ITexture
         
    type AdaptiveCubeTexture(runtime : IRuntime, format : TextureFormat, samples : int, size : IMod<int>, levels : int) =
        inherit AbstractOutputMod<ITexture>()

        let mutable handle : Option<IBackendTexture> = None

        override x.Create() = ()
        override x.Destroy() =
            match handle with
                | Some h ->
                    runtime.DeleteTexture(h)
                    handle <- None
                | None ->
                    ()

        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let size = size.GetValue(token)

            match handle with
                | Some h when h.Size.X = size -> 
                    h :> ITexture

                | Some h -> 
                    t.ReplacedResource(ResourceKind.Texture)
                    runtime.DeleteTexture(h)
                    let tex = runtime.CreateTextureCube(size, format, levels, samples)
                    handle <- Some tex
                    tex :> ITexture

                | None ->
                    t.CreatedResource(ResourceKind.Texture)
                    let tex = runtime.CreateTextureCube(size, format, levels, samples)
                    handle <- Some tex
                    tex :> ITexture

    
    type AdaptiveRenderbuffer(runtime : IRuntime, format : RenderbufferFormat, samples : int, size : IMod<V2i>) =  
        inherit AbstractOutputMod<IRenderbuffer>()

        let mutable handle : Option<IRenderbuffer> = None

        override x.Create() = ()
        override x.Destroy() =
            match handle with
                | Some h ->
                    runtime.DeleteRenderbuffer(h)
                    handle <- None
                | None ->
                    ()

        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let size = size.GetValue(token)

            match handle with
                | Some h when h.Size = size -> 
                    h

                | Some h -> 
                    t.ReplacedResource(ResourceKind.Renderbuffer)
                    runtime.DeleteRenderbuffer(h)
                    let tex = runtime.CreateRenderbuffer(size, format, samples)
                    handle <- Some tex
                    tex

                | None ->
                    t.CreatedResource(ResourceKind.Renderbuffer)
                    let tex = runtime.CreateRenderbuffer(size, format, samples)
                    handle <- Some tex
                    tex
    
    [<AbstractClass>]
    type AbstractAdaptiveFramebufferOutput(resource : IOutputMod) =
        inherit AbstractOutputMod<IFramebufferOutput>()

        override x.Create() = resource.Acquire()
        override x.Destroy() = resource.Release()
    
    type AdaptiveTextureAttachment(texture : IOutputMod<ITexture>, slice : int, level  : int) =
        inherit AbstractAdaptiveFramebufferOutput(texture)
        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let tex = texture.GetValue(token, t)
            { texture = unbox tex; slice = slice; level = level } :> IFramebufferOutput

    type AdaptiveRenderbufferAttachment(renderbuffer : IOutputMod<IRenderbuffer>) =
        inherit AbstractAdaptiveFramebufferOutput(renderbuffer)
        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let rb = renderbuffer.GetValue(token, t)
            rb :> IFramebufferOutput

    type IRuntime with
        member x.CreateTexture(format : TextureFormat, samples : int, size : IMod<V2i>) =
            AdaptiveTexture(x, format, samples, size) :> IOutputMod<ITexture>

        member x.CreateTextureCube(format : TextureFormat, samples : int, size : IMod<int>, levels : int) =
            AdaptiveCubeTexture(x, format, samples, size, levels) :> IOutputMod<ITexture>

        member x.CreateRenderbuffer(format : RenderbufferFormat, samples : int, size : IMod<V2i>) =
            AdaptiveRenderbuffer(x, format, samples, size) :> IOutputMod<IRenderbuffer>

        member x.CreateTextureAttachment(texture : IOutputMod<ITexture>, slice : int, level : int)  =
            AdaptiveTextureAttachment(texture, slice, level) :> IOutputMod<_>

        member x.CreateRenderbufferAttachment(renderbuffer : IOutputMod<IRenderbuffer>) =
            AdaptiveRenderbufferAttachment(renderbuffer) :> IOutputMod<_>

    type AdaptiveFramebufferCube(runtime : IRuntime, signature : IFramebufferSignature, textures : Set<Symbol>, size : IMod<int>, levels : int) =
        inherit AbstractOutputMod<IFramebuffer[]>()

        let store = SymDict.empty

        let createAttachment (sem : Symbol) (face : CubeSide) (level : int) (att : AttachmentSignature) =
            let isTexture = Set.contains sem textures
            if isTexture then
                
                let tex = 
                    store.GetOrCreate(sem, fun sem ->
                        runtime.CreateTextureCube(unbox (int att.format), att.samples, size, levels) :> IOutputMod
                    ) |> unbox<IOutputMod<ITexture>>

                runtime.CreateTextureAttachment(tex, int face, level)
            else
                let rb = 
                    store.GetOrCreate(sem, fun sem ->
                        runtime.CreateRenderbuffer(att.format, att.samples, size |> Mod.map(fun x -> V2i(x))) :> IOutputMod
                    ) |> unbox<IOutputMod<IRenderbuffer>>

                runtime.CreateRenderbufferAttachment(rb)

        let mutable handle : Option<IFramebuffer>[] = Array.zeroCreate (6*levels)

        let attachments =
            Array.init (6*levels) (fun t ->
                let face = unbox<CubeSide> (t % 6)
                let level = t / 6
                let attachments = SymDict.empty
                match signature.DepthAttachment with
                    | Some d -> 
                        attachments.[DefaultSemantic.Depth] <- createAttachment DefaultSemantic.Depth face level d
                    | None -> 
                        ()

                for (index, (sem, att)) in Map.toSeq signature.ColorAttachments do
                    let a = createAttachment sem face level att
                    attachments.[sem] <- a

                attachments
            )

        override x.Create() =
            for face in 0 .. attachments.Length-1 do
                for att in attachments.[face].Values do att.Acquire()

        override x.Destroy() =
            for face in 0 .. attachments.Length-1  do
                for att in attachments.[face].Values do att.Release()
                match handle.[face] with
                    | Some h -> 
                        runtime.DeleteFramebuffer(h)
                        handle.[face] <- None
                    | None -> ()

        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            attachments |> Array.mapi (fun i attachments ->
                let att = 
                    attachments
                        |> SymDict.toMap 
                        |> Map.map (fun sem att -> 
                            att.GetValue(token, t))

                Log.warn "AdaptiveFramebufferCube.compute"
                match handle.[i] with
                    | Some h -> 
                        runtime.DeleteFramebuffer(h)
                        t.ReplacedResource(ResourceKind.Framebuffer)
                    | None ->
                        t.CreatedResource(ResourceKind.Framebuffer)

                let fbo = runtime.CreateFramebuffer(signature, att)

                handle.[i] <- Some fbo
                fbo
            )

    type AdaptiveRenderingResultCube(tasks : IRenderTask[], target : IOutputMod<IFramebuffer[]>) =
        inherit AbstractOutputMod<IFramebuffer[]>()

        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let fbo = target.GetValue(token, t)
            Log.warn "AdaptiveRenderingResultCube.compute"
            for face in 0..5 do
                Log.warn "AdaptiveRenderingResultCube.compute1"
                tasks.[face].Run(token, t, OutputDescription.ofFramebuffer fbo.[face])
            fbo

        override x.Inputs =
            seq {
                yield! (Array.toSeq tasks |> Seq.cast<IAdaptiveObject>)
                yield target :> _
            }

        override x.Create() =
            Log.line "result created"
            target.Acquire()

        override x.Destroy() =
            Log.line "result deleted"
            target.Release()

 
    type AdaptiveOutputCubeTexture(semantic : Symbol, res : IOutputMod<IFramebuffer[]>) =
        inherit AbstractOutputMod<ITexture>()

        override x.Compute(token : AdaptiveToken, t : RenderToken) =
            let res = res.GetValue(token, t)
            Log.warn "AdaptiveOutputCubeTexture.compute"
            match Map.tryFind semantic res.[0].Attachments with
                | Some (:? IBackendTextureOutputView as t) ->
                    t.texture :> ITexture
                | _ ->
                    failwithf "could not get result for semantic %A as texture" semantic

        override x.Inputs =
            Seq.singleton (res :> _)

        override x.Create() =
            Log.line "texture created"
            res.Acquire()

        override x.Destroy() =
            Log.line "texture deleted"
            res.Release()
    



[<AbstractClass; Sealed; Extension>]
type RuntimeFramebufferExtensions private() =
 
    [<Extension>]
    static member CreateFramebufferCube (this : IRuntime, signature : IFramebufferSignature, textures : Set<Symbol>, size : IMod<int>,  levels : int) : IOutputMod<IFramebuffer[]> =
        AdaptiveFramebufferCube(this, signature, textures, size, levels) :> IOutputMod<IFramebuffer[]>

    [<Extension>]
    static member RenderToCube(this : IRenderTask[], output : IOutputMod<IFramebuffer[]>) =
        AdaptiveRenderingResultCube(this, output) :> IOutputMod<_>

    [<Extension>]
    static member GetOutputCubeTexture (this : IOutputMod<IFramebuffer[]>, semantic : Symbol) =
        AdaptiveOutputCubeTexture(semantic, this) :> IOutputMod<_>



module RenderTask =

    type SequentialRenderTask(tasks : IRenderTask[]) =
        inherit AbstractRenderTask()

        let signature =
            lazy (
                let signatures = tasks |> Array.choose (fun t -> t.FramebufferSignature)

                if signatures.Length = 0 then None
                elif signatures.Length = 1 then Some signatures.[0]
                else 
                    let s0 = signatures.[0]
                    let all = signatures |> Array.forall (fun s -> s0.IsAssignableFrom s0)
                    if all then Some s0
                    else failwithf "cannot compose RenderTasks with different FramebufferSignatures: %A" signatures
            )

        let runtime = tasks |> Array.tryPick (fun t -> t.Runtime)
        member x.Tasks = tasks

        override x.Use(f : unit -> 'a) =
            lock x (fun () ->
                let rec run (i : int) =
                    if i >= tasks.Length then f()
                    else tasks.[i].Use (fun () -> run (i + 1))

                run 0
            )

        override x.Release() =
            for t in tasks do t.Dispose()

        override x.PerformUpdate(token : AdaptiveToken, rt : RenderToken) =
            for t in tasks do
                t.Update(token, rt)


        override x.Perform(token : AdaptiveToken, rt : RenderToken, output : OutputDescription) =
            for t in tasks do
                t.Run(token, rt, output)


        override x.FramebufferSignature = signature.Value
        override x.Runtime = runtime

    let renderToCube (target : IOutputMod<IFramebuffer[]>) (tasks : IRenderTask[]) : IOutputMod<IFramebuffer[]> =
        tasks.RenderToCube target

    let getResultCube (sem : Symbol) (t : IOutputMod<IFramebuffer[]>) =
        t.GetOutputCubeTexture sem

    let renderSemanticsCube (sem : Set<Symbol>) (size : IMod<int>) (levels : int) (tasks' : int -> CubeSide -> IRenderTask) =
        let tasks =
            Array.init (6 * levels)  (fun t ->
                let face = unbox<CubeSide> (t % 6)
                let level  = t / 6 
                tasks' level face
            )
        let runtime = tasks.[0].Runtime.Value
        let signature = tasks.[0].FramebufferSignature.Value

        let clearColors = 
            sem |> Set.toList |> List.filter (fun s -> s <> DefaultSemantic.Depth) |> List.map (fun s -> s,C4f.Black)
        let clear = runtime.CompileClear(signature, ~~clearColors, ~~1.0)
        let fbo = runtime.CreateFramebufferCube(signature, sem, size, levels)

        let res = 
            Array.map (fun  task -> new SequentialRenderTask([|clear; task|]) :> IRenderTask) tasks
            |> renderToCube fbo
        sem |> Seq.map (fun k -> k, getResultCube k res) |> Map.ofSeq

    let renderToColorCubeMip (size : IMod<int>) (levels : int) (tasks : int -> CubeSide -> IRenderTask) =
       tasks |> renderSemanticsCube (Set.singleton DefaultSemantic.Colors) size levels |> Map.find DefaultSemantic.Colors

    let renderToColorCube (size : IMod<int>) (tasks : CubeSide -> IRenderTask) =
        (fun _ -> tasks) |> renderToColorCubeMip size 1 

 

