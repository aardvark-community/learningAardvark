namespace Aardvark.Base

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering

module RenderTaskExtensions =

    let CreateFramebufferWithExistingAttachments (runtime : IFramebufferRuntime) (signature : IFramebufferSignature) (size : aval<V2i>) (output : Set<Symbol>) (attachments : Map<Symbol, aval<IFramebufferOutput>>)=

        let inline createAttachment (sem : Symbol) (att : AttachmentSignature) =
            attachments
            |> Map.tryFind sem 
            |> Option.defaultValue (
                if output |> Set.contains sem then
                    let tex = runtime.CreateTexture(TextureFormat.ofRenderbufferFormat att.format, att.samples, size)
                    runtime.CreateTextureAttachment(tex, 0) :> aval<_>
                else
                    let rb = runtime.CreateRenderbuffer(att.format, att.samples, size)
                    runtime.CreateRenderbufferAttachment(rb) :> aval<_>
                )
 
        let atts = SymDict.empty

        signature.DepthAttachment |> Option.iter (fun d ->
            atts.[DefaultSemantic.Depth] <- createAttachment DefaultSemantic.Depth d
        )

        signature.StencilAttachment |> Option.iter (fun s ->
            atts.[DefaultSemantic.Stencil] <- createAttachment DefaultSemantic.Stencil s
        )

        for (_, (sem, att)) in Map.toSeq signature.ColorAttachments do
            atts.[sem] <- createAttachment sem att

        runtime.CreateFramebuffer(signature, SymDict.toMap atts)

    let getResult (sem : Symbol) (t : IAdaptiveResource<IFramebuffer>) =
        t.GetOutputTexture sem

    let renderSemanticsCustom (output : Set<Symbol>) (size : aval<V2i>) (clearColors : Map<Symbol,C4f>) (attachments : Map<Symbol, aval<IFramebufferOutput>>) (task : IRenderTask) =
        let runtime = task.Runtime.Value
        let signature = task.FramebufferSignature.Value

        let clear = runtime.CompileClear(signature, clearColors)
        let fbo = CreateFramebufferWithExistingAttachments runtime signature size output attachments

        let task' = new SequentialRenderTask([|clear; task|])
        let res = task'.RenderTo(fbo, dispose = true)
        output |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq

    let renderSemanticsCustom' (output : Set<Symbol>) (size : aval<V2i>) (attachments : Map<Symbol, aval<IFramebufferOutput>>) (task : IRenderTask) =
        let runtime = task.Runtime.Value
        let signature = task.FramebufferSignature.Value

        let clear = runtime.CompileClear(signature, C4f.Black, 1.0)
        let fbo = CreateFramebufferWithExistingAttachments runtime signature size output attachments

        let task' = new SequentialRenderTask([|clear; task|])
        let res = task'.RenderTo(fbo, dispose = true)
        output |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq

    let renderSemanticsCustomClear (output : Set<Symbol>) (size : aval<V2i>) (clearColors : Map<Symbol,C4f>) (task : IRenderTask) =
        renderSemanticsCustom output size clearColors Map.empty task
