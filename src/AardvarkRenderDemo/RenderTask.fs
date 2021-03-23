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
                    let tex = runtime.CreateTexture2D(TextureFormat.ofRenderbufferFormat att.format, att.samples, size)
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

    ///render with custom clear colors and preexisting fbo attachments
    let renderSemanticsCustom (output : Set<Symbol>) (size : aval<V2i>) (clearValues : ClearValues) (attachments : Map<Symbol, aval<IFramebufferOutput>>) (task : IRenderTask) =
        let runtime = task.Runtime.Value
        let signature = task.FramebufferSignature.Value

        let fbo = CreateFramebufferWithExistingAttachments runtime signature size output attachments
        let res = task.RenderTo(fbo,clearValues)
        output |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq

    let private defaultClearValues =
        clear {
            color C4f.Black
            depth 1.0
            stencil 0
        }

    ///render with preexisting fbo attachments
    let renderSemanticsCustom' (output : Set<Symbol>) (size : aval<V2i>) (attachments : Map<Symbol, aval<IFramebufferOutput>>) (task : IRenderTask) =
        renderSemanticsCustom output size defaultClearValues attachments task

    ///render with custom clear colors
    let renderSemanticsCustomClear (output : Set<Symbol>) (size : aval<V2i>) (clearValues : ClearValues) (task : IRenderTask) =
        renderSemanticsCustom output size clearValues Map.empty task

    let renderSemanticsCubeMip' (output : Set<Symbol>) levels size (tasks : CubeSide -> int -> IRenderTask) =
        tasks
        |> CubeMap.init levels 
        |> RenderTask.renderSemanticsCubeMip (Set.singleton DefaultSemantic.Colors) size
