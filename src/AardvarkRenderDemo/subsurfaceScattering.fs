namespace SLEAardvarkRenderDemo

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering.Effects
open Aardvark.SceneGraph
open SLEAardvarkRenderDemo.Model
open Aardvark.UI
open Aardvark.UI.Primitives

module subSurfaceShader =
   open fshadeExt


    type UniformScope with
        member x.Samples : int = 25
        member x.horizontal : bool = x?horizontal
        member x.sssWidth :  Arr<N<200>, float> = x?sssWidth
        member x.camFoVy : float = x?camFoVy
        member x.kernelRange : float = 3.0
        member x.kernel : Arr<N<200>, V4d> = uniform?kernel


    let inputImage =
        sampler2d {
            texture uniform?inputImage
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let depth =
        sampler2d {
            texture uniform?Depth
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    let albedoAndProfileIndex =
        sampler2d {
            texture uniform?Colors
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            filter Filter.MinMagLinear
        }

    [<ReflectedDefinition>]
    let extractProfileIndex (w : float) =
        let i = truncate (w / 10.0)
        if (i >= 0.0) && (i <= 7.0) && ( w >= 0.0) then  int i else -1

    [<ReflectedDefinition>]
    let getLinearDepth (ndc : V2d) =
        let tc = 0.5 * (ndc + V2d.II)
        let z = 2.0 * depth.Sample(tc, 0.0).X - 1.0

        let pp = V4d(ndc.X, ndc.Y, z, 1.0) 
        let temp = uniform.ProjTrafoInv * pp
        temp.Z / temp.W

    let  ssssBlur (v : Vertex) =   
        fragment {
            let index  = 
                albedoAndProfileIndex.Sample(v.tc).W
                |> extractProfileIndex
            let sampleM = inputImage.Sample(v.tc).XYZ

            let mutable blurred = V3d.OOO

            if index < 0 then
                blurred <- sampleM
            else
                let ndc = v.pos.XY / v.pos.W

                let deepM = getLinearDepth ndc

                // Calculate the sssWidth scale (1.0 for a unit plane sitting on the
                // projection window):
                let distanceToProjectionWindow = 1.0 / tan (0.5 * uniform.camFoVy)
                let scale = distanceToProjectionWindow / deepM

                let dir = if uniform.horizontal then V2d.OI else  V2d.IO
                let step = uniform.sssWidth.[index] * scale  * dir * 0.7 / uniform.kernelRange

                let mutable sumWeights = V3d.OOO

                let samples = uniform.Samples
                let  kernelBase = index * samples
                for si in kernelBase .. kernelBase + samples - 1 do
                    let offset = uniform.kernel.[si].W * step
                    let samplePos  = v.tc  + offset
                    let sampleColor = inputImage.Sample(samplePos).XYZ

                    let weight = uniform.kernel.[si].XYZ

                    blurred <- blurred + sampleColor * weight
                    sumWeights <- sumWeights + weight
                if sumWeights = V3d.OOO then
                    blurred <- V3d.IOO
                else
                    blurred <- blurred / sumWeights

            return V4d(blurred,1.0)
        } 

module subSurface =

    let defaultProfile = {
        Strength = C3d(0.48, 0.41, 0.28)
        Width = 0.005
        Falloff = C3d(1.0, 0.37, 0.3)
        Name = "Default Skin"
    }

    let gaussian (variance : float) (r : float) (falloff : V3d) =
        let rr = r / (falloff + 0.001)
        exp -(rr*rr) / (2.0  * variance) / (2.0 * Constant.Pi * variance)
    
    let profile (r : float) (falloff : V3d)= 
        0.100 * gaussian 0.0484 r falloff +
        0.118 * gaussian 0.187  r falloff +
        0.113 * gaussian 0.567  r falloff +
        0.358 * gaussian 1.99   r falloff +
        0.078 * gaussian 7.41   r falloff   

    let kernel (samples : int) (strength : V3d) (falloff : V3d) =
        let range  = 2.0
        let exponent = 2.0

        let step = range / float(samples/2)


        let calcOffset i = 
            let o  = -range + float(i) * step
            let s = if o <  0.0 then -1.0 else 1.0
            range * s * (abs (pow o exponent)) / (pow range exponent)

        let offsets = Array.init samples calcOffset

        let calcWeights i =
            let w0 = if i < 0 then abs (offsets.[i] - offsets.[i-1]) else  0.0
            let w1 = if i < samples - 1 then abs (offsets.[i] - offsets.[i+1]) else  0.0
            let area = (w0+w1) / 2.0
            let weight = area * profile (offsets.[i]) falloff
            V4d(weight, offsets.[i])

        let weights = Array.init samples calcWeights

        //reorder so that the element with offset 0 comes first
        let reorder i =
            let i' = if  i = 0 then samples/2 else if  i <= samples/2 then  i-1 else i
            weights.[i']

        let weights' = Array.init samples reorder

        let sumWeights = Array.sumBy (fun (v : V4d) -> v.XYZ) weights'
        let normalized = Array.map (fun (v : V4d) -> V4d(v.XYZ / sumWeights,v.W)) weights'

        let applayStrength i (v : V4d) =
            let weight =
                if i = 0 then 
                    (V3d.III - strength) + strength * v.XYZ
                else
                    strength * v.XYZ
            V4d(weight,v.W)

        Array.mapi applayStrength normalized

    let kernel' (samples : int) (strength : C3d) (falloff : C3d) =
        kernel  samples (strength.ToV3d())  (falloff.ToV3d()) 

    let makeKernelBuffer (profiles : amap<int,AdaptiveSssProfile>) =
        let empty = Array.init 25 (fun _ -> V4d.OOOO)
        let ps =
            profiles
            |> AMap.mapA (fun _ p -> AVal.map2 (kernel' 25) p.Strength p.Falloff)
        let mapper i = 
            adaptive  {
                let! c = AMap.tryFind i ps
                return  Option.defaultValue empty c
            }
        AList.init (AVal.constant 8) id
        |> AList.mapA mapper
        |> AList.fold Array.append [||]

    let makeWidthBuffer (profiles : amap<int,AdaptiveSssProfile>) =
        let empty = AVal.constant 0.0
        let mapper i = 
            adaptive  {
                let! c = AMap.tryFind i profiles
                let c' = Option.map (fun (p :AdaptiveSssProfile) -> p.Width) c
                return!  Option.defaultValue empty c'
            }
        AList.init (AVal.constant 8) id
        |> AList.mapA mapper
        |> AList.map Array.singleton
        |> AList.fold Array.append [||]
        
    //Render-Task for the screen-space Abient Occlusion pass
    let makeSubSurfaceScatttering (runtime : IRuntime) (size : aval<V2i>) camFoVy view proj gBuffer input (profiles : amap<int,AdaptiveSssProfile>) =

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba32f
            ]
        
        let kernelBuffer = makeKernelBuffer profiles
        let widthBuffer = makeWidthBuffer profiles
        
        let blurr h i = 
            Sg.fullScreenQuad
            |> Sg.adapter
            |> Sg.blendMode (AVal.constant (BlendMode(false)))
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj
            |> Sg.texture ( Sym.ofString "inputImage")  i
            |> Sg.texture ( DefaultSemantic.Depth) (Map.find DefaultSemantic.Depth gBuffer)
            |> Sg.texture ( DefaultSemantic.Colors) (Map.find DefaultSemantic.Colors gBuffer)
            |> Sg.uniform "horizontal" (AVal.constant h)
            |> Sg.uniform "sssWidth"  widthBuffer
            |> Sg.uniform "camFoVy" camFoVy
            |> Sg.uniform "kernel" kernelBuffer
            |> Sg.shader {
                do! subSurfaceShader.ssssBlur
                }    
            |> Sg.compile runtime signature

        let r1 = blurr true input |> RenderTask.renderToColor  size
        let r2 = blurr false r1 |> RenderTask.renderToColor  size

        r2

module sssProfile =

    type Message =
        |SetName of string
        |SetWidth of float
        |SetStrength of C3d
        |SetFallOff of C3d
    
    let update (m : SssProfile) (msg : Message) =
        match msg with
        |SetName n -> {m with Name = n} 
        |SetWidth w -> {m with Width = w} 
        |SetStrength s -> {m with Strength = s} 
        |SetFallOff f -> {m with Falloff = f} 

    let view (m : AdaptiveSssProfile) =
        let numInput name changed state = labeledFloatInput name 0.0 0.1 0.001 changed state
        Html.table [ 
            tr [] [ td [attribute "colspan" "2"] [Html.SemUi.textBox m.Name SetName] ]                          
            tr [] [ td [] [text "Width"]; td [] [numInput "Width" SetWidth m.Width]]
            tr [] [ td [] [text "Strength"]; td [] [ColorPicker.viewSimple (AVal.map (fun (c: C3d) ->c.ToC4b()) m.Strength ) (C3d.FromC4b >> SetStrength)]]
            tr [] [ td [] [text "Falloff"]; td [] [ColorPicker.viewSimple (AVal.map (fun (c: C3d) ->c.ToC4b()) m.Falloff ) (C3d.FromC4b >> SetFallOff)]]
         ] 

module sssProfiles =
    type Message =
        |AddProfile
        |ChangeProfile of int * sssProfile.Message
    
    let update (m : HashMap<int, SssProfile>) (msg : Message) =
        match msg with
        |AddProfile -> 
            let i = 
                if HashMap.isEmpty m then
                    1
                else
                    HashMap.keys m
                    |> Seq.max
                    |> max 0
                    |> (+) 1
            if i > 8 then 
                m
            else
                HashMap.add i subSurface.defaultProfile m
        |ChangeProfile (i, pms) -> 
            let pi' = HashMap.tryFind i m 
            match  pi' with 
            |Some pi ->
                let p = sssProfile.update pi pms
                HashMap.update i (fun _ -> p ) m
            |None ->  m

    let view (m : amap<int,AdaptiveSssProfile>) =
        let buttonAdd =
            AMap.count m
            |> AVal.map (fun c -> 
                if  c < 8 then 
                    [button [clazz "ui button"; onClick (fun () -> AddProfile); style "margin-bottom: 5px; width: 100%;" ]  [text "AddProfile"]]
                else [])
            |> AList.ofAVal   
        
        let profileList =
            m
            |> AMap.fold 
                ( fun items i p -> 
                    let d = 
                            sssProfile.view p |> UI.map (fun msg -> ChangeProfile (i, msg)) 
                            |> AList.single
                            |> Incremental.div AttributeMap.empty
                    
                    let item = 
                        sprintf "Profile %i" i, [
                            d
                            ]
                    item::items
                ) []
             //feed that into a accordeon
            |> AVal.map (Html.SemUi.accordionMenu true "ui vertical inverted fluid accordion menu" >> IndexList.single)
            // and that  into  a incremantal div to handel the case that the numbers of lights change
            |> AList.ofAVal

        let domList = AList.append profileList buttonAdd

        Incremental.div AttributeMap.empty domList
                    