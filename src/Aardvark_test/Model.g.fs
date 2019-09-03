namespace Aardvark_test.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark_test.Model

[<AutoOpen>]
module Mutable =

    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MLight() =
        abstract member TryUpdate : Aardvark_test.Model.Light -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Aardvark_test.Model.Light) = 
            match __model with
                | DirectionalLight(item) -> MDirectionalLight(__model, item) :> MLight
                | PointLight(item) -> MPointLight(__model, item) :> MLight
        
        static member Create(v : Aardvark_test.Model.Light) =
            ResetMod.Create(MLight.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MLight>, v : Aardvark_test.Model.Light) =
            let m = unbox<ResetMod<MLight>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MLight.CreateValue v)
    
    and private MDirectionalLight(__initial : Aardvark_test.Model.Light, item : Aardvark_test.Model.DirectionalLightData) =
        inherit MLight()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark_test.Model.Light) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | DirectionalLight(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MPointLight(__initial : Aardvark_test.Model.Light, item : Aardvark_test.Model.PointLightData) =
        inherit MLight()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark_test.Model.Light) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | PointLight(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MLightPatterns =
        let (|MDirectionalLight|MPointLight|) (m : MLight) =
            match m with
            | :? MDirectionalLight as v -> MDirectionalLight(v.item)
            | :? MPointLight as v -> MPointLight(v.item)
            | _ -> failwith "impossible"
    
    
    
    
    
    
    type MModel(__initial : Aardvark_test.Model.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.Model> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.Model>
        let _cameraState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraState)
        let _light = MLight.Create(__initial.light)
        
        member x.cameraState = _cameraState
        member x.light = _light
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                MLight.Update(_light, v.light)
                
        
        static member Create(__initial : Aardvark_test.Model.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : Aardvark_test.Model.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Aardvark_test.Model.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let cameraState =
                { new Lens<Aardvark_test.Model.Model, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraState
                    override x.Set(r,v) = { r with cameraState = v }
                    override x.Update(r,f) = { r with cameraState = f r.cameraState }
                }
            let light =
                { new Lens<Aardvark_test.Model.Model, Aardvark_test.Model.Light>() with
                    override x.Get(r) = r.light
                    override x.Set(r,v) = { r with light = v }
                    override x.Update(r,f) = { r with light = f r.light }
                }
