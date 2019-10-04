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
    
    
    
    
    
    
    type MPBRMaterial(__initial : Aardvark_test.Model.PBRMaterial) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.PBRMaterial> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.PBRMaterial>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.PBRMaterial>
        let _metallic = ResetMod.Create(__initial.metallic)
        let _roughness = ResetMod.Create(__initial.roughness)
        let _albedoFactor = ResetMod.Create(__initial.albedoFactor)
        
        member x.metallic = _metallic :> IMod<_>
        member x.roughness = _roughness :> IMod<_>
        member x.albedoFactor = _albedoFactor :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.PBRMaterial) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_metallic,v.metallic)
                ResetMod.Update(_roughness,v.roughness)
                ResetMod.Update(_albedoFactor,v.albedoFactor)
                
        
        static member Create(__initial : Aardvark_test.Model.PBRMaterial) : MPBRMaterial = MPBRMaterial(__initial)
        static member Update(m : MPBRMaterial, v : Aardvark_test.Model.PBRMaterial) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Aardvark_test.Model.PBRMaterial> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PBRMaterial =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let metallic =
                { new Lens<Aardvark_test.Model.PBRMaterial, System.Double>() with
                    override x.Get(r) = r.metallic
                    override x.Set(r,v) = { r with metallic = v }
                    override x.Update(r,f) = { r with metallic = f r.metallic }
                }
            let roughness =
                { new Lens<Aardvark_test.Model.PBRMaterial, System.Double>() with
                    override x.Get(r) = r.roughness
                    override x.Set(r,v) = { r with roughness = v }
                    override x.Update(r,f) = { r with roughness = f r.roughness }
                }
            let albedoFactor =
                { new Lens<Aardvark_test.Model.PBRMaterial, System.Double>() with
                    override x.Get(r) = r.albedoFactor
                    override x.Set(r,v) = { r with albedoFactor = v }
                    override x.Update(r,f) = { r with albedoFactor = f r.albedoFactor }
                }
    
    
    type MGlobalEnviorment(__initial : Aardvark_test.Model.GlobalEnviorment) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.GlobalEnviorment> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.GlobalEnviorment>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.GlobalEnviorment>
        let _skyMap = ResetMod.Create(__initial.skyMap)
        let _skyMapRotation = ResetMod.Create(__initial.skyMapRotation)
        let _skyMapIntensity = ResetMod.Create(__initial.skyMapIntensity)
        let _ambientLightIntensity = ResetMod.Create(__initial.ambientLightIntensity)
        
        member x.skyMap = _skyMap :> IMod<_>
        member x.skyMapRotation = _skyMapRotation :> IMod<_>
        member x.skyMapIntensity = _skyMapIntensity :> IMod<_>
        member x.ambientLightIntensity = _ambientLightIntensity :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.GlobalEnviorment) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_skyMap,v.skyMap)
                ResetMod.Update(_skyMapRotation,v.skyMapRotation)
                ResetMod.Update(_skyMapIntensity,v.skyMapIntensity)
                ResetMod.Update(_ambientLightIntensity,v.ambientLightIntensity)
                
        
        static member Create(__initial : Aardvark_test.Model.GlobalEnviorment) : MGlobalEnviorment = MGlobalEnviorment(__initial)
        static member Update(m : MGlobalEnviorment, v : Aardvark_test.Model.GlobalEnviorment) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Aardvark_test.Model.GlobalEnviorment> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GlobalEnviorment =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let skyMap =
                { new Lens<Aardvark_test.Model.GlobalEnviorment, System.String>() with
                    override x.Get(r) = r.skyMap
                    override x.Set(r,v) = { r with skyMap = v }
                    override x.Update(r,f) = { r with skyMap = f r.skyMap }
                }
            let skyMapRotation =
                { new Lens<Aardvark_test.Model.GlobalEnviorment, System.Double>() with
                    override x.Get(r) = r.skyMapRotation
                    override x.Set(r,v) = { r with skyMapRotation = v }
                    override x.Update(r,f) = { r with skyMapRotation = f r.skyMapRotation }
                }
            let skyMapIntensity =
                { new Lens<Aardvark_test.Model.GlobalEnviorment, System.Double>() with
                    override x.Get(r) = r.skyMapIntensity
                    override x.Set(r,v) = { r with skyMapIntensity = v }
                    override x.Update(r,f) = { r with skyMapIntensity = f r.skyMapIntensity }
                }
            let ambientLightIntensity =
                { new Lens<Aardvark_test.Model.GlobalEnviorment, System.Double>() with
                    override x.Get(r) = r.ambientLightIntensity
                    override x.Set(r,v) = { r with ambientLightIntensity = v }
                    override x.Update(r,f) = { r with ambientLightIntensity = f r.ambientLightIntensity }
                }
    
    
    type MModel(__initial : Aardvark_test.Model.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.Model> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.Model>
        let _cameraState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraState)
        let _lights = MMap.Create(__initial.lights, (fun v -> MLight.Create(v)), (fun (m,v) -> MLight.Update(m, v)), (fun v -> v))
        let _material = MPBRMaterial.Create(__initial.material)
        let _enviorment = MGlobalEnviorment.Create(__initial.enviorment)
        let _expousure = ResetMod.Create(__initial.expousure)
        
        member x.cameraState = _cameraState
        member x.lights = _lights :> amap<_,_>
        member x.material = _material
        member x.enviorment = _enviorment
        member x.expousure = _expousure :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                MMap.Update(_lights, v.lights)
                MPBRMaterial.Update(_material, v.material)
                MGlobalEnviorment.Update(_enviorment, v.enviorment)
                ResetMod.Update(_expousure,v.expousure)
                
        
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
            let lights =
                { new Lens<Aardvark_test.Model.Model, Aardvark.Base.hmap<System.Int32,Aardvark_test.Model.Light>>() with
                    override x.Get(r) = r.lights
                    override x.Set(r,v) = { r with lights = v }
                    override x.Update(r,f) = { r with lights = f r.lights }
                }
            let material =
                { new Lens<Aardvark_test.Model.Model, Aardvark_test.Model.PBRMaterial>() with
                    override x.Get(r) = r.material
                    override x.Set(r,v) = { r with material = v }
                    override x.Update(r,f) = { r with material = f r.material }
                }
            let enviorment =
                { new Lens<Aardvark_test.Model.Model, Aardvark_test.Model.GlobalEnviorment>() with
                    override x.Get(r) = r.enviorment
                    override x.Set(r,v) = { r with enviorment = v }
                    override x.Update(r,f) = { r with enviorment = f r.enviorment }
                }
            let expousure =
                { new Lens<Aardvark_test.Model.Model, System.Double>() with
                    override x.Get(r) = r.expousure
                    override x.Set(r,v) = { r with expousure = v }
                    override x.Update(r,f) = { r with expousure = f r.expousure }
                }
