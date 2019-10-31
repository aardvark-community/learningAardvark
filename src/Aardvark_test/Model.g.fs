namespace Aardvark_test.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark_test.Model

[<AutoOpen>]
module Mutable =

    
    
    type MPBRMaterial(__initial : Aardvark_test.Model.PBRMaterial) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.PBRMaterial> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.PBRMaterial>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.PBRMaterial>
        let _metallic = ResetMod.Create(__initial.metallic)
        let _roughness = ResetMod.Create(__initial.roughness)
        let _albedoFactor = ResetMod.Create(__initial.albedoFactor)
        let _normalMapStrenght = ResetMod.Create(__initial.normalMapStrenght)
        let _discard = ResetMod.Create(__initial.discard)
        let _displacmentMap = ResetMod.Create(__initial.displacmentMap)
        let _displacmentStrength = ResetMod.Create(__initial.displacmentStrength)
        
        member x.metallic = _metallic :> IMod<_>
        member x.roughness = _roughness :> IMod<_>
        member x.albedoFactor = _albedoFactor :> IMod<_>
        member x.normalMapStrenght = _normalMapStrenght :> IMod<_>
        member x.discard = _discard :> IMod<_>
        member x.displacmentMap = _displacmentMap :> IMod<_>
        member x.displacmentStrength = _displacmentStrength :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.PBRMaterial) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_metallic,v.metallic)
                ResetMod.Update(_roughness,v.roughness)
                ResetMod.Update(_albedoFactor,v.albedoFactor)
                ResetMod.Update(_normalMapStrenght,v.normalMapStrenght)
                ResetMod.Update(_discard,v.discard)
                ResetMod.Update(_displacmentMap,v.displacmentMap)
                ResetMod.Update(_displacmentStrength,v.displacmentStrength)
                
        
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
            let normalMapStrenght =
                { new Lens<Aardvark_test.Model.PBRMaterial, System.Double>() with
                    override x.Get(r) = r.normalMapStrenght
                    override x.Set(r,v) = { r with normalMapStrenght = v }
                    override x.Update(r,f) = { r with normalMapStrenght = f r.normalMapStrenght }
                }
            let discard =
                { new Lens<Aardvark_test.Model.PBRMaterial, System.Boolean>() with
                    override x.Get(r) = r.discard
                    override x.Set(r,v) = { r with discard = v }
                    override x.Update(r,f) = { r with discard = f r.discard }
                }
            let displacmentMap =
                { new Lens<Aardvark_test.Model.PBRMaterial, Aardvark.Base.ITexture>() with
                    override x.Get(r) = r.displacmentMap
                    override x.Set(r,v) = { r with displacmentMap = v }
                    override x.Update(r,f) = { r with displacmentMap = f r.displacmentMap }
                }
            let displacmentStrength =
                { new Lens<Aardvark_test.Model.PBRMaterial, System.Double>() with
                    override x.Get(r) = r.displacmentStrength
                    override x.Set(r,v) = { r with displacmentStrength = v }
                    override x.Update(r,f) = { r with displacmentStrength = f r.displacmentStrength }
                }
    
    
    type MSceneObject(__initial : Aardvark_test.Model.SceneObject) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.SceneObject> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.SceneObject>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.SceneObject>
        let _name = ResetMod.Create(__initial.name)
        let _file = ResetMod.Create(__initial.file)
        let _scale = ResetMod.Create(__initial.scale)
        let _translation = ResetMod.Create(__initial.translation)
        let _rotation = ResetMod.Create(__initial.rotation)
        let _materials = MMap.Create(__initial.materials, (fun v -> MPBRMaterial.Create(v)), (fun (m,v) -> MPBRMaterial.Update(m, v)), (fun v -> v))
        let _currentMaterial = ResetMod.Create(__initial.currentMaterial)
        let _object = ResetMod.Create(__initial.object)
        
        member x.name = _name :> IMod<_>
        member x.file = _file :> IMod<_>
        member x.scale = _scale :> IMod<_>
        member x.translation = _translation :> IMod<_>
        member x.rotation = _rotation :> IMod<_>
        member x.materials = _materials :> amap<_,_>
        member x.currentMaterial = _currentMaterial :> IMod<_>
        member x.object = _object :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.SceneObject) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_file,v.file)
                ResetMod.Update(_scale,v.scale)
                ResetMod.Update(_translation,v.translation)
                ResetMod.Update(_rotation,v.rotation)
                MMap.Update(_materials, v.materials)
                ResetMod.Update(_currentMaterial,v.currentMaterial)
                ResetMod.Update(_object,v.object)
                
        
        static member Create(__initial : Aardvark_test.Model.SceneObject) : MSceneObject = MSceneObject(__initial)
        static member Update(m : MSceneObject, v : Aardvark_test.Model.SceneObject) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Aardvark_test.Model.SceneObject> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SceneObject =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let name =
                { new Lens<Aardvark_test.Model.SceneObject, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let file =
                { new Lens<Aardvark_test.Model.SceneObject, System.String>() with
                    override x.Get(r) = r.file
                    override x.Set(r,v) = { r with file = v }
                    override x.Update(r,f) = { r with file = f r.file }
                }
            let scale =
                { new Lens<Aardvark_test.Model.SceneObject, System.Double>() with
                    override x.Get(r) = r.scale
                    override x.Set(r,v) = { r with scale = v }
                    override x.Update(r,f) = { r with scale = f r.scale }
                }
            let translation =
                { new Lens<Aardvark_test.Model.SceneObject, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.translation
                    override x.Set(r,v) = { r with translation = v }
                    override x.Update(r,f) = { r with translation = f r.translation }
                }
            let rotation =
                { new Lens<Aardvark_test.Model.SceneObject, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.rotation
                    override x.Set(r,v) = { r with rotation = v }
                    override x.Update(r,f) = { r with rotation = f r.rotation }
                }
            let materials =
                { new Lens<Aardvark_test.Model.SceneObject, Aardvark.Base.hmap<System.String,Aardvark_test.Model.PBRMaterial>>() with
                    override x.Get(r) = r.materials
                    override x.Set(r,v) = { r with materials = v }
                    override x.Update(r,f) = { r with materials = f r.materials }
                }
            let currentMaterial =
                { new Lens<Aardvark_test.Model.SceneObject, System.String>() with
                    override x.Get(r) = r.currentMaterial
                    override x.Set(r,v) = { r with currentMaterial = v }
                    override x.Update(r,f) = { r with currentMaterial = f r.currentMaterial }
                }
            let object =
                { new Lens<Aardvark_test.Model.SceneObject, Aardvark.SceneGraph.IO.Loader.Scene>() with
                    override x.Get(r) = r.object
                    override x.Set(r,v) = { r with object = v }
                    override x.Update(r,f) = { r with object = f r.object }
                }
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
    
    
    
    
    
    
    type MAmbientOcclusionSettings(__initial : Aardvark_test.Model.AmbientOcclusionSettings) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.AmbientOcclusionSettings> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.AmbientOcclusionSettings>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.AmbientOcclusionSettings>
        let _occlusionStrength = ResetMod.Create(__initial.occlusionStrength)
        let _scale = ResetMod.Create(__initial.scale)
        let _radius = ResetMod.Create(__initial.radius)
        let _samples = ResetMod.Create(__initial.samples)
        let _threshold = ResetMod.Create(__initial.threshold)
        let _sigma = ResetMod.Create(__initial.sigma)
        let _sharpness = ResetMod.Create(__initial.sharpness)
        
        member x.occlusionStrength = _occlusionStrength :> IMod<_>
        member x.scale = _scale :> IMod<_>
        member x.radius = _radius :> IMod<_>
        member x.samples = _samples :> IMod<_>
        member x.threshold = _threshold :> IMod<_>
        member x.sigma = _sigma :> IMod<_>
        member x.sharpness = _sharpness :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.AmbientOcclusionSettings) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_occlusionStrength,v.occlusionStrength)
                ResetMod.Update(_scale,v.scale)
                ResetMod.Update(_radius,v.radius)
                ResetMod.Update(_samples,v.samples)
                ResetMod.Update(_threshold,v.threshold)
                ResetMod.Update(_sigma,v.sigma)
                ResetMod.Update(_sharpness,v.sharpness)
                
        
        static member Create(__initial : Aardvark_test.Model.AmbientOcclusionSettings) : MAmbientOcclusionSettings = MAmbientOcclusionSettings(__initial)
        static member Update(m : MAmbientOcclusionSettings, v : Aardvark_test.Model.AmbientOcclusionSettings) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Aardvark_test.Model.AmbientOcclusionSettings> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AmbientOcclusionSettings =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let occlusionStrength =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.occlusionStrength
                    override x.Set(r,v) = { r with occlusionStrength = v }
                    override x.Update(r,f) = { r with occlusionStrength = f r.occlusionStrength }
                }
            let scale =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.scale
                    override x.Set(r,v) = { r with scale = v }
                    override x.Update(r,f) = { r with scale = f r.scale }
                }
            let radius =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.radius
                    override x.Set(r,v) = { r with radius = v }
                    override x.Update(r,f) = { r with radius = f r.radius }
                }
            let samples =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Int32>() with
                    override x.Get(r) = r.samples
                    override x.Set(r,v) = { r with samples = v }
                    override x.Update(r,f) = { r with samples = f r.samples }
                }
            let threshold =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.threshold
                    override x.Set(r,v) = { r with threshold = v }
                    override x.Update(r,f) = { r with threshold = f r.threshold }
                }
            let sigma =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.sigma
                    override x.Set(r,v) = { r with sigma = v }
                    override x.Update(r,f) = { r with sigma = f r.sigma }
                }
            let sharpness =
                { new Lens<Aardvark_test.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.sharpness
                    override x.Set(r,v) = { r with sharpness = v }
                    override x.Update(r,f) = { r with sharpness = f r.sharpness }
                }
    
    
    type MGlobalEnviorment(__initial : Aardvark_test.Model.GlobalEnviorment) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.GlobalEnviorment> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.GlobalEnviorment>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.GlobalEnviorment>
        let _skyMap = ResetMod.Create(__initial.skyMap)
        let _skyMapRotation = ResetMod.Create(__initial.skyMapRotation)
        let _skyMapIntensity = ResetMod.Create(__initial.skyMapIntensity)
        let _ambientLightIntensity = ResetMod.Create(__initial.ambientLightIntensity)
        let _occlusionSettings = MAmbientOcclusionSettings.Create(__initial.occlusionSettings)
        
        member x.skyMap = _skyMap :> IMod<_>
        member x.skyMapRotation = _skyMapRotation :> IMod<_>
        member x.skyMapIntensity = _skyMapIntensity :> IMod<_>
        member x.ambientLightIntensity = _ambientLightIntensity :> IMod<_>
        member x.occlusionSettings = _occlusionSettings
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.GlobalEnviorment) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_skyMap,v.skyMap)
                ResetMod.Update(_skyMapRotation,v.skyMapRotation)
                ResetMod.Update(_skyMapIntensity,v.skyMapIntensity)
                ResetMod.Update(_ambientLightIntensity,v.ambientLightIntensity)
                MAmbientOcclusionSettings.Update(_occlusionSettings, v.occlusionSettings)
                
        
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
            let occlusionSettings =
                { new Lens<Aardvark_test.Model.GlobalEnviorment, Aardvark_test.Model.AmbientOcclusionSettings>() with
                    override x.Get(r) = r.occlusionSettings
                    override x.Set(r,v) = { r with occlusionSettings = v }
                    override x.Update(r,f) = { r with occlusionSettings = f r.occlusionSettings }
                }
    
    
    type MModel(__initial : Aardvark_test.Model.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.Model> = Aardvark.Base.Incremental.EqModRef<Aardvark_test.Model.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Aardvark_test.Model.Model>
        let _cameraState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraState)
        let _lights = MMap.Create(__initial.lights, (fun v -> MLight.Create(v)), (fun (m,v) -> MLight.Update(m, v)), (fun v -> v))
        let _enviorment = MGlobalEnviorment.Create(__initial.enviorment)
        let _expousure = ResetMod.Create(__initial.expousure)
        let _objects = MMap.Create(__initial.objects, (fun v -> MSceneObject.Create(v)), (fun (m,v) -> MSceneObject.Update(m, v)), (fun v -> v))
        let _selectedObject = ResetMod.Create(__initial.selectedObject)
        
        member x.cameraState = _cameraState
        member x.lights = _lights :> amap<_,_>
        member x.enviorment = _enviorment
        member x.expousure = _expousure :> IMod<_>
        member x.objects = _objects :> amap<_,_>
        member x.selectedObject = _selectedObject :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Aardvark_test.Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                MMap.Update(_lights, v.lights)
                MGlobalEnviorment.Update(_enviorment, v.enviorment)
                ResetMod.Update(_expousure,v.expousure)
                MMap.Update(_objects, v.objects)
                ResetMod.Update(_selectedObject,v.selectedObject)
                
        
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
            let objects =
                { new Lens<Aardvark_test.Model.Model, Aardvark.Base.hmap<System.String,Aardvark_test.Model.SceneObject>>() with
                    override x.Get(r) = r.objects
                    override x.Set(r,v) = { r with objects = v }
                    override x.Update(r,f) = { r with objects = f r.objects }
                }
            let selectedObject =
                { new Lens<Aardvark_test.Model.Model, System.String>() with
                    override x.Get(r) = r.selectedObject
                    override x.Set(r,v) = { r with selectedObject = v }
                    override x.Update(r,f) = { r with selectedObject = f r.selectedObject }
                }
