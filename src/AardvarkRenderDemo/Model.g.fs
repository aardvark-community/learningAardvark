namespace SLEAardvarkRenderDemo.Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open SLEAardvarkRenderDemo.Model

[<AutoOpen>]
module Mutable =

    
    
    type MTextureMappedValue(__initial : SLEAardvarkRenderDemo.Model.TextureMappedValue) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.TextureMappedValue> = Aardvark.Base.Incremental.EqModRef<SLEAardvarkRenderDemo.Model.TextureMappedValue>(__initial) :> Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.TextureMappedValue>
        let _fileName = MOption.Create(__initial.fileName)
        let _factor = ResetMod.Create(__initial.factor)
        
        member x.fileName = _fileName :> IMod<_>
        member x.factor = _factor :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : SLEAardvarkRenderDemo.Model.TextureMappedValue) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MOption.Update(_fileName, v.fileName)
                ResetMod.Update(_factor,v.factor)
                
        
        static member Create(__initial : SLEAardvarkRenderDemo.Model.TextureMappedValue) : MTextureMappedValue = MTextureMappedValue(__initial)
        static member Update(m : MTextureMappedValue, v : SLEAardvarkRenderDemo.Model.TextureMappedValue) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<SLEAardvarkRenderDemo.Model.TextureMappedValue> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TextureMappedValue =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let fileName =
                { new Lens<SLEAardvarkRenderDemo.Model.TextureMappedValue, Microsoft.FSharp.Core.Option<System.String>>() with
                    override x.Get(r) = r.fileName
                    override x.Set(r,v) = { r with fileName = v }
                    override x.Update(r,f) = { r with fileName = f r.fileName }
                }
            let factor =
                { new Lens<SLEAardvarkRenderDemo.Model.TextureMappedValue, System.Double>() with
                    override x.Get(r) = r.factor
                    override x.Set(r,v) = { r with factor = v }
                    override x.Update(r,f) = { r with factor = f r.factor }
                }
    
    
    type MPBRMaterial(__initial : SLEAardvarkRenderDemo.Model.PBRMaterial) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.PBRMaterial> = Aardvark.Base.Incremental.EqModRef<SLEAardvarkRenderDemo.Model.PBRMaterial>(__initial) :> Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.PBRMaterial>
        let _metallic = MTextureMappedValue.Create(__initial.metallic)
        let _roughness = MTextureMappedValue.Create(__initial.roughness)
        let _albedo = MTextureMappedValue.Create(__initial.albedo)
        let _normal = MTextureMappedValue.Create(__initial.normal)
        let _discard = ResetMod.Create(__initial.discard)
        let _displacment = MTextureMappedValue.Create(__initial.displacment)
        
        member x.metallic = _metallic
        member x.roughness = _roughness
        member x.albedo = _albedo
        member x.normal = _normal
        member x.discard = _discard :> IMod<_>
        member x.displacment = _displacment
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : SLEAardvarkRenderDemo.Model.PBRMaterial) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MTextureMappedValue.Update(_metallic, v.metallic)
                MTextureMappedValue.Update(_roughness, v.roughness)
                MTextureMappedValue.Update(_albedo, v.albedo)
                MTextureMappedValue.Update(_normal, v.normal)
                ResetMod.Update(_discard,v.discard)
                MTextureMappedValue.Update(_displacment, v.displacment)
                
        
        static member Create(__initial : SLEAardvarkRenderDemo.Model.PBRMaterial) : MPBRMaterial = MPBRMaterial(__initial)
        static member Update(m : MPBRMaterial, v : SLEAardvarkRenderDemo.Model.PBRMaterial) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<SLEAardvarkRenderDemo.Model.PBRMaterial> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PBRMaterial =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let metallic =
                { new Lens<SLEAardvarkRenderDemo.Model.PBRMaterial, SLEAardvarkRenderDemo.Model.TextureMappedValue>() with
                    override x.Get(r) = r.metallic
                    override x.Set(r,v) = { r with metallic = v }
                    override x.Update(r,f) = { r with metallic = f r.metallic }
                }
            let roughness =
                { new Lens<SLEAardvarkRenderDemo.Model.PBRMaterial, SLEAardvarkRenderDemo.Model.TextureMappedValue>() with
                    override x.Get(r) = r.roughness
                    override x.Set(r,v) = { r with roughness = v }
                    override x.Update(r,f) = { r with roughness = f r.roughness }
                }
            let albedo =
                { new Lens<SLEAardvarkRenderDemo.Model.PBRMaterial, SLEAardvarkRenderDemo.Model.TextureMappedValue>() with
                    override x.Get(r) = r.albedo
                    override x.Set(r,v) = { r with albedo = v }
                    override x.Update(r,f) = { r with albedo = f r.albedo }
                }
            let normal =
                { new Lens<SLEAardvarkRenderDemo.Model.PBRMaterial, SLEAardvarkRenderDemo.Model.TextureMappedValue>() with
                    override x.Get(r) = r.normal
                    override x.Set(r,v) = { r with normal = v }
                    override x.Update(r,f) = { r with normal = f r.normal }
                }
            let discard =
                { new Lens<SLEAardvarkRenderDemo.Model.PBRMaterial, System.Boolean>() with
                    override x.Get(r) = r.discard
                    override x.Set(r,v) = { r with discard = v }
                    override x.Update(r,f) = { r with discard = f r.discard }
                }
            let displacment =
                { new Lens<SLEAardvarkRenderDemo.Model.PBRMaterial, SLEAardvarkRenderDemo.Model.TextureMappedValue>() with
                    override x.Get(r) = r.displacment
                    override x.Set(r,v) = { r with displacment = v }
                    override x.Update(r,f) = { r with displacment = f r.displacment }
                }
    
    
    type MSceneObject(__initial : SLEAardvarkRenderDemo.Model.SceneObject) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.SceneObject> = Aardvark.Base.Incremental.EqModRef<SLEAardvarkRenderDemo.Model.SceneObject>(__initial) :> Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.SceneObject>
        let _name = ResetMod.Create(__initial.name)
        let _file = ResetMod.Create(__initial.file)
        let _scale = ResetMod.Create(__initial.scale)
        let _translation = ResetMod.Create(__initial.translation)
        let _rotation = ResetMod.Create(__initial.rotation)
        let _materials = MMap.Create(__initial.materials, (fun v -> MPBRMaterial.Create(v)), (fun (m,v) -> MPBRMaterial.Update(m, v)), (fun v -> v))
        let _currentMaterial = ResetMod.Create(__initial.currentMaterial)
        
        member x.name = _name :> IMod<_>
        member x.file = _file :> IMod<_>
        member x.scale = _scale :> IMod<_>
        member x.translation = _translation :> IMod<_>
        member x.rotation = _rotation :> IMod<_>
        member x.materials = _materials :> amap<_,_>
        member x.currentMaterial = _currentMaterial :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : SLEAardvarkRenderDemo.Model.SceneObject) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_file,v.file)
                ResetMod.Update(_scale,v.scale)
                ResetMod.Update(_translation,v.translation)
                ResetMod.Update(_rotation,v.rotation)
                MMap.Update(_materials, v.materials)
                ResetMod.Update(_currentMaterial,v.currentMaterial)
                
        
        static member Create(__initial : SLEAardvarkRenderDemo.Model.SceneObject) : MSceneObject = MSceneObject(__initial)
        static member Update(m : MSceneObject, v : SLEAardvarkRenderDemo.Model.SceneObject) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<SLEAardvarkRenderDemo.Model.SceneObject> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SceneObject =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let name =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let file =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, System.String>() with
                    override x.Get(r) = r.file
                    override x.Set(r,v) = { r with file = v }
                    override x.Update(r,f) = { r with file = f r.file }
                }
            let scale =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, System.Double>() with
                    override x.Get(r) = r.scale
                    override x.Set(r,v) = { r with scale = v }
                    override x.Update(r,f) = { r with scale = f r.scale }
                }
            let translation =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.translation
                    override x.Set(r,v) = { r with translation = v }
                    override x.Update(r,f) = { r with translation = f r.translation }
                }
            let rotation =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.rotation
                    override x.Set(r,v) = { r with rotation = v }
                    override x.Update(r,f) = { r with rotation = f r.rotation }
                }
            let materials =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, Aardvark.Base.hmap<System.String,SLEAardvarkRenderDemo.Model.PBRMaterial>>() with
                    override x.Get(r) = r.materials
                    override x.Set(r,v) = { r with materials = v }
                    override x.Update(r,f) = { r with materials = f r.materials }
                }
            let currentMaterial =
                { new Lens<SLEAardvarkRenderDemo.Model.SceneObject, System.String>() with
                    override x.Get(r) = r.currentMaterial
                    override x.Set(r,v) = { r with currentMaterial = v }
                    override x.Update(r,f) = { r with currentMaterial = f r.currentMaterial }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MLight() =
        abstract member TryUpdate : SLEAardvarkRenderDemo.Model.Light -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : SLEAardvarkRenderDemo.Model.Light) = 
            match __model with
                | DirectionalLight(item) -> MDirectionalLight(__model, item) :> MLight
                | PointLight(item) -> MPointLight(__model, item) :> MLight
        
        static member Create(v : SLEAardvarkRenderDemo.Model.Light) =
            ResetMod.Create(MLight.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MLight>, v : SLEAardvarkRenderDemo.Model.Light) =
            let m = unbox<ResetMod<MLight>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MLight.CreateValue v)
    
    and private MDirectionalLight(__initial : SLEAardvarkRenderDemo.Model.Light, item : SLEAardvarkRenderDemo.Model.DirectionalLightData) =
        inherit MLight()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : SLEAardvarkRenderDemo.Model.Light) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | DirectionalLight(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MPointLight(__initial : SLEAardvarkRenderDemo.Model.Light, item : SLEAardvarkRenderDemo.Model.PointLightData) =
        inherit MLight()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : SLEAardvarkRenderDemo.Model.Light) = 
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
    
    
    
    
    
    
    type MAmbientOcclusionSettings(__initial : SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings> = Aardvark.Base.Incremental.EqModRef<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings>(__initial) :> Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings>
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
        member x.Update(v : SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_occlusionStrength,v.occlusionStrength)
                ResetMod.Update(_scale,v.scale)
                ResetMod.Update(_radius,v.radius)
                ResetMod.Update(_samples,v.samples)
                ResetMod.Update(_threshold,v.threshold)
                ResetMod.Update(_sigma,v.sigma)
                ResetMod.Update(_sharpness,v.sharpness)
                
        
        static member Create(__initial : SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings) : MAmbientOcclusionSettings = MAmbientOcclusionSettings(__initial)
        static member Update(m : MAmbientOcclusionSettings, v : SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AmbientOcclusionSettings =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let occlusionStrength =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.occlusionStrength
                    override x.Set(r,v) = { r with occlusionStrength = v }
                    override x.Update(r,f) = { r with occlusionStrength = f r.occlusionStrength }
                }
            let scale =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.scale
                    override x.Set(r,v) = { r with scale = v }
                    override x.Update(r,f) = { r with scale = f r.scale }
                }
            let radius =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.radius
                    override x.Set(r,v) = { r with radius = v }
                    override x.Update(r,f) = { r with radius = f r.radius }
                }
            let samples =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Int32>() with
                    override x.Get(r) = r.samples
                    override x.Set(r,v) = { r with samples = v }
                    override x.Update(r,f) = { r with samples = f r.samples }
                }
            let threshold =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.threshold
                    override x.Set(r,v) = { r with threshold = v }
                    override x.Update(r,f) = { r with threshold = f r.threshold }
                }
            let sigma =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.sigma
                    override x.Set(r,v) = { r with sigma = v }
                    override x.Update(r,f) = { r with sigma = f r.sigma }
                }
            let sharpness =
                { new Lens<SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings, System.Double>() with
                    override x.Get(r) = r.sharpness
                    override x.Set(r,v) = { r with sharpness = v }
                    override x.Update(r,f) = { r with sharpness = f r.sharpness }
                }
    
    
    type MGlobalEnviorment(__initial : SLEAardvarkRenderDemo.Model.GlobalEnviorment) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.GlobalEnviorment> = Aardvark.Base.Incremental.EqModRef<SLEAardvarkRenderDemo.Model.GlobalEnviorment>(__initial) :> Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.GlobalEnviorment>
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
        member x.Update(v : SLEAardvarkRenderDemo.Model.GlobalEnviorment) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_skyMap,v.skyMap)
                ResetMod.Update(_skyMapRotation,v.skyMapRotation)
                ResetMod.Update(_skyMapIntensity,v.skyMapIntensity)
                ResetMod.Update(_ambientLightIntensity,v.ambientLightIntensity)
                MAmbientOcclusionSettings.Update(_occlusionSettings, v.occlusionSettings)
                
        
        static member Create(__initial : SLEAardvarkRenderDemo.Model.GlobalEnviorment) : MGlobalEnviorment = MGlobalEnviorment(__initial)
        static member Update(m : MGlobalEnviorment, v : SLEAardvarkRenderDemo.Model.GlobalEnviorment) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<SLEAardvarkRenderDemo.Model.GlobalEnviorment> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GlobalEnviorment =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let skyMap =
                { new Lens<SLEAardvarkRenderDemo.Model.GlobalEnviorment, System.String>() with
                    override x.Get(r) = r.skyMap
                    override x.Set(r,v) = { r with skyMap = v }
                    override x.Update(r,f) = { r with skyMap = f r.skyMap }
                }
            let skyMapRotation =
                { new Lens<SLEAardvarkRenderDemo.Model.GlobalEnviorment, System.Double>() with
                    override x.Get(r) = r.skyMapRotation
                    override x.Set(r,v) = { r with skyMapRotation = v }
                    override x.Update(r,f) = { r with skyMapRotation = f r.skyMapRotation }
                }
            let skyMapIntensity =
                { new Lens<SLEAardvarkRenderDemo.Model.GlobalEnviorment, System.Double>() with
                    override x.Get(r) = r.skyMapIntensity
                    override x.Set(r,v) = { r with skyMapIntensity = v }
                    override x.Update(r,f) = { r with skyMapIntensity = f r.skyMapIntensity }
                }
            let ambientLightIntensity =
                { new Lens<SLEAardvarkRenderDemo.Model.GlobalEnviorment, System.Double>() with
                    override x.Get(r) = r.ambientLightIntensity
                    override x.Set(r,v) = { r with ambientLightIntensity = v }
                    override x.Update(r,f) = { r with ambientLightIntensity = f r.ambientLightIntensity }
                }
            let occlusionSettings =
                { new Lens<SLEAardvarkRenderDemo.Model.GlobalEnviorment, SLEAardvarkRenderDemo.Model.AmbientOcclusionSettings>() with
                    override x.Get(r) = r.occlusionSettings
                    override x.Set(r,v) = { r with occlusionSettings = v }
                    override x.Update(r,f) = { r with occlusionSettings = f r.occlusionSettings }
                }
    
    
    type MModel(__initial : SLEAardvarkRenderDemo.Model.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.Model> = Aardvark.Base.Incremental.EqModRef<SLEAardvarkRenderDemo.Model.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<SLEAardvarkRenderDemo.Model.Model>
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
        member x.Update(v : SLEAardvarkRenderDemo.Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                MMap.Update(_lights, v.lights)
                MGlobalEnviorment.Update(_enviorment, v.enviorment)
                ResetMod.Update(_expousure,v.expousure)
                MMap.Update(_objects, v.objects)
                ResetMod.Update(_selectedObject,v.selectedObject)
                
        
        static member Create(__initial : SLEAardvarkRenderDemo.Model.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : SLEAardvarkRenderDemo.Model.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<SLEAardvarkRenderDemo.Model.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let cameraState =
                { new Lens<SLEAardvarkRenderDemo.Model.Model, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraState
                    override x.Set(r,v) = { r with cameraState = v }
                    override x.Update(r,f) = { r with cameraState = f r.cameraState }
                }
            let lights =
                { new Lens<SLEAardvarkRenderDemo.Model.Model, Aardvark.Base.hmap<System.Int32,SLEAardvarkRenderDemo.Model.Light>>() with
                    override x.Get(r) = r.lights
                    override x.Set(r,v) = { r with lights = v }
                    override x.Update(r,f) = { r with lights = f r.lights }
                }
            let enviorment =
                { new Lens<SLEAardvarkRenderDemo.Model.Model, SLEAardvarkRenderDemo.Model.GlobalEnviorment>() with
                    override x.Get(r) = r.enviorment
                    override x.Set(r,v) = { r with enviorment = v }
                    override x.Update(r,f) = { r with enviorment = f r.enviorment }
                }
            let expousure =
                { new Lens<SLEAardvarkRenderDemo.Model.Model, System.Double>() with
                    override x.Get(r) = r.expousure
                    override x.Set(r,v) = { r with expousure = v }
                    override x.Update(r,f) = { r with expousure = f r.expousure }
                }
            let objects =
                { new Lens<SLEAardvarkRenderDemo.Model.Model, Aardvark.Base.hmap<System.String,SLEAardvarkRenderDemo.Model.SceneObject>>() with
                    override x.Get(r) = r.objects
                    override x.Set(r,v) = { r with objects = v }
                    override x.Update(r,f) = { r with objects = f r.objects }
                }
            let selectedObject =
                { new Lens<SLEAardvarkRenderDemo.Model.Model, System.String>() with
                    override x.Get(r) = r.selectedObject
                    override x.Set(r,v) = { r with selectedObject = v }
                    override x.Update(r,f) = { r with selectedObject = f r.selectedObject }
                }
