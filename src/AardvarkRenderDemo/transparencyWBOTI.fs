namespace SLEAardvarkRenderDemo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open Aardvark.Base.Rendering.Effects
open System
(*
    Colored Weighed Blended Order Independent Trransparency
    http://casual-effects.blogspot.com/2015/03/colored-blended-order-independent.html

    with additional Featurs from  
    http://casual-effects.com/research/McGuire2016Transparency/index.html
*)

module WBOTI =

    type FragmentOut = {
        [<Color>]                 Color     : V4d
        [<Semantic("Accum")>]     Accum     : V4d
        [<Semantic("Revealage")>] Revealage : V4d
       }