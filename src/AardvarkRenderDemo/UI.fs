namespace Aardvark.UI
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Operators
open FSharp.Data.Adaptive
open System

(*
    some small UI components and bugfixes
*)
[<AutoOpen>]
module Simple =

    //a customized float input
    let floatInput (minValue : float) (maxValue : float) (step : float) (changed : float -> 'msg) (value : aval<float>) =
        labeledFloatInput' "" minValue maxValue step changed value (AttributeMap.ofList [ clazz "ui small input"; style "width: 60pt"]) (AttributeMap.ofList []) 

     // slider with linked input
    let inputSlider (cfg : SliderConfig<float>) (atts : (string * AttributeValue<'msg>) list )  (value : aval<float>) (update : float -> 'msg) =
        div atts [
            labeledFloatInput' "" cfg.min cfg.max (cfg.step*10.0) update value (AttributeMap.ofList [ clazz "ui small input"; style "width: 60pt; float: left"]) (AttributeMap.ofList []) 
            slider cfg (AttributeMap.ofList [style "width: auto; margin-left: 68pt"])  value update
        ]

    // a logaritmic slider with linked input
    let inputLogSlider (cfg : SliderConfig<float>) (atts : (string * AttributeValue<'msg>) list )  (value : aval<float>) (update : float -> 'msg) =
        if cfg.min <= 0.0 then failwith "min must be positve for log silder"
        let value' = AVal.map Math.Log10 value
        let update' = fun v -> update (Math.Pow(10.0,v))
        let cfg' = {cfg with  min = Math.Log10 cfg.min; max = Math.Log10 cfg.max}
        div atts [
            labeledFloatInput' "" cfg.min cfg.max (cfg.step*10.0) update value (AttributeMap.ofList [ clazz "ui small input"; style "width: 60pt; float: left"]) (AttributeMap.ofList []) 
            slider cfg' (AttributeMap.ofList [style "width: auto; margin-left: 68pt"])  value' update'
        ]

module  Html =   
    module SemUi =

        //some fixes to get nested accordionMenus working
        let accordionMenu (subMenue : bool) (c : string )(entries : list<string * list<DomNode<'msg>>>) =
            let acc = 
                div [ clazz c ] (
                    entries |> List.map (fun (name, children) ->
                        div [clazz "item"] [
                                a [ clazz "title"] [
                                    i [clazz"dropdown icon"] []
                                    text  name
                                ]
                                div  [clazz "content"] children
                            ]
                    )
                )
            if subMenue then acc else onBoot "$('#__ID__').accordion();"(acc)
 
        
        let subAccordion text' icon active content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"
            
            div [clazz "ui inverted segment"] [
                div [clazz "ui inverted accordion fluid"] [
                    div [clazz title] [
                            i [clazz (icon + " icon circular")][] 
                            text text'
                    ]
                    div [clazz content] content'
                ]
            ]

        let adornerAccordeonMenu (sectionsAndItems : list<string * list<DomNode<'msg>>>) (rest : list<DomNode<'msg>>) =
            let pushButton() = 
                div [
                    clazz "ui black big launch right attached fixed button menubutton"
                    js "onclick"        "$('.sidebar').sidebar('toggle');"
                    style "z-index:1"
                ] [
                    i [clazz "content icon"] [] 
                    span [clazz "text"] [text "Menu"]
                ]
            [
                yield 
                    div [clazz "pusher"] [
                        yield pushButton()                    
                        yield! rest                    
                    ]
                yield 
                     onBoot "$('#__ID__').sidebar('setting', 'dimPage', false);" (accordionMenu false "ui vertical inverted sidebar very wide accordion menu" sectionsAndItems)
            ] 

        //Bugfix:toggle Box needs to set the attribute "checked" to the  value "checked" to work correctly
        let toggleBox (state : aval<bool>) (toggle : 'msg) =

            let attributes = 
                amap {
                     yield "type" => "checkbox"
                     yield onChange (fun _ -> toggle)

                     let! check = state
                     if check then
                        yield "checked" => "checked"
                }

      //      div [clazz "ui toggle checkbox"] [
            Incremental.input (AttributeMap.ofAMap attributes) 

//a simple Input view vor V3d
module V3dInput =

    type Message = 
        | SetX of float
        | SetY of float
        | SetZ of float

    let update  (m : V3d) (msg : Message) =
        match msg with
        | SetX x -> V3d(x, m.Y, m.Z)
        | SetY y -> V3d(m.X, y, m.Z)
        | SetZ z -> V3d(m.X, m.Y, z)

    let numInput name changed state  = labeledFloatInput name Double.MinValue Double.MaxValue 1.0 changed state
    let view header (m : aval<V3d>) =
        Html.table [ 
            tr [] [ td [attribute "colspan" "3"] [text header] ]                          
            tr [] [ td [] [numInput "X" SetX (AVal.map(fun (v :  V3d)-> v.X) m)]
                    td [] [numInput "Y" SetY (AVal.map(fun (v :  V3d)-> v.Y) m)]
                    td [] [numInput "Z" SetZ (AVal.map(fun (v :  V3d)-> v.Z) m)]
                  ]
        ]  