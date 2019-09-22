namespace Aardvark.UI
open Aardvark.UI

 module  Html =   
    module SemUi =

        let accordionMenu (c : string )(entries : list<string * list<DomNode<'msg>>>) =
            onBoot "$('#__ID__').accordion();"(
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
            )

        let accordion text' icon active content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"
            
            onBoot "$('#__ID__').accordion();" (
                div [clazz "ui inverted segment"] [
                    div [clazz "ui inverted accordion fluid"] [
                        div [clazz title] [
                                i [clazz (icon + " icon circular")][] 
                                text text'
                                //Static.a [clazz "ui label"] [
                                //    i [clazz (icon + " icon circular inverted")] []
                                //    text text'
                                //]
                        ]
                        div [clazz content] content'
                    ]
                ]
            )        
        
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
                     onBoot "$('#__ID__').sidebar('setting', 'dimPage', false);" (accordionMenu "ui vertical inverted sidebar very wide accordion menu" sectionsAndItems)
            ] 