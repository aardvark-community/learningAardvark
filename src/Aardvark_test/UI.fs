namespace Aardvark.UI
open Aardvark.UI

 module  Html =   
    module SemUi =

        let adornerMenuWitoutDim (sectionsAndItems : list<string * list<DomNode<'msg>>>) (rest : list<DomNode<'msg>>) =
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
                     onBoot "$('#__ID__').sidebar('setting', 'dimPage', false);" (Html.SemUi.menu "ui vertical inverted sidebar menu" sectionsAndItems)
            ] 