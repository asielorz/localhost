module Fontawesome exposing (fontawesome, fontawesome_external_backup, fontawesome_text)

import Element.Font as Font
import Element as UI

fontawesome : Font.Font
fontawesome = Font.external { name = "FontAwesome", url = "http://localhost:8080/fontawesome/css/all.css" }

fontawesome_external_backup : Font.Font
fontawesome_external_backup = Font.external { name = "FontAwesome", url = "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.css" }

fontawesome_text : List (UI.Attribute msg) -> String -> UI.Element msg
fontawesome_text attributes text = UI.el (Font.family [ fontawesome, fontawesome_external_backup ] :: attributes) <| UI.text <| text
