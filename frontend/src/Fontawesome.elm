module Fontawesome exposing (fontawesome, fontawesome_text)

import Element.Font as Font
import Element as UI

fontawesome : Font.Font
fontawesome = Font.external { name = "FontAwesome", url = "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css" }

fontawesome_text : List (UI.Attribute msg) -> String -> UI.Element msg
fontawesome_text attributes text = UI.el (Font.family [ fontawesome ] :: attributes) <| UI.text <| text
