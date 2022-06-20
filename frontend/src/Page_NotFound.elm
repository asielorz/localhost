module Page_NotFound exposing (title, view)

import Element as UI
import Element.Font as Font

title : String
title = "404"

view : UI.Element msg
view = UI.column
  [ UI.width UI.fill
  , UI.height UI.fill
  , Font.family [ Font.monospace ]
  , Font.center
  , UI.centerX
  , UI.centerY
  ]
  [ UI.el [Font.size 600, UI.width UI.fill] <| UI.text "404"
  , UI.el [Font.size 125, UI.width UI.fill] <| UI.text "Página no encontrada"
  ]
