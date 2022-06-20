module Banner exposing (..)

import Element as UI exposing (px, rgb)
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Config

header_banner : UI.Element msg
header_banner = UI.row 
  [ UI.width UI.fill
  , UI.height (px 56)
  , Border.widthXY 0 1
  , Border.color Config.banner_border_color
  , Background.color Config.banner_background_color
  , Font.color (rgb 1 1 1)
  ]
  [ UI.row [ UI.alignLeft, UI.spacing 20, UI.paddingXY 20 0 ]
    [ UI.el [ Font.size 30 ] <| UI.text "localhost"
    , UI.el [ Font.size 15, UI.moveDown 5 ] <| UI.text "There is no place like 127.0.0.1"
  ]
  , UI.row [ Font.size 20, UI.alignRight, UI.spacing 30, UI.paddingXY 30 0 ]
    [ UI.link [] { url = "/search", label = UI.text "Buscar" }
    , UI.link [] { url = "/new_entry", label = UI.text "Nueva entrada" }
    ]
  ]

with_banners : UI.Element msg -> UI.Element msg
with_banners view_model = UI.column [ UI.width UI.fill, UI.height UI.fill ] [ header_banner, view_model ]
