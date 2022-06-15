module Config exposing (..)

import Element as UI exposing (rgb, rgba)
import Element.Background as Background
import Element.Border as Border

background_color : UI.Color
background_color = (rgb 0.094 0.094 0.094)

widget_background_color : UI.Color
widget_background_color = (rgb 0.129 0.129 0.129)

widget_border_color : UI.Color
widget_border_color = (rgb 0.729 0.729 0.729)

transparentish_widget_border_color : UI.Color
transparentish_widget_border_color = (rgba 0.729 0.729 0.729 0.5)

widget_hovered_background_color : UI.Color
widget_hovered_background_color = (UI.rgb 0.219 0.219 0.219)

banner_background_color : UI.Color
banner_background_color = (UI.rgb 0.125 0.125 0.125)

banner_border_color : UI.Color
banner_border_color = (UI.rgb 0.219 0.219 0.219)

widget_common_border_attributes : List (UI.Attribute msg)
widget_common_border_attributes = 
  [ Border.color widget_border_color
  , Border.width 1
  , Border.rounded 3
  ]

widget_common_attributes : List (UI.Attribute msg)
widget_common_attributes = 
  [ Background.color widget_background_color
  , UI.padding 10
  ]
  ++ widget_common_border_attributes
