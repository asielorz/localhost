module Entry exposing (Entry, view, view_full, from_json)

import Calendar
import Element as UI exposing (px, rgb)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Fontawesome exposing (fontawesome_text)
import Config
import DateUtils
import Json.Decode as Json
import Utils exposing (chunk)

type alias Entry =
  { id : Int
  , link : String
  , title : String
  , description : String
  , author : String
  , category : String
  , themes : List String
  , works_mentioned : List String
  , tags : List String
  , date_published : Calendar.Date
  , date_saved : Calendar.Date
  , exceptional : Bool
  }

-- view

type ExtraInfo = ExtraInfo_Theme | ExtraInfo_Work | ExtraInfo_Tag

extra_info_fontawesome_icon : ExtraInfo -> String
extra_info_fontawesome_icon info = case info of
  ExtraInfo_Theme -> "\u{f075}" -- comment
  ExtraInfo_Work -> "\u{f02d}" -- book
  ExtraInfo_Tag -> "\u{f02b}" -- tag

view_extra_info : ExtraInfo -> String -> UI.Element msg
view_extra_info info label = UI.row
  [ Border.rounded 10
  , Background.color (rgb 0.047 0.329 0.723)
  , Font.size 15
  , UI.paddingXY 5 3
  , UI.spacing 5
  , UI.mouseOver [ Background.color (rgb 0.11 0.447 0.945) ]
  ]
  [ fontawesome_text [] (extra_info_fontawesome_icon info)
  , UI.text label
  ]

view_image : Entry -> UI.Element msg
view_image entry = UI.el 
  [ UI.width (px 304)
  , UI.height (px 173) 
  , Border.color Config.widget_border_color
  , Border.width 2
  , UI.inFront <| UI.link [ UI.moveRight 7, UI.moveDown 7 ]
    { url = "/edit/" ++ String.fromInt entry.id
    , label = fontawesome_text [ Font.size 40 ] "\u{f14b}" -- square-pen
    }
  ]
  <| UI.link
    [ UI.width (px 300)
    , UI.height (px 169)
    , Background.color Config.widget_background_color
    , UI.centerX
    , UI.centerY
    ]
    { label = UI.el 
      [ UI.centerX
      , UI.centerY
      , Font.size 25
      , Font.center
      ] 
      <| UI.text "Imagen"
    , url = entry.link
    }

title_row : Entry -> UI.Element msg
title_row entry = UI.row [ Font.size 25, UI.width UI.fill ] 
  [ UI.link [ UI.alignLeft ] { url = entry.link, label = UI.text entry.title }
  , fontawesome_text [ UI.alignRight, Font.color <| if entry.exceptional then (rgb 1 1 0) else (rgb 0.4 0.4 0.4) ] "\u{f005}" --fa-star
  ]

author_dates_row : Entry -> UI.Element msg
author_dates_row entry = UI.el [ Font.size 15 ] <| UI.text <| 
  entry.author ++ " · " ++ (DateUtils.date_to_string entry.date_published) ++ " · " ++ (DateUtils.date_to_string entry.date_saved)

view_tags_wrapped : Int -> Entry -> UI.Element msg
view_tags_wrapped chunk_by entry = Utils.grid
  { column_attributes = [ UI.spacing 5 ]
  , row_attributes = [ UI.spacing 10 ]
  , view_element = identity
  , grid_elements = chunk chunk_by <| 
    (List.map (view_extra_info ExtraInfo_Theme) entry.themes) ++ 
    (List.map (view_extra_info ExtraInfo_Work) entry.works_mentioned) ++ 
    (List.map (view_extra_info ExtraInfo_Tag) entry.tags)
  }

view_description : Entry -> UI.Element msg
view_description entry = UI.paragraph
  [ Font.size 15
  , Font.color (rgb 0.7 0.7 0.7)
  , Font.justify
  ]
  [ UI.text entry.description ]

view_entry_data : (Entry -> msg) -> Entry -> UI.Element msg
view_entry_data message entry = UI.column 
  [ UI.spacing 5
  , UI.alignTop
  , UI.width (px 800)
  , UI.clipY
  , UI.height (px 173)
  , Events.onClick <| message entry
  ]
  [ title_row entry
  , author_dates_row entry
  , view_tags_wrapped 6 entry
  , view_description entry
  ]

view : (Entry -> msg) -> Entry -> UI.Element msg
view message entry = UI.row 
  [ UI.spacing 10
  ]
  [ view_image entry
  , view_entry_data message entry
  ]

view_full : Entry -> UI.Element msg
view_full entry = UI.column
  [ UI.width UI.fill
  , UI.spacing 5
  ]
  [ title_row entry
  , author_dates_row entry
  , UI.el [ UI.centerX, UI.paddingXY 0 20 ] <| view_image entry
  , UI.el [ UI.centerX ] <| view_tags_wrapped 4 entry
  -- Separated in two elements because if padding is in the same element as scrollbar, the padding pixels are considered
  -- as part of the element for the clipping and so the content leaks into the padding when scrolling.
  , UI.el [ UI.paddingEach { left = 20, top = 20, bottom = 20, right = 0 } ] 
    <| UI.el [ UI.scrollbarY, UI.height (px 400), UI.paddingEach { left = 0, top = 0, bottom = 0, right = 20 } ] <| view_description entry
  ]

-- from_json

from_json : Json.Decoder Entry
from_json = 
  let 
    fields = Json.map8 Entry
      (Json.field "id" Json.int)
      (Json.field "link" Json.string) 
      (Json.field "title" Json.string) 
      (Json.field "description" Json.string) 
      (Json.field "author" Json.string) 
      (Json.field "category" Json.string) 
      (Json.field "themes" (Json.list Json.string)) 
      (Json.field "works_mentioned" (Json.list Json.string))
  in 
    Json.map5 (<|)
      fields
      (Json.field "tags" (Json.list Json.string))
      (Json.field "date_published" DateUtils.date_from_json)
      (Json.field "date_saved" DateUtils.date_from_json)
      (Json.field "exceptional" Json.bool)
