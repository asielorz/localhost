module Entry exposing (Entry, view, view_full, from_json)

import EntryType exposing (EntryType(..))
import Calendar
import Element as UI exposing (px, rgb, rgba)
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
  , entry_type : EntryType
  , image : Maybe String
  , backup : Maybe String
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

entry_type_metadata_text : EntryType -> String
entry_type_metadata_text entry_type = case entry_type of
  Type_Article t -> String.fromInt t.pages ++ " páginas"
  Type_Paper t -> String.fromInt t.pages ++ " páginas"
  Type_Book t -> String.fromInt t.pages ++ " páginas"
  Type_Video t -> Utils.format_seconds_as_hours_minutes_seconds t.length_in_seconds
  Type_Audio t -> Utils.format_seconds_as_hours_minutes_seconds t.length_in_seconds

view_image : Entry -> UI.Element msg
view_image entry = UI.el 
  [ UI.width (px 304)
  , UI.height (px 173) 
  , Border.color Config.widget_border_color
  , Border.width 2
  , UI.inFront <| UI.el 
    [ UI.padding 5
    , UI.alignBottom
    , UI.alignRight
    , Background.color (rgba 0 0 0 0.5)
    , Font.size 15
    , Border.rounded 10
    ]
    <| UI.text <| entry_type_metadata_text entry.entry_type
  ]
  <| UI.el
    [ UI.width (px 300)
    , UI.height (px 169)
    , case entry.image of
      Nothing -> Background.color Config.widget_background_color
      Just image_url -> Background.image image_url
    , UI.centerX
    , UI.centerY
    ]
    <| if entry.image /= Nothing
      then UI.none
      else UI.el 
        [ UI.centerX
        , UI.centerY
        , Font.size 25
        , Font.center
        ] 
        <| UI.text "Imagen"

backup_button : Maybe String -> UI.Element msg
backup_button url =
  let
    (color, attributes) = case url of
      Just _ -> (rgb 1 1 1, Config.image_button_attributes)
      Nothing -> (rgb 0.7 0.7 0.7, Config.widget_common_attributes ++ [ Background.color <| Utils.set_alpha 0.5 Config.widget_background_color ])
    label = fontawesome_text (attributes ++ [ Font.color color, Border.color color ]) "\u{f019}" -- download
  in
    case url of
      Nothing -> label
      Just actual_url -> UI.newTabLink [] { label = label, url = actual_url }

view_image_with_buttons : Entry -> UI.Element msg
view_image_with_buttons entry = UI.el 
  [ UI.inFront <| UI.row [ UI.padding 7, UI.spacing 5 ]
    [ UI.link []
      { url = "/edit/" ++ String.fromInt entry.id
      , label = fontawesome_text Config.image_button_attributes "\u{f303}" -- pen-to-square
      }
    , backup_button entry.backup
    ]
  ]
  <| UI.link []
    { label = view_image entry
    , url = entry.link
    }

title_row : Entry -> UI.Element msg
title_row entry = UI.row 
  [ Font.size 25
  , UI.width UI.fill
  , UI.spacing 10
  ] 
  [ fontawesome_text [] <| EntryType.fontawesome_icon entry.entry_type
  , UI.link [ UI.alignLeft ] { url = entry.link, label = UI.text entry.title }
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
view_description entry = 
  let
    paragraphs = entry.description 
      |> String.split "\n"
      |> List.filter (not << String.isEmpty)
  in
    UI.column
      [ Font.size 15
      , Font.color (rgb 0.7 0.7 0.7)
      , Font.justify
      , UI.spacing 10
      ]
      <| List.map (\p -> UI.paragraph [] [ UI.text p ]) paragraphs

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
  [ view_image_with_buttons entry
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
    Json.map8 (<|)
      fields
      (Json.field "tags" (Json.list Json.string))
      (Json.field "date_published" DateUtils.date_from_json)
      (Json.field "date_saved" DateUtils.date_from_json)
      (Json.field "exceptional" Json.bool)
      (Json.field "entry_type" EntryType.from_json)
      (Json.field "image" (Json.nullable Json.string))
      (Json.field "backup" (Json.nullable Json.string))
