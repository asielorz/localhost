module Page_Search exposing (Model, Msg, title, init, update, view, navigate_to)

import Element as UI exposing (px, rgb, rgba)
import Element.Font as Font
import Element.Border as Border
import Element.Events as Events
import Element.Background as Background
import Config
import Entry exposing (Entry)
import Http
import Json.Decode
import InputBox exposing (..)
import ListWidget
import DatePicker
import Time
import SearchQuery exposing (EntryTypeToSearch(..))
import Url exposing (Url)
import Url.Parser exposing (query)
import Fontawesome exposing (fontawesome_text)
import Combo

type ComboId
  = ComboId_Type
  | ComboId_Author
  | ComboId_Category
  | ComboId_WorksMentioned Int
  | ComboId_Themes Int
  | ComboId_Tags Int

type alias Model = 
  { entries : List Entry

  -- search
  , link : String
  , title : String
  , author : String
  , description : String
  , category : String
  , works_mentioned : List String
  , themes : List String
  , tags : List String
  , type_to_search : EntryTypeToSearch
  , published_between_from : DatePicker.State
  , published_between_until : DatePicker.State
  , saved_between_from : DatePicker.State
  , saved_between_until : DatePicker.State
  , exceptional : Bool
  , currently_open_combo : Maybe ComboId

  -- dialog
  , dialog_entry : Maybe Entry

  -- cached
  , all_authors : List String
  , all_categories : List String
  , all_works : List String
  , all_themes : List String
  , all_tags : List String
  }

type Msg
  -- Edit query
  = Msg_Noop
  
  | Msg_LinkChanged String
  | Msg_TitleChanged String
  | Msg_AuthorChanged String
  | Msg_DescriptionChanged String
  | Msg_TypeToSearchChanged (Combo.Msg EntryTypeToSearch)
  | Msg_CategoryChanged String
  | Msg_WorksMentioned (ListWidget.Msg String)
  | Msg_Themes (ListWidget.Msg String)
  | Msg_Tags (ListWidget.Msg String)
  | Msg_DatePublishedFromChanged DatePicker.Msg
  | Msg_DatePublishedUntilChanged DatePicker.Msg
  | Msg_DateSavedFromChanged DatePicker.Msg
  | Msg_DateSavedUntilChanged DatePicker.Msg
  | Msg_ExceptionalChanged Bool
  | Msg_OpenComboChanged (Maybe ComboId)

  -- Http from server
  | Msg_ReceivedSearchResults (Result Http.Error (List Entry))
  | Msg_ReceivedCategories (Result Http.Error (List String))
  | Msg_ReceivedAuthors (Result Http.Error (List String))
  | Msg_ReceivedThemes (Result Http.Error (List String))
  | Msg_ReceivedWorks (Result Http.Error (List String))
  | Msg_ReceivedTags (Result Http.Error (List String))

  -- Manage select entry dialog
  | Msg_EntrySelected Entry
  | Msg_CloseDialog

title : String
title = "Buscar"

init : (Model, Cmd Msg)
init = 
  let
    default_model =
      { entries = []

      , link = ""
      , title = ""
      , author = ""
      , description = ""
      , category = ""
      , works_mentioned = []
      , themes = []
      , tags = []
      , type_to_search = TypeToSearch_Any
      , published_between_from = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , published_between_until = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , saved_between_from = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , saved_between_until = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , exceptional = False
      , currently_open_combo = Nothing

      , dialog_entry = Nothing

      , all_authors = []
      , all_categories = []
      , all_works = []
      , all_themes = []
      , all_tags = []
      }

    initial_commands = 
      [ Http.get { url = "http://localhost:8080/api/authors", expect = Http.expectJson Msg_ReceivedAuthors (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/api/categories", expect = Http.expectJson Msg_ReceivedCategories (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/api/themes", expect = Http.expectJson Msg_ReceivedThemes (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/api/works", expect = Http.expectJson Msg_ReceivedWorks (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/api/tags", expect = Http.expectJson Msg_ReceivedTags (Json.Decode.list Json.Decode.string) }
      ]
  in
    (default_model, Cmd.batch initial_commands)

navigate_to : Url -> Model -> (Model, Cmd Msg)
navigate_to url model = case url.query of
    Nothing -> (model, texts_query_command "")
    Just query_string -> case SearchQuery.search_ui_state_from_query query_string of
      Nothing -> (model, Cmd.none)
      Just query -> 
        let 
          updated_model = 
            { model
            | link = query.link
            , title = query.title
            , author = query.author
            , description = query.description
            , category = query.category
            , works_mentioned = query.works_mentioned
            , themes = query.themes
            , tags = query.tags
            , published_between_from = DatePicker.set_date query.published_between_from model.published_between_from
            , published_between_until = DatePicker.set_date query.published_between_until model.published_between_until
            , saved_between_from = DatePicker.set_date query.saved_between_from model.saved_between_from
            , saved_between_until = DatePicker.set_date query.saved_between_until model.saved_between_until
            , exceptional = query.exceptional
            }
        in
          (updated_model, texts_query_command ("?" ++ query_string))

-- query_string is only the parameters. The part that starts with '?'. It is assumed to start with '?'.
texts_query_command : String -> Cmd Msg
texts_query_command query_string = Http.get 
  { url = "http://localhost:8080/api/texts" ++ query_string
  , expect = Http.expectJson Msg_ReceivedSearchResults (Json.Decode.list Entry.from_json) 
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Msg_Noop ->
    (model, Cmd.none)

  Msg_LinkChanged new_link -> 
    ({ model | link = new_link }, Cmd.none)
  
  Msg_TitleChanged new_title -> 
    ({ model | title = new_title }, Cmd.none)
  
  Msg_AuthorChanged new_author -> 
    ({ model | author = new_author }, Cmd.none)
  
  Msg_DescriptionChanged new_description -> 
    ({ model | description = new_description }, Cmd.none)

  Msg_TypeToSearchChanged combo_msg ->
    ( Combo.update
      { set_value = (\t m -> { m | type_to_search = t })
      , set_open_combo = (\id m -> { m | currently_open_combo = id })
      , id = ComboId_Type
      , model = model
      }
      combo_msg
    , Cmd.none
    )
  
  Msg_CategoryChanged new_category -> 
    ({ model | category = new_category }, Cmd.none)

  Msg_WorksMentioned list_msg ->
    ({ model | works_mentioned = (ListWidget.update list_msg model.works_mentioned) }, Cmd.none)

  Msg_Themes list_msg ->
    ({ model | themes = (ListWidget.update list_msg model.themes) }, Cmd.none)

  Msg_Tags list_msg ->
    ({ model | tags = (ListWidget.update list_msg model.tags) }, Cmd.none)

  Msg_DatePublishedFromChanged date_picker_msg ->
    ({ model | published_between_from = (DatePicker.update date_picker_msg model.published_between_from) }, Cmd.none)

  Msg_DatePublishedUntilChanged date_picker_msg ->
    ({ model | published_between_until = (DatePicker.update date_picker_msg model.published_between_until) }, Cmd.none)

  Msg_DateSavedFromChanged date_picker_msg ->
    ({ model | saved_between_from = (DatePicker.update date_picker_msg model.saved_between_from) }, Cmd.none)

  Msg_DateSavedUntilChanged date_picker_msg ->
    ({ model | saved_between_until = (DatePicker.update date_picker_msg model.saved_between_until) }, Cmd.none)

  Msg_ExceptionalChanged new_exceptional ->
    ({ model | exceptional = new_exceptional }, Cmd.none)

  Msg_OpenComboChanged new_open_combo ->
     ({ model | currently_open_combo = new_open_combo }, Cmd.none)

  Msg_ReceivedSearchResults new_results -> case new_results of
    Ok (actual_results) -> ({ model | entries = actual_results }, Cmd.none)
    Err _ -> (model, Cmd.none)

  Msg_ReceivedCategories result ->
    case result of
      Ok received_categories -> ({ model | all_categories = List.sort received_categories }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedAuthors result ->
    case result of
      Ok received_authors -> ({ model | all_authors = List.sort received_authors }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedThemes result ->
    case result of
      Ok received_themes -> ({ model | all_themes = List.sort received_themes }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedWorks result ->
    case result of
      Ok received_works -> ({ model | all_works = List.sort received_works }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedTags result ->
    case result of
      Ok received_tags -> ({ model | all_tags = List.sort received_tags }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_EntrySelected entry ->
    ({ model | dialog_entry = Just entry }, Cmd.none)

  Msg_CloseDialog ->
    ({ model | dialog_entry = Nothing }, Cmd.none)

with_label : String -> UI.Element msg -> UI.Element msg
with_label label element = UI.column [ UI.width UI.fill, UI.spacing 5 ]
  [ UI.text label
  , element
  ]

exceptional_toggle_button : Bool -> UI.Element Msg
exceptional_toggle_button is_exceptional = fontawesome_text
  [ Font.size 30
  , Font.color <| if is_exceptional then (rgb 1 1 0) else (rgb 0.4 0.4 0.4)
  , Events.onClick <| Msg_ExceptionalChanged (not is_exceptional)
  , UI.pointer
  ]
  "\u{f005}" --fa-star

search_button: Model -> UI.Element Msg
search_button model = 
  let
    query = SearchQuery.search_query
      { link = model.link
      , title = model.title
      , author = model.author
      , description = model.description
      , category = model.category
      , works_mentioned = model.works_mentioned
      , themes = model.themes
      , tags = model.tags
      , type_to_search = model.type_to_search
      , published_between_from = DatePicker.date model.published_between_from
      , published_between_until = DatePicker.date model.published_between_until
      , saved_between_from = DatePicker.date model.saved_between_from
      , saved_between_until = DatePicker.date model.saved_between_until
      , exceptional = model.exceptional
      }
  in
    UI.link 
      (Config.widget_common_attributes ++
      [ Background.color (rgb 0 0.6 0)
      , Font.center
      , UI.width UI.fill
      ])
      { url = "/search" ++ query
      , label = UI.text "Buscar"
      }

entry_type_display_string : EntryTypeToSearch -> String
entry_type_display_string entry_type = case entry_type of
  TypeToSearch_Any -> "Todos"
  TypeToSearch_Article -> "Artículo"
  TypeToSearch_Paper -> "Paper"
  TypeToSearch_Book -> "Libro"
  TypeToSearch_Video -> "Vídeo"
  TypeToSearch_Audio -> "Audio"

entry_type_fontawesome_icon : EntryTypeToSearch -> String
entry_type_fontawesome_icon entry_type = case entry_type of
  TypeToSearch_Any -> "\u{f00c}" -- fa-check
  TypeToSearch_Article -> "\u{f15b}" -- fa-file
  TypeToSearch_Paper -> "\u{f15c}" -- fa-file-lines
  TypeToSearch_Book -> "\u{f02d}" -- fa-book
  TypeToSearch_Video -> "\u{f03d}" -- fa-video
  TypeToSearch_Audio -> "\u{f027}" -- fa-volume-low

type_combo_alternative : EntryTypeToSearch -> UI.Element Msg
type_combo_alternative entry_type = 
  UI.el 
  [ UI.padding 5
  ]
  (UI.row [ UI.spacing 10 ]
    [ fontawesome_text [] <| entry_type_fontawesome_icon entry_type
    , UI.text <| entry_type_display_string entry_type
    ]
  )

type_combo : EntryTypeToSearch -> Maybe ComboId -> UI.Element Msg
type_combo entry_type currently_open_combo = Combo.view 
  (Config.widget_common_attributes ++ [ UI.padding 5, UI.width UI.fill ])
  { combo_state = entry_type
  , alternatives = [ TypeToSearch_Any, TypeToSearch_Article, TypeToSearch_Paper, TypeToSearch_Book, TypeToSearch_Video, TypeToSearch_Audio ]
  , id = ComboId_Type
  , currently_open_id = currently_open_combo
  , message = Msg_TypeToSearchChanged
  , view_alternative = type_combo_alternative
  }

view_search_column : List (UI.Attribute Msg) -> Model -> UI.Element Msg
view_search_column attributes model = UI.column 
  (attributes ++ 
    [ UI.padding 20
    , UI.spacing 10
    , Border.widthEach { top = 0, bottom = 0, left = 0, right = 3 }
    , Border.color Config.widget_border_color
    ]
  ) 
  [ with_label "Link"       <| UI.row [ UI.width UI.fill, UI.spacing 10 ] [ input_box [] model.link Msg_LinkChanged, exceptional_toggle_button model.exceptional ]
  , with_label "Título"       <| input_box [] model.title Msg_TitleChanged
  , with_label "Autor"        <| input_box_with_suggestions [] 
    { text = model.author
    , suggestions = model.all_authors
    , message = Msg_AuthorChanged
    , id = ComboId_Author
    , currently_open_id = model.currently_open_combo
    , change_open = Msg_OpenComboChanged
    }
  , with_label "Descripción"  <| input_box [] model.description Msg_DescriptionChanged
  , with_label "Tipo"  <| type_combo model.type_to_search model.currently_open_combo
  , with_label "Categoría"    <| input_box_with_suggestions []
    { text = model.category
    , suggestions = model.all_categories
    , message = Msg_CategoryChanged
    , id = ComboId_Category
    , currently_open_id = model.currently_open_combo
    , change_open = Msg_OpenComboChanged
    }
  , view_string_list model.works_mentioned model.all_works "Obras mencionadas" ComboId_WorksMentioned model.currently_open_combo Msg_WorksMentioned Msg_OpenComboChanged
  , view_string_list model.themes model.all_themes "Temas" ComboId_Themes model.currently_open_combo Msg_Themes Msg_OpenComboChanged
  , view_string_list model.tags model.all_tags "Etiquetas" ComboId_Tags model.currently_open_combo Msg_Tags Msg_OpenComboChanged
  , with_label "Publicado entre" <| UI.row [ UI.width UI.fill, UI.spacing 10 ] 
    [ DatePicker.view Config.widget_common_attributes Msg_DatePublishedFromChanged model.published_between_from
    , DatePicker.view Config.widget_common_attributes Msg_DatePublishedUntilChanged model.published_between_until
    ]
  , with_label "Guardado entre" <| UI.row [ UI.width UI.fill, UI.spacing 10 ] 
    [ DatePicker.view Config.widget_common_attributes Msg_DateSavedFromChanged model.saved_between_from
    , DatePicker.view Config.widget_common_attributes Msg_DateSavedUntilChanged model.saved_between_until
    ]
  , search_button model
  ]

view_model : Model -> UI.Element Msg
view_model model = UI.row 
  [ UI.centerX
  , UI.width UI.fill
  , UI.height UI.fill
  ]
  [ view_search_column [ UI.width (UI.fillPortion 1), UI.height UI.fill, UI.centerX ] model
  , UI.el 
    [ UI.width (UI.fillPortion 3)
    , UI.height UI.fill
    , UI.centerX
    , UI.centerY
    , UI.padding 20
    ]
     <| UI.column [ UI.centerX, UI.spacing 20 ] (List.map (Entry.view Msg_EntrySelected) model.entries)
  ]

dialog_background : UI.Element Msg
dialog_background = UI.el
  [ Events.onClick Msg_CloseDialog 
  , Background.color (rgba 0 0 0 0.75)
  , UI.width UI.fill
  , UI.height UI.fill
  ]
  UI.none

view_selected_entry_dialog : Entry -> UI.Element msg
view_selected_entry_dialog entry = UI.el
  [ Background.color Config.background_color
  , Border.rounded 5
  , Border.width 3
  , Border.color Config.widget_border_color
  , UI.centerX
  , UI.centerY
  , UI.padding 10
  , UI.spacing 20
  , UI.width (px 700)
  , UI.height (px 800)
  ]
  <| Entry.view_full entry

view : Model -> UI.Element Msg
view model = UI.el 
  [ UI.width UI.fill
  , UI.height UI.fill
  , UI.centerX
  , UI.centerY
  , UI.inFront <| if model.dialog_entry == Nothing then UI.none else dialog_background
  , UI.inFront <| case model.dialog_entry of 
    Nothing -> UI.none 
    Just entry -> view_selected_entry_dialog entry
  ] 
  <| view_model model
