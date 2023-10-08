module Page_Search exposing (Model, Msg, title, init, update, view, navigate_to)

import Element as UI exposing (px, rgb, rgba)
import Element.Font as Font
import Element.Input as Input
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
import Dict exposing (Dict)
import Metadata exposing (metadata_map_from_json)

type ComboId
  = ComboId_Type
  | ComboId_Author
  | ComboId_Category
  | ComboId_WorksMentioned Int
  | ComboId_Themes Int
  | ComboId_Tags Int

type alias Model = 
  { entries : List Entry
  , total_query_size : Int -- Including pages that have not been requested yet

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
  , all_categories : List String
  , all_authors : Dict String (List String)
  , all_works : Dict String (List String)
  , all_themes : Dict String (List String)
  , all_tags : Dict String (List String)
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
  | Msg_MorePressed

  -- Http from server
  | Msg_ReceivedSearchResults (Result Http.Error SearchResponse)
  | Msg_ReceivedCategories (Result Http.Error (List String))
  | Msg_ReceivedAuthors (Result Http.Error (Dict String (List String)))
  | Msg_ReceivedThemes (Result Http.Error (Dict String (List String)))
  | Msg_ReceivedWorks (Result Http.Error (Dict String (List String)))
  | Msg_ReceivedTags (Result Http.Error (Dict String (List String)))

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
      , total_query_size = 0

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

      , all_categories = []
      , all_authors = Dict.empty
      , all_works = Dict.empty
      , all_themes = Dict.empty
      , all_tags = Dict.empty
      }

    initial_commands = 
      [ Http.get { url = "/api/categories", expect = Http.expectJson Msg_ReceivedCategories (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "/api/authors", expect = Http.expectJson Msg_ReceivedAuthors metadata_map_from_json }
      , Http.get { url = "/api/themes", expect = Http.expectJson Msg_ReceivedThemes metadata_map_from_json }
      , Http.get { url = "/api/works", expect = Http.expectJson Msg_ReceivedWorks metadata_map_from_json }
      , Http.get { url = "/api/tags", expect = Http.expectJson Msg_ReceivedTags metadata_map_from_json }
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
type alias SearchResponse = 
  { entries : List Entry
  , current_offset : Int
  , next_offset : Int
  , total_size : Int
  }

search_response_from_json : Json.Decode.Decoder SearchResponse
search_response_from_json = Json.Decode.map4 SearchResponse
      (Json.Decode.field "entries" (Json.Decode.list Entry.from_json))
      (Json.Decode.field "current_offset" Json.Decode.int) 
      (Json.Decode.field "next_offset" Json.Decode.int) 
      (Json.Decode.field "total_size" Json.Decode.int) 

texts_query_command : String -> Cmd Msg
texts_query_command query_string = Http.get 
  { url = "/api/texts" ++ query_string
  , expect = Http.expectJson Msg_ReceivedSearchResults search_response_from_json
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

  Msg_MorePressed ->
    (model, texts_query_command <| search_query model (List.length model.entries))

  Msg_ReceivedSearchResults response -> case response of
    Ok (search_response) -> 
      (if search_response.current_offset == 0
        then { model | entries = search_response.entries, total_query_size = search_response.total_size }
        else { model | entries = model.entries ++ search_response.entries }
      , Cmd.none
      )
    Err _ -> (model, Cmd.none)

  Msg_ReceivedCategories result ->
    case result of
      Ok received_categories -> ({ model | all_categories = List.sort received_categories }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedAuthors result ->
    case result of
      Ok received_authors -> ({ model | all_authors = received_authors }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedThemes result ->
    case result of
      Ok received_themes -> ({ model | all_themes = received_themes }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedWorks result ->
    case result of
      Ok received_works -> ({ model | all_works = received_works }, Cmd.none)
      Err _ -> (model, Cmd.none)

  Msg_ReceivedTags result ->
    case result of
      Ok received_tags -> ({ model | all_tags = received_tags }, Cmd.none)
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

search_query : Model -> Int -> String
search_query model offset = SearchQuery.search_query
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
  , offset = offset
  }

search_button: Model -> UI.Element Msg
search_button model = UI.link 
  (Config.widget_common_attributes ++
  [ Background.color (rgb 0 0.6 0)
  , UI.mouseOver [ Background.color (rgb 0 0.7 0) ]
  , Font.center
  , UI.width UI.fill
  ])
  { url = "/search" ++ search_query model 0
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
  , with_label "Categoría"    <| input_box_with_suggestions []
    { text = model.category
    , suggestions = model.all_categories
    , message = Msg_CategoryChanged
    , id = ComboId_Category
    , currently_open_id = model.currently_open_combo
    , change_open = Msg_OpenComboChanged
    }
  , with_label "Autor"        <| input_box_with_suggestions [] 
    { text = model.author
    , suggestions = model.all_authors |> Dict.get model.category |> Maybe.withDefault []
    , message = Msg_AuthorChanged
    , id = ComboId_Author
    , currently_open_id = model.currently_open_combo
    , change_open = Msg_OpenComboChanged
    }
  , with_label "Descripción"  <| input_box [] model.description Msg_DescriptionChanged
  , with_label "Tipo"  <| type_combo model.type_to_search model.currently_open_combo
  , view_string_list 
    model.works_mentioned 
    (model.all_works |> Dict.get model.category |> Maybe.withDefault [])
    "Obras mencionadas" 
    ComboId_WorksMentioned 
    model.currently_open_combo 
    Msg_WorksMentioned 
    Msg_OpenComboChanged
  , view_string_list 
    model.themes 
    (model.all_themes |> Dict.get model.category |> Maybe.withDefault []) 
    "Temas" 
    ComboId_Themes 
    model.currently_open_combo 
    Msg_Themes 
    Msg_OpenComboChanged
  , view_string_list 
    model.tags 
    (model.all_tags |> Dict.get model.category |> Maybe.withDefault []) 
    "Etiquetas" 
    ComboId_Tags 
    model.currently_open_combo 
    Msg_Tags Msg_OpenComboChanged
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

view_entries : List (UI.Attribute Msg) -> List Entry -> Int -> UI.Element Msg
view_entries attributes entries total_query_size =
  let
    initial_message = "Mostrando " ++ String.fromInt (List.length entries) ++ " de " ++ String.fromInt total_query_size ++ " resultados:"
    entry_elements = UI.text initial_message :: (List.map (Entry.view Msg_EntrySelected) entries)

    more_button = Input.button 
      (Config.widget_common_attributes ++
      [ Background.color (rgb 0 0.6 0)
      , UI.mouseOver [ Background.color (rgb 0 0.7 0) ]
      , Font.center
      , UI.width UI.fill
      ])
      { onPress = Just Msg_MorePressed
      , label = UI.text "Más" 
      }

    column_elements = if List.length entries < total_query_size
      then entry_elements ++ [ more_button ]
      else entry_elements
  in
    UI.el attributes <| UI.column [ UI.spacing 20, UI.centerX ] column_elements

view_model : List (UI.Attribute Msg) -> Model -> UI.Element Msg
view_model attributes model = UI.row 
  attributes
  [ view_search_column 
    [ UI.width (UI.fillPortion 2)
    , UI.height UI.fill
    , UI.centerX
    , UI.scrollbarY 
    ]
    model
  , view_entries 
    [ UI.width (UI.fillPortion 5)
    , UI.height UI.fill
    , UI.scrollbarY
    , UI.centerX
    , UI.padding 20
    ]
    model.entries model.total_query_size
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
view model = view_model
  [ UI.width UI.fill
  , UI.height UI.fill
  , UI.centerX
  , UI.centerY
  , UI.inFront <| if model.dialog_entry == Nothing then UI.none else dialog_background
  , UI.inFront <| case model.dialog_entry of 
    Nothing -> UI.none 
    Just entry -> view_selected_entry_dialog entry
  , UI.scrollbarY
  , Background.color Config.background_color
  ] 
  model
