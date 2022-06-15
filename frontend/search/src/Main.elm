module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Element as UI exposing (rgb)
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Background as Background
import Config
import Entry exposing (Entry)
import Http
import Json.Decode
import InputBox exposing (..)
import ListWidget
import DatePicker
import Time
import SearchQuery
import Browser exposing (UrlRequest)
import Url
import Url exposing (Url)
import Url.Parser exposing (query)
import Banner

main : Program () Model Msg
main = Browser.application 
  { init = \() url key -> init key url
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = Msg_UrlRequest
  , onUrlChange = always Msg_Noop
  }

type ComboId
  = ComboId_Author
  | ComboId_Category
  | ComboId_WorksMentioned Int
  | ComboId_Themes Int
  | ComboId_Tags Int

type alias Model = 
  { navigation_key : Navigation.Key
    
  , entries : List Entry

  -- search
  , link : String
  , title : String
  , author : String
  , description : String
  , category : String
  , works_mentioned : List String
  , themes : List String
  , tags : List String
  , published_between_from : DatePicker.State
  , published_between_until : DatePicker.State
  , saved_between_from : DatePicker.State
  , saved_between_until : DatePicker.State
  , exceptional : Bool
  , currently_open_combo : Maybe ComboId

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
  | Msg_CategoryChanged String
  | Msg_WorksMentioned (ListWidget.Msg String)
  | Msg_Themes (ListWidget.Msg String)
  | Msg_Tags (ListWidget.Msg String)
  | Msg_DatePublishedFromChanged DatePicker.Msg
  | Msg_DatePublishedUntilChanged DatePicker.Msg
  | Msg_DateSavedFromChanged DatePicker.Msg
  | Msg_DateSavedUntilChanged DatePicker.Msg
  | Msg_OpenComboChanged (Maybe ComboId)
  | Msg_Search

  -- Http from server
  | Msg_ReceivedSearchResults (Result Http.Error (List Entry))
  | Msg_ReceivedCategories (Result Http.Error (List String))
  | Msg_ReceivedAuthors (Result Http.Error (List String))
  | Msg_ReceivedThemes (Result Http.Error (List String))
  | Msg_ReceivedWorks (Result Http.Error (List String))
  | Msg_ReceivedTags (Result Http.Error (List String))

  -- Url messages from the application
  | Msg_UrlRequest UrlRequest

init : Navigation.Key -> Url -> (Model, Cmd Msg)
init key url = 
  let
    default_model =
      { navigation_key = key
        
      , entries = []

      , link = ""
      , title = ""
      , author = ""
      , description = ""
      , category = ""
      , works_mentioned = []
      , themes = []
      , tags = []
      , published_between_from = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , published_between_until = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , saved_between_from = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , saved_between_until = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
      , exceptional = False
      , currently_open_combo = Nothing

      , all_authors = []
      , all_categories = []
      , all_works = []
      , all_themes = []
      , all_tags = []
      }

    initial_commands = 
      [ Http.get { url = "http://localhost:8080/authors", expect = Http.expectJson Msg_ReceivedAuthors (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/categories", expect = Http.expectJson Msg_ReceivedCategories (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/themes", expect = Http.expectJson Msg_ReceivedThemes (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/works", expect = Http.expectJson Msg_ReceivedWorks (Json.Decode.list Json.Decode.string) }
      , Http.get { url = "http://localhost:8080/tags", expect = Http.expectJson Msg_ReceivedTags (Json.Decode.list Json.Decode.string) }
      ]
  in case url.query of
    Nothing -> (default_model, Cmd.batch initial_commands)
    Just query_string -> case SearchQuery.search_ui_state_from_query query_string of
      Nothing -> (default_model, Cmd.batch initial_commands)
      Just query -> 
        let 
          model = 
            { default_model
            | link = query.link
            , title = query.title
            , author = query.author
            , description = query.description
            , category = query.category
            , works_mentioned = query.works_mentioned
            , themes = query.themes
            , tags = query.tags
            , published_between_from = DatePicker.set_date query.published_between_from default_model.published_between_from
            , published_between_until = DatePicker.set_date query.published_between_until default_model.published_between_until
            , saved_between_from = DatePicker.set_date query.saved_between_from default_model.saved_between_from
            , saved_between_until = DatePicker.set_date query.saved_between_until default_model.saved_between_until
            , exceptional = query.exceptional
            }

          commands =
            texts_query_command ("?" ++ query_string)
            :: initial_commands
        in
          (model, Cmd.batch commands)

-- query_string is only the parameters. The part that starts with '?'. It is assumed to start with '?'.
texts_query_command : String -> Cmd Msg
texts_query_command query_string = Http.get 
  { url = "http://localhost:8080/texts" ++ query_string
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

  Msg_OpenComboChanged new_open_combo ->
     ({ model | currently_open_combo = new_open_combo }, Cmd.none)

  Msg_Search ->
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
        , published_between_from = DatePicker.date model.published_between_from
        , published_between_until = DatePicker.date model.published_between_until
        , saved_between_from = DatePicker.date model.saved_between_from
        , saved_between_until = DatePicker.date model.saved_between_until
        , exceptional = model.exceptional
        }
    in
      (model
      , Cmd.batch
        [ Navigation.pushUrl model.navigation_key ("/search" ++ query)
        , texts_query_command query
        ]
    )

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

  Msg_UrlRequest request -> case request of
    Browser.Internal url -> if url.path == "search"
      then (model, Navigation.pushUrl model.navigation_key (Url.toString url))
      else (model, Navigation.load (Url.toString url))
    Browser.External link -> (model, Navigation.load link)

with_label : String -> UI.Element msg -> UI.Element msg
with_label label element = UI.column [ UI.width UI.fill, UI.spacing 5 ]
  [ UI.text label
  , element
  ]

search_button: UI.Element Msg
search_button = Input.button 
  (Config.widget_common_attributes ++
  [ Background.color (rgb 0 0.6 0)
  , Font.center
  , UI.width UI.fill
  ])
  { onPress = Just Msg_Search
  , label = UI.text "Buscar"
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
  [ with_label "Link"       <| input_box [] model.link Msg_LinkChanged
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
  , search_button
  ]

view_model : Model -> UI.Element Msg
view_model model = UI.row 
  [ UI.centerX
  , UI.width UI.fill
  , UI.height UI.fill
  ]
  [ view_search_column [ UI.width (UI.fillPortion 1), UI.height UI.fill, UI.centerX ] model
  , UI.el [UI.width (UI.fillPortion 3), UI.height UI.fill, UI.centerX, UI.centerY, UI.padding 20] <| UI.column [ UI.centerX, UI.spacing 20 ] (List.map Entry.view model.entries)
  ]

view : Model -> Document Msg
view model =
  { title = "Buscar | localhost"
  , body = 
    [ UI.layout 
        [ Background.color Config.background_color
        , UI.centerX
        , UI.centerY
        , Font.color (rgb 1 1 1) 
        ] 
        <| Banner.with_banners (view_model model)
    ]
  }
