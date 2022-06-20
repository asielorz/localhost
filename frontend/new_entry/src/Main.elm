module Main exposing (main)

import Browser exposing (Document)
import Element as UI exposing (rgb, px, rgba)
import Element.Background as Background
import Element.Input as Input
import Element.Font as Font
import Element.Border as Border
import Element.Events as Events
import Combo
import ListWidget
import Fontawesome exposing (fontawesome)
import DatePicker
import Time
import Http
import NewEntry
import Html exposing (form)
import Config
import Http
import Json.Decode
import InputBox exposing (..)
import Banner
import Utils
import File exposing (File)
import File.Select
import Task
import Base64
import Bytes exposing (Bytes)
import Fontawesome exposing (fontawesome_text)
import Url

main : Program () Model Msg
main = Browser.document 
  { init = \() -> (default_model, initial_commands)
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type EntryType 
  = Text { pages : Int }
  | Video { length_in_seconds : Int }

type Backup = NoBackup | Automatic | Manual (Maybe String)

type ComboId 
  = ComboId_Type 
  | ComboId_Backup 
  | ComboId_Author
  | ComboId_Category
  | ComboId_WorkMentioned Int
  | ComboId_Theme Int
  | ComboId_Tag Int

type AppState 
  = State_Editing 
  | State_Notify { message : String, reset_on_close : Bool }
  | State_InputUrl { url : String }

type EntryImage
  = Image_None
  | Image_Url String
  | Image_File { path : String, url : String, content_type : String, bytes : Bytes }

type alias Model = 
  { link : String
  , title : String
  , backup : Backup
  , image : EntryImage
  , description : String
  , author : String
  , category : String
  , themes : List String
  , works_mentioned : List String
  , tags : List String
  , date_published : DatePicker.State
  , entry_type : EntryType
  , exceptional : Bool
  , currently_open_combo : Maybe ComboId
  , app_state : AppState

  -- Cached. Received from the server. Used for autocompletion.
  , all_categories : List String
  , all_authors : List String
  , all_themes : List String
  , all_works : List String
  , all_tags : List String
  }

default_model : Model
default_model = 
  { link = ""
  , title = ""
  , backup = NoBackup
  , image = Image_None
  , description = ""
  , author = ""
  , category = ""
  , themes = []
  , works_mentioned = []
  , tags = []
  , date_published = DatePicker.make_empty { display_month = Time.Jun, display_year = 2022 }
  , entry_type = Text { pages = 0 }
  , exceptional = False
  , currently_open_combo = Nothing
  , app_state = State_Editing

  , all_categories = []
  , all_authors = []
  , all_themes = []
  , all_works = []
  , all_tags = []
  }

initial_commands : Cmd Msg
initial_commands = Cmd.batch
  [ Http.get { url = "http://localhost:8080/api/authors", expect = Http.expectJson Msg_ReceivedAuthors (Json.Decode.list Json.Decode.string) }
  , Http.get { url = "http://localhost:8080/api/categories", expect = Http.expectJson Msg_ReceivedCategories (Json.Decode.list Json.Decode.string) }
  , Http.get { url = "http://localhost:8080/api/themes", expect = Http.expectJson Msg_ReceivedThemes (Json.Decode.list Json.Decode.string) }
  , Http.get { url = "http://localhost:8080/api/works", expect = Http.expectJson Msg_ReceivedWorks (Json.Decode.list Json.Decode.string) }
  , Http.get { url = "http://localhost:8080/api/tags", expect = Http.expectJson Msg_ReceivedTags (Json.Decode.list Json.Decode.string) }
  ]

type Msg 
  = Msg_Noop

  -- UI
  | Msg_LinkChanged String
  | Msg_TitleChanged String
  | Msg_AuthorChanged String
  | Msg_DescriptionChanged String
  | Msg_CategoryChanged String
  | Msg_TypeCombo (Combo.Msg EntryType)
  | Msg_BackupCombo (Combo.Msg Backup)
  | Msg_ExceptionalChanged Bool
  | Msg_WorksMentioned (ListWidget.Msg String)
  | Msg_Themes (ListWidget.Msg String)
  | Msg_Tags (ListWidget.Msg String)
  | Msg_DatePublishedChanged DatePicker.Msg
  | Msg_Send
  | Msg_CloseResponseDialog Bool
  | Msg_OpenComboChanged (Maybe ComboId)

  -- Image
  | Msg_ImageFileButtonClicked
  | Msg_ImageFileOpened File
  | Msg_ImageLoaded { path : String, url : String, content_type : String, bytes : Bytes }
  | Msg_ImageUrlButtonClicked
  | Msg_InputUrlChanged String
  | Msg_ImageUrlChosen String
  | Msg_ImageResetButtonClicked

  -- Server
  | Msg_ResponseToSendArrived (Result Http.Error ())
  | Msg_ReceivedCategories (Result Http.Error (List String))
  | Msg_ReceivedAuthors (Result Http.Error (List String))
  | Msg_ReceivedThemes (Result Http.Error (List String))
  | Msg_ReceivedWorks (Result Http.Error (List String))
  | Msg_ReceivedTags (Result Http.Error (List String))

-- update

set_entry_type : EntryType -> Model -> Model
set_entry_type entry_type model = { model | entry_type = entry_type }

set_open_combo : Maybe ComboId -> Model -> Model
set_open_combo id model = { model | currently_open_combo = id }

set_backup : Backup -> Model -> Model
set_backup backup model = { model | backup = backup }

notify_success : AppState
notify_success = State_Notify { message = "Nueva entrada añadida correctamente.", reset_on_close = True }

notify_error : String -> AppState
notify_error error_message = State_Notify { message = "Error al añadir nueva entrada: " ++ error_message, reset_on_close = False }

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

  Msg_TypeCombo combo_msg ->
    (Combo.update { set_value = set_entry_type, set_open_combo = set_open_combo, id = ComboId_Type, model = model } combo_msg, Cmd.none)
  
  Msg_BackupCombo combo_msg ->
    (Combo.update { set_value = set_backup, set_open_combo = set_open_combo, id = ComboId_Backup, model = model } combo_msg, Cmd.none)

  Msg_ExceptionalChanged new_exceptional ->
    ({ model | exceptional = new_exceptional }, Cmd.none)

  Msg_WorksMentioned list_msg ->
    ({ model | works_mentioned = (ListWidget.update list_msg model.works_mentioned) }, Cmd.none)

  Msg_Themes list_msg ->
    ({ model | themes = (ListWidget.update list_msg model.themes) }, Cmd.none)

  Msg_Tags list_msg ->
    ({ model | tags = (ListWidget.update list_msg model.tags) }, Cmd.none)

  Msg_DatePublishedChanged date_picker_msg ->
    ({ model | date_published = DatePicker.update date_picker_msg model.date_published }, Cmd.none)

  Msg_Send -> 
    send_button_clicked model

  Msg_CloseResponseDialog reset ->
    if reset
      then (default_model, Cmd.none)
      else ({ model | app_state = State_Editing }, Cmd.none)

  Msg_OpenComboChanged new_open_combo -> 
    ({ model | currently_open_combo = new_open_combo }, Cmd.none)

  -- Image

  Msg_ImageFileButtonClicked ->
    (model, File.Select.file ["image/png","image/bmp","image/jpeg","image/gif"] Msg_ImageFileOpened)

  Msg_ImageFileOpened file ->
    (model
    , File.toBytes file
      |> Task.map 
        (\bytes -> case Base64.fromBytes bytes of 
          Nothing -> Msg_Noop
          Just base64 ->
            let 
              path = File.name file
              extension = path |> String.split "." |> Utils.last
            in
              case extension of
                Just "png"  -> Msg_ImageLoaded { path = path, url = "data:image/png;base64,"  ++ base64, content_type = "image/png",  bytes = bytes }
                Just "bmp"  -> Msg_ImageLoaded { path = path, url = "data:image/bmp;base64,"  ++ base64, content_type = "image/bmp",  bytes = bytes }
                Just "jpg"  -> Msg_ImageLoaded { path = path, url = "data:image/jpeg;base64," ++ base64, content_type = "image/jpeg", bytes = bytes }
                Just "jpeg" -> Msg_ImageLoaded { path = path, url = "data:image/jpeg;base64," ++ base64, content_type = "image/jpeg", bytes = bytes }
                Just "gif"  -> Msg_ImageLoaded { path = path, url = "data:image/gif;base64,"  ++ base64, content_type = "image/gif",  bytes = bytes }
                _ -> Msg_Noop
        )
      |> Task.perform identity
    )

  Msg_ImageLoaded image ->
    ({model | image = Image_File image }, Cmd.none)

  Msg_ImageUrlButtonClicked ->
    ({ model | app_state = State_InputUrl { url = "" } }, Cmd.none)

  Msg_InputUrlChanged new_url ->
    ({ model | app_state = State_InputUrl { url = new_url } }, Cmd.none)

  Msg_ImageUrlChosen new_url ->
    ({ model | image = Image_Url new_url, app_state = State_Editing }, Cmd.none)

  Msg_ImageResetButtonClicked ->
    ({ model | image = Image_None }, Cmd.none)

  -- Server

  Msg_ResponseToSendArrived response -> case response of
    Ok _ -> ({ model | app_state = notify_success }, Cmd.none)
    Err err -> ({ model | app_state = notify_error (http_error_to_string err) }, Cmd.none)

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

http_error_to_string : Http.Error -> String
http_error_to_string error = case error of
  Http.BadUrl str -> "Bad URL: " ++ str
  Http.Timeout -> "Timeout"
  Http.NetworkError -> "Network error"
  Http.BadStatus status -> "Bad status code (" ++ String.fromInt status ++ ")"
  Http.BadBody str -> "Bad body: " ++ str

make_new_entry_form : Model -> Result String NewEntry.Form 
make_new_entry_form model =
  let 
    form = 
      { link = model.link
      , title = model.title
      , description = model.description
      , author = model.author
      , category = model.category
      , themes = model.themes
      , works_mentioned = model.works_mentioned
      , tags = model.tags
      , date_published = DatePicker.date model.date_published
      , exceptional = model.exceptional
      }
  in case NewEntry.validate form of
    Nothing -> Ok form
    Just error -> Err error

-- Http.post 
--   { url = "http://localhost:8080/api/texts"
--   , body = Http.jsonBody <| NewEntry.to_json form
--   , expect = Http.expectString Msg_ResponseToSendArrived
--   }

send_button_clicked : Model -> (Model, Cmd Msg)
send_button_clicked model = case make_new_entry_form model of
  Ok form -> 
    let 
      resolve toResult response =
        case response of
          Http.BadUrl_ url -> Err (Http.BadUrl url)
          Http.Timeout_ -> Err Http.Timeout
          Http.NetworkError_ -> Err Http.NetworkError
          Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
          Http.GoodStatus_ _ body -> Result.mapError Http.BadBody (toResult body)

      task = Http.task 
        { method = "POST"
        , url = "http://localhost:8080/api/texts"
        , headers = []
        , body = Http.jsonBody <| NewEntry.to_json form
        , resolver = Http.stringResolver <| resolve
          (\body -> Json.Decode.decodeString (Json.Decode.field "id" Json.Decode.int) body |> Result.mapError (\err -> Json.Decode.errorToString err))
        , timeout = Nothing
        }
        |> Task.andThen 
          (
            \id -> case model.image of
                Image_File image -> Http.task
                  { method = "PUT"
                  , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/image"
                  , headers = []
                  , body = Http.bytesBody image.content_type image.bytes
                  , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
                  , timeout = Nothing
                  }
                _ -> Task.succeed ()
          )
    in
      (model
      , Task.attempt Msg_ResponseToSendArrived task 
      )

  Err err -> 
    ({model | app_state = notify_error <| "El formulario no es válido:\n" ++ err}
    , Cmd.none
    )

-- view

row : String -> UI.Element Msg -> UI.Element Msg
row name content = UI.row [ UI.paddingEach { top = 30, bottom = 0, left = 0, right = 0 } ] 
  [ UI.el [ Font.color (rgb 1 1 1), UI.alignLeft, UI.alignTop, UI.width (px 300), UI.paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ] (UI.text name)
  , UI.el [ UI.width (px 500) ] content
  ]


entry_type_display_string : EntryType -> String
entry_type_display_string entry_type = case entry_type of
  Video _ -> "Vídeo"
  Text _ -> "Texto"

entry_type_display_fontawesome_icon : EntryType -> String
entry_type_display_fontawesome_icon entry_type = case entry_type of
  Video _ -> "\u{f03d}" -- fa-video
  Text _ -> "\u{f15b}" -- fa-file

type_combo_alternative : EntryType -> UI.Element Msg
type_combo_alternative entry_type = 
  UI.el 
  [ UI.padding 5
  ]
  (UI.row [ UI.spacing 10 ]
    [ UI.el [ Font.family [ fontawesome ] ] <| UI.text <| entry_type_display_fontawesome_icon entry_type
    , UI.text <| entry_type_display_string entry_type
    ]
  )

type_combo_button : EntryType -> Maybe ComboId -> UI.Element Msg
type_combo_button entry_type currently_open_combo = Combo.view 
  (Config.widget_common_attributes ++ [ UI.padding 5, UI.width (px 500) ])
  { combo_state = entry_type
  , id = ComboId_Type
  , alternatives = [ Text { pages = 0 }, Video { length_in_seconds = 0 } ]
  , view_alternative = type_combo_alternative
  , message = Msg_TypeCombo
  , currently_open_id = currently_open_combo
  }

backup_display_string : Backup -> String
backup_display_string backup = case backup of
  NoBackup -> "Sin copia"
  Automatic -> "Automática"
  Manual _ -> "Manual"

backup_combo_alternative : Backup -> UI.Element Msg
backup_combo_alternative backup = 
  UI.el 
  [ UI.padding 5
  ]
  (UI.text <| backup_display_string backup)

backup_row : Backup -> List (UI.Element Msg)
backup_row backup = case backup of 
  Automatic -> []
  NoBackup -> []
  Manual path -> 
    let error_color = if (path /= Nothing) then (rgb 1 1 1) else (rgb 1 0 0)
    in
    [ Input.button []
      { onPress = Nothing
      , label = UI.el (Config.widget_common_attributes ++ [ Font.family [ fontawesome ] ]) (UI.text "\u{f07c}")
      }
    , UI.el 
      (Config.widget_common_attributes ++ [ UI.width UI.fill, Border.color error_color, Font.color error_color ])
      (UI.text <| Maybe.withDefault "No hay archivo elegido" path)
    ]

backup_combo_button : Backup -> Maybe ComboId -> UI.Element Msg
backup_combo_button backup currently_open_combo = UI.row [ UI.spacing 10, UI.width UI.fill ]
  <| Combo.view 
      (Config.widget_common_attributes ++ [ UI.padding 5, UI.width (px 150) ])
      { combo_state = backup
      , id = ComboId_Backup
      , alternatives = [ NoBackup, Automatic, Manual Nothing ]
      , view_alternative = backup_combo_alternative
      , message = Msg_BackupCombo
      , currently_open_id = currently_open_combo
      }
  :: backup_row backup

exceptional_toggle_button : Bool -> UI.Element Msg
exceptional_toggle_button is_exceptional = fontawesome_text
  [ Font.size 30
  , Font.color <| if is_exceptional then (rgb 1 1 0) else (rgb 0.4 0.4 0.4)
  , Events.onClick <| Msg_ExceptionalChanged (not is_exceptional)
  , UI.pointer
  ]
  "\u{f005}" --fa-star

link_row : Model -> UI.Element Msg
link_row form = UI.row [ UI.spacing 10, UI.width UI.fill ] 
  [ UI.el [ UI.width UI.fill ] <| input_box [] form.link Msg_LinkChanged
  , exceptional_toggle_button form.exceptional 
  ]

send_button: UI.Element Msg
send_button = Input.button 
  (Config.widget_common_attributes ++
  [ Background.color (rgb 0 0.6 0)
  , Font.center
  , UI.width UI.fill
  ])
  { onPress = Just Msg_Send
  , label = UI.text "Enviar"
  }

view_main_column : Model -> UI.Element Msg
view_main_column form = UI.column 
  [ UI.alignTop
  ]
  [ row "Link"                  <| link_row form
  , row "Título"                <| input_box [] form.title Msg_TitleChanged
  , row "Autor"                 <| input_box_with_suggestions [] { text = form.author, suggestions = form.all_authors, message = Msg_AuthorChanged, id = ComboId_Author, currently_open_id = form.currently_open_combo, change_open = Msg_OpenComboChanged }
  , row "Fecha de publicación"  <| DatePicker.view Config.widget_common_attributes Msg_DatePublishedChanged form.date_published
  , row "Tipo"                  <| type_combo_button form.entry_type form.currently_open_combo
  , row "Copia de seguridad"    <| backup_combo_button form.backup form.currently_open_combo
  , row "Descripción"           <| multiline_input_box form.description Msg_DescriptionChanged
  , row ""                      <| send_button
  ]

view_image : EntryImage -> UI.Element Msg
view_image image_source = 
  let
    button_attributes = Config.widget_common_attributes ++ 
      [ Background.color <| Utils.set_alpha 0.5 Config.widget_background_color
      , UI.mouseOver [ Background.color <| Utils.set_alpha 0.5 Config.widget_hovered_background_color ]
      ]
  in
    UI.el 
      [ UI.width (px 304)
      , UI.height (px 173) 
      , Border.color Config.widget_border_color
      , Border.width 2
      , UI.inFront <| UI.row [ UI.spacing 5, UI.padding 6 ] 
        [ Input.button []
          { onPress = Just Msg_ImageFileButtonClicked
          , label = fontawesome_text button_attributes "\u{f07c}" -- folder-open
          }
        , Input.button []
          { onPress = Just Msg_ImageUrlButtonClicked
          , label = fontawesome_text button_attributes "\u{f0ac}" -- globe
          }
        , Input.button []
          { onPress = Just Msg_ImageResetButtonClicked
          , label = fontawesome_text button_attributes "\u{f1f8}" -- trash
          }
        ]
      ]
      <| case image_source of
          Image_None -> 
            UI.el
              [ UI.width (px 300)
              , UI.height (px 169)
              , Background.color Config.widget_background_color
              , UI.centerX
              , UI.centerY
              ]
              <| UI.el 
                [ UI.centerX
                , UI.centerY
                , Font.size 25
                , Font.center
                ] 
                <| UI.text "Ninguna imagen\nseleccionada"

          -- The two cases below are using Background.image instead of UI.image because Background.image crops
          -- the image to fit the size while UI.image scales it, and we don't want to show the image deformed.
          Image_Url source ->
            UI.el 
              [ UI.width (px 300)
              , UI.height (px 169)
              , Background.image source
              ]
              UI.none

          Image_File file ->
            UI.el 
              [ UI.width (px 300)
              , UI.height (px 169)
              , Background.image file.url
              ]
              UI.none

view_category : String -> List String -> Maybe ComboId -> UI.Element Msg
view_category category all_categories curently_open_combo = UI.column 
  [ Border.color Config.transparentish_widget_border_color
  , Border.width 1
  , Border.rounded 3
  , UI.padding 5
  , UI.spacing 10
  , UI.width UI.fill
  ] 
  [ UI.text "Categoría"
  , input_box_with_suggestions [ UI.width UI.fill, Font.size 15, UI.padding 7 ]
    { text = category
    , suggestions = all_categories
    , message = Msg_CategoryChanged
    , change_open = Msg_OpenComboChanged
    , id = ComboId_Category
    , currently_open_id = curently_open_combo
    }
  ]

view_side_column : Model -> UI.Element Msg
view_side_column form = UI.column 
  [ UI.alignTop
  , UI.paddingEach { top = 35, bottom = 0, left = 0, right = 0 }
  , UI.spacing 10
  ]
  [ view_image form.image
  , view_category form.category form.all_categories form.currently_open_combo
  , view_string_list form.works_mentioned form.all_works "Obras mencionadas" ComboId_WorkMentioned form.currently_open_combo Msg_WorksMentioned Msg_OpenComboChanged
  , view_string_list form.themes form.all_themes "Temas" ComboId_Theme form.currently_open_combo Msg_Themes Msg_OpenComboChanged
  , view_string_list form.tags form.all_tags "Etiquetas" ComboId_Tag form.currently_open_combo Msg_Tags Msg_OpenComboChanged
  ]

view_form : Model -> UI.Element Msg
view_form form = UI.row 
  [ UI.centerX
  , UI.spacing 40
  ]
  [ view_main_column form
  , view_side_column form
  ]

dialog : AppState -> UI.Element Msg
dialog state = case state of
  State_Editing ->
    UI.none

  State_Notify args -> 
    UI.el
      [ Background.color Config.background_color
      , Border.rounded 5
      , Border.width 3
      , Border.color Config.widget_border_color
      , UI.centerX
      , UI.centerY
      , UI.paddingXY 10 30
      , UI.spacing 20
      , UI.width (px 600)
      ]
      <| UI.column [ UI.width UI.fill ] [ UI.el [ UI.centerX ] (UI.text args.message) ]

  State_InputUrl args ->
    UI.el
      [ Background.color Config.background_color
      , Border.rounded 5
      , Border.width 3
      , Border.color Config.widget_border_color
      , UI.centerX
      , UI.centerY
      , UI.paddingXY 30 30
      , UI.spacing 20
      , UI.width (px 600)
      , Utils.on_enter <| Msg_ImageUrlChosen args.url
      ]
      <| UI.column 
        [ UI.width UI.fill
        , UI.spacing 20
        ] 
        [ UI.el [] (UI.text "Elige una URL")
        , UI.row [ UI.width UI.fill, UI.spacing 10 ] 
          [ input_box [ UI.width UI.fill ] args.url Msg_InputUrlChanged
          , Input.button (UI.mouseOver [ Background.color Config.widget_hovered_background_color ] :: Config.widget_common_attributes)
            { onPress = if Url.fromString args.url == Nothing then Just <| Msg_CloseResponseDialog False else Just <| Msg_ImageUrlChosen args.url
            , label = fontawesome_text [] "\u{f00c}" -- check
            }
          ]
        ]

dialog_background : AppState -> UI.Element Msg
dialog_background state = case state of
  State_Editing -> UI.none
  _ -> 
    let
      reset_on_close = case state of 
        State_Notify args -> args.reset_on_close
        _ -> False
    in 
      UI.el
      [ Events.onClick <| Msg_CloseResponseDialog reset_on_close
      , Background.color (rgba 0 0 0 0.75)
      , UI.width UI.fill
      , UI.height UI.fill
      ]
      UI.none

view : Model -> Document Msg
view model =
  { title = "Nueva entrada | localhost"
  , body = 
    [ UI.layout 
        [ Background.color Config.background_color
        , UI.centerX
        , UI.centerY
        , Font.color (rgb 1 1 1) 
        , UI.inFront <| dialog_background model.app_state
        , UI.inFront <| dialog model.app_state
        ]
        <| Banner.with_banners (view_form model)
    ]
  }
