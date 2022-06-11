module Main exposing (main)

import Browser exposing (Document)
import Element as UI exposing (rgb, rgba, px)
import Element.Background as Background
import Element.Input as Input
import Element.Font as Font
import Element.Border as Border
import Element.Input exposing (labelHidden)
import Combo
import ListWidget
import Fontawesome exposing (fontawesome)
import Element.Events as Events
import DatePicker
import Time

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

type ComboId = ComboId_Type | ComboId_Backup

type alias NewEntryForm = 
  { link : String
  , title : String
  , backup : Backup
  , image : Maybe String
  , description : String
  , author : String
  , category : String
  , themes : List String
  , works_mentioned : List String
  , tags : List String
  , date_publised : DatePicker.State
  , entry_type : EntryType
  , exceptional : Bool
  , currently_open_combo : Maybe ComboId
  }

type alias Model = NewEntryForm

default_model : Model
default_model = 
  { link = ""
  , title = ""
  , backup = NoBackup
  , image = Nothing
  , description = ""
  , author = ""
  , category = ""
  , themes = []
  , works_mentioned = []
  , tags = []
  , date_publised = DatePicker.make { day = 11, month = Time.May, year = 2022 }
  , entry_type = Text { pages = 0 }
  , exceptional = False
  , currently_open_combo = Nothing
  }

initial_commands : Cmd Msg
initial_commands = Cmd.none

type Msg 
  = Msg_Noop
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

set_entry_type : EntryType -> Model -> Model
set_entry_type entry_type model = { model | entry_type = entry_type }

set_open_combo : Maybe ComboId -> Model -> Model
set_open_combo id model = { model | currently_open_combo = id }

set_backup : Backup -> Model -> Model
set_backup backup model = { model | backup = backup }

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
    ({ model | date_publised = DatePicker.update date_picker_msg model.date_publised }, Cmd.none)

row : String -> UI.Element Msg -> UI.Element Msg
row name content = UI.row [ UI.paddingEach { top = 30, bottom = 0, left = 0, right = 0 } ] 
  [ UI.el [ Font.color (rgb 1 1 1), UI.alignLeft, UI.alignTop, UI.width (px 300), UI.paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ] (UI.text name)
  , UI.el [ UI.width (px 500) ] content
  ]

widget_background_color : UI.Color
widget_background_color = (rgb 0.129 0.129 0.129)

widget_border_color : UI.Color
widget_border_color = (rgb 0.729 0.729 0.729)

transparentish_widget_border_color : UI.Color
transparentish_widget_border_color = (rgba 0.729 0.729 0.729 0.5)

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

input_box : List (UI.Attribute msg) -> String -> (String -> msg) -> UI.Element msg
input_box attributes text onChange = Input.text
  (widget_common_attributes ++ attributes)
  { onChange = onChange
  , text = text
  , placeholder = Nothing
  , label = labelHidden ""
  }

multiline_input_box : String -> (String -> Msg) -> UI.Element Msg
multiline_input_box text onChange = Input.multiline
  (widget_common_attributes ++
    [ Font.size 13
    , UI.height (px 200)
    ])
  { onChange = onChange
  , text = text
  , placeholder = Nothing
  , label = labelHidden ""
  , spellcheck = True
  }

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
  (widget_common_attributes ++ [ UI.padding 5, UI.width (px 500) ])
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
      , label = UI.el (widget_common_attributes ++ [ Font.family [ fontawesome ] ]) (UI.text "\u{f07c}")
      }
    , UI.el 
      (widget_common_attributes ++ [ UI.width (px 287), Border.color error_color, Font.color error_color ])
      (UI.text <| Maybe.withDefault "No hay archivo elegido" path)
    ]

backup_combo_button : Backup -> Maybe ComboId -> UI.Element Msg
backup_combo_button backup currently_open_combo = UI.row [ UI.spacing 10 ]
  <| Combo.view 
      (widget_common_attributes ++ [ UI.padding 5, UI.width (px 150) ])
      { combo_state = backup
      , id = ComboId_Backup
      , alternatives = [ NoBackup, Automatic, Manual Nothing ]
      , view_alternative = backup_combo_alternative
      , message = Msg_BackupCombo
      , currently_open_id = currently_open_combo
      }
  :: backup_row backup

exceptional_toggle_button : Bool -> UI.Element Msg
exceptional_toggle_button is_exceptional = UI.el 
  [ Font.family [ fontawesome ]
  , Font.size 30
  , Font.color <| if is_exceptional then (rgb 1 1 0) else (rgb 0.4 0.4 0.4)
  , Events.onClick <| Msg_ExceptionalChanged (not is_exceptional)
  ]
  <| UI.text "\u{f005}" --fa-star

link_row : NewEntryForm -> UI.Element Msg
link_row form = UI.row [ UI.spacing 10 ] 
  [ UI.el [ UI.width (px 460) ] <| input_box [] form.link Msg_LinkChanged
  , exceptional_toggle_button form.exceptional 
  ]

send_button: UI.Element Msg
send_button = Input.button 
  (widget_common_attributes ++
  [ Background.color (rgb 0 0.6 0)
  , Font.center
  , UI.width (px 500)
  ])
  { onPress = Nothing
  , label = UI.text "Enviar"
  }

view_main_column : NewEntryForm -> UI.Element Msg
view_main_column form = UI.column 
  [ UI.alignTop
  ]
  [ row "Link"                  <| link_row form
  , row "Título"                <| input_box [] form.title Msg_TitleChanged
  , row "Autor"                 <| input_box [] form.author Msg_AuthorChanged
  , row "Fecha de publicación"  <| DatePicker.view widget_common_attributes Msg_DatePublishedChanged form.date_publised
  , row "Tipo"                  <| type_combo_button form.entry_type form.currently_open_combo
  , row "Copia de seguridad"    <| backup_combo_button form.backup form.currently_open_combo
  , row "Descripción"           <| multiline_input_box form.description Msg_DescriptionChanged
  , row ""                      <| send_button
  ]

view_image : Maybe String -> UI.Element Msg
view_image image_source = UI.el 
  [ UI.width (px 304)
  , UI.height (px 173) 
  , Border.color widget_border_color
  , Border.width 2
  ]
  <| case image_source of
      Nothing -> 
        UI.el
          [ UI.width (px 300)
          , UI.height (px 169)
          , Background.color widget_background_color
          , UI.centerX
          , UI.centerY
          , UI.inFront <| UI.row [ UI.spacing 5, UI.padding 5 ] 
            [ Input.button []
              { onPress = Nothing
              , label = UI.el (widget_common_attributes ++ [ Font.family [ fontawesome ] ]) (UI.text "\u{f07c}")
              }
            , Input.button []
              { onPress = Nothing
              , label = UI.el (widget_common_attributes ++ [ Font.family [ fontawesome ] ]) (UI.text "\u{f0ac}")
              }
            ]
          ]
          <| UI.el 
            [ UI.centerX
            , UI.centerY
            , Font.size 25
            , Font.center
            ] 
            <| UI.text "Ninguna imagen\nseleccionada"

      Just source ->
        UI.image 
          [ UI.width (px 300)
          , UI.height (px 169)
          ]
          { src = source
          , description = "" 
          }

view_category : String -> UI.Element Msg
view_category category = UI.column 
  [ Border.color transparentish_widget_border_color
  , Border.width 1
  , Border.rounded 3
  , UI.padding 5
  , UI.spacing 10
  ] 
  [ UI.text "Categoría"
  , input_box [ UI.width (px 285), Font.size 15, UI.padding 7 ] category Msg_CategoryChanged 
  ]

view_string_list : List String -> String -> (ListWidget.Msg String -> Msg) -> UI.Element Msg
view_string_list list name message = UI.el 
  [ Border.color transparentish_widget_border_color
  , Border.width 1
  , Border.rounded 3
  , UI.padding 5
  ]
  <| ListWidget.view 
    { state = list
    , name = name
    , default = ""
    , view_element = (\s -> input_box [ Font.size 15, UI.padding 7 ] s (\x -> x))
    , message = message
    , button_attributes = widget_common_attributes ++ [ Font.size 15, UI.padding 7 ]
    , width = px 250
    }

view_side_column : NewEntryForm -> UI.Element Msg
view_side_column form = UI.column 
  [ UI.alignTop
  , UI.paddingEach { top = 35, bottom = 0, left = 0, right = 0 }
  , UI.spacing 10
  ]
  [ view_image form.image
  , view_category form.category
  , view_string_list form.works_mentioned "Obras mencionadas" Msg_WorksMentioned
  , view_string_list form.themes "Temas" Msg_Themes
  , view_string_list form.tags "Etiquetas" Msg_Tags
  ]

view_form : NewEntryForm -> UI.Element Msg
view_form form = UI.row 
  [ UI.centerX
  , UI.spacing 40
  ]
  [ view_main_column form
  , view_side_column form
  ]

view : Model -> Document Msg
view model =
  { title = "Nueva entrada"
  , body = 
    [ UI.layout 
        [ Background.color (rgb 0.094 0.094 0.094), UI.centerX, UI.centerY, Font.color (rgb 1 1 1) ] 
        (view_form model) 
    ]
  }
