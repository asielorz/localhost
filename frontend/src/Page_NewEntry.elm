module Page_NewEntry exposing (Model, Msg, init, edit, update, view, title)

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
import Utils
import File exposing (File)
import File.Select
import Task exposing (Task)
import Base64
import Bytes exposing (Bytes)
import Fontawesome exposing (fontawesome_text)
import Url
import Entry exposing (Entry)
import EntryType exposing (EntryType(..))
import Json.Encode
import Json.Decode
import Dict exposing (Dict)
import ParseMetaTags

init : (Model, Cmd Msg)
init = (default_model, initial_commands)

edit : Int -> (Model, Cmd Msg)
edit id = 
  let
    default_form = 
      { link = ""
      , title = ""
      , description = ""
      , author = ""
      , category = ""
      , themes = []
      , works_mentioned = []
      , tags = []
      , date_published = Nothing
      , exceptional = False
      , entry_type = Type_Article { pages = 0 }
      }
  in
    ( { default_model 
        | edited_entry = Just { id = id, original_form = default_form, original_image = Image_None, original_backup = NoBackup }
        , app_state = State_Notify { message = "Loading...", close = do_not_close }
      }
    , Cmd.batch 
      [ initial_commands 
      , Http.get 
        { url = "http://localhost:8080/api/texts/" ++ String.fromInt id
        , expect = Http.expectJson Msg_ReceivedEntryToEdit Entry.from_json 
        }
      ]
    )

title : String
title = "Nueva entrada"

type Backup 
  = NoBackup 
  | Automatic String
  | Manual (Maybe { filename : String, content_type : String, content : Bytes })

type ComboId 
  = ComboId_Type 
  | ComboId_Backup 
  | ComboId_Author
  | ComboId_Category
  | ComboId_WorkMentioned Int
  | ComboId_Theme Int
  | ComboId_Tag Int

type alias CloseDialogFunction = Model -> (Model, Cmd Msg)

type AppState 
  = State_Editing 
  | State_Notify { message : String, close : CloseDialogFunction }
  | State_InputUrl { url : String }

type EntryImage
  = Image_None
  | Image_Url String
  | Image_File { url : String, content_type : String, bytes : Bytes }

type alias EditedEntry =
  { id : Int
  , original_form : NewEntry.Form
  , original_image : EntryImage
  , original_backup : Backup
  }

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
  , type_metadata_input_string : String

  -- If this is none, a new entry is being added. If it's set, an existing entry is being edited.
  -- This alters the behavior of the send button, which will either post or put.
  , edited_entry : Maybe EditedEntry

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
  , entry_type = Type_Article { pages = 0 }
  , exceptional = False
  , currently_open_combo = Nothing
  , app_state = State_Editing
  , type_metadata_input_string = ""

  , edited_entry = Nothing

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
  | Msg_TypeMetadataChanged String
  | Msg_BackupCombo (Combo.Msg Backup)
  | Msg_ExceptionalChanged Bool
  | Msg_WorksMentioned (ListWidget.Msg String)
  | Msg_Themes (ListWidget.Msg String)
  | Msg_Tags (ListWidget.Msg String)
  | Msg_DatePublishedChanged DatePicker.Msg
  | Msg_Send
  | Msg_Save
  | Msg_Delete
  | Msg_CloseResponseDialog CloseDialogFunction
  | Msg_OpenComboChanged (Maybe ComboId)

  -- Image
  | Msg_ImageFileButtonClicked
  | Msg_ImageFileOpened File
  | Msg_ImageLoaded { url : String, content_type : String, bytes : Bytes }
  | Msg_ImageUrlButtonClicked
  | Msg_InputUrlChanged String
  | Msg_ImageUrlChosen String
  | Msg_ImageClearButtonClicked
  | Msg_ImageResetButtonClicked

  -- Backup
  | Msg_BackupFileButtonClicked
  | Msg_BackupFileOpened File
  | Msg_BackupLoaded { filename : String, content_type : String, content : Bytes }
  | Msg_BackupAutomaticUrlChanged String

  -- Server
  | Msg_ResponseToSendArrived (Result Http.Error ())
  | Msg_ResponseToSaveArrived (Result Http.Error ())
  | Msg_ResponseToDeleteArrived (Result Http.Error ())

  | Msg_ReceivedCategories (Result Http.Error (List String))
  | Msg_ReceivedAuthors (Result Http.Error (List String))
  | Msg_ReceivedThemes (Result Http.Error (List String))
  | Msg_ReceivedWorks (Result Http.Error (List String))
  | Msg_ReceivedTags (Result Http.Error (List String))
  | Msg_ReceivedEntryToEdit (Result Http.Error Entry)

  -- Autocomplete
  | Msg_AutocompleteRequested
  | Msg_ReceivedPageToAutocomplete (Result Http.Error (Dict String String))

-- update

sides : { top : Int, bottom : Int, left : Int, right : Int }
sides = { top = 0, bottom = 0, left = 0, right = 0 }

set_entry_type : EntryType -> Model -> Model
set_entry_type entry_type model = { model | entry_type = entry_type }

set_open_combo : Maybe ComboId -> Model -> Model
set_open_combo id model = { model | currently_open_combo = id }

set_backup : Backup -> Model -> Model
set_backup backup model = { model | backup = backup }

just_close_dialog : CloseDialogFunction
just_close_dialog model = ({ model | app_state = State_Editing }, Cmd.none)

reset_form : CloseDialogFunction
reset_form model = 
  (
    { default_model 
      | edited_entry = model.edited_entry 
      , all_categories = model.all_categories
      , all_authors = model.all_authors
      , all_themes = model.all_themes
      , all_works = model.all_works
      , all_tags = model.all_tags
    }
  , Cmd.none
  )

do_not_close : CloseDialogFunction
do_not_close model = (model, Cmd.none)

notify_success : AppState
notify_success = State_Notify { message = "Nueva entrada añadida correctamente.", close = reset_form }

notify_error : String -> AppState
notify_error error_message = State_Notify { message = error_message, close = just_close_dialog }

list_of_pairs_of_strings_from_json : Json.Decode.Decoder (Dict String String)
list_of_pairs_of_strings_from_json =
  (Json.Decode.list Json.Decode.string)
  |> Json.Decode.andThen 
    (\list -> case list of
      first::second::[] -> Json.Decode.succeed (first, second)
      _ -> Json.Decode.fail "List does not have 2 elements for key and value"
    )
  |> Json.Decode.list
  |> Json.Decode.map Dict.fromList

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
    let 
      updated_model = case combo_msg of
        Combo.Msg_Select _ -> { model | type_metadata_input_string = "" }
        _ -> model
    in
      ( Combo.update 
        { set_value = set_entry_type
        , set_open_combo = set_open_combo
        , id = ComboId_Type
        , model = updated_model 
        }
        combo_msg
      , Cmd.none
      )
  
  Msg_TypeMetadataChanged new_type_metadata ->
    let
      string_to_int str = Maybe.withDefault 0 <| String.toInt str

      parse_duration duration_str = case String.split ":" duration_str of
        [] -> 0 
        seconds :: [] -> string_to_int seconds
        minutes :: seconds :: [] -> 60 * (string_to_int minutes) + (string_to_int seconds)
        hours :: minutes :: seconds :: _ -> 3600 * (string_to_int hours) + 60 * (string_to_int minutes) + (string_to_int seconds)

      only_digits = new_type_metadata
        |> String.filter Char.isDigit
        |> String.left 8

      duration = new_type_metadata
        |> Utils.remove_all_but_n ":" 2
        |> String.filter (\c -> Char.isDigit c || c == ':')
        |> String.left 8

      (fixed, updated_type) = case model.entry_type of
        Type_Article _ -> (only_digits, Type_Article { pages = string_to_int only_digits })
        Type_Paper _ -> (only_digits, Type_Paper { pages = string_to_int only_digits })
        Type_Book _ -> (only_digits, Type_Book { pages = string_to_int only_digits })
        Type_Video _ -> (duration, Type_Video { length_in_seconds = parse_duration duration })
        Type_Audio _ -> (duration, Type_Audio { length_in_seconds = parse_duration duration })
    in
      ({ model | type_metadata_input_string = fixed, entry_type = updated_type }, Cmd.none)

  Msg_BackupCombo combo_msg ->
    ( Combo.update { set_value = set_backup, set_open_combo = set_open_combo, id = ComboId_Backup, model = model} combo_msg
    , Cmd.none
    )

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

  Msg_Save ->
    save_button_clicked model

  Msg_Delete -> case model.edited_entry of
    Nothing -> (model, Cmd.none)
    Just edited_entry ->
      (model
      , Http.request
        { method = "DELETE"
        , url = "http://localhost:8080/api/texts/" ++ String.fromInt edited_entry.id
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectWhatever Msg_ResponseToDeleteArrived
        , timeout = Nothing
        , tracker = Nothing
        }
      )

  Msg_CloseResponseDialog close_function -> 
    close_function model

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
                Just "png"  -> Msg_ImageLoaded { url = "data:image/png;base64,"  ++ base64, content_type = "image/png",  bytes = bytes }
                Just "bmp"  -> Msg_ImageLoaded { url = "data:image/bmp;base64,"  ++ base64, content_type = "image/bmp",  bytes = bytes }
                Just "jpg"  -> Msg_ImageLoaded { url = "data:image/jpeg;base64," ++ base64, content_type = "image/jpeg", bytes = bytes }
                Just "jpeg" -> Msg_ImageLoaded { url = "data:image/jpeg;base64," ++ base64, content_type = "image/jpeg", bytes = bytes }
                Just "gif"  -> Msg_ImageLoaded { url = "data:image/gif;base64,"  ++ base64, content_type = "image/gif",  bytes = bytes }
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

  Msg_ImageClearButtonClicked ->
    ({ model | image = Image_None }, Cmd.none)

  Msg_ImageResetButtonClicked -> case model.edited_entry of
    Nothing -> (model, Cmd.none)
    Just edited_entry -> ({ model | image = edited_entry.original_image }, Cmd.none)

  -- Backup

  Msg_BackupFileButtonClicked -> 
    (model, File.Select.file ["application/pdf","text/html"] Msg_BackupFileOpened)

  Msg_BackupFileOpened file ->
    (model
    , File.toBytes file
      |> Task.map 
        (\bytes -> 
          let 
            path = File.name file
            extension = path |> String.split "." |> Utils.last
          in
            case extension of
              Just "pdf"  -> Msg_BackupLoaded { filename = path, content_type = "application/pdf", content = bytes }
              Just "html"  -> Msg_BackupLoaded { filename = path, content_type = "text/html", content = bytes }
              _ -> Msg_Noop
        )
      |> Task.perform identity
    )

  Msg_BackupLoaded backup -> 
    ({ model | backup = Manual <| Just backup }, Cmd.none)

  Msg_BackupAutomaticUrlChanged new_url ->
    ({ model | backup = Automatic new_url }, Cmd.none)

  -- Server

  Msg_ResponseToSendArrived response -> case response of
    Ok _ -> ({ model | app_state = notify_success }, Cmd.none)
    Err err -> ({ model | app_state = notify_error ("Error al añadir nueva entrada:\n" ++ http_error_to_string err) }, Cmd.none)

  Msg_ResponseToSaveArrived response -> case response of
    Ok _ -> ({ model | app_state = notify_error "Los cambios a la entrada se han guardado correctamente." }, Cmd.none)
    Err err -> ({ model | app_state = notify_error ("Error al guardar cambios:\n" ++ http_error_to_string err) }, Cmd.none)

  Msg_ResponseToDeleteArrived response -> case response of
    Ok _ -> ({ model | app_state = State_Notify { message = "La entrada se ha borrado.", close = do_not_close } }, Cmd.none)
    Err err -> ({ model | app_state = notify_error ("Error al borrar la entrada:\n" ++ http_error_to_string err) }, Cmd.none)

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

  Msg_ReceivedEntryToEdit result ->
    case result of
      Ok entry ->
        let
          image = case entry.image of
                Nothing -> Image_None
                Just url -> Image_Url url
          backup = case entry.backup of
                Nothing -> NoBackup
                Just url -> Automatic url
        in
          ( { model 
            | edited_entry = model.edited_entry |> Maybe.map (\e -> { e 
              | original_form =
                { link = entry.link
                , title = entry.title
                , description = entry.description
                , author = entry.author
                , category = entry.category
                , themes = entry.themes
                , works_mentioned = entry.works_mentioned
                , tags = entry.tags
                , date_published = Just entry.date_published
                , exceptional = entry.exceptional
                , entry_type = entry.entry_type
                }
              , original_image = image
              , original_backup = backup
              })
            , link = entry.link
            , title = entry.title
            , description = entry.description
            , author = entry.author
            , category = entry.category
            , themes = entry.themes
            , works_mentioned = entry.works_mentioned
            , tags = entry.tags
            , date_published = DatePicker.set_date (Just entry.date_published) model.date_published
            , entry_type = entry.entry_type
            , exceptional = entry.exceptional
            , app_state = State_Editing
            , type_metadata_input_string = entry_type_edit_metadata_string entry.entry_type
            , image = image
            , backup = backup
            }
          , Cmd.none
          )
      Err err -> ({ model | app_state = State_Notify { message = "Error al buscar entrada para editar:\n" ++ http_error_to_string err, close = do_not_close } }, Cmd.none)

  Msg_AutocompleteRequested ->
    if Utils.is_url model.link
      then
        ( model
        , Http.get
          { url = "http://localhost:8080/api/meta_headers/" ++ model.link
          , expect = Http.expectJson Msg_ReceivedPageToAutocomplete list_of_pairs_of_strings_from_json
          }
        )
      else (model, Cmd.none)

  Msg_ReceivedPageToAutocomplete result ->
    case result of
      Ok meta_headers ->
        let
          meta_info = ParseMetaTags.get_meta_info meta_headers
        in
          ( { model 
            | title = Maybe.withDefault model.title meta_info.title
            , author = Maybe.withDefault model.author meta_info.author
            , date_published = Maybe.withDefault model.date_published (Maybe.map (\d -> DatePicker.set_date (Just d) model.date_published) meta_info.date_published)
            , image = Maybe.withDefault model.image <| Maybe.map Image_Url meta_info.image
            }
          , Cmd.none
          )
      Err err -> 
        ({ model | app_state = notify_error <| "Could not autocomplete fields from url " ++ model.link ++ "\n" ++ http_error_to_string err }, Cmd.none)

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
      , entry_type = model.entry_type
      }
  in case NewEntry.validate form of
    Nothing -> Ok form
    Just error -> Err error

resolve : (body -> Result String a) -> Http.Response body -> Result Http.Error a
resolve toResult response = resolve_with_metadata (\_ b -> toResult b) response

resolve_with_metadata : (Http.Metadata -> body -> Result String a) -> Http.Response body -> Result Http.Error a
resolve_with_metadata toResult response =
        case response of
          Http.BadUrl_ url -> Err (Http.BadUrl url)
          Http.Timeout_ -> Err Http.Timeout
          Http.NetworkError_ -> Err Http.NetworkError
          Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
          Http.GoodStatus_ metadata body -> Result.mapError Http.BadBody (toResult metadata body)

-- If delete_if_none is true, then Image_None means that a DELETE requests should be sent
-- to that image resource. Otherwise, it means that Image_None is a noop. When creating a
-- new entry, there is no point in sending a request for Image_None because not having an
-- image is already the default. However, when updating an entry, the user may want to delete
-- the image of an entry and make that entry have no image, so Image_None needs to send a
-- DELETE request in that case.
put_image_task : { delete_if_none : Bool } -> EntryImage -> Int -> Task Http.Error ()
put_image_task args image id = 
  case image of
    Image_File image_file -> Http.task
      { method = "PUT"
      , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/image"
      , headers = []
      , body = Http.bytesBody image_file.content_type image_file.bytes
      , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
      , timeout = Nothing
      }
    Image_Url url -> Http.task
      { method = "PUT"
      , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/image"
      , headers = []
      , body = Http.jsonBody <| Json.Encode.object [ ("image_url", Json.Encode.string url) ]
      , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
      , timeout = Nothing
      }
    Image_None -> 
      if args.delete_if_none
        then Http.task
          { method = "DELETE"
          , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/image"
          , headers = []
          , body = Http.emptyBody
          , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
          , timeout = Nothing
          }
        else Task.succeed ()

put_backup_task : { delete_if_none : Bool } -> Backup -> Int -> Task Http.Error ()
put_backup_task args backup id = 
  case backup of
    NoBackup ->
      if args.delete_if_none
        then Http.task
          { method = "DELETE"
          , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/backup"
          , headers = []
          , body = Http.emptyBody
          , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
          , timeout = Nothing
          }
        else Task.succeed ()
    Manual maybe_file -> case maybe_file of
      Nothing -> Task.succeed ()
      Just file -> Http.task
        { method = "PUT"
        , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/backup"
        , headers = []
        , body = Http.bytesBody file.content_type file.content
        , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
        , timeout = Nothing
        }
    Automatic url -> Http.task
      { method = "GET"
      , url = url
      , headers = []
      , body = Http.emptyBody
      , resolver = Http.bytesResolver <| resolve_with_metadata 
        (\metadata bytes -> 
          case Utils.case_insensitive_dict_get "content-type" metadata.headers of
            Nothing -> Err <| "Missing Content-Type header in response from url " ++ url
            Just content_type -> Ok (content_type, bytes))
      , timeout = Nothing
      }
      |> (Task.andThen 
        (\(content_type, bytes) -> Http.task
          { method = "PUT"
          , url = "http://localhost:8080/api/texts/" ++ String.fromInt id ++ "/backup"
          , headers = []
          , body = Http.bytesBody content_type bytes
          , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
          , timeout = Nothing
          }
        )
      )

send_button_clicked : Model -> (Model, Cmd Msg)
send_button_clicked model = case make_new_entry_form model of
  Ok form -> 
    let
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
          (\id -> Task.sequence
            [ put_image_task { delete_if_none = False } model.image id
            , put_backup_task { delete_if_none = False } model.backup id
            ]
          )
        |> Task.map (always ())
    in
      (model
      , Task.attempt Msg_ResponseToSendArrived task 
      )

  Err err -> 
    ({model | app_state = notify_error <| "El formulario no es válido:\n" ++ err}
    , Cmd.none
    )

save_button_clicked : Model -> (Model, Cmd Msg)
save_button_clicked model = case make_new_entry_form model of
  Ok form -> case model.edited_entry of
    Nothing -> (model, Cmd.none)
    Just edited_entry ->
      let
        send_form_task =
          if form == edited_entry.original_form
            then Task.succeed ()
            else Http.task
              { method = "PUT"
              , url = "http://localhost:8080/api/texts/" ++ String.fromInt edited_entry.id
              , headers = []
              , body = Http.jsonBody <| NewEntry.to_json form
              , resolver = Http.stringResolver <| resolve (\_ -> Ok ())
              , timeout = Nothing
              }

        send_image_task =
          if model.image == edited_entry.original_image
            then Task.succeed ()
            else put_image_task { delete_if_none = True } model.image edited_entry.id

        send_backup_task =
          if model.backup == edited_entry.original_backup
            then Task.succeed()
            else put_backup_task { delete_if_none = True } model.backup edited_entry.id
        
        command = Task.sequence [ send_form_task, send_image_task, send_backup_task ]
          |> Task.map (\_ -> ()) -- Discard result. The result is a List () anyway so we don't really care
          |> Task.attempt Msg_ResponseToSaveArrived
      in
        (model, command)

  Err err -> 
    ({model | app_state = notify_error <| "El formulario no es válido:\n" ++ err}
    , Cmd.none
    )

-- view

row : String -> UI.Element Msg -> UI.Element Msg
row name content = UI.row [ UI.paddingEach { sides | top = 30 } ] 
  [ UI.el [ Font.color (rgb 1 1 1), UI.alignLeft, UI.alignTop, UI.width (px 300), UI.paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ] (UI.text name)
  , UI.el [ UI.width (px 500) ] content
  ]

entry_type_display_string : EntryType -> String
entry_type_display_string entry_type = case entry_type of
  Type_Article _ -> "Artículo"
  Type_Paper _ -> "Paper"
  Type_Book _ -> "Libro"
  Type_Video _ -> "Vídeo"
  Type_Audio _ -> "Audio"

entry_type_edit_metadata_string : EntryType -> String
entry_type_edit_metadata_string entry_type = case entry_type of
  Type_Article x -> String.fromInt x.pages
  Type_Paper x -> String.fromInt x.pages
  Type_Book x -> String.fromInt x.pages
  Type_Video x -> Utils.format_seconds_as_hours_minutes_seconds x.length_in_seconds
  Type_Audio x -> Utils.format_seconds_as_hours_minutes_seconds x.length_in_seconds

type_combo_alternative : EntryType -> UI.Element Msg
type_combo_alternative entry_type = 
  UI.el 
  [ UI.padding 5
  ]
  (UI.row [ UI.spacing 10 ]
    [ fontawesome_text [] <| EntryType.fontawesome_icon entry_type
    , UI.text <| entry_type_display_string entry_type
    ]
  )

type_hint_text : EntryType -> UI.Element msg
type_hint_text entry_type = 
  let
    text = case entry_type of
      Type_Article _ -> "páginas"
      Type_Paper _ -> "páginas"
      Type_Book _ -> "páginas"
      Type_Video _ -> "segundos"
      Type_Audio _ -> "segundos"
  in
    UI.el 
      [ UI.centerY
      , UI.alignRight
      , Font.color (rgb 0.7 0.7 0.7)
      , UI.paddingEach { sides | right = 10 }
      ]
      <| UI.text text

type_row : EntryType -> String -> List (UI.Element Msg)
type_row entry_type input_string = 
  [ input_box [ UI.inFront <| type_hint_text entry_type ] input_string Msg_TypeMetadataChanged
  ]

type_combo_button : EntryType -> String -> Maybe ComboId -> UI.Element Msg
type_combo_button entry_type input_string currently_open_combo = UI.row [ UI.spacing 10, UI.width UI.fill ]
  (Combo.view 
    (Config.widget_common_attributes ++ [ UI.padding 5, UI.width (px 150) ])
    { combo_state = entry_type
    , id = ComboId_Type
    , alternatives = 
      [ Type_Article { pages = 0 }
      , Type_Paper { pages = 0 }
      , Type_Book { pages = 0 }
      , Type_Video { length_in_seconds = 0 }
      , Type_Audio { length_in_seconds = 0 }
      ]
    , view_alternative = type_combo_alternative
    , message = Msg_TypeCombo
    , currently_open_id = currently_open_combo
    }
  :: type_row entry_type input_string)

backup_display_string : Backup -> String
backup_display_string backup = case backup of
  NoBackup -> "Sin copia"
  Automatic _ -> "Automática"
  Manual _ -> "Manual"

backup_combo_alternative : Backup -> UI.Element Msg
backup_combo_alternative backup = 
  UI.el 
  [ UI.padding 5
  ]
  (UI.text <| backup_display_string backup)

backup_row : Backup -> List (UI.Element Msg)
backup_row backup = case backup of 
  Automatic link -> [ input_box [] link Msg_BackupAutomaticUrlChanged ]
  NoBackup -> []
  Manual file -> 
    [ Input.button []
      { onPress = Just Msg_BackupFileButtonClicked
      , label = UI.el (Config.widget_common_attributes ++ [ Font.family [ fontawesome ] ]) (UI.text "\u{f07c}")
      }
    , case file of 
      Nothing -> UI.el 
        (Config.widget_common_attributes ++ [ UI.width UI.fill, Border.color (rgb 1 0 0), Font.color (rgb 1 0 0) ])
        (UI.text "No hay archivo elegido")
      Just actual_file ->
        input_box [] actual_file.filename (always Msg_Noop)
    ]

backup_combo_button : Backup -> String -> Maybe ComboId -> UI.Element Msg
backup_combo_button backup link currently_open_combo = UI.row [ UI.spacing 10, UI.width UI.fill ]
  <| Combo.view 
      (Config.widget_common_attributes ++ [ UI.padding 5, UI.width (px 150) ])
      { combo_state = backup
      , id = ComboId_Backup
      , alternatives = [ NoBackup, Automatic link, Manual Nothing ]
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
  [ UI.el [ UI.width UI.fill ] <| input_box [ Utils.on_enter Msg_AutocompleteRequested ] form.link Msg_LinkChanged
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

save_and_delete_buttons: UI.Element Msg
save_and_delete_buttons = UI.column 
  [ UI.spacing 10
  , UI.width UI.fill
  ]
  [ Input.button 
      (Config.widget_common_attributes ++
      [ Background.color (rgb 0 0.6 0)
      , Font.center
      , UI.width UI.fill
      ])
      { onPress = Just Msg_Save
      , label = UI.text "Guardar"
      }
  , Input.button 
      (Config.widget_common_attributes ++
      [ Background.color (rgb 0.8 0 0)
      , Font.center
      , UI.width UI.fill
      ])
      { onPress = Just Msg_Delete
      , label = UI.text "Borrar"
      }
  ]

view_main_column : Model -> UI.Element Msg
view_main_column form = UI.column 
  [ UI.alignTop
  ]
  [ row "Link"                  <| link_row form
  , row "Título"                <| input_box [] form.title Msg_TitleChanged
  , row "Autor"                 <| input_box_with_suggestions [] { text = form.author, suggestions = form.all_authors, message = Msg_AuthorChanged, id = ComboId_Author, currently_open_id = form.currently_open_combo, change_open = Msg_OpenComboChanged }
  , row "Fecha de publicación"  <| DatePicker.view Config.widget_common_attributes Msg_DatePublishedChanged form.date_published
  , row "Tipo"                  <| type_combo_button form.entry_type form.type_metadata_input_string form.currently_open_combo
  , row "Copia de seguridad"    <| backup_combo_button form.backup form.link form.currently_open_combo
  , row "Descripción"           <| multiline_input_box form.description Msg_DescriptionChanged
  , row ""                      <| if form.edited_entry == Nothing then send_button else save_and_delete_buttons
  ]

view_image : Maybe EditedEntry -> EntryImage -> UI.Element Msg
view_image edited_entry image_source = 
  UI.el 
    [ UI.width (px 304)
    , UI.height (px 173) 
    , Border.color Config.widget_border_color
    , Border.width 2
    , UI.inFront <| UI.row [ UI.spacing 5, UI.padding 6 ] 
      [ Input.button []
        { onPress = Just Msg_ImageFileButtonClicked
        , label = fontawesome_text Config.image_button_attributes "\u{f07c}" -- folder-open
        }
      , Input.button []
        { onPress = Just Msg_ImageUrlButtonClicked
        , label = fontawesome_text Config.image_button_attributes "\u{f0ac}" -- globe
        }
      , Input.button []
        { onPress = Just Msg_ImageClearButtonClicked
        , label = fontawesome_text Config.image_button_attributes "\u{f1f8}" -- trash
        }
      , case edited_entry of
        Nothing -> UI.none
        Just entry ->
          if entry.original_image /= image_source
            then Input.button []
              { onPress = Just Msg_ImageResetButtonClicked
              , label = fontawesome_text Config.image_button_attributes "\u{f0e2}" -- arrow-rotate-left
              }
            else UI.none
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
  , UI.paddingEach { sides | top = 35 }
  , UI.spacing 10
  ]
  [ view_image form.edited_entry form.image
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
      <| UI.column [ UI.width UI.fill ] [ UI.paragraph [ UI.centerX ] [ UI.text args.message ] ]

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
            { onPress = if Url.fromString args.url == Nothing then Just <| Msg_CloseResponseDialog just_close_dialog else Just <| Msg_ImageUrlChosen args.url
            , label = fontawesome_text [] "\u{f00c}" -- check
            }
          ]
        ]

dialog_background : AppState -> UI.Element Msg
dialog_background state = case state of
  State_Editing -> UI.none
  _ -> 
    let
      close_function = case state of 
        State_Notify args -> args.close
        _ -> just_close_dialog
    in 
      UI.el
      [ Events.onClick <| Msg_CloseResponseDialog close_function
      , Background.color (rgba 0 0 0 0.75)
      , UI.width UI.fill
      , UI.height UI.fill
      ]
      UI.none

view : Model -> UI.Element Msg
view model = UI.el 
  [ UI.inFront <| dialog_background model.app_state
  , UI.inFront <| dialog model.app_state
  , UI.width UI.fill
  , UI.height UI.fill
  ]
  <| UI.el
    [ UI.width UI.fill
    , UI.alignTop
    ]
    <| view_form model
