module NewEntry exposing (Type(..), Form, to_json, validate)

import Calendar
import Json.Encode as Json
import Url
import DateUtils
import Utils
import Time

type Type 
  = Type_Article { pages : Int }
  | Type_Paper { pages : Int }
  | Type_Book { pages : Int }
  | Type_Video { length_in_seconds : Int }
  | Type_Audio { length_in_seconds : Int }

type alias Form =
  { link : String
  , title : String
  , description : String
  , author : String
  , category : String
  , themes : List String
  , works_mentioned : List String
  , tags : List String
  , date_published : Maybe Calendar.Date
  , exceptional : Bool
  , entry_type : Type
  }

type_to_json : Type -> Json.Value
type_to_json entry_type = Json.object
  [ case entry_type of
    Type_Article t -> ("Article", Json.object [ ("pages", Json.int t.pages) ])
    Type_Paper t -> ("Paper", Json.object [ ("pages", Json.int t.pages) ])
    Type_Book t -> ("Book", Json.object [ ("pages", Json.int t.pages) ])
    Type_Video t -> ("Video", Json.object [ ("length_in_seconds", Json.int t.length_in_seconds) ])
    Type_Audio t -> ("Audio", Json.object [ ("length_in_seconds", Json.int t.length_in_seconds) ])
  ]

to_json : Form -> Json.Value
to_json form = Json.object
  [ ("link", Json.string form.link)
  , ("title", Json.string form.title)
  , ("description", Json.string form.description)
  , ("author", Json.string form.author)
  , ("category", Json.string form.category)
  , ("themes", Json.list Json.string form.themes)
  , ("works_mentioned", Json.list Json.string form.works_mentioned)
  , ("tags", Json.list Json.string form.tags)
    -- A Nothing should never happen here because validate detects that the date is not nothing.
  , ("date_published", DateUtils.date_to_json (Maybe.withDefault (Calendar.fromPosix <| Time.millisToPosix 0) form.date_published))
  , ("exceptional", Json.bool form.exceptional)
  , ("entry_type", type_to_json form.entry_type)
  ]

is_url : String -> Bool
is_url str = case Url.fromString str of
  Just _ -> True
  Nothing -> False

make_error : List (Bool, String) -> Maybe String
make_error conditions =
  let errors = List.filterMap (\(cond, msg) -> if cond then Just msg else Nothing) conditions in
  if List.isEmpty errors
    then Nothing
    else Just <| List.foldr (++) "" (List.intersperse "\n" errors)

validate : Form -> Maybe String
validate form = make_error
  [ (not <| is_url form.link, "- La URL no es válida")
  , (String.isEmpty form.title, "- El título está vacío")
  , (String.isEmpty form.description, "- La descripción está vacía")
  , (String.isEmpty form.author, "- El autor está vacío")
  , (String.isEmpty form.category, "- La categoría está vacía")
  , (List.any String.isEmpty form.themes, "- Uno de los temas está vacío")
  , (Utils.has_duplicates form.themes, "- Uno de los temas está dos veces en la lista")
  , (List.any String.isEmpty form.works_mentioned, "- Una de las obras mencionadas está vacía")
  , (Utils.has_duplicates form.works_mentioned, "- Una de los obras mencionadas está dos veces en la lista")
  , (List.any String.isEmpty form.tags, "- Una de los etiquetas está vacía")
  , (Utils.has_duplicates form.tags, "- Una de los etiquetas está dos veces en la lista")
  , (form.date_published == Nothing, "- La fecha está vacía")
  ]