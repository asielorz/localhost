module NewEntry exposing (Form, to_json, validate)

import EntryType exposing (EntryType(..))
import Calendar
import Json.Encode as Json
import DateUtils
import Utils
import Time

type alias Form =
  { link : String
  , title : String
  , description : String
  , authors : List String
  , category : String
  , themes : List String
  , works_mentioned : List String
  , tags : List String
  , date_published : Maybe Calendar.Date
  , exceptional : Bool
  , entry_type : EntryType
  }

to_json : Form -> Json.Value
to_json form = Json.object
  [ ("link", Json.string form.link)
  , ("title", Json.string form.title)
  , ("description", Json.string form.description)
  , ("authors", Json.list Json.string form.authors)
  , ("category", Json.string form.category)
  , ("themes", Json.list Json.string form.themes)
  , ("works_mentioned", Json.list Json.string form.works_mentioned)
  , ("tags", Json.list Json.string form.tags)
    -- A Nothing should never happen here because validate detects that the date is not nothing.
  , ("date_published", DateUtils.date_to_json (Maybe.withDefault (Calendar.fromPosix <| Time.millisToPosix 0) form.date_published))
  , ("exceptional", Json.bool form.exceptional)
  , ("entry_type", EntryType.to_json form.entry_type)
  ]

make_error : List (Bool, String) -> Maybe String
make_error conditions =
  let errors = List.filterMap (\(cond, msg) -> if cond then Just msg else Nothing) conditions in
  if List.isEmpty errors
    then Nothing
    else Just <| String.join "\n" errors

validate : Form -> Maybe String
validate form = make_error
  [ (not <| Utils.is_url form.link, "- La URL no es válida")
  , (String.isEmpty form.title, "- El título está vacío")
  , (String.isEmpty form.description, "- La descripción está vacía")
  , (List.isEmpty form.authors, "- No hay ningún autor")
  , (List.any String.isEmpty form.authors, "- El autor está vacío")
  , (String.isEmpty form.category, "- La categoría está vacía")
  , (List.any String.isEmpty form.themes, "- Uno de los temas está vacío")
  , (Utils.has_duplicates form.themes, "- Uno de los temas está dos veces en la lista")
  , (List.any String.isEmpty form.works_mentioned, "- Una de las obras mencionadas está vacía")
  , (Utils.has_duplicates form.works_mentioned, "- Una de los obras mencionadas está dos veces en la lista")
  , (List.any String.isEmpty form.tags, "- Una de los etiquetas está vacía")
  , (Utils.has_duplicates form.tags, "- Una de los etiquetas está dos veces en la lista")
  , (form.date_published == Nothing, "- La fecha está vacía")
  ]
