module NewEntry exposing (Form, to_json, validate)

import Calendar
import Json.Encode as Json
import Time
import Url

type alias Form =
  { link : String
  , title : String
  , description : String
  , author : String
  , category : String
  , themes : List String
  , works_mentioned : List String
  , tags : List String
  , date_published : Calendar.Date
  , exceptional : Bool
  }

month_to_json_string : Time.Month -> String
month_to_json_string month = case month of 
  Time.Jan -> "January"
  Time.Feb -> "February"
  Time.Mar -> "March"
  Time.Apr -> "April"
  Time.May -> "May"
  Time.Jun -> "June"
  Time.Jul -> "July"
  Time.Aug -> "August"
  Time.Sep -> "September"
  Time.Oct -> "October"
  Time.Nov -> "November"
  Time.Dec -> "December"

-- export date to json in the format expected by the backend
date_to_json : Calendar.Date -> Json.Value
date_to_json date = Json.object
  [ ("day", Json.int <| Calendar.getDay date)
  , ("month", Json.string <| month_to_json_string <| Calendar.getMonth date)
  , ("year", Json.int <| Calendar.getYear date)
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
  , ("date_published", date_to_json form.date_published)
  , ("exceptional", Json.bool form.exceptional)
  ]

is_url : String -> Bool
is_url str = case Url.fromString str of
  Just _ -> True
  Nothing -> False

any_is_empty : List String -> Bool
any_is_empty strings = List.any String.isEmpty strings

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
  , (List.any String.isEmpty form.works_mentioned, "- Una de las obras mencionadas está vacía")
  , (List.any String.isEmpty form.tags, "- Una de los etiquetas está vacía")
  ]
