module SearchQuery exposing (EntryTypeToSearch(..), empty_query, search_query, search_ui_state_from_query)

import Calendar
import Url.Builder
import Utils
import DateUtils exposing (parse_date_from_url)
import Url.Parser exposing (query)
import Url

type EntryTypeToSearch
  = TypeToSearch_Any
  | TypeToSearch_Article
  | TypeToSearch_Paper
  | TypeToSearch_Book
  | TypeToSearch_Video
  | TypeToSearch_Audio

type alias QueryArgs =
  { link : String
  , title : String
  , author : String
  , description : String
  , category : String
  , works_mentioned : List String
  , themes : List String
  , tags : List String
  , type_to_search : EntryTypeToSearch
  , published_between_from : Maybe Calendar.Date
  , published_between_until : Maybe Calendar.Date
  , saved_between_from : Maybe Calendar.Date
  , saved_between_until : Maybe Calendar.Date
  , exceptional : Bool
  }

empty_query : QueryArgs
empty_query = 
  { link = ""
  , title = ""
  , author = ""
  , description = ""
  , category = ""
  , works_mentioned = []
  , themes = []
  , tags  = []
  , type_to_search = TypeToSearch_Any
  , published_between_from = Nothing
  , published_between_until = Nothing
  , saved_between_from = Nothing
  , saved_between_until = Nothing
  , exceptional = False
  }

date_to_string : Calendar.Date -> String
date_to_string date = 
  String.fromInt (Calendar.getYear date) ++
  "-" ++
  String.fromInt (DateUtils.month_to_int <| Calendar.getMonth date) ++
  "-" ++
  String.fromInt (Calendar.getDay date)

type_to_string : EntryTypeToSearch -> String
type_to_string t = case t of
  TypeToSearch_Any      -> "any"
  TypeToSearch_Article  -> "article"
  TypeToSearch_Paper    -> "paper"
  TypeToSearch_Book     -> "book"
  TypeToSearch_Video    -> "video"
  TypeToSearch_Audio    -> "audio"

string_to_type : String -> Maybe EntryTypeToSearch
string_to_type str = case str of
  "any"     -> Just TypeToSearch_Any      
  "article" -> Just TypeToSearch_Article  
  "paper"   -> Just TypeToSearch_Paper    
  "book"    -> Just TypeToSearch_Book     
  "video"   -> Just TypeToSearch_Video    
  "audio"   -> Just TypeToSearch_Audio
  _         -> Nothing

search_query : QueryArgs -> String
search_query args = 
  let
    add_string name str = Utils.add_if (not <| String.isEmpty str) (Url.Builder.string name str)

    add_list name list = 
      let filtered_list = List.filter (not << String.isEmpty) list in
      let as_string = filtered_list |> List.intersperse "|" |> List.foldr (++) "" in
      Utils.add_if (not <| List.isEmpty filtered_list) (Url.Builder.string name as_string)

    add_date name maybe_date = case maybe_date of
      Just date -> Utils.add_if True (Url.Builder.string name (date_to_string date))
      Nothing -> identity

    parameters = []
      |> add_string "link" args.link
      |> add_string "title" args.title
      |> add_string "author" args.author
      |> add_string "description" args.description
      |> add_string "category" args.category
      |> add_list "works_mentioned" args.works_mentioned
      |> add_list "themes" args.themes
      |> add_list "tags" args.tags
      |> Utils.add_if (args.type_to_search /= TypeToSearch_Any) (Url.Builder.string "type" (type_to_string args.type_to_search))
      |> add_date "published_between_from" args.published_between_from
      |> add_date "published_between_until" args.published_between_until
      |> add_date "saved_between_from" args.saved_between_from
      |> add_date "saved_between_until" args.saved_between_until
      |> Utils.add_if args.exceptional (Url.Builder.string "exceptional" "true")
  in
    Url.Builder.toQuery parameters

parse_bool_from_url : String -> Maybe Bool
parse_bool_from_url parameter = case parameter of 
  "true" -> Just True
  "false" -> Just False
  _ -> Nothing

apply_changes_to_query : String -> QueryArgs -> Maybe QueryArgs
apply_changes_to_query parameter query = 
  let key_value = String.split "=" parameter in
  case key_value of
    (key::value::[]) -> case key of
      "link" -> Just { query | link = value }
      "title" -> Just { query | title = value }
      "author" -> Just { query | author = value }
      "description" -> Just { query | description = value }
      "type" -> string_to_type value |> Maybe.map (\t -> { query | type_to_search = t })
      "category" -> Just { query | category = value }
      "works_mentioned" -> Just { query | works_mentioned = String.split "|" value }
      "themes" -> Just { query | themes = String.split "|" value }
      "tags" -> Just { query | tags = String.split "|" value }
      "published_between_from" -> parse_date_from_url value |> Maybe.map (\date -> { query | published_between_from = Just date })
      "published_between_until" -> parse_date_from_url value |> Maybe.map (\date -> { query | published_between_until = Just date })
      "saved_between_from" -> parse_date_from_url value |> Maybe.map (\date -> { query | saved_between_from = Just date })
      "saved_between_until" -> parse_date_from_url value |> Maybe.map (\date -> { query | saved_between_until = Just date })
      "exceptional" -> parse_bool_from_url value |> Maybe.map (\b -> { query | exceptional = b })
      _ -> Nothing
    _ -> Nothing

search_ui_state_from_query : String -> Maybe QueryArgs
search_ui_state_from_query query_string = query_string
  |> Url.percentDecode
  |> Maybe.map (String.split "&")
  |> Maybe.map (List.map apply_changes_to_query)
  |> Maybe.andThen (List.foldr (\f q -> Maybe.andThen f q) (Just empty_query))
