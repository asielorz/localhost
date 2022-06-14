module SearchQuery exposing (empty_query, search_query)
import Calendar
import Url.Builder
import Utils
import DateUtils

date_to_string : Calendar.Date -> String
date_to_string date = 
  String.fromInt (Calendar.getDay date) ++
  "-" ++
  String.fromInt (DateUtils.month_to_int <| Calendar.getMonth date) ++
  "-" ++
  String.fromInt (Calendar.getYear date)

type alias QueryArgs =
  { link : String
  , title : String
  , author : String
  , description : String
  , category : String
  , works_mentioned : List String
  , themes : List String
  , tags : List String
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
  , published_between_from = Nothing
  , published_between_until = Nothing
  , saved_between_from = Nothing
  , saved_between_until = Nothing
  , exceptional = False
  }

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
      |> add_date "published_between_from" args.published_between_from
      |> add_date "published_between_until" args.published_between_until
      |> add_date "saved_between_from" args.saved_between_from
      |> add_date "saved_between_until" args.saved_between_until
      |> Utils.add_if args.exceptional (Url.Builder.string "exceptional" "true")
  in
    Url.Builder.crossOrigin "http://localhost:8080" [ "texts" ] parameters
  