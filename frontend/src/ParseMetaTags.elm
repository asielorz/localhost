module ParseMetaTags exposing (MetaInfo, get_meta_info)

import Dict exposing (Dict)
import Calendar
import DateUtils

type alias MetaInfo =
  { title : Maybe String
  , author : Maybe String
  , date_published : Maybe Calendar.Date
  , image : Maybe String
  }

find_one_of : Dict String String -> List String -> Maybe String
find_one_of dict keys = case keys of
  [] -> Nothing
  first::rest -> case Dict.get first dict of
    Just x -> Just x
    Nothing -> find_one_of dict rest

parse_date : String -> Maybe Calendar.Date
parse_date iso_date = 
  iso_date
  |> String.split "T"
  |> List.head
  |> Maybe.andThen DateUtils.parse_date_from_url

get_meta_info : Dict String String -> MetaInfo
get_meta_info meta_tags =
  { title = find_one_of meta_tags [ "og:title", "twitter:title", "title" ]
  , author = find_one_of meta_tags [ "author", "og:author" ]
  , date_published = find_one_of meta_tags [ "article:published_time" ] |> Maybe.andThen parse_date
  , image = find_one_of meta_tags [ "og:image", "twitter:image" ]
  }
