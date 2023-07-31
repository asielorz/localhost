module Metadata exposing (..)

import Json.Decode as Json
import Dict exposing (Dict)

metadata_map_from_json : Json.Decoder (Dict String (List String))
metadata_map_from_json = 
  let
    parse_element = Json.map2 Tuple.pair
      (Json.field "value" Json.string) 
      (Json.field "category" Json.string)

    add_or_create_new item maybe_list = case maybe_list of
      Nothing -> Just [item]
      Just list -> Just <| item :: list
    accumulate_into_dict (value, category) dict = dict |> Dict.update category (add_or_create_new value)
    raw_element_list_to_dict list = list
      |> List.foldl accumulate_into_dict Dict.empty
      |> Dict.map (\_ -> List.sort)
  in
    Json.list parse_element |> Json.map raw_element_list_to_dict
