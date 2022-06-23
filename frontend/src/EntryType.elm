module EntryType exposing (EntryType(..), to_json, from_json, fontawesome_icon)

import Json.Encode
import Json.Decode

type EntryType 
  = Type_Article { pages : Int }
  | Type_Paper { pages : Int }
  | Type_Book { pages : Int }
  | Type_Video { length_in_seconds : Int }
  | Type_Audio { length_in_seconds : Int }

fontawesome_icon : EntryType -> String
fontawesome_icon entry_type = case entry_type of
  Type_Article _ -> "\u{f15b}" -- fa-file
  Type_Paper _ -> "\u{f15c}" -- fa-file-lines
  Type_Book _ -> "\u{f02d}" -- fa-book
  Type_Video _ -> "\u{f03d}" -- fa-video
  Type_Audio _ -> "\u{f027}" -- fa-volume-low

to_json : EntryType -> Json.Encode.Value
to_json entry_type = Json.Encode.object
  [ case entry_type of
    Type_Article t -> ("Article", Json.Encode.object [ ("pages", Json.Encode.int t.pages) ])
    Type_Paper t -> ("Paper", Json.Encode.object [ ("pages", Json.Encode.int t.pages) ])
    Type_Book t -> ("Book", Json.Encode.object [ ("pages", Json.Encode.int t.pages) ])
    Type_Video t -> ("Video", Json.Encode.object [ ("length_in_seconds", Json.Encode.int t.length_in_seconds) ])
    Type_Audio t -> ("Audio", Json.Encode.object [ ("length_in_seconds", Json.Encode.int t.length_in_seconds) ])
  ]

from_json : Json.Decode.Decoder EntryType
from_json = 
  let
    pages x = { pages = x }
    length_in_seconds x = { length_in_seconds = x }
  in
    Json.Decode.oneOf
      [ Json.Decode.field "Article" <| Json.Decode.map (Type_Article << pages) <| Json.Decode.field "pages" Json.Decode.int
      , Json.Decode.field "Paper" <| Json.Decode.map (Type_Paper << pages) <| Json.Decode.field "pages" Json.Decode.int
      , Json.Decode.field "Book" <| Json.Decode.map (Type_Book << pages) <| Json.Decode.field "pages" Json.Decode.int
      , Json.Decode.field "Video" <| Json.Decode.map (Type_Video << length_in_seconds) <| Json.Decode.field "length_in_seconds" Json.Decode.int
      , Json.Decode.field "Audio" <| Json.Decode.map (Type_Audio << length_in_seconds) <| Json.Decode.field "length_in_seconds" Json.Decode.int
      ]
