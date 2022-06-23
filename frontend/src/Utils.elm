module Utils exposing (
  add_if, enumerate, at, remove_at, replace_at, chunk, adjacent, has_duplicates, last,
  toupper_first, remove_all_but_n,
  missing_to_be_a_multiple_of, 
  fail_if_nothing, 
  grid, on_enter, set_alpha
  )

import Json.Decode
import Element as UI
import Html.Events
import String exposing (indices)

---------------------------------------------------------------------------------------------------------
-- operations on lists

add_if : Bool -> a -> List a -> List a
add_if cond elem list = if cond
  then list ++ [elem]
  else list

enumerate_impl : Int -> List a -> List (Int, a)
enumerate_impl index list = case list of
  [] -> []
  (first::rest) -> (index, first) :: enumerate_impl (index + 1) rest

enumerate : List a -> List (Int, a)
enumerate list = enumerate_impl 0 list

at : Int -> List a -> Maybe a
at index list = list |> List.drop index |> List.head

remove_at : Int -> List a -> List a
remove_at index list = List.take index list ++ List.drop (index + 1) list

replace_at : Int -> a -> List a -> List a
replace_at index new_value list = 
  if (index < 0 || index >= List.length list)
    then list
    else List.take index list ++ new_value :: List.drop (index + 1) list

chunk : Int -> List a -> List (List a)
chunk size list =
  if size == 0
  then []
  else
    if List.length list <= size
      then [list]
      else (List.take size list) :: chunk size (List.drop size list)

adjacent : List a -> List (a, a)
adjacent l = case l of 
  [] -> []
  (_::[]) -> []
  (first::second::rest) -> (first, second) :: adjacent (second::rest)

has_duplicates : List comparable -> Bool
has_duplicates l = l
  |> List.sort
  |> adjacent
  |> List.any (\(a, b) -> a == b)

last : List a -> Maybe a
last l = List.head <| List.drop ((List.length l) - 1) l

---------------------------------------------------------------------------------------------------------
-- operations on strings

toupper_first : String -> String
toupper_first str =
  if String.isEmpty str
    then ""
    else String.toUpper (String.left 1 str) ++ (String.dropLeft 1 str) 

remove_all_but_n : String -> Int -> String -> String
remove_all_but_n to_remove max_repetitions str = 
  let
    indices = String.indices to_remove str
  in
    case at max_repetitions indices of
      Nothing -> str
      Just index -> String.left index str ++ String.replace to_remove "" (String.dropLeft index str)

---------------------------------------------------------------------------------------------------------
-- operations on numbers

-- throws an exception when divisor is 0.
missing_to_be_a_multiple_of : Int -> Int -> Int
missing_to_be_a_multiple_of multiplier num = 
  let mod = modBy multiplier num in
  if mod == 0
    then 0
    else multiplier - mod

---------------------------------------------------------------------------------------------------------
-- operations on json types

-- Given a decoder that returns a maybe, makes it fail when the maybe is nothing.
fail_if_nothing : String -> Json.Decode.Decoder (Maybe a) -> Json.Decode.Decoder a
fail_if_nothing error_message decoder = 
  Json.Decode.andThen 
    (\maybe_x -> case maybe_x of 
      Just x -> Json.Decode.succeed x
      Nothing -> Json.Decode.fail error_message
    )
    decoder

---------------------------------------------------------------------------------------------------------
-- operations on UI elements

grid : 
  { column_attributes : List (UI.Attribute msg) 
  , row_attributes : List (UI.Attribute msg)
  , view_element : (a -> UI.Element msg)
  , grid_elements : List (List a)
  }
  -> UI.Element msg
grid args = UI.column 
  args.column_attributes 
  <| List.map (\row_elements -> UI.row args.row_attributes <| List.map args.view_element row_elements) args.grid_elements 

-- copied from https://ellie-app.com/5X6jBKtxzdpa1 mentioned in https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Input
on_enter : msg -> UI.Attribute msg
on_enter msg =
  UI.htmlAttribute
    (Html.Events.on "keyup"
      (Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
              if key == "Enter" then
                Json.Decode.succeed msg

              else
              Json.Decode.fail "Not the enter key"
            )
      )
    )

set_alpha : Float -> UI.Color -> UI.Color
set_alpha alpha color = 
  let rgb = UI.toRgb color in
  UI.fromRgb { rgb | alpha = alpha } 
