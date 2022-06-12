module Utils exposing (const, add_if, enumerate, remove_at, replace_at, toupper_first, missing_to_be_a_multiple_of, chunk)

const : a -> b -> a
const x _ = x

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

remove_at : Int -> List a -> List a
remove_at index list = List.take index list ++ List.drop (index + 1) list

replace_at : Int -> a -> List a -> List a
replace_at index new_value list = 
  if (index < 0 || index >= List.length list)
    then list
    else List.take index list ++ new_value :: List.drop (index + 1) list

toupper_first : String -> String
toupper_first str =
  if String.isEmpty str
    then ""
    else String.toUpper (String.left 1 str) ++ (String.dropLeft 1 str) 

-- throws an exception when divisor is 0.
missing_to_be_a_multiple_of : Int -> Int -> Int
missing_to_be_a_multiple_of multiplier num = 
  let mod = modBy multiplier num in
  if mod == 0
    then 0
    else multiplier - mod

chunk : Int -> List a -> List (List a)
chunk size list =
  if size == 0
  then []
  else
    if List.length list <= size
      then [list]
      else (List.take size list) :: chunk size (List.drop size list)
