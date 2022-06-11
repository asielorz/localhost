module InputInt exposing (State, Msg(..), make, make_empty, update, view)

import Element as UI
import Element.Input as Input

type alias State =
  { value : Int
  , text : String
  }

type Msg
 = Msg_Input String

make : Int -> State
make value = { value = value, text = String.fromInt value }

make_empty : State
make_empty = { value = 0, text = "" }

update : Msg -> State -> State
update msg state = case msg of
  Msg_Input new_text -> 
    if String.isEmpty new_text
      then { state | text = "" }
      else case String.toInt new_text of
        Nothing -> state
        Just new_value -> { value = new_value, text = new_text }

view : List (UI.Attribute msg) -> { state : State, message : (Msg -> msg), placeholder : Maybe (Input.Placeholder msg), label : Input.Label msg } -> UI.Element msg
view attributes args = Input.text attributes 
  { text = args.state.text
  , placeholder = args.placeholder
  , onChange = args.message << Msg_Input 
  , label = args.label
  } 
