module ListWidget exposing (Msg(..), IntermediateMsg(..), update, view)

import Element as UI
import Element.Input as Input
import Element.Font as Font
import Fontawesome exposing (fontawesome)
import Utils exposing (enumerate, remove_at, replace_at)

type Msg a 
  = Msg_Add a
  | Msg_Remove Int
  | Msg_Edit Int a

type IntermediateMsg a msg
  = EditElement a
  | PassThrough msg

update : Msg a -> List a -> List a
update msg state = case msg of
  Msg_Add new_element -> state ++ [new_element]
  Msg_Remove index -> remove_at index state
  Msg_Edit index new_value -> replace_at index new_value state

add_button : List (UI.Attribute msg) -> a -> (Msg a -> msg) -> UI.Element msg
add_button attributes default message = Input.button (UI.alignRight :: attributes) 
  { onPress = Just <| message <| Msg_Add default
  , label = UI.text "+"
  }

remove_button : List (UI.Attribute msg) -> Int -> (Msg a -> msg) -> UI.Element msg
remove_button attributes index message = Input.button (UI.alignRight :: attributes) 
  { onPress = Just <| message <| Msg_Remove index
  , label = UI.el [ Font.family [ fontawesome ] ] <| UI.text "\u{f1f8}"
  }

type alias Args a msg = 
  { state : List a
  , name : String
  , default : a
  , view_element : Int -> a -> UI.Element (IntermediateMsg a msg)
  , message : (Msg a -> msg)
  , button_attributes : List (UI.Attribute msg)
  }

header_row : Args a msg -> UI.Element msg
header_row args = UI.row [ UI.spacing 10, UI.width UI.fill ] 
  [ UI.el [ UI.width UI.fill ] <| UI.text args.name
  , add_button args.button_attributes args.default args.message 
  ]

view_element : (Int -> a -> UI.Element (IntermediateMsg a msg)) -> (Msg a -> msg) -> a -> Int -> UI.Element msg
view_element view_fn message element index = UI.map 
  (\intermediate_msg -> case intermediate_msg of
    EditElement new_value -> message <| Msg_Edit index new_value
    PassThrough msg -> msg
  ) (view_fn index element)

element_row : Args a msg -> (Int, a) -> UI.Element msg
element_row args (index, element) = UI.row [ UI.spacing 10, UI.width UI.fill ] 
  [ UI.el [ UI.width UI.fill ] <| view_element args.view_element args.message element index
  , remove_button args.button_attributes index args.message 
  ]

view : Args a msg -> UI.Element msg
view args = UI.column 
  [ UI.spacing 10, UI.width UI.fill ] 
  (header_row args :: List.map (element_row args) (enumerate args.state))
