module InputBox exposing (..)

import Element as UI exposing (px)
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events
import Element.Background as Background
import Config
import Utils
import ListWidget

input_box : List (UI.Attribute msg) -> String -> (String -> msg) -> UI.Element msg
input_box attributes text onChange = Input.text
  (Config.widget_common_attributes ++ attributes)
  { onChange = onChange
  , text = text
  , placeholder = Nothing
  , label = Input.labelHidden ""
  }

input_box_with_suggestions : List (UI.Attribute msg) -> 
  { suggestions : List String
  , text : String
  , message : (String -> msg)
  , change_open : (Maybe id -> msg)
  , id : id
  , currently_open_id : Maybe id 
  } -> UI.Element msg
input_box_with_suggestions attributes args = 
  let 
    filtered_suggestions = args.suggestions |> List.filter (\suggestion -> String.contains (String.toLower args.text) (String.toLower suggestion))

    is_open = 
      not (List.isEmpty filtered_suggestions) && 
      args.currently_open_id == Just args.id

    suggestion_box = filtered_suggestions
      |> List.map 
        (\s -> UI.el 
          [ UI.mouseOver [ Background.color Config.widget_hovered_background_color ]
          , UI.width UI.fill
          , UI.padding 5
          , Events.onClick <| args.message s
          ] 
          <| UI.text s
          )
      |> UI.column (UI.width UI.fill :: Config.widget_common_attributes)

    final_attributes = attributes 
      |> List.append [ Events.onFocus <| args.change_open <| Just args.id, Events.onLoseFocus <| args.change_open Nothing ]
      |> Utils.add_if is_open (UI.below suggestion_box)
      |> Utils.add_if is_open (Utils.on_enter <| args.message (Maybe.withDefault "" <| List.head filtered_suggestions))
  in
    input_box final_attributes args.text args.message

multiline_input_box : String -> (String -> msg) -> UI.Element msg
multiline_input_box text onChange = Input.multiline
  (Config.widget_common_attributes ++
    [ Font.size 13
    , UI.height (px 200)
    ])
  { onChange = onChange
  , text = text
  , placeholder = Nothing
  , label = Input.labelHidden ""
  , spellcheck = True
  }

view_string_list : List String -> List String -> String -> (Int -> id) -> Maybe id -> (ListWidget.Msg String -> msg) -> (Maybe id -> msg) -> UI.Element msg
view_string_list list suggestions name id currently_open_combo message change_open = UI.el 
  [ Border.color Config.transparentish_widget_border_color
  , Border.width 1
  , Border.rounded 3
  , UI.padding 5
  , UI.width UI.fill
  ]
  <| ListWidget.view 
    { state = list
    , name = name
    , default = ""
    , view_element = (\i s -> input_box_with_suggestions [ Font.size 15, UI.padding 7 ] 
      { text = s
      , suggestions = suggestions 
      -- Filter out '|' because it is forbidden in strings that are part of lists as we use it as a delimiter when serializing lists of strings as strings.
      , message = ListWidget.EditElement << String.filter (\c -> c /= '|')
      , id = id i
      , currently_open_id = currently_open_combo
      , change_open = (\new_state -> ListWidget.PassThrough <| change_open new_state)
      })
    , message = message
    , button_attributes = Config.widget_common_attributes ++ [ Font.size 15, UI.padding 7 ]
    }
