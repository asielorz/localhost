module Combo exposing (update, view, Msg(..))

import Element as UI
import Element.Input as Input
import Element.Events as Events
import Element.Background as Background
import Element.Font as Font
import Fontawesome exposing (fontawesome)
import Utils exposing (..)

type Msg a
  = Msg_Open
  | Msg_Select a
  | Msg_Close

update : { set_value : (a -> model -> model), set_open_combo : (Maybe id  -> model -> model), id : id, model : model } -> Msg a -> model
update args msg = case msg of 
  Msg_Open -> args.set_open_combo (Just args.id) args.model
  Msg_Close -> args.set_open_combo Nothing args.model
  Msg_Select new_value -> args.set_value new_value args.model |> args.set_open_combo Nothing

widget_hovered_background_color : UI.Color
widget_hovered_background_color = (UI.rgb 0.219 0.219 0.219)

view_combo_content : List (UI.Attribute msg) -> { alternatives : List a, view_alternative : (a -> UI.Element msg), message : (Msg a -> msg) } -> UI.Element msg
view_combo_content attributes args = UI.column 
  (attributes ++ [ UI.spacing 5 ])
  (args.alternatives |> List.map (\alternative -> UI.el [ Events.onClick <| args.message <| Msg_Select alternative, UI.mouseOver [ Background.color widget_hovered_background_color ] ] <| args.view_alternative alternative))

is_open : id -> Maybe id -> Bool
is_open combo_id currently_open_id = case currently_open_id of
  Nothing -> False
  Just id -> combo_id == id

view : List (UI.Attribute msg) -> { combo_state : a, id : id, alternatives : List a, view_alternative : (a -> UI.Element msg), message : (Msg a -> msg), currently_open_id : Maybe id } -> UI.Element msg
view attributes args = 
  let open = is_open args.id args.currently_open_id
  in UI.el 
    (if open then [ UI.below (view_combo_content attributes { alternatives = args.alternatives, view_alternative = args.view_alternative, message = args.message }) ] else [])
    <| Input.button 
      (add_if open (Events.onClick <| args.message Msg_Close) [])
      { onPress = Just (args.message Msg_Open)
      , label = UI.row 
        (attributes ++ [ UI.spacing 10 ])
        [ args.view_alternative args.combo_state
        , UI.el [ Font.family [ fontawesome ], UI.alignRight ] <| UI.text "\u{f0d7} " -- caret-down
        ]
      }
