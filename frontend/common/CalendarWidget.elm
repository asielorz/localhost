module CalendarWidget exposing (State, Msg(..), make, update, view)

import Time
import Calendar
import Element as UI exposing (px, rgb)
import Element.Border as Border
import Element.Background as Background
import Element.Input as Input
import Element.Font as Font
import Fontawesome exposing (fontawesome_text)
import DateUtils exposing (..)
import Utils
import List
import Debug exposing (toString)
import Config

widget_background_color : UI.Color
widget_background_color = (rgb 0.129 0.129 0.129)

widget_border_color : UI.Color
widget_border_color = (rgb 0.729 0.729 0.729)

selected_color : UI.Color
selected_color = (rgb 0 0.388 0.694)

selected_hovered_color : UI.Color
selected_hovered_color = (rgb 0.2 0.588 0.894)

error_background_color : UI.Color
error_background_color = (rgb 0.329 0.129 0.129)

calendar_element_size : number
calendar_element_size = 41

type alias State =
  { date : Maybe Calendar.Date
  , displayed_month : Time.Month
  , displayed_year : Int
  , enter_day_text : String
  , enter_month_text : String
  , enter_year_text : String
  , text_fields_represent_valid_date : Bool
  }

type Msg
  = Msg_DisplayedMonthChanged MonthInYear
  | Msg_DateSelected Calendar.Date
  | Msg_DateCleared
  | Msg_DayTextChanged String
  | Msg_MonthTextChanged String
  | Msg_YearTextChanged String

default_date : Calendar.Date
default_date = Calendar.fromPosix <| Time.millisToPosix 0

make : { day : Int, month : Time.Month, year : Int } -> State
make initial = 
  { date = Calendar.fromRawParts { month = initial.month, year = initial.year, day = initial.day }
  , displayed_month = initial.month
  , displayed_year = initial.year
  , enter_day_text = ""
  , enter_month_text = ""
  , enter_year_text = ""
  , text_fields_represent_valid_date = True
  }

validate_number_string : Int -> String -> Maybe String
validate_number_string max_length string =
  let trimmed = String.left max_length string in
  let as_list = String.toList trimmed in
  if as_list |> List.map Char.isDigit |> List.foldl (&&) True
    then Just trimmed
    else Nothing

parse_text_fields : State -> State
parse_text_fields state = case String.toInt state.enter_day_text of
  Nothing -> { state | text_fields_represent_valid_date = False }
  Just day -> case String.toInt state.enter_month_text of
    Nothing -> { state | text_fields_represent_valid_date = False }
    Just month -> case String.toInt state.enter_year_text of
      Nothing -> { state | text_fields_represent_valid_date = False }
      Just year -> case Calendar.fromRawParts { day = day, month = int_to_month month, year = year } of
        Nothing -> { state | text_fields_represent_valid_date = False }
        Just date -> { state 
          | date = Just date
          , displayed_month = Calendar.getMonth date
          , displayed_year = Calendar.getYear date
          , text_fields_represent_valid_date = True 
          }

update : Msg -> State -> State
update msg state = case msg of
  Msg_DisplayedMonthChanged new_month_year -> { state | displayed_month = new_month_year.month, displayed_year = new_month_year.year }
  
  Msg_DateSelected new_date -> { state | date = Just new_date }
  
  Msg_DateCleared -> { state | date = Nothing }
  
  Msg_DayTextChanged new_text -> case validate_number_string 2 new_text of
    Nothing -> state
    Just valid_new_text -> parse_text_fields { state | enter_day_text = valid_new_text }

  Msg_MonthTextChanged new_text -> case validate_number_string 2 new_text of
    Nothing -> state
    Just valid_new_text -> parse_text_fields { state | enter_month_text = valid_new_text }

  Msg_YearTextChanged new_text -> case validate_number_string 4 new_text of
    Nothing -> state
    Just valid_new_text -> parse_text_fields { state | enter_year_text = valid_new_text }

-- view

view_first_row : State -> (Msg -> msg) -> UI.Element msg
view_first_row state message = UI.row [ UI.padding 5, UI.width (px 300) ] 
  [ Input.button [UI.alignLeft] { onPress = Just <| message <| Msg_DisplayedMonthChanged <| prev_month { month = state.displayed_month, year = state.displayed_year }, label = fontawesome_text [] "\u{f0d9}" } -- caret-left
  , UI.row [ UI.centerX ] [ UI.text <| Utils.toupper_first <| month_to_string state.displayed_month ++ " de " ++ String.fromInt state.displayed_year ]
  , Input.button [UI.alignLeft] { onPress = Just <| message <| Msg_DisplayedMonthChanged <| next_month { month = state.displayed_month, year = state.displayed_year }, label = fontawesome_text [] "\u{f0da}" } -- caret-right
  ]

pad_dates : List Calendar.Date -> List (Maybe Calendar.Date)
pad_dates dates = case dates of
  [] -> []
  (first::_) -> 
    let first_weekday = Calendar.getWeekday first in
    let padded_front = List.repeat (weekday_to_int first_weekday) Nothing ++ List.map Just dates in
    let back_pad = Utils.missing_to_be_a_multiple_of 7 (List.length padded_front) in
    padded_front ++ List.repeat back_pad Nothing

view_calendar_day : (Msg -> msg) -> Maybe Calendar.Date -> Maybe Calendar.Date -> UI.Element msg
view_calendar_day message selected_date maybe_date = case maybe_date of
  Nothing -> UI.el
    [ UI.width (px calendar_element_size)
    , UI.height (px calendar_element_size)
    , Background.color widget_background_color
    , Border.color widget_border_color
    , Border.width 1
    ]
    UI.none
  Just date -> Input.button 
    [ UI.width (px calendar_element_size)
    , UI.height (px calendar_element_size)
    , Background.color <| if selected_date == Just date then selected_color else widget_background_color
    , Border.color widget_border_color
    , Border.width 1
    , UI.mouseOver [ Background.color <| if selected_date == Just date then selected_hovered_color else (rgb 0.3 0.3 0.3) ]
    ]
    { label = UI.el [ UI.centerX, UI.centerY ] (date |> Calendar.getDay |> toString |> UI.text)
    , onPress = Just <| message <| Msg_DateSelected date
    }

display_date : State -> Calendar.Date
display_date state = Maybe.withDefault default_date <| Calendar.fromRawParts { month = state.displayed_month, year = state.displayed_year, day = 1 }

view_calendar_body : State -> (Msg -> msg) -> UI.Element msg
--view_calendar_body state message = UI.row [] []
view_calendar_body state message = 
  let dates = Utils.chunk 7 <| pad_dates <| Calendar.getDatesInMonth <| display_date state in
  UI.column [ UI.centerX ] <| List.map (\row_dates -> UI.row [] <| List.map (view_calendar_day message state.date) row_dates) dates 

view_text_boxes : State -> (Msg -> msg) -> UI.Element msg
view_text_boxes state message = UI.row 
  [ UI.centerX
  , UI.spacing 10
  , UI.padding 5
  , Font.family [ Font.monospace ]
  ]
  [ Input.text
    [ UI.padding 4
    , Background.color <| if state.text_fields_represent_valid_date then widget_background_color else error_background_color
    , Border.color widget_border_color
    , UI.width (px 32)
    ]
    { onChange = \new_text ->  message (Msg_DayTextChanged new_text)
    , text = state.enter_day_text
    , placeholder = Just <| Input.placeholder [] <| UI.text "dd"
    , label = Input.labelHidden ""
    }
  , Input.text
    [ UI.padding 4
    , Background.color <| if state.text_fields_represent_valid_date then widget_background_color else error_background_color
    , Border.color widget_border_color
    , UI.width (px 32)
    ]
    { onChange = \new_text ->  message (Msg_MonthTextChanged new_text)
    , text = state.enter_month_text
    , placeholder = Just <| Input.placeholder [] <| UI.text "mm"
    , label = Input.labelHidden ""
    }
  , Input.text
    [ UI.padding 4
    , Background.color <| if state.text_fields_represent_valid_date then widget_background_color else error_background_color
    , Border.color widget_border_color
    , UI.width (px 55)
    ]
    { onChange = \new_text ->  message (Msg_YearTextChanged new_text)
    , text = state.enter_year_text
    , placeholder = Just <| Input.placeholder [] <| UI.text "yyyy"
    , label = Input.labelHidden ""
    }
  ]

clear_button : (Msg -> msg) -> UI.Element msg
clear_button message = Input.button 
  (Config.widget_common_attributes ++ 
    [ UI.mouseOver [ Background.color Config.widget_hovered_background_color ]
    , UI.alignRight
    , UI.padding 5
    , UI.moveLeft 7
    ]) 
  { onPress = Just <| message Msg_DateCleared
  , label = fontawesome_text [] "\u{f1f8}" -- trash
  }

view : State -> (Msg -> msg) -> UI.Element msg
view state message = UI.column 
  [ Border.color widget_border_color
  , Border.width 3
  , Background.color widget_background_color
  ]
  [ view_first_row state message
  , view_calendar_body state message
  , UI.row [ UI.width UI.fill ] [ view_text_boxes state message, clear_button message ]
  ]
