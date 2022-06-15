module DatePicker exposing (State, Msg, date, set_date, make, make_empty, update, view)

import Element as UI
import Element.Input as Input
import CalendarWidget
import Calendar
import Time
import Fontawesome
import DateUtils

type alias State = 
  { calendar : CalendarWidget.State
  , is_open : Bool
  }

type Msg 
  = Msg_Open Bool
  | Msg_CalendarChanged CalendarWidget.Msg

date : State -> Maybe Calendar.Date
date state = state.calendar.date

set_date : Maybe Calendar.Date -> State -> State
set_date new_date state = 
  let 
    calendar = state.calendar
    updated_calendar = { calendar | date = new_date }
  in
    { state | calendar = updated_calendar }

make : { day : Int, month : Time.Month, year : Int } -> State
make initial = { calendar = CalendarWidget.make initial, is_open = False }

make_empty : { display_month : Time.Month, display_year : Int } -> State
make_empty initial = { calendar = CalendarWidget.make {day = -1, month = initial.display_month, year = initial.display_year }, is_open = False }

update : Msg -> State -> State
update msg state = case msg of
  Msg_Open new_is_open -> if new_is_open
    then 
      { state | is_open = True
      , calendar = case date state of
        Nothing -> state.calendar
        Just selected_date -> CalendarWidget.make { day = Calendar.getDay selected_date, month = Calendar.getMonth selected_date, year = Calendar.getYear selected_date }
      }
    else { state | is_open = False }

  Msg_CalendarChanged calendar_msg -> { state 
    | calendar = CalendarWidget.update calendar_msg state.calendar
    , is_open = case calendar_msg of 
      CalendarWidget.Msg_DateSelected _ -> False
      CalendarWidget.Msg_DateCleared -> False
      _ -> True
    }

view : List (UI.Attribute msg) -> (Msg -> msg) -> State -> UI.Element msg
view attributes message state = UI.row
  ([ UI.width UI.fill, UI.spacing 5 ] ++ attributes)
  [ UI.el [ UI.alignLeft ] <| UI.text (date state |> Maybe.map DateUtils.date_to_string |> Maybe.withDefault "")
  , UI.el 
    [ UI.below <| if state.is_open then CalendarWidget.view state.calendar (message << Msg_CalendarChanged) else UI.none 
    , UI.alignRight
    ] 
    <| Input.button []
      { onPress = Just <| message <| Msg_Open <| not state.is_open 
      , label = Fontawesome.fontawesome_text [] "\u{f073}" -- calendar-days
      }
  , Input.button []
    { onPress = Just <| message <| Msg_CalendarChanged <| CalendarWidget.Msg_DateCleared 
    , label = Fontawesome.fontawesome_text [] "\u{f00d}" -- xmark
    }
  ]
