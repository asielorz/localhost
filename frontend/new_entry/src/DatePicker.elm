module DatePicker exposing (State, Msg, date, make, update, view)

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

date : State -> Calendar.Date
date state = state.calendar.date

make : { day : Int, month : Time.Month, year : Int } -> State
make initial = { calendar = CalendarWidget.make initial, is_open = False }

update : Msg -> State -> State
update msg state = case msg of
  Msg_Open new_is_open -> if new_is_open
    then { state | is_open = True, calendar = CalendarWidget.make { day = Calendar.getDay state.calendar.date, month = Calendar.getMonth state.calendar.date, year = Calendar.getYear state.calendar.date } }
    else { state | is_open = False }

  Msg_CalendarChanged calendar_msg -> { state 
    | calendar = CalendarWidget.update calendar_msg state.calendar
    , is_open = case calendar_msg of 
      CalendarWidget.Msg_DateSelected _ -> False
      _ -> True
    }

view : List (UI.Attribute msg) -> (Msg -> msg) -> State -> UI.Element msg
view attributes message state = UI.row
  (UI.width UI.fill :: attributes)
  [ UI.el [ UI.alignLeft ] <| UI.text <| DateUtils.date_to_string <| date state
  , UI.el 
  [ UI.below <| if state.is_open then CalendarWidget.view state.calendar (message << Msg_CalendarChanged) else UI.none 
  , UI.alignRight
  ] 
  <| Input.button []
    { onPress = Just <| message <| Msg_Open <| not state.is_open 
    , label = Fontawesome.fontawesome_text [] "\u{f073}"
    }
  ]
