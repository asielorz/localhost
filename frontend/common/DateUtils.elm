module DateUtils exposing (..)

import Calendar
import Time

type alias MonthInYear = { month : Time.Month, year : Int }

int_to_month : Int -> Time.Month
int_to_month month_index = case month_index of
  1  -> Time.Jan
  2  -> Time.Feb
  3  -> Time.Mar
  4  -> Time.Apr
  5  -> Time.May
  6  -> Time.Jun
  7  -> Time.Jul
  8  -> Time.Aug
  9  -> Time.Sep
  10 -> Time.Oct
  11 -> Time.Nov
  _  -> Time.Dec

month_to_int : Time.Month -> Int
month_to_int month = case month of
  Time.Jan -> 1  
  Time.Feb -> 2  
  Time.Mar -> 3  
  Time.Apr -> 4  
  Time.May -> 5  
  Time.Jun -> 6  
  Time.Jul -> 7  
  Time.Aug -> 8  
  Time.Sep -> 9  
  Time.Oct -> 10 
  Time.Nov -> 11 
  Time.Dec -> 12

month_to_string : Time.Month -> String
month_to_string month = case month of
  Time.Jan -> "enero"
  Time.Feb -> "febrero"
  Time.Mar -> "marzo"
  Time.Apr -> "abril"
  Time.May -> "mayo"
  Time.Jun -> "junio"
  Time.Jul -> "julio"
  Time.Aug -> "agosto"
  Time.Sep -> "septiembre"
  Time.Oct -> "octubre"
  Time.Nov -> "noviembre"
  Time.Dec -> "diciembre"

weekday_to_int : Time.Weekday -> Int
weekday_to_int month = case month of
  Time.Mon -> 0  
  Time.Tue -> 1  
  Time.Wed -> 2  
  Time.Thu -> 3  
  Time.Fri -> 4  
  Time.Sat -> 5  
  Time.Sun -> 6  

date_to_string : Calendar.Date -> String
date_to_string date = 
  String.fromInt (Calendar.getDay date) ++ 
  " de " ++ 
  month_to_string (Calendar.getMonth date) ++
  " de " ++
  String.fromInt (Calendar.getYear date)

prev_month : MonthInYear -> MonthInYear
prev_month m =
  if (m.month == Time.Jan)
    then { month = Time.Dec, year = m.year - 1 }
    else { month = int_to_month <| month_to_int m.month - 1, year = m.year }

next_month : MonthInYear -> MonthInYear
next_month m =
  if (m.month == Time.Dec)
    then { month = Time.Jan, year = m.year + 1 }
    else { month = int_to_month <| month_to_int m.month + 1, year = m.year }
