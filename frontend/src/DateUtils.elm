module DateUtils exposing (..)

import Calendar
import Time
import Json.Encode
import Json.Decode
import Utils

type alias MonthInYear = { month : Time.Month, year : Int }

make_literal_date : { day : Int, month : Time.Month, year : Int } -> Calendar.Date
make_literal_date parts = Maybe.withDefault (Calendar.fromPosix <| Time.millisToPosix 0) <| Calendar.fromRawParts { month = parts.month, year = parts.year, day = parts.day }

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

month_to_json_string : Time.Month -> String
month_to_json_string month = case month of 
  Time.Jan -> "January"
  Time.Feb -> "February"
  Time.Mar -> "March"
  Time.Apr -> "April"
  Time.May -> "May"
  Time.Jun -> "June"
  Time.Jul -> "July"
  Time.Aug -> "August"
  Time.Sep -> "September"
  Time.Oct -> "October"
  Time.Nov -> "November"
  Time.Dec -> "December"

json_string_to_month : String -> Maybe Time.Month
json_string_to_month month = case month of 
  "January"   -> Just Time.Jan 
  "February"  -> Just Time.Feb
  "March"     -> Just Time.Mar
  "April"     -> Just Time.Apr
  "May"       -> Just Time.May
  "June"      -> Just Time.Jun
  "July"      -> Just Time.Jul
  "August"    -> Just Time.Aug
  "September" -> Just Time.Sep
  "October"   -> Just Time.Oct
  "November"  -> Just Time.Nov
  "December"  -> Just Time.Dec
  _           -> Nothing

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

parse_date_from_url : String -> Maybe Calendar.Date
parse_date_from_url parameter = 
  case String.split "-" parameter of
    (year_str::month_str::day_str::[]) -> case (String.toInt year_str, String.toInt month_str, String.toInt day_str) of
      (Just year, Just month, Just day) -> if month >= 1 && month <= 12 
        then Calendar.fromRawParts { day = day, month = int_to_month month, year = year }
        else Nothing
      _ -> Nothing
    _ -> Nothing 

-- export date to json in the format expected by the backend
date_to_json : Calendar.Date -> Json.Encode.Value
date_to_json date = Json.Encode.object
  [ ("day", Json.Encode.int <| Calendar.getDay date)
  , ("month", Json.Encode.string <| month_to_json_string <| Calendar.getMonth date)
  , ("year", Json.Encode.int <| Calendar.getYear date)
  ]

date_from_json : Json.Decode.Decoder Calendar.Date
date_from_json = Utils.fail_if_nothing "Combination of day-month-year does not represent a real date" <| Json.Decode.map3 (\day month year -> Calendar.fromRawParts { month = month, year = year, day = day })
  (Json.Decode.field "day" Json.Decode.int)
  (Json.Decode.field "month" (Utils.fail_if_nothing "String does not name a month" <| Json.Decode.map json_string_to_month Json.Decode.string))
  (Json.Decode.field "year" Json.Decode.int)
