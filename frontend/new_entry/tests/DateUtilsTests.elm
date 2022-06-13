module DateUtilsTests exposing (..)

import DateUtils exposing (..)
import Expect
import Test exposing (..)
import Time
import Json.Decode

-- date_from_json

date_from_json__valid_date : Test
date_from_json__valid_date = test
  "A valid date is parsed correctly"
  (\_ -> Expect.equal
    (Ok <| DateUtils.make_literal_date { day = 19, month = Time.Feb, year = 1965 })
    <| Json.Decode.decodeString DateUtils.date_from_json 
      """
      { "day" : 19, "month" : "February", "year" : 1965 }
      """
  )

date_from_json__missing_field_day : Test
date_from_json__missing_field_day = test
  "A date json missing day field is not valid"
  (\_ -> Expect.err
    <| Json.Decode.decodeString DateUtils.date_from_json 
      """
      { "month" : "February", "year" : 1965 }
      """
  )

date_from_json__missing_field_month : Test
date_from_json__missing_field_month = test
  "A date json missing month field is not valid"
  (\_ -> Expect.err
    <| Json.Decode.decodeString DateUtils.date_from_json 
      """
      { "day" : 19, "year" : 1965 }
      """
  )

date_from_json__missing_field_year : Test
date_from_json__missing_field_year = test
  "A date json missing year field is not valid"
  (\_ -> Expect.err
    <| Json.Decode.decodeString DateUtils.date_from_json 
      """
      { "day" : 19, "month" : "February" }
      """
  )

date_from_json__day_out_of_range : Test
date_from_json__day_out_of_range = test
  "A date with a day out of range (for example 30th of February) is not valid"
  (\_ -> Expect.err
    <| Json.Decode.decodeString DateUtils.date_from_json 
      """
      { "day" : 30, "month" : "February", year : "1965" }
      """
  )

date_from_json__invalid_month : Test
date_from_json__invalid_month = test
  "A date with a string that does not name a month in the month field is not valid"
  (\_ -> Expect.err
    <| Json.Decode.decodeString DateUtils.date_from_json 
      """
      { "day" : 19, "month" : "This is not a month", year : "1965" }
      """
  )
