module SearchQueryTests exposing (..)

import SearchQuery exposing (..)
import Expect
import Test exposing (..)
import Time
import Calendar

-- search_query

search_query__no_parameters : Test
search_query__no_parameters = test
  "An empty query is represented by the empty string"
  (\_ -> Expect.equal
    ""
    (search_query empty_query)
  )

search_query__link : Test
search_query__link = test
  "A query that specifies the link contains a only parameter for the link"
  (\_ -> Expect.equal
    "?link=foo"
    (search_query { empty_query | link = "foo" })
  )

search_query__title : Test
search_query__title = test
  "A query that specifies the title contains a only parameter for the title"
  (\_ -> Expect.equal
    "?title=foo"
    (search_query { empty_query | title = "foo" })
  )

search_qury__author : Test
search_qury__author = test
  "A query that specifies the author contains a only parameter for the autor"
  (\_ -> Expect.equal
    "?author=foo"
    (search_query { empty_query | author = "foo" })
  )

search_qury__category : Test
search_qury__category = test
  "A query that specifies the category contains a only parameter for the category"
  (\_ -> Expect.equal
    "?category=foo"
    (search_query { empty_query | category = "foo" })
  )

search_qury__description : Test
search_qury__description = test
  "A query that specifies the description contains a only parameter for the description"
  (\_ -> Expect.equal
    "?description=foo"
    (search_query { empty_query | description = "foo" })
  )

search_qury__two_params : Test
search_qury__two_params = test
  "A query that specifies two parameters contains both and nothing else"
  (\_ -> Expect.equal
    "?title=foo&description=bar"
    (search_query { empty_query | title = "foo", description = "bar" })
  )

search_query__one_tag : Test
search_query__one_tag = test
  "A query that specifies a single tag contains a parameter for tags with that tag as a string"
  (\_ -> Expect.equal
    "?tags=foo"
    (search_query { empty_query | tags = [ "foo" ] })
  )

search_query__two_tags : Test
search_query__two_tags = test
  "A query that specifies two tags separates them with |, coded as %7C in percent codes"
  (\_ -> Expect.equal
    "?tags=foo%7Cbar"
    (search_query { empty_query | tags = [ "foo", "bar" ] })
  )

search_query__two_works : Test
search_query__two_works = test
  "A query that specifies two works separates them with |, coded as %7C in percent codes"
  (\_ -> Expect.equal
    "?works_mentioned=foo%7Cbar"
    (search_query { empty_query | works_mentioned = [ "foo", "bar" ] })
  )

search_query__two_themes : Test
search_query__two_themes = test
  "A query that specifies two themes separates them with |, coded as %7C in percent codes"
  (\_ -> Expect.equal
    "?themes=foo%7Cbar"
    (search_query { empty_query | themes = [ "foo", "bar" ] })
  )

search_query__date : Test
search_query__date = test
  "Dates are formatted as day-month-year, with month coded as a number from 1 to 12"
  (\_ -> Expect.equal
    "?published_between_from=16-10-1997"
    (search_query { empty_query | published_between_from = Calendar.fromRawParts { day = 16, month = Time.Oct, year = 1997 } })
  )

search_query__date__single_digit_day : Test
search_query__date__single_digit_day = test
  "Single digit days take a single byte"
  (\_ -> Expect.equal
    "?published_between_until=7-10-1997"
    (search_query { empty_query | published_between_until = Calendar.fromRawParts { day = 7, month = Time.Oct, year = 1997 } })
  )

search_query__date__single_digit_month : Test
search_query__date__single_digit_month = test
  "Single digit months take a single byte"
  (\_ -> Expect.equal
    "?saved_between_from=7-3-1997"
    (search_query { empty_query | saved_between_from = Calendar.fromRawParts { day = 7, month = Time.Mar, year = 1997 } })
  )

search_query__date__short_year : Test
search_query__date__short_year = test
  "Years with less than 4 digits take only as much space as they need"
  (\_ -> Expect.equal
    "?saved_between_until=7-3-33"
    (search_query { empty_query | saved_between_until = Calendar.fromRawParts { day = 7, month = Time.Mar, year = 33 } })
  )

search_query__exceptional : Test
search_query__exceptional = test
  "Exceptional only appears as a parameter when it is true"
  (\_ -> Expect.equal
    "?exceptional=true"
    (search_query { empty_query | exceptional = True })
  )

-- search_ui_state_from_query

search_ui_state_from_query__empty_string : Test
search_ui_state_from_query__empty_string = test
  "An empty string generates nothing"
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "")
  )

search_ui_state_from_query__link : Test
search_ui_state_from_query__link = test
  "A query string with only the link generates a query equal the empty query but with the link set"
  (\_ -> Expect.equal
    (Just { empty_query | link = "foo" })
    (search_ui_state_from_query "link=foo")
  )

search_ui_state_from_query__title : Test
search_ui_state_from_query__title = test
  "A query string with only the title generates a query equal the empty query but with the title set"
  (\_ -> Expect.equal
    (Just { empty_query | title = "foo" })
    (search_ui_state_from_query "title=foo")
  )

search_ui_state_from_query__author : Test
search_ui_state_from_query__author = test
  "A query string with only the author generates a query equal the empty query but with the author set"
  (\_ -> Expect.equal
    (Just { empty_query | author = "foo" })
    (search_ui_state_from_query "author=foo")
  )

search_ui_state_from_query__category : Test
search_ui_state_from_query__category = test
  "A query string with only the title generates a query equal the empty query but with the category set"
  (\_ -> Expect.equal
    (Just { empty_query | category = "foo" })
    (search_ui_state_from_query "category=foo")
  )

search_ui_state_from_query__two_fields : Test
search_ui_state_from_query__two_fields = test
  "A query string with more than one field separates them with &"
  (\_ -> Expect.equal
    (Just { empty_query | category = "foo", author = "bar" })
    (search_ui_state_from_query "category=foo&author=bar")
  )

search_ui_state_from_query__percent_escaping : Test
search_ui_state_from_query__percent_escaping = test
  "Non ASCII characters that are percent escaped in the query string are converted into the actual character"
  (\_ -> Expect.equal
    (Just { empty_query | category = "foo", author = "Foobar the second" })
    (search_ui_state_from_query "category=foo&author=Foobar%20the%20second")
  )

search_ui_state_from_query__list_of_strings : Test
search_ui_state_from_query__list_of_strings = test
  "Lists have their elements separated by '|', percent encoded as %7C"
  (\_ -> Expect.equal
    (Just { empty_query | tags = [ "foo", "bar", "quux" ] })
    (search_ui_state_from_query "tags=foo%7Cbar%7Cquux")
  )

search_ui_state_from_query__good_date : Test
search_ui_state_from_query__good_date = test
  "A date is represented as day-month-year with the month as a number in the range [1-12]" 
  (\_ -> Expect.equal
    (Just { empty_query | published_between_from = Calendar.fromRawParts { day = 11, month = Time.May, year = 2006 } })
    (search_ui_state_from_query "published_between_from=11-5-2006")
  )

search_ui_state_from_query__bad_date_missing_field : Test
search_ui_state_from_query__bad_date_missing_field = test
  "Parsing a date fails if one of the fields is missing" 
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "published_between_from=11-5")
  )

search_ui_state_from_query__bad_date_too_many_fields : Test
search_ui_state_from_query__bad_date_too_many_fields = test
  "Parsing a date fails if the parameter string has more than 3 fields" 
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "published_between_from=11-5-2006-47")
  )

search_ui_state_from_query__bad_date_not_a_number : Test
search_ui_state_from_query__bad_date_not_a_number = test
  "Parsing a date fails if one of the fields contains something that cannot be converted to a number" 
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "published_between_from=11-May-2006")
  )

search_ui_state_from_query__bad_date_month_out_of_range : Test
search_ui_state_from_query__bad_date_month_out_of_range = test
  "Parsing a date fails if the month is not in the range [1-12]" 
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "published_between_from=11-20-2006")
  )

search_ui_state_from_query__bad_date_day_out_of_range : Test
search_ui_state_from_query__bad_date_day_out_of_range = test
  "Parsing a date fails if the day is not valid for the given month" 
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "published_between_from=40-5-2006")
  )

search_ui_state_from_query__exceptional_true : Test
search_ui_state_from_query__exceptional_true = test
  "Exceptional is mapped to True if given the parameter string true" 
  (\_ -> Expect.equal
    (Just { empty_query | exceptional = True })
    (search_ui_state_from_query "exceptional=true")
  )

search_ui_state_from_query__exceptional_false : Test
search_ui_state_from_query__exceptional_false = test
  "Exceptional is mapped to False if given the parameter string false" 
  (\_ -> Expect.equal
    (Just { empty_query | exceptional = False })
    (search_ui_state_from_query "exceptional=false")
  )

search_ui_state_from_query__exceptional_bad_parameter : Test
search_ui_state_from_query__exceptional_bad_parameter = test
  "Parsing fails if exceptional is given any other value than true or false" 
  (\_ -> Expect.equal
    Nothing
    (search_ui_state_from_query "exceptional=hello")
  )
