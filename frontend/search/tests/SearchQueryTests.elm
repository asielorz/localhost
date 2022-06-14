module SearchQueryTests exposing (..)

import SearchQuery exposing (..)
import Expect
import Test exposing (..)
import Time
import Calendar

search_query__no_parameters : Test
search_query__no_parameters = test
  "An empty query just contains the base URL"
  (\_ -> Expect.equal
    "http://localhost:8080/texts"
    (search_query empty_query)
  )

search_query__link : Test
search_query__link = test
  "A query that specifies the link contains a only parameter for the link"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?link=foo"
    (search_query { empty_query | link = "foo" })
  )

search_query__title : Test
search_query__title = test
  "A query that specifies the title contains a only parameter for the title"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?title=foo"
    (search_query { empty_query | title = "foo" })
  )

search_qury__author : Test
search_qury__author = test
  "A query that specifies the author contains a only parameter for the autor"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?author=foo"
    (search_query { empty_query | author = "foo" })
  )

search_qury__category : Test
search_qury__category = test
  "A query that specifies the category contains a only parameter for the category"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?category=foo"
    (search_query { empty_query | category = "foo" })
  )

search_qury__description : Test
search_qury__description = test
  "A query that specifies the description contains a only parameter for the description"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?description=foo"
    (search_query { empty_query | description = "foo" })
  )

search_qury__two_params : Test
search_qury__two_params = test
  "A query that specifies two parameters contains both and nothing else"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?title=foo&description=bar"
    (search_query { empty_query | title = "foo", description = "bar" })
  )

search_query__one_tag : Test
search_query__one_tag = test
  "A query that specifies a single tag contains a parameter for tags with that tag as a string"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?tags=foo"
    (search_query { empty_query | tags = [ "foo" ] })
  )

search_query__two_tags : Test
search_query__two_tags = test
  "A query that specifies two tags separates them with |, coded as %7C in percent codes"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?tags=foo%7Cbar"
    (search_query { empty_query | tags = [ "foo", "bar" ] })
  )

search_query__two_works : Test
search_query__two_works = test
  "A query that specifies two works separates them with |, coded as %7C in percent codes"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?works_mentioned=foo%7Cbar"
    (search_query { empty_query | works_mentioned = [ "foo", "bar" ] })
  )

search_query__two_themes : Test
search_query__two_themes = test
  "A query that specifies two themes separates them with |, coded as %7C in percent codes"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?themes=foo%7Cbar"
    (search_query { empty_query | themes = [ "foo", "bar" ] })
  )

search_query__date : Test
search_query__date = test
  "Dates are formatted as day-month-year, with month coded as a number from 1 to 12"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?published_between_from=16-10-1997"
    (search_query { empty_query | published_between_from = Calendar.fromRawParts { day = 16, month = Time.Oct, year = 1997 } })
  )

search_query__date__single_digit_day : Test
search_query__date__single_digit_day = test
  "Single digit days take a single byte"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?published_between_until=7-10-1997"
    (search_query { empty_query | published_between_until = Calendar.fromRawParts { day = 7, month = Time.Oct, year = 1997 } })
  )

search_query__date__single_digit_month : Test
search_query__date__single_digit_month = test
  "Single digit months take a single byte"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?saved_between_from=7-3-1997"
    (search_query { empty_query | saved_between_from = Calendar.fromRawParts { day = 7, month = Time.Mar, year = 1997 } })
  )

search_query__date__short_year : Test
search_query__date__short_year = test
  "Years with less than 4 digits take only as much space as they need"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?saved_between_until=7-3-33"
    (search_query { empty_query | saved_between_until = Calendar.fromRawParts { day = 7, month = Time.Mar, year = 33 } })
  )

search_query__exceptional : Test
search_query__exceptional = test
  "Exceptional only appears as a parameter when it is true"
  (\_ -> Expect.equal
    "http://localhost:8080/texts?exceptional=true"
    (search_query { empty_query | exceptional = True })
  )

