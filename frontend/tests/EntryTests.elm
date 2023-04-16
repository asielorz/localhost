module EntryTests exposing (..)

import Entry
import Expect
import Test exposing (..)
import Json.Decode

valid_entry_json : String
valid_entry_json = """
{ "id" : 0
, "link" : "http://www.wikipedia.org/"
, "title" : "Test title"
, "description" : "This is a test"
, "authors" : ["Foobar the second"]
, "category" : "Tests"
, "themes" : ["Bullshit"]
, "works_mentioned" : ["Hamlet","King Lear","MacBeth"]
, "tags" : ["Great soundtrack","Soulslike","Female protagonist"]
, "date_published" : { "day" : 17, "month" : "March", "year" : 1965 }
, "date_saved" : { "day" : 1, "month":"April", "year":2000}
, "exceptional" : true
, "entry_type" : { "Article" : { "pages" : 0 } }
, "image" : null
, "backup" : null
}
"""

from_json__parse_valid_entry_correctly : Test
from_json__parse_valid_entry_correctly = test
  "A valid entry can be parsed correctly"
  (\_ ->
    Expect.ok <| Json.Decode.decodeString Entry.from_json valid_entry_json
  )