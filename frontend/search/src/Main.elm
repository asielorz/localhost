module Main exposing (main)

import Browser exposing (Document)
import Element as UI exposing (rgb)
import Element.Background as Background
import Element.Font as Font
import Config
import Entry exposing (Entry)
import Time
import Calendar

main : Program () Model Msg
main = Browser.document 
  { init = \() -> (default_model, initial_commands)
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model = List Entry

type Msg = Msg_Noop

-- Test data. Delete.
default_date : Calendar.Date
default_date = Calendar.fromPosix <| Time.millisToPosix 0

make_date : { day : Int, month : Time.Month, year : Int } -> Calendar.Date
make_date parts = Maybe.withDefault default_date <| Calendar.fromRawParts { month = parts.month, year = parts.year, day = parts.day }

wikipedia_test_entry : Entry
wikipedia_test_entry = 
  { id = 5
  , link = "https://www.wikipedia.org/"
  , title = "Wikipedia, la enciclopedia libre"
  , description = "Wikipedia es una enciclopedia libre, políglota y editada de manera colaborativa. Es administrada por la Fundación Wikimedia, una organización sin ánimo de lucro cuya financiación está basada en donaciones. Sus más de 58 millones de artículos en 326 idiomas han sido redactados en conjunto por voluntarios de todo el mundo, lo que suma más de 3000 millones de ediciones, y permite que cualquier persona pueda sumarse al proyecto para editarlos, a menos que la página se encuentre protegida contra vandalismos para evitar problemas o disputas."
  , author = "Fundación Wikimedia"
  , category = "Enciclopedia"
  , themes = [ "Tema 1", "Tema 2" ]
  , works_mentioned = [ "Obra 1", "Obra 2" ]
  , tags = [ "Etiqueta 1", "Etiqueta 2" ]
  , date_published = make_date { day = 15, month = Time.Jan, year = 2001 }
  , date_saved = make_date { day = 12, month = Time.Jun, year = 2022 }
  , exceptional = False
  }
-- End test data.

default_model : Model
default_model = [ wikipedia_test_entry ]

initial_commands : Cmd Msg
initial_commands = Cmd.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Buscar"
  , body = 
    [ UI.layout 
        [ Background.color Config.background_color
        , UI.centerX
        , UI.centerY
        , Font.color (rgb 1 1 1) 
        ] 
        <| UI.column [ UI.centerX, UI.centerY ] (List.map Entry.view model)
    ]
  }
