module Main exposing (main)

import Browser exposing (Document)
import Element as UI exposing (rgb)
import Element.Background as Background
import Element.Font as Font
import Config
import Entry exposing (Entry)
import Time
import Calendar
import Http
import Json.Decode
import DateUtils

main : Program () Model Msg
main = Browser.document 
  { init = \() -> (default_model, initial_commands)
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model = List Entry

type Msg = Msg_SearchResultArrived (Result (Http.Error) (List Entry))

-- Test data. Delete.

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
  , date_published = DateUtils.make_literal_date { day = 15, month = Time.Jan, year = 2001 }
  , date_saved = DateUtils.make_literal_date { day = 12, month = Time.Jun, year = 2022 }
  , exceptional = False
  }
-- End test data.

default_model : Model
default_model = [ wikipedia_test_entry ]

initial_commands : Cmd Msg
initial_commands = Http.get { url = "http://localhost:8080/texts", expect = Http.expectJson Msg_SearchResultArrived (Json.Decode.list Entry.from_json) }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Msg_SearchResultArrived new_results -> case new_results of
    Ok (actual_results) -> (actual_results, Cmd.none)
    Err _ -> (model, Cmd.none)

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
        <| UI.column [ UI.centerX, UI.centerY, UI.spacing 20 ] (List.map Entry.view model)
    ]
  }
