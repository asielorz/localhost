module Main exposing (main)

import Browser exposing (Document)
import Element as UI exposing (rgb)
import Element.Background as Background
import Element.Font as Font
import Config
import Entry exposing (Entry)
import Http
import Json.Decode

main : Program () Model Msg
main = Browser.document 
  { init = \() -> (default_model, initial_commands)
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model = List Entry

type Msg = Msg_SearchResultArrived (Result (Http.Error) (List Entry))

default_model : Model
default_model = []

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
