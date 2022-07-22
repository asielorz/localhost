module Main exposing (main)

-- Pages
import Page_NotFound
import Page_NewEntry
import Page_Search

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Element as UI exposing (rgb)
import Element.Font as Font
import Element.Background as Background
import Url exposing (Url)
import Url.Parser exposing ((</>))
import Config
import Banner
import Http exposing (request)
import Browser exposing (UrlRequest(..))

main : Program () Model Msg
main = Browser.application 
  { init = \() url key -> init key url
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = Msg_UrlRequest
  , onUrlChange = Msg_UrlChange
  }

type PageModel
  = PageModel_NotFound
  | PageModel_NewEntry Page_NewEntry.Model
  | PageModel_Search Page_Search.Model
  | PageModel_Edit Page_NewEntry.Model

type alias Model =
  { navigation_key : Navigation.Key
  , page_model : PageModel
  }

type Msg
  = Msg_UrlRequest UrlRequest
  | Msg_UrlChange Url
  | Msg_NewEntry Page_NewEntry.Msg
  | Msg_Search Page_Search.Msg
  | Msg_Edit Page_NewEntry.Msg

map_init : (model -> PageModel) -> (msg -> Msg) -> (model, Cmd msg) -> (PageModel, Cmd Msg)
map_init map_model map_msg (model, cmd) = (map_model model, Cmd.map map_msg cmd)

navigate_to : Url -> PageModel -> (PageModel, Cmd Msg)
navigate_to url model =
  let
    s = Url.Parser.s
    map = Url.Parser.map
    int = Url.Parser.int

    route = Url.Parser.oneOf
      [ s "new_entry" |> map (map_init PageModel_NewEntry Msg_NewEntry Page_NewEntry.init)
      , s "search" |> map 
        (case model of
          PageModel_Search search_page_model -> map_init PageModel_Search Msg_Search (Page_Search.navigate_to url search_page_model)
          _ ->
            let
              (initial_model, initial_cmd) = Page_Search.init
              (updated_model, extra_cmd) = Page_Search.navigate_to url initial_model
            in
              map_init PageModel_Search Msg_Search (updated_model, Cmd.batch [ initial_cmd, extra_cmd ])
        )
      , s "edit" </> int |> map (\i -> map_init PageModel_Edit Msg_Edit (Page_NewEntry.edit i))
      ]
  in
    Url.Parser.parse route url |> Maybe.withDefault (PageModel_NotFound, Cmd.none)

init : Navigation.Key -> Url -> (Model, Cmd Msg)
init key url = 
  let
    (model, cmd) = navigate_to url PageModel_NotFound
  in
    ( { navigation_key = key
      , page_model = model
      }
    , cmd
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of

  Msg_UrlRequest request -> case request of
    Internal url -> (model, Navigation.pushUrl model.navigation_key (Url.toString url))
    External url -> (model, Navigation.load url)

  Msg_UrlChange url ->
    let 
      (new_page_model, cmd) = navigate_to url model.page_model
    in
      ({ model | page_model = new_page_model }, cmd)

  Msg_NewEntry new_entry_page_msg -> case model.page_model of 
    PageModel_NewEntry new_entry_page_model -> 
      let
        (new_model, cmd) = Page_NewEntry.update new_entry_page_msg new_entry_page_model
      in
        ({ model | page_model = PageModel_NewEntry new_model }, Cmd.map Msg_NewEntry cmd)
    _ -> (model, Cmd.none)

  Msg_Search search_page_msg -> case model.page_model of
    PageModel_Search search_page_model -> 
      let
        (new_model, cmd) = Page_Search.update search_page_msg search_page_model
      in
        ({ model | page_model = PageModel_Search new_model }, Cmd.map Msg_Search cmd)
    _ -> (model, Cmd.none)

  Msg_Edit edit_page_msg -> case model.page_model of 
    PageModel_Edit edit_page_model ->
      let
        (new_model, cmd) = Page_NewEntry.update edit_page_msg edit_page_model
      in
        ({ model | page_model = PageModel_Edit new_model }, Cmd.map Msg_Edit cmd)
    _ -> (model, Cmd.none)

view : Model -> Document Msg
view model =
  { title = 
    (case model.page_model of
      PageModel_NotFound -> Page_NotFound.title
      PageModel_NewEntry _ -> Page_NewEntry.title
      PageModel_Search _ -> Page_Search.title
      PageModel_Edit _ -> "Editar"
    ) ++ " | localhost"
  , body = 
    let 
      page_content = case model.page_model of
        PageModel_NotFound -> Page_NotFound.view
        PageModel_NewEntry new_entry_page_model -> UI.map Msg_NewEntry <| Page_NewEntry.view new_entry_page_model
        PageModel_Search search_page_model -> UI.map Msg_Search <| Page_Search.view search_page_model
        PageModel_Edit edit_page_model -> UI.map Msg_Edit <| Page_NewEntry.view edit_page_model
    in
      [ UI.layout 
          [ Background.color Config.background_color
          , UI.centerX
          , UI.centerY
          , UI.width UI.fill
          , UI.height UI.fill
          , Font.color (rgb 1 1 1)
          , UI.scrollbarY
          ]
          <| Banner.with_banners page_content
      ]
  }
