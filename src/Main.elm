module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Counter exposing (Model, Msg, init, update, view)
import Page.Top exposing (view)
import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, top)
import Url.Parser.Query as Q



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url TopPage, Cmd.none )



-- UPDATE


type Route
    = Top
    | Counter


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Top top
        , map Counter (s "counter")
        ]


urlToRoute : Url.Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url


type Page
    = NotFound
    | TopPage
    | CounterPage Page.Counter.Model


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | CounterMsg Page.Counter.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    urlToRoute url
            in
            goTo route model

        CounterMsg counterMsg ->
            case model.page of
                CounterPage counterModel ->
                    let
                        ( newCounterModel, counterCmd ) =
                            Page.Counter.update counterMsg counterModel
                    in
                    ( { model | page = CounterPage newCounterModel }, Cmd.map CounterMsg counterCmd )

                _ ->
                    ( model, Cmd.none )


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Top ->
            ( { model | page = TopPage }, Cmd.none )

        Just Counter ->
            let
                ( counterModel, counterCmd ) =
                    Page.Counter.init
            in
            ( { model | page = CounterPage counterModel }, Cmd.map CounterMsg counterCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/"
            , viewLink "/counter"
            , viewLink "/notfound"
            ]
        , case model.page of
            NotFound ->
                h1 [] [ text "Not Found" ]

            TopPage ->
                Page.Top.view

            CounterPage counterModel ->
                Page.Counter.view counterModel |> Html.map CounterMsg
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
