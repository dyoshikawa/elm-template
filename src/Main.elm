module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Page.Counter
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, top)
import Url.Parser.Query as Q


type Route
    = Top
    | Login
    | Articles (Maybe String)
    | Article Int
    | ArticleSettings Int


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Top top
        , map Login (s "Login")
        , map Articles (s "articles" <?> Q.string "search")
        , map Article (s "articles" </> int)
        , map ArticleSettings (s "articles" </> int </> s "settings")
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage
    | UserPage (List Repo)
    | ReoPage (List Issue)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key TopPage |> goTo (Route.parse url)



--UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)


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
            goTo (Route.parse url) model

        Loaded result ->
            ( { model
                | page =
                    case result of
                        Ok page ->
                            page

                        Err e ->
                            ErrorPage e
              }
            , Cmd.none
            )


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            ( { model | page = TopPage }, Cmd.none )

        Just (Route.User userName) ->
            ( model
            , Http.get
                { url = Url.Builder.crossOrigin "https://api.github.com" [ "users", userName, "repos" ] []
                , expect =
                    Http.expectJson (Result.map UserPage >> Loaded) reposDecoder
                }
            )

        Just (Route.Repo userName projectName) ->
            ( model
            , Http.get
                { url = Url.Builder.crossOrigin "https://api.github.com" [ "repos", userName, projectName, "issues" ] []
                , expect = Http.expectJson (Result.map RepoPage >> Loaded) issuesDecoder
                }
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Intercepter"
    , body =
        [ b [] [ text "The current URL is: " ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            , viewLink "/reviews/the-century-of-the-self"
            ]
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
